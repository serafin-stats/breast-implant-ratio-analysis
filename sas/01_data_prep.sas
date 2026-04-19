/* ============================================================================
   01_data_prep.sas
   Breast Augmentation Ratio Analysis — Data Preparation
   Keck School of Medicine, USC | Casandra Serafin

   Purpose:
     Replicates R/01_clean.R in SAS 9.4 / SAS Studio.
     Reads raw Excel survey exports and patient ratio measurements,
     merges cohort data, derives analysis variables, and outputs
     analysis-ready datasets for surgeon and lay cohorts.

   Inputs  (update paths to match your environment):
     RAW_SURG  = RAW_SURVEY_DATA_SURGEON_COHORT.xlsx
     RAW_LAY   = RAW_SURVEY_DATA_LAY_COHORT.xlsx
     RAW_RATIO = Patient_Ratios_Malluci_Measurements_data.xlsx

   Outputs:
     WORK.SURGEON_DATA   — long-format surgeon ratings with ratio covariates
     WORK.LAY_DATA       — long-format lay ratings with ratio covariates
     WORK.COMBINED_DATA  — both cohorts stacked with cohort indicator

   R Equivalent: R/01_clean.R
   SAS Version:  9.4 (tested in SAS Studio)
   Last Updated: 2025
   ============================================================================ */


/* ── 0. MACRO VARIABLES — update these paths ──────────────────────────────── */

%let raw_path  = /home/your_username/breast-ratio-analysis/data/raw;
%let out_path  = /home/your_username/breast-ratio-analysis/data/processed;

/* Set to 1 on first run to re-import raw Excel files;
   set to 0 on subsequent runs to skip slow PROC IMPORT steps */
%let reimport  = 1;


/* ── 1. IMPORT RAW DATA ───────────────────────────────────────────────────── */

/* NOTE: PROC IMPORT reads the first row as variable names (GETNAMES=YES).
   The Qualtrics exports contain a second header row of question text —
   this is deleted in the DATA step immediately after import, mirroring
   the promote_header_row() + normalise_qualtrics_codes() logic in utils.R. */

%macro import_if(&flag, &dsn, &file, &sheet);
  %if &flag = 1 %then %do;
    proc import
      datafile = "&raw_path/&file"
      out      = WORK.&dsn
      dbms     = xlsx
      replace;
      sheet    = "&sheet";
      getnames = yes;
    run;
  %end;
%mend;

%import_if(&reimport, raw_surg,  RAW_SURVEY_DATA_SURGEON_COHORT.xlsx,             Sheet1)
%import_if(&reimport, raw_lay,   RAW_SURVEY_DATA_LAY_COHORT.xlsx,                 Sheet1)
%import_if(&reimport, raw_ratio, Patient_Ratios_Malluci_Measurements_data.xlsx,   Sheet1)


/* ── 2. CLEAN RATIO / PATIENT DATA ───────────────────────────────────────── */

/* R equivalent: df_patients and implant_data creation in 01_clean.R
   Rename columns to remove spaces and special characters that SAS
   cannot handle as variable names. */

data ratio_clean;
  set WORK.raw_ratio;

  /* Drop the Qualtrics second-header row if present
     (identified by PatientID being non-numeric) */
  if notdigit(strip(PatientID)) > 0 then delete;

  /* Rename columns with spaces — SAS variable names cannot contain spaces.
     These match the column names in the Excel file; adjust if yours differ. */
  rename
    'RATIO DIFFERENCE'n = RATIO_DIFFERENCE
    'RATIO Pre Op'n     = RATIO_Pre_Op
    'RATIO Post Op'n    = RATIO_Post_Op
    'Upper Prop'n       = upper_prop;

  /* Cast PatientID to numeric for merging */
  PatientID_num = input(strip(PatientID), 8.);
  drop PatientID;
  rename PatientID_num = PatientID;
run;

/* Verify import */
proc contents data=ratio_clean varnum; run;
proc print data=ratio_clean (obs=5); run;


/* ── 3. CLEAN SURGEON SURVEY DATA ────────────────────────────────────────── */

/* R equivalent: surgeon_data creation block in 01_clean.R
   Surgeon Qualtrics export uses "1.0", "2.0" format for rating values.
   These are converted to integers below. */

data surg_clean;
  set WORK.raw_surg;

  /* Delete Qualtrics second header row */
  if _N_ = 2 then delete;

  /* Delete incomplete responses (Finished != 1) */
  if input(strip(Finished), 8.) ne 1 then delete;

  /* Convert Qualtrics .0-suffix codes to integers
     R equivalent: normalise_qualtrics_codes() in utils.R */
  array rating_vars {*} Aesthetic Naturalness;
  do i = 1 to dim(rating_vars);
    rating_vars{i} = round(input(strip(vvalue(rating_vars{i})), best.), 1);
  end;
  drop i;

  /* Cast IDs to numeric */
  PatientID_num      = input(strip(PatientID),      8.);
  SurveyID_num       = input(strip(Surgeon_SurveyID), 8.);
  drop PatientID Surgeon_SurveyID;
  rename PatientID_num  = PatientID
         SurveyID_num   = Surgeon_SurveyID;

  /* Recode text ratings to ordered numeric
     R equivalent: label_rating_factors() in utils.R
     Aesthetic:   1=Very Unattractive ... 5=Very Attractive
     Naturalness: 1=Very Unnatural    ... 5=Very Natural    */
  AestheticScore_num = Aesthetic;
  NaturalScore_num   = Naturalness;

  /* Binary outcomes: 1 if rating >= 4 (Attractive / Natural) */
  Aesthetic_bin   = (Aesthetic   >= 4);
  Naturalness_bin = (Naturalness >= 4);

  /* Collapsed 3-level aesthetic outcome for surgeon models
     R equivalent: fct_collapse(Aesthetic, Low=c(1,2), Mid=3, High=c(4,5))
     1=Low (Very Unattractive / Unattractive)
     2=Mid (Neutral)
     3=High (Attractive / Very Attractive)                              */
  if      Aesthetic in (1 2) then Aesthetic_3 = 1;  /* Low  */
  else if Aesthetic =  3     then Aesthetic_3 = 2;  /* Mid  */
  else if Aesthetic in (4 5) then Aesthetic_3 = 3;  /* High */

  /* Cohort indicator */
  Cohort = 'Surgeon';

  label
    AestheticScore_num = 'Aesthetic rating (1-5 numeric)'
    NaturalScore_num   = 'Naturalness rating (1-5 numeric)'
    Aesthetic_bin      = 'Binary: Aesthetic >= 4 (1=High)'
    Naturalness_bin    = 'Binary: Naturalness >= 4 (1=High)'
    Aesthetic_3        = 'Collapsed aesthetic: 1=Low 2=Mid 3=High'
    Cohort             = 'Rater cohort (Surgeon / Lay)';
run;


/* ── 4. CLEAN LAY SURVEY DATA ────────────────────────────────────────────── */

/* R equivalent: lay_data creation block in 01_clean.R
   Lay Qualtrics export uses integer codes (no .0 suffix). */

data lay_clean;
  set WORK.raw_lay;

  if _N_ = 2 then delete;
  if input(strip(Finished), 8.) ne 1 then delete;

  PatientID_num  = input(strip(PatientID),    8.);
  SurveyID_num   = input(strip(Lay_SurveyID), 8.);
  drop PatientID Lay_SurveyID;
  rename PatientID_num = PatientID
         SurveyID_num  = Lay_SurveyID;

  AestheticScore_num = Aesthetic;
  NaturalScore_num   = Naturalness;
  Aesthetic_bin      = (Aesthetic   >= 4);
  Naturalness_bin    = (Naturalness >= 4);

  /* Lay models use full 5-level Aesthetic (no collapse to Aesthetic_3) */

  Cohort = 'Lay';

  label
    AestheticScore_num = 'Aesthetic rating (1-5 numeric)'
    NaturalScore_num   = 'Naturalness rating (1-5 numeric)'
    Aesthetic_bin      = 'Binary: Aesthetic >= 4 (1=High)'
    Naturalness_bin    = 'Binary: Naturalness >= 4 (1=High)'
    Cohort             = 'Rater cohort (Surgeon / Lay)';
run;


/* ── 5. MERGE RATIO DATA INTO EACH COHORT ───────────────────────────────── */

/* R equivalent: left_join(ratings, ratios, by="PatientID") in 01_clean.R */

proc sort data=surg_clean;  by PatientID; run;
proc sort data=lay_clean;   by PatientID; run;
proc sort data=ratio_clean; by PatientID; run;

data surgeon_merged;
  merge surg_clean (in=a) ratio_clean (in=b);
  by PatientID;
  if a;  /* keep all surgeon ratings; ratio data should match all 40 patients */
run;

data lay_merged;
  merge lay_clean (in=a) ratio_clean (in=b);
  by PatientID;
  if a;
run;

/* Quick merge check — should show 0 unmatched surgeon rows */
proc sql;
  select count(*) as n_unmatched_surg
  from surgeon_merged
  where RATIO_Post_Op is missing;

  select count(*) as n_unmatched_lay
  from lay_merged
  where RATIO_Post_Op is missing;
quit;


/* ── 6. DERIVE ANALYSIS VARIABLES ───────────────────────────────────────── */

/* R equivalent: compute_ratio_features() + add_group_centred_covariates()
   in utils.R, plus the mutate() blocks in 01_clean.R.

   IMPORTANT NOTE ON CENTERING:
   In R, centering is done on the FULL pooled dataset (both patients × all raters).
   In SAS, we compute the mean/SD from the patient-level ratios first (n=40),
   then apply to the long-format rating data.
   This produces identical values since the ratio columns are patient-level
   constants repeated across raters.                                         */

/* 6a. Compute patient-level means and SDs for standardisation */
proc means data=ratio_clean noprint;
  var RATIO_Post_Op RATIO_Pre_Op RATIO_DIFFERENCE upper_prop
      Months_Post_Op Implant_size_scaled;
  output out=ratio_stats
    mean = mean_postop mean_preop mean_rdiff mean_upper mean_months mean_implant
    std  = sd_postop   sd_preop   sd_rdiff   sd_upper   sd_months   sd_implant;
run;

/* Extract scalar macro variables for use in DATA step */
data _null_;
  set ratio_stats;
  call symputx('mean_postop',  mean_postop);
  call symputx('sd_postop',    sd_postop);
  call symputx('mean_preop',   mean_preop);
  call symputx('sd_preop',     sd_preop);
  call symputx('mean_rdiff',   mean_rdiff);
  call symputx('sd_rdiff',     sd_rdiff);
  call symputx('mean_upper',   mean_upper);
  call symputx('sd_upper',     sd_upper);
  call symputx('mean_months',  mean_months);
run;


/* 6b. Derive all ratio and predictor variables for SURGEON cohort */
data WORK.surgeon_data;
  set surgeon_merged;

  /* Post-Op ratio: centred (percentage points) and z-standardised
     R equivalent: RATIO_Post_Op_pct_centered, RATIO_Post_Op_pct_z          */
  RATIO_Post_Op_pct_centered    = (RATIO_Post_Op - &mean_postop) * 100;
  RATIO_Post_Op_pct_centered_sq = RATIO_Post_Op_pct_centered ** 2;
  RATIO_Post_Op_pct_z           = RATIO_Post_Op_pct_centered / (&sd_postop * 100);
  RATIO_Post_Op_pct_z_sq        = RATIO_Post_Op_pct_z ** 2;

  /* Pre-Op ratio */
  RATIO_Pre_Op_pct_centered     = (RATIO_Pre_Op - &mean_preop) * 100;
  RATIO_Pre_Op_pct_centered_sq  = RATIO_Pre_Op_pct_centered ** 2;

  /* Ratio difference */
  RATIO_DIFF_pct                = RATIO_DIFFERENCE * 100;
  RATIO_DIFF_pct_centered       = RATIO_DIFF_pct - &mean_rdiff;
  RATIO_DIFF_pct_centered_sq    = RATIO_DIFF_pct_centered ** 2;
  RATIO_DIFF_pct_z              = RATIO_DIFF_pct_centered / (&sd_rdiff);
  RATIO_DIFF_pct_z_sq           = RATIO_DIFF_pct_z ** 2;

  /* Upper proportion: z-standardised
     R equivalent: upper_prop_z, upper_prop_z_sq                             */
  upper_prop_z                  = (upper_prop - &mean_upper) / &sd_upper;
  upper_prop_z_sq               = upper_prop_z ** 2;

  /* Months post-op: centred
     Surgeon models use "Months.Post.Op" (not .x suffix)                     */
  Months_Post_Op_centered       = Months_Post_Op - &mean_months;

  label
    RATIO_Post_Op_pct_centered = 'Post-Op ratio centred (pct pts)'
    RATIO_Post_Op_pct_z        = 'Post-Op ratio z-standardised'
    RATIO_DIFF_pct_centered    = 'Ratio difference centred (pct pts)'
    RATIO_DIFF_pct_z           = 'Ratio difference z-standardised'
    upper_prop_z               = 'Upper proportion z-standardised'
    Months_Post_Op_centered    = 'Months post-op mean-centred';
run;


/* 6c. Same derivations for LAY cohort */
data WORK.lay_data;
  set lay_merged;

  RATIO_Post_Op_pct_centered    = (RATIO_Post_Op - &mean_postop) * 100;
  RATIO_Post_Op_pct_centered_sq = RATIO_Post_Op_pct_centered ** 2;
  RATIO_Post_Op_pct_z           = RATIO_Post_Op_pct_centered / (&sd_postop * 100);
  RATIO_Post_Op_pct_z_sq        = RATIO_Post_Op_pct_z ** 2;

  RATIO_Pre_Op_pct_centered     = (RATIO_Pre_Op - &mean_preop) * 100;
  RATIO_Pre_Op_pct_centered_sq  = RATIO_Pre_Op_pct_centered ** 2;

  RATIO_DIFF_pct                = RATIO_DIFFERENCE * 100;
  RATIO_DIFF_pct_centered       = RATIO_DIFF_pct - &mean_rdiff;
  RATIO_DIFF_pct_centered_sq    = RATIO_DIFF_pct_centered ** 2;
  RATIO_DIFF_pct_z              = RATIO_DIFF_pct_centered / (&sd_rdiff);
  RATIO_DIFF_pct_z_sq           = RATIO_DIFF_pct_z ** 2;

  upper_prop_z                  = (upper_prop - &mean_upper) / &sd_upper;
  upper_prop_z_sq               = upper_prop_z ** 2;

  Months_Post_Op_centered       = Months_Post_Op - &mean_months;

  label
    RATIO_Post_Op_pct_centered = 'Post-Op ratio centred (pct pts)'
    RATIO_Post_Op_pct_z        = 'Post-Op ratio z-standardised'
    RATIO_DIFF_pct_centered    = 'Ratio difference centred (pct pts)'
    RATIO_DIFF_pct_z           = 'Ratio difference z-standardised'
    upper_prop_z               = 'Upper proportion z-standardised'
    Months_Post_Op_centered    = 'Months post-op mean-centred';
run;


/* 6d. Stack both cohorts — mirrors combined_data in R */
data WORK.combined_data;
  set WORK.surgeon_data (in=s)
      WORK.lay_data     (in=l);
  if s then Cohort = 'Surgeon';
  if l then Cohort = 'Lay';

  /* Unified rater ID regardless of cohort */
  if s then Rater_ID = Surgeon_SurveyID;
  if l then Rater_ID = Lay_SurveyID;

  label Cohort   = 'Rater cohort (Surgeon / Lay)'
        Rater_ID = 'Unified rater ID across cohorts';
run;


/* ── 7. DOCUMENTATION ────────────────────────────────────────────────────── */

proc contents data=WORK.surgeon_data  varnum; title 'Surgeon Data — Variable List'; run;
proc contents data=WORK.lay_data      varnum; title 'Lay Data — Variable List';     run;
proc contents data=WORK.combined_data varnum; title 'Combined Data — Variable List'; run;

/* Row counts — expected: surgeon ~1688 rows, lay ~7988 rows */
proc sql;
  select 'surgeon_data' as dataset, count(*) as n_rows,
         count(distinct PatientID) as n_patients,
         count(distinct Surgeon_SurveyID) as n_raters
  from WORK.surgeon_data
  union all
  select 'lay_data', count(*), count(distinct PatientID),
         count(distinct Lay_SurveyID)
  from WORK.lay_data;
quit;


/* ── 8. SAVE PERMANENT DATASETS (optional) ───────────────────────────────── */

/* Uncomment and update libname path to save processed datasets permanently */
/*
libname processed "&out_path";
data processed.surgeon_data; set WORK.surgeon_data; run;
data processed.lay_data;     set WORK.lay_data;     run;
data processed.combined_data; set WORK.combined_data; run;
*/

/* ============================================================================
   END OF 01_data_prep.sas
   Run 02_models.sas next — assumes WORK.surgeon_data and WORK.lay_data exist.
   ============================================================================ */
