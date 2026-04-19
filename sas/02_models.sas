/* ============================================================================
   02_models.sas
   Breast Augmentation Ratio Analysis — Mixed Effects Models
   Keck School of Medicine, USC | Casandra Serafin

   Purpose:
     Replicates R/03_models.R using PROC GLIMMIX (SAS 9.4).
     Fits binary logistic mixed models with cross-classified random effects
     for PatientID and RaterID, mirroring the lme4::glmer() calls in R.

   R Equivalent: R/03_models.R
   Inputs:       WORK.surgeon_data, WORK.lay_data (from 01_data_prep.sas)

   ESTIMATION NOTE — IMPORTANT:
   -----------------------------------------------------------------------
   R (lme4): glmer() uses Laplace approximation by default (nAGQ=1).
   SAS:      PROC GLIMMIX uses Pseudo-likelihood (RSPL) by default.

   To maximise comparability with lme4, all models below specify:
     METHOD = LAPLACE
   This requests the Laplace approximation in PROC GLIMMIX, which is
   the closest equivalent to lme4's default. Results will be similar
   but not identical due to:
     - Numerical integration differences
     - Convergence tolerance differences
     - Treatment of boundary variance estimates

   See 03_outputs.sas for a side-by-side coefficient comparison table.
   Reference: Bolker et al. (2009) TREE; SAS GLIMMIX documentation §4.
   -----------------------------------------------------------------------

   CROSS-CLASSIFIED RANDOM EFFECTS NOTE:
   -----------------------------------------------------------------------
   lme4 syntax:  (1 | PatientID) + (1 | RaterID)
   PROC GLIMMIX: RANDOM intercept / subject=PatientID;
                 RANDOM intercept / subject=RaterID;
   Two separate RANDOM statements = cross-classified (not nested).
   Using a single RANDOM statement with interaction would specify nesting.
   -----------------------------------------------------------------------

   ============================================================================ */


/* ── 0. ODS OUTPUT DESTINATION ───────────────────────────────────────────── */
/* Capture key output to a results dataset for 03_outputs.sas */

ods output ParameterEstimates = WORK.all_param_ests (persist=run);
ods output CovParms            = WORK.all_cov_parms  (persist=run);


/* ── 1. MACRO: FIT ONE BINARY GLIMMIX MODEL ─────────────────────────────── */

/* Macro parameters:
   &dsn       = input dataset (surgeon_data or lay_data)
   &outcome   = binary outcome variable (Aesthetic_bin or Naturalness_bin)
   &predictor = linear term variable name
   &pred_sq   = squared term variable name
   &rater_id  = rater random effect variable (Surgeon_SurveyID or Lay_SurveyID)
   &covars    = additional fixed-effect covariates (blank for unadjusted)
   &model_lbl = short label for ODS output identification               */

%macro fit_binary_model(dsn=, outcome=, predictor=, pred_sq=,
                        rater_id=, covars=, model_lbl=);

  %put === Fitting model: &model_lbl ===;

  proc glimmix data=WORK.&dsn method=laplace;
    class PatientID &rater_id Method;
    model &outcome (event='1') = &predictor &pred_sq &covars
          / dist=binary link=logit solution cl oddsratio;
    random intercept / subject=PatientID  type=vc;
    random intercept / subject=&rater_id  type=vc;
    /* nloptions maxiter=200 technique=newrap; */  /* uncomment if convergence issues */

    /* Add model label to ODS output for downstream identification */
    ods output
      ParameterEstimates = _pe_&model_lbl
      CovParms           = _cp_&model_lbl
      OddsRatios         = _or_&model_lbl;
  run;

  /* Append to master results datasets */
  data _pe_&model_lbl; set _pe_&model_lbl; model_label = "&model_lbl"; run;
  data _cp_&model_lbl; set _cp_&model_lbl; model_label = "&model_lbl"; run;

  proc append base=WORK.all_param_ests data=_pe_&model_lbl force; run;
  proc append base=WORK.all_cov_parms  data=_cp_&model_lbl force; run;

%mend fit_binary_model;


/* ── 2. UNADJUSTED BINARY MODELS (primary PP curve models) ──────────────── */

/* ---- 2a. Surgeon Cohort — Aesthetic_bin ---------------------------------- */

/* R: surg_binary_uni_aes_upper
   glmer(Aesthetic_bin ~ upper_prop_z + upper_prop_z_sq +
         (1|PatientID) + (1|Surgeon.SurveyID), family=binomial)            */
%fit_binary_model(
  dsn       = surgeon_data,
  outcome   = Aesthetic_bin,
  predictor = upper_prop_z,
  pred_sq   = upper_prop_z_sq,
  rater_id  = Surgeon_SurveyID,
  covars    = ,
  model_lbl = surg_uni_aes_upper
)

/* R: surg_binary_uni_aes_postop */
%fit_binary_model(
  dsn       = surgeon_data,
  outcome   = Aesthetic_bin,
  predictor = RATIO_Post_Op_pct_centered,
  pred_sq   = RATIO_Post_Op_pct_centered_sq,
  rater_id  = Surgeon_SurveyID,
  covars    = ,
  model_lbl = surg_uni_aes_postop
)

/* R: surg_binary_uni_aes_ratiodiff */
%fit_binary_model(
  dsn       = surgeon_data,
  outcome   = Aesthetic_bin,
  predictor = RATIO_DIFF_pct_centered,
  pred_sq   = RATIO_DIFF_pct_centered_sq,
  rater_id  = Surgeon_SurveyID,
  covars    = ,
  model_lbl = surg_uni_aes_ratiodiff
)


/* ---- 2b. Surgeon Cohort — Naturalness_bin -------------------------------- */

%fit_binary_model(
  dsn       = surgeon_data,
  outcome   = Naturalness_bin,
  predictor = upper_prop_z,
  pred_sq   = upper_prop_z_sq,
  rater_id  = Surgeon_SurveyID,
  covars    = ,
  model_lbl = surg_uni_nat_upper
)

%fit_binary_model(
  dsn       = surgeon_data,
  outcome   = Naturalness_bin,
  predictor = RATIO_Post_Op_pct_centered,
  pred_sq   = RATIO_Post_Op_pct_centered_sq,
  rater_id  = Surgeon_SurveyID,
  covars    = ,
  model_lbl = surg_uni_nat_postop
)

%fit_binary_model(
  dsn       = surgeon_data,
  outcome   = Naturalness_bin,
  predictor = RATIO_DIFF_pct_centered,
  pred_sq   = RATIO_DIFF_pct_centered_sq,
  rater_id  = Surgeon_SurveyID,
  covars    = ,
  model_lbl = surg_uni_nat_ratiodiff
)


/* ---- 2c. Lay Cohort — Aesthetic_bin -------------------------------------- */

/* NOTE: Lay models use z-standardised predictors (_z suffix) rather than
   centred-only (_centered suffix) to stabilise convergence — this matches
   the original lme4 model specifications in the R QMD analysis.           */

/* R: lay_binary_uni_aes_upper */
%fit_binary_model(
  dsn       = lay_data,
  outcome   = Aesthetic_bin,
  predictor = upper_prop_z,
  pred_sq   = upper_prop_z_sq,
  rater_id  = Lay_SurveyID,
  covars    = ,
  model_lbl = lay_uni_aes_upper
)

/* R: lay_binary_uni_aes_postop
   Uses RATIO_Post_Op_pct_z (z-standardised) unlike surgeon model         */
%fit_binary_model(
  dsn       = lay_data,
  outcome   = Aesthetic_bin,
  predictor = RATIO_Post_Op_pct_z,
  pred_sq   = RATIO_Post_Op_pct_z_sq,
  rater_id  = Lay_SurveyID,
  covars    = ,
  model_lbl = lay_uni_aes_postop
)

/* R: lay_binary_uni_aes_ratiodiff */
%fit_binary_model(
  dsn       = lay_data,
  outcome   = Aesthetic_bin,
  predictor = RATIO_DIFF_pct_z,
  pred_sq   = RATIO_DIFF_pct_z_sq,
  rater_id  = Lay_SurveyID,
  covars    = ,
  model_lbl = lay_uni_aes_ratiodiff
)


/* ---- 2d. Lay Cohort — Naturalness_bin ------------------------------------ */

%fit_binary_model(
  dsn       = lay_data,
  outcome   = Naturalness_bin,
  predictor = upper_prop_z,
  pred_sq   = upper_prop_z_sq,
  rater_id  = Lay_SurveyID,
  covars    = ,
  model_lbl = lay_uni_nat_upper
)

%fit_binary_model(
  dsn       = lay_data,
  outcome   = Naturalness_bin,
  predictor = RATIO_Post_Op_pct_z,
  pred_sq   = RATIO_Post_Op_pct_z_sq,
  rater_id  = Lay_SurveyID,
  covars    = ,
  model_lbl = lay_uni_nat_postop
)

%fit_binary_model(
  dsn       = lay_data,
  outcome   = Naturalness_bin,
  predictor = RATIO_DIFF_pct_z,
  pred_sq   = RATIO_DIFF_pct_z_sq,
  rater_id  = Lay_SurveyID,
  covars    = ,
  model_lbl = lay_uni_nat_ratiodiff
)


/* ── 3. ADJUSTED BINARY MODELS (Months.Post.Op + Method as covariates) ───── */

/* R equivalent: surg_bin_aes_upper_adj etc. — glmer with Months.Post.Op + Method
   METHOD covariate is a 2-level factor: Above the Muscle / Below the Muscle
   Reference level: Above the Muscle (first alphabetically)
   To match R's reference (Below the Muscle), use PARAM=REF option or
   reorder levels using a format.                                           */

/* Define format to set reference level for Method */
proc format;
  value $methfmt
    'Above the Muscle' = 'Above the Muscle'
    'Below the Muscle' = 'Below the Muscle';
run;

%macro fit_adj_model(dsn=, outcome=, predictor=, pred_sq=,
                     rater_id=, model_lbl=);

  %put === Fitting adjusted model: &model_lbl ===;

  proc glimmix data=WORK.&dsn method=laplace;
    class PatientID &rater_id Method (ref='Above the Muscle');
    model &outcome (event='1') = &predictor &pred_sq Months_Post_Op Method
          / dist=binary link=logit solution cl oddsratio;
    random intercept / subject=PatientID  type=vc;
    random intercept / subject=&rater_id  type=vc;
    ods output
      ParameterEstimates = _pe_&model_lbl
      CovParms           = _cp_&model_lbl;
  run;

  data _pe_&model_lbl; set _pe_&model_lbl; model_label = "&model_lbl"; run;
  data _cp_&model_lbl; set _cp_&model_lbl; model_label = "&model_lbl"; run;
  proc append base=WORK.all_param_ests data=_pe_&model_lbl force; run;
  proc append base=WORK.all_cov_parms  data=_cp_&model_lbl force; run;

%mend fit_adj_model;


/* Surgeon adjusted models */
%fit_adj_model(dsn=surgeon_data, outcome=Aesthetic_bin,
  predictor=upper_prop_z, pred_sq=upper_prop_z_sq,
  rater_id=Surgeon_SurveyID, model_lbl=surg_adj_aes_upper)

%fit_adj_model(dsn=surgeon_data, outcome=Aesthetic_bin,
  predictor=RATIO_Post_Op_pct_centered, pred_sq=RATIO_Post_Op_pct_centered_sq,
  rater_id=Surgeon_SurveyID, model_lbl=surg_adj_aes_postop)

%fit_adj_model(dsn=surgeon_data, outcome=Aesthetic_bin,
  predictor=RATIO_DIFF_pct_centered, pred_sq=RATIO_DIFF_pct_centered_sq,
  rater_id=Surgeon_SurveyID, model_lbl=surg_adj_aes_ratiodiff)

%fit_adj_model(dsn=surgeon_data, outcome=Naturalness_bin,
  predictor=upper_prop_z, pred_sq=upper_prop_z_sq,
  rater_id=Surgeon_SurveyID, model_lbl=surg_adj_nat_upper)

%fit_adj_model(dsn=surgeon_data, outcome=Naturalness_bin,
  predictor=RATIO_Post_Op_pct_centered, pred_sq=RATIO_Post_Op_pct_centered_sq,
  rater_id=Surgeon_SurveyID, model_lbl=surg_adj_nat_postop)

%fit_adj_model(dsn=surgeon_data, outcome=Naturalness_bin,
  predictor=RATIO_DIFF_pct_centered, pred_sq=RATIO_DIFF_pct_centered_sq,
  rater_id=Surgeon_SurveyID, model_lbl=surg_adj_nat_ratiodiff)

/* Lay adjusted models */
%fit_adj_model(dsn=lay_data, outcome=Aesthetic_bin,
  predictor=upper_prop_z, pred_sq=upper_prop_z_sq,
  rater_id=Lay_SurveyID, model_lbl=lay_adj_aes_upper)

%fit_adj_model(dsn=lay_data, outcome=Aesthetic_bin,
  predictor=RATIO_Post_Op_pct_z, pred_sq=RATIO_Post_Op_pct_z_sq,
  rater_id=Lay_SurveyID, model_lbl=lay_adj_aes_postop)

%fit_adj_model(dsn=lay_data, outcome=Aesthetic_bin,
  predictor=RATIO_DIFF_pct_z, pred_sq=RATIO_DIFF_pct_z_sq,
  rater_id=Lay_SurveyID, model_lbl=lay_adj_aes_ratiodiff)

%fit_adj_model(dsn=lay_data, outcome=Naturalness_bin,
  predictor=upper_prop_z, pred_sq=upper_prop_z_sq,
  rater_id=Lay_SurveyID, model_lbl=lay_adj_nat_upper)

%fit_adj_model(dsn=lay_data, outcome=Naturalness_bin,
  predictor=RATIO_Post_Op_pct_z, pred_sq=RATIO_Post_Op_pct_z_sq,
  rater_id=Lay_SurveyID, model_lbl=lay_adj_nat_postop)

%fit_adj_model(dsn=lay_data, outcome=Naturalness_bin,
  predictor=RATIO_DIFF_pct_z, pred_sq=RATIO_DIFF_pct_z_sq,
  rater_id=Lay_SurveyID, model_lbl=lay_adj_nat_ratiodiff)


/* ── 4. NULL INTERCEPT MODELS FOR ICC CALCULATION ───────────────────────── */

/* R equivalent: power_surg_bin_aes_model etc. — glmer with intercept only.
   ICC = variance_rater / (variance_rater + pi^2/3)
   where pi^2/3 is the logistic residual variance.
   Computed in 03_outputs.sas from the COVPARMS estimates here.            */

%macro fit_null_model(dsn=, outcome=, rater_id=, model_lbl=);

  %put === Fitting null model: &model_lbl ===;

  proc glimmix data=WORK.&dsn method=laplace;
    class PatientID &rater_id;
    model &outcome (event='1') = / dist=binary link=logit solution;
    random intercept / subject=PatientID  type=vc;
    random intercept / subject=&rater_id  type=vc;
    ods output CovParms = _cp_null_&model_lbl;
  run;

  data _cp_null_&model_lbl;
    set _cp_null_&model_lbl;
    model_label = "&model_lbl";
  run;

  proc append base=WORK.null_cov_parms data=_cp_null_&model_lbl force; run;

%mend fit_null_model;

/* Initialise null cov parms collector */
data WORK.null_cov_parms; length model_label $40; stop; run;

%fit_null_model(dsn=surgeon_data, outcome=Aesthetic_bin,
  rater_id=Surgeon_SurveyID, model_lbl=surg_null_aes)
%fit_null_model(dsn=surgeon_data, outcome=Naturalness_bin,
  rater_id=Surgeon_SurveyID, model_lbl=surg_null_nat)
%fit_null_model(dsn=lay_data,     outcome=Aesthetic_bin,
  rater_id=Lay_SurveyID,     model_lbl=lay_null_aes)
%fit_null_model(dsn=lay_data,     outcome=Naturalness_bin,
  rater_id=Lay_SurveyID,     model_lbl=lay_null_nat)


/* ── 5. VERIFY OUTPUT DATASETS ───────────────────────────────────────────── */

proc sql;
  select model_label, count(*) as n_rows
  from WORK.all_param_ests
  group by model_label
  order by model_label;
quit;

proc print data=WORK.null_cov_parms; run;

/* ============================================================================
   END OF 02_models.sas
   Run 03_outputs.sas next for formatted tables and ICC calculations.
   ============================================================================ */
