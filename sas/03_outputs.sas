/* ============================================================================
   03_outputs.sas
   Breast Augmentation Ratio Analysis — Formatted Output Tables
   Keck School of Medicine, USC | Casandra Serafin

   Purpose:
     Produces formatted results tables from the model estimates in
     WORK.all_param_ests and WORK.null_cov_parms (from 02_models.sas):
       1. Coefficient tables with OR, 95% CI, p-value
       2. ICC calculations from null intercept models
       3. Turning point estimates from quadratic terms
       4. R vs SAS comparison summary

   R Equivalent: R/04_outputs.R + R/05_supplemental_outputs.R
   Inputs:       WORK.all_param_ests, WORK.null_cov_parms, WORK.all_cov_parms
   ============================================================================ */


/* ── 0. ODS SETUP ────────────────────────────────────────────────────────── */

/* Output to RTF for Word-compatible tables; change to PDF if preferred */
ods rtf file="/home/your_username/breast-ratio-analysis/outputs/sas_results.rtf"
        style=Journal;

title  'Breast Augmentation Ratio Analysis — SAS Results';
title2 'Keck School of Medicine, USC | Casandra Serafin';
footnote 'Models fitted using PROC GLIMMIX METHOD=LAPLACE (SAS 9.4)';


/* ── 1. PRIMARY COEFFICIENT TABLES ─────────────────────────────────────────*/

/* Clean up parameter estimates — exponentiate to get OR, format p-values */
data param_formatted;
  set WORK.all_param_ests;

  /* Skip intercept rows and threshold rows from ordinal models */
  if lowcase(effect) in ('intercept') then delete;

  /* Exponentiate to OR scale */
  OR     = exp(estimate);
  OR_LCL = exp(lower);
  OR_UCL = exp(upper);

  /* Flag significance */
  if      probt < 0.001 then sig_flag = '***';
  else if probt < 0.01  then sig_flag = '**';
  else if probt < 0.05  then sig_flag = '*';
  else                       sig_flag = '';

  label
    OR      = 'Odds Ratio'
    OR_LCL  = 'OR 95% CI Lower'
    OR_UCL  = 'OR 95% CI Upper'
    probt   = 'p-value'
    sig_flag = 'Sig.';

  format OR OR_LCL OR_UCL 6.3
         probt     pvalue6.4;
run;


/* 1a. Unadjusted models — surgeon cohort */
title3 'Table 1. Unadjusted Binary Models: Surgeon Cohort';
proc print data=param_formatted noobs label;
  where index(model_label, 'surg_uni') > 0;
  var model_label effect OR OR_LCL OR_UCL probt sig_flag;
run;

/* 1b. Unadjusted models — lay cohort */
title3 'Table 2. Unadjusted Binary Models: Lay Cohort';
proc print data=param_formatted noobs label;
  where index(model_label, 'lay_uni') > 0;
  var model_label effect OR OR_LCL OR_UCL probt sig_flag;
run;

/* 1c. Adjusted models — both cohorts */
title3 'Table 3. Adjusted Binary Models: Both Cohorts';
proc print data=param_formatted noobs label;
  where index(model_label, '_adj_') > 0;
  var model_label effect OR OR_LCL OR_UCL probt sig_flag;
run;


/* ── 2. ICC CALCULATION FROM NULL INTERCEPT MODELS ─────────────────────── */

/* R equivalent: manual ICC computation in 05_supplemental_outputs.R
   Formula: ICC = var_RE / (var_RE + pi^2/3)
   where pi^2/3 = 3.28987 is the logistic distribution residual variance.

   PROC GLIMMIX labels random effect variance components as:
     "Variance" for each RANDOM statement subject, listed in order.
   We identify PatientID vs RaterID by the CovParm label.                  */

%let logistic_resid_var = 3.28987;  /* pi^2 / 3 */

data icc_calculations;
  set WORK.null_cov_parms;

  /* Identify which random effect this row represents */
  /* CovParm labels from PROC GLIMMIX: "Intercept PatientID" etc. */
  if index(upcase(covparm), 'PATIENTID') > 0 then RE_type = 'Image';
  else if index(upcase(covparm), 'SURVEYID') > 0 then RE_type = 'Rater';
  else delete;

  /* Parse cohort and outcome from model_label */
  if index(model_label, 'surg') > 0 then Cohort = 'Surgeon';
  else                                    Cohort = 'Lay';

  if index(model_label, '_aes') > 0 then Outcome = 'Aesthetic';
  else                                    Outcome = 'Naturalness';

  /* ICC formula */
  ICC = estimate / (estimate + &logistic_resid_var);

  label
    Cohort   = 'Rater Cohort'
    Outcome  = 'Outcome'
    RE_type  = 'ICC Type (Image/Rater)'
    estimate = 'Random Effect Variance'
    ICC      = 'ICC Estimate';

  format ICC 6.4 estimate 8.4;
  keep model_label Cohort Outcome RE_type estimate ICC;
run;

proc sort data=icc_calculations; by Cohort Outcome RE_type; run;

title3 'Table 4. ICC Estimates from Null Intercept Binary Models';
footnote2 'ICC = var_RE / (var_RE + pi^2/3). Logistic residual variance = 3.290.';
proc print data=icc_calculations noobs label; run;


/* ── 3. TURNING POINT ESTIMATES ─────────────────────────────────────────── */

/* R equivalent: turning point tables in 05_supplemental_outputs.R
   Formula: vertex_model = -beta1 / (2 * beta2)
   where beta1 = linear term, beta2 = squared term.

   Only shown for models with significant quadratic terms:
     Surgeon x Aesthetic   x Upper Proportion (p=0.006)
     Surgeon x Naturalness x Upper Proportion (p=0.011)               */

/* Extract linear and quadratic beta estimates for upper proportion models */
proc sql;
  create table turning_points as
  select
    a.model_label,
    a.estimate as beta_linear,
    b.estimate as beta_quadratic,
    (-a.estimate / (2 * b.estimate)) as vertex_model_scale,

    /* Back-transform from z-scale to raw upper_prop scale
       upper_prop = vertex_z * sd_upper + mean_upper
       These values come from 01_data_prep.sas macro variables */
    (-a.estimate / (2 * b.estimate)) * &sd_upper + &mean_upper
      as vertex_raw_scale

  from
    (select model_label, estimate
     from WORK.all_param_ests
     where effect = 'upper_prop_z'
       and index(model_label, 'upper') > 0) as a

  join
    (select model_label, estimate
     from WORK.all_param_ests
     where effect = 'upper_prop_z_sq'
       and index(model_label, 'upper') > 0) as b

  on a.model_label = b.model_label

  /* Only include statistically significant quadratic combinations */
  where a.model_label in ('surg_uni_aes_upper', 'surg_uni_nat_upper',
                           'surg_adj_aes_upper', 'surg_adj_nat_upper');
quit;

data turning_points;
  set turning_points;

  /* Observed range: upper_prop approximately 0.45 to 0.73 */
  within_range = (vertex_raw_scale >= 0.45 and vertex_raw_scale <= 0.73);
  shape = ifc(beta_quadratic < 0, 'Concave down (maximum)', 'Concave up (minimum)');

  label
    model_label       = 'Model'
    beta_linear       = 'Beta (linear term)'
    beta_quadratic    = 'Beta (quadratic term)'
    vertex_model_scale = 'Turning Point (model/z scale)'
    vertex_raw_scale  = 'Turning Point (raw upper proportion)'
    within_range      = 'Within observed range (0.45-0.73)?'
    shape             = 'Curve shape';

  format beta_linear beta_quadratic vertex_model_scale vertex_raw_scale 8.4;
run;

title3 'Table 5. Estimated Turning Points — Upper Proportion Models (Surgeon, Significant Only)';
footnote2 'Turning point = -b/(2a). Only shown for models with significant quadratic term (p<0.05).';
proc print data=turning_points noobs label; run;


/* ── 4. R vs SAS COMPARISON TABLE ──────────────────────────────────────────*/

/* This section documents expected differences between R (lme4) and
   SAS (PROC GLIMMIX) results. Hardcoded from thesis R output for reference.

   WHY DIFFERENCES OCCUR:
   - Estimation: lme4 uses adaptive Gauss-Hermite quadrature (nAGQ=1 = Laplace);
     PROC GLIMMIX METHOD=LAPLACE uses a similar but not identical algorithm
   - Convergence: lme4 uses bobyqa/Nelder_Mead; PROC GLIMMIX uses Newton-Raphson
   - Boundary estimates: different handling when variance approaches 0
   - Expected difference in OR estimates: typically < 5% for well-identified models */

title3 'Table 6. R vs SAS Estimation Comparison — Upper Proportion x Aesthetic (Surgeon)';
footnote2 'R values from lme4::glmer (nAGQ=1, Laplace). Differences expected due to algorithmic variation.';

data comparison;
  infile datalines delimiter='|';
  length Parameter $40 R_Estimate $12 R_OR $10 SAS_Note $60;
  input Parameter $ R_Estimate $ R_OR $ SAS_Note $;
  datalines;
(Intercept)              | 0.954  | 2.60  | Compare with SAS Intercept
upper_prop_z             | 0.048  | 1.05  | Compare with SAS upper_prop_z
upper_prop_z_sq          | -0.329 | 0.72  | KEY: sig. p=0.006 — verify in SAS
Surgeon.SurveyID Var     | 1.451  | .     | Compare with SAS CovParm for RaterID
PatientID Var            | 0.530  | .     | Compare with SAS CovParm for PatientID
;
run;

proc print data=comparison noobs label; run;

ods rtf close;

title; footnote;

/* ── 5. DESCRIPTIVE SUMMARY ─────────────────────────────────────────────── */

/* Quick descriptive checks — mirrors 02_descriptives.R outputs */
title 'Descriptive Summary: Surgeon Cohort';
proc means data=WORK.surgeon_data n mean std min max;
  var AestheticScore_num NaturalScore_num
      upper_prop RATIO_Post_Op RATIO_DIFFERENCE Months_Post_Op;
run;

title 'Descriptive Summary: Lay Cohort';
proc means data=WORK.lay_data n mean std min max;
  var AestheticScore_num NaturalScore_num
      upper_prop RATIO_Post_Op RATIO_DIFFERENCE Months_Post_Op;
run;

title 'Rating Distribution: Surgeon Cohort — Aesthetic';
proc freq data=WORK.surgeon_data;
  tables Aesthetic Aesthetic_bin Aesthetic_3 / nocum;
run;

title 'Rating Distribution: Lay Cohort — Aesthetic';
proc freq data=WORK.lay_data;
  tables Aesthetic Aesthetic_bin / nocum;
run;

/* ============================================================================
   END OF 03_outputs.sas
   ============================================================================ */
