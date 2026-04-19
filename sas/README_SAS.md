# SAS Reproduction — Breast Augmentation Ratio Analysis

## Overview

This folder contains a full SAS 9.4 reproduction of the R analysis pipeline
(`R/01_clean.R`, `R/03_models.R`, `R/04_outputs.R`). The SAS code replicates
the data preparation, binary mixed-effects models, ICC calculation, and turning
point estimation using SAS procedures that are standard in pharmaceutical and
clinical research environments.

---

## Files

| File | R Equivalent | Purpose |
|---|---|---|
| `01_data_prep.sas` | `R/01_clean.R` | Import, merge, derive analysis variables |
| `02_models.sas` | `R/03_models.R` | Fit binary cross-classified mixed models |
| `03_outputs.sas` | `R/04_outputs.R` + `R/05_supplemental_outputs.R` | Formatted tables, ICC, turning points |

---

## How to Run

### In SAS Studio (browser-based, SAS 9.4)

1. Upload the project folder or map a library to your data directory
2. Open each `.sas` file in order
3. Update the `%let raw_path` and `%let out_path` macro variables at the
   top of `01_data_prep.sas` to match your folder paths
4. Run files sequentially — each depends on WORK datasets from the previous:

```
01_data_prep.sas  →  02_models.sas  →  03_outputs.sas
```

### Expected Runtime

| File | Approximate runtime |
|---|---|
| `01_data_prep.sas` | < 1 minute |
| `02_models.sas` | 5–15 minutes (24 PROC GLIMMIX calls) |
| `03_outputs.sas` | < 1 minute |

Runtime for `02_models.sas` varies with dataset size. The lay cohort models
(~7,988 rows) take longer than surgeon models (~1,688 rows).

---

## SAS vs R: Estimation Differences

### The Core Difference

| Aspect | R (lme4) | SAS (PROC GLIMMIX) |
|---|---|---|
| Default method | Laplace (nAGQ=1) | Pseudo-likelihood (RSPL) |
| Method used here | `glmer(..., nAGQ=1)` | `METHOD=LAPLACE` |
| Optimizer | bobyqa / Nelder-Mead | Newton-Raphson |
| Convergence tolerance | 1e-4 gradient | 1e-8 relative |

All models in `02_models.sas` specify `METHOD=LAPLACE` to match lme4's
default as closely as possible.

### Expected Magnitude of Differences

For well-identified models with moderate sample sizes, differences between
lme4 and PROC GLIMMIX Laplace estimates are typically:

- **Fixed-effect OR estimates**: < 5% relative difference
- **Random effect variances**: < 10% relative difference
- **P-values**: generally agree on significance direction

Larger differences may occur when:
- Random effect variance estimates approach the boundary (≈ 0)
- Models show convergence warnings in R (common in surgeon cohort models)
- The lay cohort models, which have larger sample sizes, will generally
  agree more closely

### Convergence

Some models may show convergence warnings in either platform. This is
expected given the small number of images (n=40) relative to the number
of random effects. Cross-checking parameter estimates across both platforms
is one way to assess robustness — consistent estimates despite different
algorithms supports model stability.

---

## Key Analytical Decisions Documented in Code

### 1. Cross-classified random effects

Both platforms specify random intercepts for PatientID (image) and RaterID
as separate, non-nested effects:

```sas
/* SAS */
random intercept / subject=PatientID;
random intercept / subject=Surgeon_SurveyID;
```

```r
# R
(1 | PatientID) + (1 | Surgeon.SurveyID)
```

Using a **single** RANDOM statement with a crossed factor in SAS
(e.g., `subject=PatientID*Surgeon_SurveyID`) would specify a different
model structure and is incorrect here.

### 2. Predictor scaling: Surgeon vs Lay asymmetry

Lay cohort models use z-standardised predictors (`_z` suffix) to stabilise
convergence; surgeon models use mean-centred predictors (`_centered` suffix).
This asymmetry was present in the original R analysis and is preserved here.

### 3. Reference level for Method covariate

In adjusted models, `Method` is included as a fixed-effect covariate.
The reference level is set to `'Above the Muscle'` using:

```sas
class Method (ref='Above the Muscle');
```

This matches the R factor level ordering.

### 4. ICC calculation

ICC is computed from null intercept models using the logistic residual
variance approximation:

```
ICC = σ²_RE / (σ²_RE + π²/3)
```

where `π²/3 ≈ 3.2899` is the variance of the standard logistic distribution.
This is equivalent to the latent-variable ICC for binary outcomes and matches
the approach used in the R analysis.

---

## CDISC / ADaM Awareness

While this reproduction prioritises direct comparability with the R code
(matching variable names, dataset structure), the following conventions
from pharma/ADaM practice are relevant for translation to a regulated
submission context:

| This analysis | ADaM analog | Notes |
|---|---|---|
| `surgeon_data` / `lay_data` | `ADQS` (questionnaire) | Repeated measures per subject |
| `PatientID` | `USUBJID` | Unique subject identifier |
| `Rater_ID` | `EVALUATORID` | Evaluator/rater identifier |
| `Aesthetic_bin`, `Naturalness_bin` | Analysis flag variables (`ANL01FL`) | Binary endpoint indicators |
| `Months_Post_Op` | `ADY` / `AVISIT` | Visit-relative timing variable |
| `combined_data` | `ADQS` with `COHORT` flag | Pooled analysis dataset |

In a submission context, the data preparation steps here would translate
to SDTM mapping (raw → standardised) followed by ADaM derivation
(standardised → analysis-ready), with full audit trail documentation.

---

## Troubleshooting

**"Variable not found" errors in 02_models.sas**
→ Run 01_data_prep.sas first. WORK datasets expire when SAS Studio
sessions end — re-run 01_data_prep.sas to recreate them.

**"Convergence not achieved" in PROC GLIMMIX**
→ Uncomment the `nloptions maxiter=200 technique=newrap;` line in the
relevant model call. For lay cohort models, increasing QPOINTS may help:
add `QUAD QPOINTS=7;` as a separate statement.

**Macro variable `&sd_upper` not resolved in 03_outputs.sas**
→ These macro variables are created by `01_data_prep.sas`. Run that file
first in the same SAS session.

**ODS RTF file path errors**
→ Update the `ods rtf file=` path in `03_outputs.sas` to a folder that
exists in your SAS Studio home directory.

---

## Author

Casandra Serafin  
M.S. Biostatistics Candidate | Keck School of Medicine, USC  
[LinkedIn](https://www.linkedin.com/in/casandra-serafin/) |
[GitHub](https://github.com/serafin-stats/breast-implant-ratio-analysis)
