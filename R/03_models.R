# =============================================================================
# 03_models.R
# Fit all models via the model_grid architecture.
#
# The model grid defines every model run declaratively. Fitting is a single
# pmap() call — adding a new model means adding a row to the grid in utils.R.
#
# Inputs:
#   data/processed/surgeon_data.qs
#   data/processed/lay_data.qs
#
# Outputs:
#   outputs/models/<filename>.rds   — one file per model grid row
#   outputs/models/icc_results.rds  — ICC summary table
#   outputs/models/model_grid.rds   — the grid itself (for 04_outputs.R)
# =============================================================================

source("R/utils.R")

suppressPackageStartupMessages({
  library(qs2)
  library(ordinal)
  library(lme4)
  library(dplyr)
  library(purrr)
  library(tibble)
})

dir.create("outputs/models", showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# LOAD DATA
# =============================================================================

message("Loading processed data...")
surgeon_data <- qs_read("data/processed/surgeon_data.qs")
lay_data     <- qs_read("data/processed/lay_data.qs")

# Named list used by fit_model_from_config()
data_list <- list(surgeon = surgeon_data, lay = lay_data)


# =============================================================================
# BUILD THE MODEL GRID
# =============================================================================

model_grid <- build_model_grid()

message("Model grid: ", nrow(model_grid), " models to fit")
message("  Unadjusted ordinal: ", sum(model_grid$model_set == "unadjusted"))
message("  Adjusted ordinal:   ", sum(model_grid$model_set == "adjusted" & model_grid$model_type == "ordinal"))
message("  Adjusted binary:    ", sum(model_grid$model_set == "adjusted" & model_grid$model_type == "binary"))
message("  Sensitivity:        ", sum(model_grid$model_set == "sensitivity"))

# Save the grid so 04_outputs.R can iterate over it without rebuilding
saveRDS(model_grid, "outputs/models/model_grid.rds")


# =============================================================================
# FIT ALL MODELS
# =============================================================================
# pmap() iterates over every row of the grid.
# fit_model_from_config() handles formula construction, dispatch, and saving.
# tryCatch inside the function means one failure does not abort the loop.

message("Fitting all models...")

fitted_models <- model_grid %>%
  pmap(function(...) {
    row <- list(...)
    fit_model_from_config(row, data_list)
  })

# Tag each fitted model with its filename for downstream use
names(fitted_models) <- model_grid$filename

# Report any failures
failed <- model_grid$filename[map_lgl(fitted_models, is.null)]
if (length(failed) > 0) {
  warning("The following models failed to fit:\n",
          paste(" -", failed, collapse = "\n"))
} else {
  message("All ", nrow(model_grid), " models fitted successfully.")
}


# =============================================================================
# INTRACLASS CORRELATION COEFFICIENTS
# =============================================================================
# ICC pattern is a primary result in this analysis (see design-rationale.md §5).
# Elevated here from a diagnostic to a named output.
#
# For clmm objects, ICC = var_image / (var_image + var_rater + pi^2/3)
# where pi^2/3 is the logistic residual variance.

#' Extract image-level and rater-level ICCs from a fitted clmm model
#'
#' @param model A fitted clmm object with PatientID and rater-level random effects.
#' @return Named numeric vector: image_ICC and rater_ICC.
extract_clmm_icc <- function(model) {
  tryCatch({
    vc         <- model$ST
    var_image  <- vc[["PatientID"]][1, 1]^2
    rater_name <- setdiff(names(vc), "PatientID")[1]
    var_rater  <- vc[[rater_name]][1, 1]^2
    var_resid  <- pi^2 / 3
    total      <- var_image + var_rater + var_resid
    c(image_ICC = var_image / total, rater_ICC = var_rater / total)
  }, error = function(e) c(image_ICC = NA_real_, rater_ICC = NA_real_))
}

# Compute ICCs for all adjusted ordinal models
adj_ordinal_grid <- model_grid %>%
  filter(model_set == "adjusted", model_type == "ordinal")

icc_results <- adj_ordinal_grid %>%
  mutate(
    icc = map(filename, function(fn) {
      m <- fitted_models[[fn]]
      if (is.null(m)) return(tibble(image_ICC = NA_real_, rater_ICC = NA_real_))
      as_tibble(as.list(extract_clmm_icc(m)))
    })
  ) %>%
  tidyr::unnest(icc) %>%
  dplyr::select(cohort, outcome, predictor_key, model_type,
                image_ICC, rater_ICC, filename)

saveRDS(icc_results, "outputs/models/icc_results.rds")

message("ICC results (adjusted ordinal models):")
print(icc_results %>%
        dplyr::select(cohort, outcome, predictor_key, image_ICC, rater_ICC) %>%
        mutate(across(c(image_ICC, rater_ICC), ~ round(.x, 3))))

message("03_models.R complete.")
