# =============================================================================
# utils.R
# Shared helper functions for the breast augmentation ratio analysis pipeline.
#
# Source this file at the top of every pipeline script:
#   source("R/utils.R")
#
# Note:
#   Models are defined declaratively in a model_grid tibble (see 03_models.R).
#   fit_model_from_config() dispatches each row to the correct fitting function.
#   Adding a new model = adding one row to the grid, not writing new code.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(readxl)
  library(writexl)
  library(ggplot2)
  library(gt)
  library(qs2)
  library(purrr)
  library(tibble)
  library(glue)
})

# -----------------------------------------------------------------------------
# DATA INGESTION
# -----------------------------------------------------------------------------

#' Remove administrative columns from a raw Qualtrics survey export
#'
#' @param path Character. Relative path to the raw .xlsx file.
#' @param new_path Character. Where the trimmed file will be saved.
#' @param extra_drop Character vector of additional column names to drop.
#' @return Invisibly returns the trimmed data frame.
trim_survey <- function(path, new_path, extra_drop = character(0)) {
  standard_drop <- c(
    "StartDate", "EndDate", "Status", "RecordedDate",
    "RecipientLastName", "RecipientFirstName", "RecipientEmail",
    "ExternalReference", "LocationLatitude", "LocationLongitude",
    "DistributionChannel", "UserLanguage", "IPAddress", "Response.ID",
    "Q2_5_TEXT", "Q208_4_TEXT", "Q225_4_TEXT"
  )
  data <- read_excel(path) %>%
    select(-any_of(union(standard_drop, extra_drop)))
  message("Trimmed: ", nrow(data), " rows x ", ncol(data), " cols -> ", new_path)
  write_xlsx(data, new_path, col_names = TRUE)
  invisible(data)
}

#' Promote the first data row to column names and append a UniqueID integer
#'
#' @param data Data frame whose first row holds the true column labels
#'   (common in raw Qualtrics exports).
#' @return Data frame with corrected, unique column names and UniqueID appended.
promote_header_row <- function(data) {
  colnames(data) <- make.names(as.character(unlist(data[1, ])), unique = TRUE)
  data[-1, ] %>% mutate(UniqueID = row_number())
}

#' Normalise Qualtrics numeric response codes to integer strings
#'
#' Converts "1.0", "2.0" etc. to "1", "2" so that label_rating_factors()
#' @param data A data frame.
#' @param cols Character vector of column names to normalise.
#'   Defaults to all character and factor columns.
#' @return Data frame with normalised response codes.
normalise_qualtrics_codes <- function(data, cols = NULL) {
  if (is.null(cols)) {
    cols <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
  }
  data %>%
    mutate(across(all_of(cols), ~ str_replace_all(as.character(.), "\\.0$", "")))
}

# -----------------------------------------------------------------------------
# WIDE -> LONG RESHAPING
# -----------------------------------------------------------------------------

#' Pivot a Qualtrics image-rating survey from wide to long format
#'
#' @param wide_data Wide data frame with columns matching X<PatientID>.<Attribute>.
#' @param rating_names Length-4 character vector naming the four attributes.
#'   Defaults to c("Aesthetic", "Naturalness", "Shape", "Position").
#' @return Long data frame: one row per respondent-image pair.
pivot_ratings_long <- function(wide_data,
                               rating_names = c("Aesthetic", "Naturalness",
                                                "Shape", "Position")) {
  wide_data %>%
    pivot_longer(
      cols          = matches("X\\d+\\.\\d+"),
      names_to      = c("PatientID", "Attribute"),
      names_pattern = "X(\\d+)\\.(\\d+)",
      values_to     = "Response"
    ) %>%
    pivot_wider(
      names_from   = Attribute,
      values_from  = Response,
      names_prefix = "Attribute_"
    ) %>%
    rename(
      !!rating_names[1] := Attribute_1,
      !!rating_names[2] := Attribute_2,
      !!rating_names[3] := Attribute_3,
      !!rating_names[4] := Attribute_4
    ) %>%
    mutate(PatientID = as.numeric(PatientID))
}


# -----------------------------------------------------------------------------
# GEOGRAPHY STANDARDISATION
# -----------------------------------------------------------------------------

#' Standardize free-text country names for the lay person cohort
#' @param x Character vector of raw country strings.
#' @return Character vector: USA variants -> "USA", everything else -> "Other".
standardise_country_lay <- function(x) {
  case_when(
    str_detect(x, regex("(?i)united states|usa|u\\.s\\.?a|u\\.s\\.?|america")) ~ "USA",
    str_detect(x, regex("(?i)california|texas|new york|indiana|alabama|washington|sc|la|tx|alaska")) ~ "USA",
    TRUE ~ "Other"
  )
}

#' Standardize free-text country names for the surgeon cohort
#' @param x Character vector of raw country strings.
#' @return Character vector with common variants collapsed to canonical names.
standardise_country_surgeon <- function(x) {
  case_when(
    str_detect(x, regex("(?i)united states|usa|u.s.a|u.s|america")) ~ "USA",
    str_detect(x, regex("(?i)united kingdom|uk"))                    ~ "United Kingdom",
    str_detect(x, regex("(?i)australia|aust|ausy"))                  ~ "Australia",
    str_detect(x, regex("(?i)brasil|brazil"))                        ~ "Brazil",
    str_detect(x, regex("(?i)mexico"))                               ~ "Mexico",
    str_detect(x, regex("(?i)spain"))                                ~ "Spain",
    str_detect(x, regex("(?i)argentina"))                            ~ "Argentina",
    str_detect(x, regex("(?i)switzerland"))                          ~ "Switzerland",
    str_detect(x, regex("(?i)poland"))                               ~ "Poland",
    TRUE ~ x
  )
}

#' Assign continent from a standardized surgeon country name
#' @param country Character vector of standardized country names.
#' @return Character vector of continent labels.
assign_continent <- function(country) {
  case_when(
    country %in% c("USA", "Mexico", "Brazil", "Argentina")                         ~ "Americas",
    country %in% c("United Kingdom", "Spain", "Switzerland", "Poland", "Belgium")  ~ "Europe",
    country == "Australia"                                                           ~ "Oceania",
    country %in% c("India", "Cyprus")                                               ~ "Asia",
    country %in% c("Egypt", "Rwanda")                                               ~ "Africa",
    TRUE                                                                             ~ "Other"
  )
}


# -----------------------------------------------------------------------------
# FACTOR LABELLING  (identical coding for both cohorts)
# -----------------------------------------------------------------------------

#' Apply ordered factor labels to the four standard Likert rating columns
#'
#' Converts Qualtrics numeric string responses ("1"–"5") to labelled
#' ordered/unordered factors. Identical for both cohorts — call once per dataset.
#'
#' @param data Data frame with Aesthetic, Naturalness, Shape, Position columns.
#' @return Data frame with those four columns replaced by factors.
label_rating_factors <- function(data) {
  data %>% mutate(
    Aesthetic = factor(Aesthetic, ordered = TRUE,
      levels = c("1","2","3","4","5"),
      labels = c("Very Unattractive","Unattractive","Neutral","Attractive","Very Attractive")),
    Naturalness = factor(Naturalness, ordered = TRUE,
      levels = c("1","2","3","4","5"),
      labels = c("Very Unnatural","Unnatural","Neutral","Natural","Very Natural")),
    Shape = factor(Shape,
      levels = c("1","2","3"),
      labels = c("Round","Teardrop","I Don't Know")),
    Position = factor(Position,
      levels = c("1","2","3"),
      labels = c("Above the Muscle","Below the Muscle","I Don't Know"))
  )
}

#' Apply ordered factor labels to Age (shared coding, both cohorts)
#' @param data Data frame with Age column coded "1"–"5".
label_age <- function(data) {
  data %>% mutate(Age = factor(Age,
    levels = c("1","2","3","4","5"),
    labels = c("18-24","25-34","34-44","45-54","65+")))
}

#' Apply factor labels to Gender (shared coding, both cohorts)
#' @param data Data frame with Gender column coded "1"–"3".
label_gender <- function(data) {
  data %>% mutate(Gender = factor(Gender,
    levels = c("1","2","3"),
    labels = c("Male","Female","Other")))
}


# -----------------------------------------------------------------------------
# RATIO FEATURE ENGINEERING
# -----------------------------------------------------------------------------

#' Compute all centered, z-scored, and quadratic ratio features
#'
#' Adds derived columns for the three primary ratio predictors and upper_prop.
#' Called identically on surgeon_data and lay_data — no duplication.
#'
#' @param data Data frame with RATIO_Post_Op, RATIO_Pre_Op,
#'   `RATIO DIFFERENCE`, and upper_prop columns.
#' @return Input data frame with all derived ratio columns appended.
compute_ratio_features <- function(data) {
  data %>% mutate(
    RATIO_DIFFERENCE              = `RATIO DIFFERENCE`,
    RATIO_DIFF_pct                = RATIO_DIFFERENCE * 100,
    RATIO_DIFF_pct_centered       = RATIO_DIFF_pct - mean(RATIO_DIFF_pct, na.rm = TRUE),
    RATIO_DIFF_pct_z              = as.numeric(scale(RATIO_DIFF_pct)),
    RATIO_DIFF_pct_centered_sq    = RATIO_DIFF_pct_centered^2,
    RATIO_DIFF_pct_z_sq           = RATIO_DIFF_pct_z^2,
    RATIO_DIFF_pct_centered_sd    = sd(RATIO_DIFF_pct_centered, na.rm = TRUE),
    RATIO_Pre_Op_pct_centered     = (RATIO_Pre_Op - mean(RATIO_Pre_Op, na.rm = TRUE)) * 100,
    RATIO_Pre_Op_pct_z            = as.numeric(scale(RATIO_Pre_Op)),
    RATIO_Pre_Op_pct_z_sq         = RATIO_Pre_Op_pct_z^2,
    RATIO_Pre_Op_pct_centered_sq  = RATIO_Pre_Op_pct_centered^2,
    RATIO_Post_Op_pct_centered    = (RATIO_Post_Op - mean(RATIO_Post_Op, na.rm = TRUE)) * 100,
    RATIO_Post_Op_pct_z           = as.numeric(scale(RATIO_Post_Op)),
    RATIO_Post_Op_pct_z_sq        = RATIO_Post_Op_pct_z^2,
    RATIO_Post_Op_pct_centered_sq = RATIO_Post_Op_pct_centered^2,
    RATIO_Post_Op_pct_centered_sd = sd(RATIO_Post_Op_pct_centered, na.rm = TRUE),
    upper_prop_z                  = as.numeric(scale(upper_prop)),
    upper_prop_z_sq               = upper_prop_z^2,
    upper_prop_sd                 = sd(upper_prop, na.rm = TRUE),
    upper_prop_mean               = mean(upper_prop, na.rm = TRUE)
  )
}

#' Compute Mallucci-Branford pole proportion metrics
#'
#' Calculates upper/lower pole proportions relative to the nipple midpoint,
#' deviation from the 45:55 ideal, and flags patients within +/-5%.
#' Called identically on both cohort datasets and on df_patients.
#'
#' @param data Data frame with `UPL-NM` and `NM-LPL` columns.
#' @return Input data frame with Mallucci metrics appended.
make_mallucci <- function(data) {
  data %>% mutate(
    total_vert      = `UPL-NM` + `NM-LPL`,
    upper_prop_m    = `UPL-NM` / total_vert,
    lower_prop_m    = `NM-LPL` / total_vert,
    dev_upper_45    = upper_prop_m - 0.45,
    dev_lower_55    = lower_prop_m - 0.55,
    abs_dev_ideal   = abs(dev_upper_45) + abs(dev_lower_55),
    within_ideal_5p = abs(dev_upper_45) <= 0.05 & abs(dev_lower_55) <= 0.05,
    meets_upper     = abs(upper_prop_m - 0.45) <= 0.05,
    meets_lower     = abs(lower_prop_m - 0.55) <= 0.05,
    meets_both      = factor(meets_upper & meets_lower,
                             levels = c(TRUE, FALSE),
                             labels = c("Ideal", "Not Ideal"))
  )
}


# -----------------------------------------------------------------------------
# DERIVED OUTCOME AND COVARIATE VARIABLES
# -----------------------------------------------------------------------------

#' Create binary outcomes and method-guess accuracy flag
#'
#' @param data Data frame with Aesthetic, Naturalness, Position, Method
#'   already factor-labelled.
#' @return Data frame with Aesthetic_bin, Naturalness_bin, Pleasing,
#'   Pleasing_natural, Guess.Correctly, Guess.Correctly_Num appended.
add_binary_outcomes <- function(data) {
  data %>% mutate(
    Aesthetic_bin       = if_else(Aesthetic   %in% c("Very Attractive", "Attractive"), 1L, 0L),
    Naturalness_bin     = if_else(Naturalness %in% c("Very Natural",    "Natural"),    1L, 0L),
    Pleasing            = Aesthetic_bin,
    Pleasing_natural    = Naturalness_bin,
    Guess.Correctly = factor(
      if_else(as.character(Position) == as.character(Method), "Correct", "Incorrect"),
      levels = c("Incorrect", "Correct")
    ),
    Guess.Correctly_Num = as.numeric(Guess.Correctly) - 1L
  )
}

#' Compute group-mean centred implant size and months post-op within Method groups
#'
#' @param data Data frame with Method, Implant.size_scaled, Months.Post.Op
#' @return Data frame with group-centred covariate columns appended.
add_group_centred_covariates <- function(data) {
  data %>%
    group_by(Method) %>%
    mutate(
      Method_Mean_ImplantSize     = mean(Implant.size_scaled, na.rm = TRUE),
      Method_Mean_MonthsPostOp    = mean(Months.Post.Op,      na.rm = TRUE),
      Implant_Size_GroupCentered  = Implant.size_scaled - Method_Mean_ImplantSize,
      Months_PostOp_GroupCentered = Months.Post.Op      - Method_Mean_MonthsPostOp
    ) %>%
    ungroup()
}

#' Create a three-level post-op timing grouping variable
#'
#' @param data Data frame with Months.Post.Op.x.
#' @return Data frame with MonthGroup3 ordered factor appended.
add_month_group <- function(data) {
  data %>% mutate(
    MonthGroup3 = factor(case_when(
      Months.Post.Op < 6                           ~ "3-6",
      Months.Post.Op >= 6 & Months.Post.Op < 12 ~ "6-12",
      Months.Post.Op >= 12                         ~ "12+",
      TRUE                                           ~ NA_character_
    ), levels = c("3-6", "6-12", "12+"))
  )
}


# -----------------------------------------------------------------------------
# MODEL GRID
# -----------------------------------------------------------------------------

#' Build the canonical model configuration grid
#'
#' Returns a tibble where every row is one model run. This is the single
#' source of truth for 03_models.R (fitting) and 04_outputs.R (tables +
#' figures). To add a new model: add a row here. Nothing else changes.
#'
#' Key columns:
#'   cohort        "surgeon" or "lay"
#'   rater_id      name of the rater-level random effect column
#'   outcome       human-readable outcome label (Aesthetic / Naturalness)
#'   outcome_col   actual column name in the data (may be "_bin" for binary)
#'   predictor     focal predictor (linear term column name)
#'   predictor_sq  squared term column name (for quadratic)
#'   covariates    additional fixed effects string (NA = unadjusted)
#'   model_type    "ordinal" (clmm) or "binary" (glmer binomial)
#'   model_set     "unadjusted" | "adjusted" | "sensitivity"
#'   predictor_key short label for filename construction
#'   filename      output .rds filename under outputs/models/
#'
#' @return A tibble with one row per model.

build_model_grid <- function() {
  
  cohort_meta <- tribble(
    ~cohort,    ~rater_id,
    "surgeon",  "Surgeon.SurveyID",
    "lay",      "Lay.SurveyID"
  )
  
  # Primary predictors
  primary_preds <- tribble(
    ~predictor_key, ~predictor,                   ~predictor_sq,
    "upper",        "upper_prop_z",               "upper_prop_z_sq",
    "postop",       "RATIO_Post_Op_pct_centered", "RATIO_Post_Op_pct_centered_sq",
    "ratiodiff",    "RATIO_DIFF_pct_centered",    "RATIO_DIFF_pct_centered_sq"
  )
  
  # Pre-op predictor for sensitivity analyses
  preop_pred <- tribble(
    ~predictor_key, ~predictor,                  ~predictor_sq,
    "preop",        "RATIO_Pre_Op_pct_centered", "RATIO_Pre_Op_pct_centered_sq"
  )
  
  adj_cov <- "Months.Post.Op + Method"
  
  # ------------------------------------------------------------------
  # UNADJUSTED ORDINAL
  # ------------------------------------------------------------------
  # Surgeon aesthetic: Aesthetic_3 (3-level collapsed) — matches thesis
  # All others: full 5-level Aesthetic or Naturalness
  # ------------------------------------------------------------------
  
  # Surgeon aesthetic unadjusted — Aesthetic_3
  surg_uni_aes3 <- crossing(
    tibble(cohort = "surgeon", rater_id = "Surgeon.SurveyID"),
    primary_preds
  ) %>%
    mutate(
      outcome     = "Aesthetic_3",
      outcome_col = "Aesthetic_3",
      covariates  = NA_character_,
      model_type  = "ordinal",
      model_set   = "unadjusted"
    )
  
  # Surgeon naturalness unadjusted — full Naturalness
  surg_uni_nat <- crossing(
    tibble(cohort = "surgeon", rater_id = "Surgeon.SurveyID"),
    primary_preds
  ) %>%
    mutate(
      outcome     = "Naturalness",
      outcome_col = "Naturalness",
      covariates  = NA_character_,
      model_type  = "ordinal",
      model_set   = "unadjusted"
    )
  
  # Lay unadjusted — both outcomes, full 5-level
  lay_uni <- crossing(
    tibble(cohort = "lay", rater_id = "Lay.SurveyID"),
    outcome = c("Aesthetic", "Naturalness"),
    primary_preds
  ) %>%
    mutate(
      outcome_col = outcome,
      covariates  = NA_character_,
      model_type  = "ordinal",
      model_set   = "unadjusted"
    )
  
  unadj <- bind_rows(surg_uni_aes3, surg_uni_nat, lay_uni)
  
  # ------------------------------------------------------------------
  # ADJUSTED ORDINAL  (same outcome mapping as unadjusted)
  # ------------------------------------------------------------------
  
  surg_adj_aes3 <- crossing(
    tibble(cohort = "surgeon", rater_id = "Surgeon.SurveyID"),
    primary_preds
  ) %>%
    mutate(outcome = "Aesthetic_3", outcome_col = "Aesthetic_3",
           covariates = adj_cov, model_type = "ordinal", model_set = "adjusted")
  
  surg_adj_nat <- crossing(
    tibble(cohort = "surgeon", rater_id = "Surgeon.SurveyID"),
    primary_preds
  ) %>%
    mutate(outcome = "Naturalness", outcome_col = "Naturalness",
           covariates = adj_cov, model_type = "ordinal", model_set = "adjusted")
  
  lay_adj_ord <- crossing(
    tibble(cohort = "lay", rater_id = "Lay.SurveyID"),
    outcome = c("Aesthetic", "Naturalness"),
    primary_preds
  ) %>%
    mutate(outcome_col = outcome, covariates = adj_cov,
           model_type = "ordinal", model_set = "adjusted")
  
  adj_ord <- bind_rows(surg_adj_aes3, surg_adj_nat, lay_adj_ord)
  
  # ------------------------------------------------------------------
  # ADJUSTED BINARY  (Aesthetic_bin / Naturalness_bin — both cohorts)
  # ------------------------------------------------------------------
  
  adj_bin <- crossing(cohort_meta, outcome = c("Aesthetic", "Naturalness"), primary_preds) %>%
    mutate(outcome_col = paste0(outcome, "_bin"), covariates = adj_cov,
           model_type = "binary", model_set = "adjusted")
  
  # ------------------------------------------------------------------
  # SENSITIVITY — pre-op ratios
  # ------------------------------------------------------------------
  
  sens_ord <- crossing(cohort_meta, primary_preds) %>%
    # Surgeon aesthetic still uses Aesthetic_3 in sensitivity
    mutate(
      outcome = case_when(
        cohort == "surgeon" ~ "Aesthetic_3",
        TRUE ~ "Aesthetic"
      ),
      outcome_col = outcome,
      predictor_key = preop_pred$predictor_key,
      predictor     = preop_pred$predictor,
      predictor_sq  = preop_pred$predictor_sq,
      covariates  = adj_cov,
      model_type  = "ordinal",
      model_set   = "sensitivity"
    ) %>%
    # Also Naturalness sensitivity
    bind_rows(
      crossing(cohort_meta, primary_preds) %>%
        mutate(
          outcome = "Naturalness", outcome_col = "Naturalness",
          predictor_key = preop_pred$predictor_key,
          predictor     = preop_pred$predictor,
          predictor_sq  = preop_pred$predictor_sq,
          covariates = adj_cov, model_type = "ordinal", model_set = "sensitivity"
        )
    ) %>%
    distinct()
  
  sens_bin <- crossing(cohort_meta, outcome = c("Aesthetic", "Naturalness"), primary_preds) %>%
    mutate(
      outcome_col   = paste0(outcome, "_bin"),
      predictor_key = preop_pred$predictor_key,
      predictor     = preop_pred$predictor,
      predictor_sq  = preop_pred$predictor_sq,
      covariates = adj_cov, model_type = "binary", model_set = "sensitivity"
    )
  
  # ------------------------------------------------------------------
  # COMBINE AND BUILD FILENAMES
  # ------------------------------------------------------------------
  
  bind_rows(unadj, adj_ord, adj_bin, sens_ord, sens_bin) %>%
    mutate(
      cohort_abbrev  = if_else(cohort == "surgeon", "surg", "lay"),
      outcome_abbrev = case_when(
        str_detect(outcome_col, "Aesthetic_3")    ~ "aes3",
        str_detect(outcome_col, "(?i)aesth")      ~ "aes",
        str_detect(outcome_col, "(?i)natural")    ~ "nat",
        TRUE                                      ~ outcome_col
      ),
      type_prefix = case_when(
        model_set  == "unadjusted" ~ "uni",
        model_type == "ordinal"    ~ "ord",
        TRUE                       ~ "bin"
      ),
      adj_suffix = if_else(model_set == "unadjusted", "", "_adj"),
      filename   = glue(
        "{cohort_abbrev}_{type_prefix}_{outcome_abbrev}_{predictor_key}{adj_suffix}.rds"
      )
    ) %>%
    dplyr::select(
      cohort, rater_id, outcome, outcome_col,
      predictor, predictor_sq, covariates,
      model_type, model_set, predictor_key,
      cohort_abbrev, outcome_abbrev, type_prefix, filename
    )
}

#' Fit a single model from a model grid row
#'
#' Constructs the full formula from grid columns and dispatches to clmm()
#' or glmer(). Wraps fitting in tryCatch so one failure does not abort
#' the entire pmap() loop.
#'
#' @param row A single-row tibble or named list from build_model_grid().
#' @param data_list Named list: list(surgeon = surgeon_data, lay = lay_data).
#' @return The fitted model object, invisibly. NULL on failure.
fit_model_from_config <- function(row, data_list) {

  suppressPackageStartupMessages({
    library(ordinal)
    library(lme4)
  })

  data <- data_list[[row$cohort]]

  # Assemble fixed effects
  fixed_parts <- c(row$predictor, row$predictor_sq)
  if (!is.na(row$covariates) && nzchar(row$covariates)) {
    fixed_parts <- c(fixed_parts, trimws(strsplit(row$covariates, "\\+")[[1]]))
  }
  fixed_str <- paste(fixed_parts, collapse = " + ")
  re_str    <- paste0("(1 | PatientID) + (1 | ", row$rater_id, ")")
  fml       <- as.formula(paste(row$outcome_col, "~", fixed_str, "+", re_str))

  message("Fitting: ", row$filename)

  model <- tryCatch(
    {
      if (row$model_type == "ordinal") {
        clmm(fml, data = data)
      } else {
        glmer(fml, data = data, family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl   = list(maxfun = 2e5)))
      }
    },
    error = function(e) {
      warning("FAILED [", row$filename, "]: ", e$message)
      NULL
    }
  )

  if (!is.null(model)) save_model(model, row$filename)
  invisible(model)
}


# -----------------------------------------------------------------------------
# OUTPUT HELPERS
# -----------------------------------------------------------------------------

#' Apply consistent gt() styling to a table
#'
#' @param gt_tbl A gt_tbl object.
#' @param source_note Optional source note string.
#' @return Styled gt_tbl.
gt_style <- function(gt_tbl, source_note = NULL) {
  out <- gt_tbl %>%
    tab_style(style = cell_text(size = "12pt", weight = "bold"),
              locations = cells_title(groups = "title")) %>%
    tab_options(heading.align = "center") %>%
    tab_style(style = cell_text(size = "11pt"),
              locations = list(cells_body(), cells_stub())) %>%
    tab_style(style = cell_text(size = "11pt", weight = "bold"),
              locations = cells_column_labels()) %>%
    tab_style(style = cell_text(size = "10pt"),
              locations = cells_source_notes()) %>%
    tab_options(data_row.padding = px(6))
  if (!is.null(source_note)) out <- out %>% tab_source_note(source_note = source_note)
  out
}

#' Save ggplot to outputs/figures/
#' @param plot ggplot object.
#' @param filename File name only (no path).
#' @param width,height Numeric inches. Defaults 12 x 5.
#' @param dpi Resolution. Default 300.
save_figure <- function(plot, filename, width = 12, height = 5, dpi = 300) {
  path <- file.path("outputs", "figures", filename)
  ggsave(path, plot = plot, width = width, height = height, units = "in", dpi = dpi)
  message("Saved: ", path)
  invisible(path)
}

#' Save gt table as PNG to outputs/tables/
#' @param gt_tbl gt_tbl object.
#' @param filename File name only (no path).
#' @param zoom Zoom factor. Default 2.
save_gt_table <- function(gt_tbl, filename, zoom = 2) {
  path <- file.path("outputs", "tables", filename)
  gt::gtsave(gt_tbl, path, zoom = zoom)
  message("Saved: ", path)
  invisible(path)
}

#' Save model RDS to outputs/models/
#' @param model Fitted model object.
#' @param filename File name only (no path).
save_model <- function(model, filename) {
  path <- file.path("outputs", "models", filename)
  saveRDS(model, path)
  message("Saved: ", path)
  invisible(path)
}

#' Load model RDS from outputs/models/
#' @param filename File name only (no path).
#' @return The loaded model object.
load_model <- function(filename) {
  readRDS(file.path("outputs", "models", filename))
}
