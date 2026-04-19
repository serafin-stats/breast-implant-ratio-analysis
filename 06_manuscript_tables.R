# =============================================================================
# 06_manuscript_tables.R
# Breast Augmentation Ratio Analysis — Manuscript Tables
# Keck School of Medicine, USC | Casandra Serafin
#
# Generates all manuscript-ready tables as JSON data for Word document export.
# Run this script, then open outputs/manuscript_tables.docx for the final file.
#
# Tables produced:
#   Table 1  — Rater demographics by cohort
#   Table 2A — Aesthetic rating distributions by cohort
#   Table 2B — Naturalness rating distributions by cohort
#   Table 3A — Unadjusted models: Upper Proportion × Aesthetic
#   Table 3B — Unadjusted models: Upper Proportion × Naturalness
#   Table 3C — Unadjusted models: Post-Op Ratio × Aesthetic
#   Table 3D — Unadjusted models: Post-Op Ratio × Naturalness
#   Table 3E — Unadjusted models: Ratio Difference × Aesthetic
#   Table 3F — Unadjusted models: Ratio Difference × Naturalness
#   Table 4  — ICC summary
#   Table 5  — Aesthetic vs Naturalness concordance
#   Supp S1  — Estimated turning points (significant models only)
# =============================================================================

source("R/utils.R")

suppressPackageStartupMessages({
  library(qs2)
  library(dplyr)
  library(tidyr)
  library(broom.mixed)
  library(lme4)
  library(jsonlite)
  library(glue)
})

dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

message("Loading data and models...")
surgeon_data <- qs_read("data/processed/surgeon_data.qs")
lay_data     <- qs_read("data/processed/lay_data.qs")

THESIS_MODELS <- "/Users/lserafin/Documents/PM 516 Stat Consulting/Models"

load_uni <- function(fn) {
  tryCatch(readRDS(file.path(THESIS_MODELS, fn)),
           error = function(e) { warning("Could not load: ", fn); NULL })
}

# =============================================================================
# HELPER: Format model results as a clean 2-column table
# (Surgeon OR [95% CI] p | Lay OR [95% CI] p)
# =============================================================================

fmt_or <- function(estimate, conf.low, conf.high) {
  sprintf("%.2f (%.2f, %.2f)", exp(estimate), exp(conf.low), exp(conf.high))
}

fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p)   ~ "—",
    p < 0.001  ~ "< 0.001",
    TRUE       ~ sprintf("%.3f", p)
  )
}

extract_model_row <- function(model, term_label, term_name, sq_name) {
  if (is.null(model)) return(data.frame(
    Term = term_label,
    OR_CI = "Model unavailable",
    P = "—",
    stringsAsFactors = FALSE
  ))
  tidy_df <- tryCatch(
    broom.mixed::tidy(model, conf.int = TRUE, effects = "fixed"),
    error = function(e) NULL
  )
  if (is.null(tidy_df)) return(NULL)

  rows <- tidy_df %>%
    filter(term %in% c(term_name, sq_name)) %>%
    mutate(
      Term  = case_when(
        term == term_name ~ paste0(term_label, " (standardized)"),
        term == sq_name   ~ paste0(term_label, " (standardized, squared)"),
        TRUE              ~ term
      ),
      OR_CI = fmt_or(estimate, conf.low, conf.high),
      P     = fmt_p(p.value)
    ) %>%
    dplyr::select(Term, OR_CI, P)
  rows
}

build_predictor_table <- function(surg_model, lay_model,
                                  term_label, term_name, sq_name) {
  surg_rows <- extract_model_row(surg_model, term_label, term_name, sq_name)
  lay_rows  <- extract_model_row(lay_model,  term_label, term_name, sq_name)

  if (is.null(surg_rows) || is.null(lay_rows)) return(NULL)

  # Merge by Term label
  merged <- full_join(
    surg_rows %>% rename(Surg_OR_CI = OR_CI, Surg_P = P),
    lay_rows  %>% rename(Lay_OR_CI  = OR_CI, Lay_P  = P),
    by = "Term"
  )
  merged
}

# =============================================================================
# TABLE 1: Demographics
# =============================================================================

message("Building Table 1 — Demographics...")

fmt_np <- function(n, total) sprintf("%d (%.1f%%)", n, 100 * n / total)

make_demo_block <- function(data, var, label, n_total) {
  data %>%
    mutate(val = as.character(.data[[var]])) %>%
    count(val) %>%
    mutate(
      Variable = label,
      Category = val,
      N_pct    = fmt_np(n, n_total)
    ) %>%
    dplyr::select(Variable, Category, N_pct)
}

n_surg <- surgeon_data %>% distinct(Surgeon.SurveyID) %>% nrow()
n_lay  <- lay_data     %>% distinct(Lay.SurveyID)     %>% nrow()

surg_demo <- surgeon_data %>% distinct(Surgeon.SurveyID, .keep_all = TRUE)
lay_demo  <- lay_data     %>% distinct(Lay.SurveyID,     .keep_all = TRUE)

table1_surg <- bind_rows(
  make_demo_block(surg_demo, "Age",                   "Age (years)",          n_surg),
  make_demo_block(surg_demo, "Gender",                "Gender",               n_surg),
  make_demo_block(surg_demo, "Continent",             "Continent",            n_surg),
  make_demo_block(surg_demo, "Surgeon.Setting",       "Practice Setting",     n_surg),
  make_demo_block(surg_demo, "Surgeon.Length.of.Practice", "Years in Practice", n_surg)
) %>% rename(`Surgeon (n = 78)` = N_pct)

table1_lay <- bind_rows(
  make_demo_block(lay_demo, "Age",              "Age (years)",       n_lay),
  make_demo_block(lay_demo, "Gender",           "Gender",            n_lay),
  make_demo_block(lay_demo, "Country.of.Residence", "Country",       n_lay),
  make_demo_block(lay_demo, "Racial.Identity",  "Racial Identity",   n_lay)
) %>% rename(`Lay Person (n = 243)` = N_pct)

# =============================================================================
# TABLE 2: Rating Distributions
# =============================================================================

message("Building Table 2 — Rating distributions...")

aes_labels  <- c("1" = "Very Unattractive", "2" = "Unattractive",
                 "3" = "Neutral", "4" = "Attractive", "5" = "Very Attractive")
nat_labels  <- c("1" = "Very Unnatural", "2" = "Unnatural",
                 "3" = "Neutral", "4" = "Natural", "5" = "Very Natural")

make_rating_dist <- function(data, var, labels, cohort_label, n_ratings) {
  data %>%
    mutate(level = as.character(as.numeric(.data[[var]]))) %>%
    count(level) %>%
    mutate(
      Label = labels[level],
      N_pct = fmt_np(n, n_ratings)
    ) %>%
    arrange(level) %>%
    dplyr::select(Label, N_pct) %>%
    rename(!!cohort_label := N_pct)
}

n_surg_ratings <- nrow(surgeon_data)
n_lay_ratings  <- nrow(lay_data)

table2a <- full_join(
  make_rating_dist(surgeon_data, "AestheticScore_num", aes_labels,
                   paste0("Surgeon (n = ", n_surg_ratings, ")"), n_surg_ratings),
  make_rating_dist(lay_data, "AestheticScore_num", aes_labels,
                   paste0("Lay Person (n = ", n_lay_ratings, ")"), n_lay_ratings),
  by = "Label"
)

table2b <- full_join(
  make_rating_dist(surgeon_data, "NaturalScore_num", nat_labels,
                   paste0("Surgeon (n = ", n_surg_ratings, ")"), n_surg_ratings),
  make_rating_dist(lay_data, "NaturalScore_num", nat_labels,
                   paste0("Lay Person (n = ", n_lay_ratings, ")"), n_lay_ratings),
  by = "Label"
)

# =============================================================================
# TABLES 3A-3F: Model Results
# =============================================================================

message("Loading models for Tables 3A-3F...")

# Upper Proportion
surg_aes_upper <- load_uni("surg_binary_uni_aes_upper.rds")
lay_aes_upper  <- load_uni("lay_binary_uni_aes_upper.rds")
surg_nat_upper <- load_uni("surg_binary_uni_nat_upper.rds")
lay_nat_upper  <- load_uni("lay_binary_uni_nat_upper.rds")

# Post-Op Ratio
surg_aes_postop <- load_uni("surg_binary_uni_aes_postop.rds")
lay_aes_postop  <- load_uni("lay_binary_uni_aes_postop.rds")
surg_nat_postop <- load_uni("surg_binary_uni_nat_postop.rds")
lay_nat_postop  <- load_uni("lay_binary_uni_nat_postop.rds")

# Ratio Difference
surg_aes_rdiff <- load_uni("surg_binary_uni_aes_ratiodiff.rds")
lay_aes_rdiff  <- load_uni("lay_binary_uni_aes_ratiodiff.rds")
surg_nat_rdiff <- load_uni("surg_binary_uni_nat_ratiodiff.rds")
lay_nat_rdiff  <- load_uni("lay_binary_uni_nat_ratiodiff.rds")

message("Building Tables 3A-3F...")

table3a <- build_predictor_table(surg_aes_upper, lay_aes_upper,
  "Upper Proportion", "upper_prop_z", "upper_prop_z_sq")

table3b <- build_predictor_table(surg_nat_upper, lay_nat_upper,
  "Upper Proportion", "upper_prop_z", "upper_prop_z_sq")

table3c <- build_predictor_table(surg_aes_postop, lay_aes_postop,
  "Post-Op Ratio", "RATIO_Post_Op_pct_centered", "RATIO_Post_Op_pct_centered_sq")

table3d <- build_predictor_table(surg_nat_postop, lay_nat_postop,
  "Post-Op Ratio", "RATIO_Post_Op_pct_centered", "RATIO_Post_Op_pct_centered_sq")

table3e <- build_predictor_table(surg_aes_rdiff, lay_aes_rdiff,
  "Ratio Difference", "RATIO_DIFF_pct_centered", "RATIO_DIFF_pct_centered_sq")

table3f <- build_predictor_table(surg_nat_rdiff, lay_nat_rdiff,
  "Ratio Difference", "RATIO_DIFF_pct_centered", "RATIO_DIFF_pct_centered_sq")

# =============================================================================
# TABLE 4: ICC Summary
# =============================================================================

message("Building Table 4 — ICC...")

table4 <- tibble::tribble(
  ~`ICC Type`,  ~Outcome,       ~`Surgeon ICC`, ~`Lay ICC`,
  "Image-level", "Aesthetic",   "0.28",         "0.01",
  "Image-level", "Naturalness", "0.34",         "0.00",
  "Rater-level", "Aesthetic",   "0.27",         "0.53",
  "Rater-level", "Naturalness", "0.28",         "0.48"
)

# =============================================================================
# TABLE 5: Concordance
# =============================================================================

message("Building Table 5 — Concordance...")

table5 <- tibble::tribble(
  ~Cohort,      ~`Aesthetic Category`, ~`Low Naturalness`, ~`High Naturalness`,
  ~`Row Total`, ~`% High Naturalness`, ~`Odds Ratio (95% CI)`, ~`p-value`,
  "Surgeon", "Low Aesthetic (<4)",  "611", "78",  "689", "11.3%", "11.84 (9.11, 15.57)", "< 0.001",
  "Surgeon", "High Aesthetic (≥4)", "397", "602", "999", "60.3%", "",                    "",
  "Lay",     "Low Aesthetic (<4)",  "1583","1097","2680","40.9%", "3.04 (2.76, 3.34)",   "< 0.001",
  "Lay",     "High Aesthetic (≥4)", "1710","3598","5308","67.8%", "",                    ""
)

# =============================================================================
# SUPPLEMENTAL TABLE S1: Turning Points
# =============================================================================

message("Building Supplemental Table S1 — Turning Points...")

upper_prop_mean <- mean(surgeon_data$upper_prop, na.rm = TRUE)
upper_prop_sd   <- sd(surgeon_data$upper_prop,   na.rm = TRUE)

get_vertex <- function(model, linear_term, sq_term, model_to_raw) {
  if (is.null(model)) return(list(vertex = NA, in_range = NA, shape = NA))
  beta <- tryCatch(lme4::fixef(model), error = function(e) NULL)
  if (is.null(beta)) return(list(vertex = NA, in_range = NA, shape = NA))
  b1 <- tryCatch(unname(beta[linear_term]), error = function(e) NA_real_)
  b2 <- tryCatch(unname(beta[sq_term]),     error = function(e) NA_real_)
  if (is.na(b1) || is.na(b2) || b2 == 0)
    return(list(vertex = NA, in_range = NA, shape = NA))
  vm  <- -b1 / (2 * b2)
  vr  <- model_to_raw(vm)
  list(
    vertex   = round(vr, 3),
    in_range = vr >= 0.45 && vr <= 0.73,
    shape    = ifelse(b2 < 0, "Concave down (maximum)", "Concave up (minimum)")
  )
}

m2r_upper <- function(z) z * upper_prop_sd + upper_prop_mean

v_surg_aes <- get_vertex(surg_aes_upper, "upper_prop_z", "upper_prop_z_sq", m2r_upper)
v_surg_nat <- get_vertex(surg_nat_upper, "upper_prop_z", "upper_prop_z_sq", m2r_upper)

tableS1 <- tibble::tribble(
  ~`Rater Group`, ~Outcome,      ~`Curve Shape`,              ~`Turning Point`, ~`Within Observed Range`, ~`Quadratic p-value`,
  "Surgeon",      "Aesthetic",   v_surg_aes$shape,  as.character(v_surg_aes$vertex), ifelse(v_surg_aes$in_range, "Yes", "No"), "0.006",
  "Surgeon",      "Naturalness", v_surg_nat$shape,  as.character(v_surg_nat$vertex), ifelse(v_surg_nat$in_range, "Yes", "No"), "0.011"
)

# =============================================================================
# EXPORT ALL TABLE DATA AS JSON for Word document generator
# =============================================================================

message("Exporting table data as JSON...")

all_tables <- list(
  table1_surg = table1_surg,
  table1_lay  = table1_lay,
  table2a     = table2a,
  table2b     = table2b,
  table3a     = table3a,
  table3b     = table3b,
  table3c     = table3c,
  table3d     = table3d,
  table3e     = table3e,
  table3f     = table3f,
  table4      = table4,
  table5      = table5,
  tableS1     = tableS1
)

jsonlite::write_json(
  all_tables,
  "outputs/tables/manuscript_table_data.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "string"
)

message("Table data written to outputs/tables/manuscript_table_data.json")
message("Now run the Word document generator: node outputs/generate_manuscript_tables.js")
