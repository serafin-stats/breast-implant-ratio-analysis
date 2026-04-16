# =============================================================================
# 02_descriptives.R
# Demographic tables, survey completion, and missingness analyses.
# Uses combined_data for all group comparisons — no parallel cohort code.
#
# Inputs:
#   data/processed/combined_data.qs
#   data/processed/implant_data.rds
#   data/processed/lay_survey_trimmed.xlsx   (for item missingness)
#   data/processed/surgeon_survey_trimmed.xlsx
#
# Outputs:
#   outputs/tables/  — table1, completion, missingness, MAR tables
#   outputs/figures/ — duration plot, missingness plot
# =============================================================================

source("R/utils.R")

suppressPackageStartupMessages({
  library(qs2)
  library(gtsummary)
  library(gt)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(rlang)
  library(glue)
  library(knitr)
})

dir.create("outputs/tables",  showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# LOAD DATA
# =============================================================================

combined_data <- qs_read("data/processed/combined_data.qs")
implant_data  <- readRDS("data/processed/implant_data.rds")


# =============================================================================
# SECTION 1: TABLE 1 — COMBINED DEMOGRAPHICS
# =============================================================================
# combined_data has a cohort column, so all demographic summaries are a
# single group_by(cohort) rather than two parallel code blocks.

message("Building Table 1...")

#' Tabulate a variable across both cohorts and return a tidy data frame
#'
#' @param data Combined data frame with a cohort column.
#' @param var Unquoted variable name.
#' @param var_label Character label for the Variable column.
#' @param level_order Optional character vector to enforce category ordering.
#' @return Tidy data frame ready for stacking into Table 1.
tab_by_cohort <- function(data, var, var_label, level_order = NULL) {
  v <- ensym(var)
  out <- data %>%
    mutate(.x = as.character(!!v),
           .x = if_else(is.na(.x) | .x == "", "Unknown", .x)) %>%
    count(cohort, .x, name = "n") %>%
    group_by(cohort) %>%
    mutate(p = 100 * n / sum(n),
           value = sprintf("%d (%.1f%%)", n, p)) %>%
    ungroup() %>%
    mutate(Variable = var_label, Category = .x) %>%
    dplyr::select(Variable, Category, cohort, value) %>%
    pivot_wider(names_from = cohort, values_from = value,
                values_fill = "--")
  if (!is.null(level_order)) {
    out <- out %>%
      mutate(Category = factor(Category, levels = level_order)) %>%
      arrange(Category) %>%
      mutate(Category = as.character(Category))
  }
  out
}

# One row per participant for demographics
demo <- combined_data %>%
  distinct(cohort, rater_id, .keep_all = TRUE)

# Shared variables (both cohorts)
shared_block <- bind_rows(
  tab_by_cohort(demo, Age,    "Age (years)",
                level_order = c("18-24","25-34","34-44","45-54","65+","Unknown")),
  tab_by_cohort(demo, Gender, "Gender",
                level_order = c("Female","Male","Unknown"))
)

# Cohort-specific variables
# Surgeon-only: filter before passing so lay rows don't appear
surg_only <- demo %>% filter(cohort == "surgeon")
lay_only  <- demo %>% filter(cohort == "lay")

# Surgeon-only block — lay column will be "--" from values_fill
surg_block <- combined_data %>%
  filter(cohort == "surgeon") %>%
  distinct(rater_id, .keep_all = TRUE) %>%
  {bind_rows(
    tab_by_cohort(., Surgeon.Length.of.Practice, "Length of Practice"),
    tab_by_cohort(., Surgeon.Setting,            "Practice Setting"),
    tab_by_cohort(., SubSpecialization_Count,    "Subspecialization Count"),
    tab_by_cohort(., Continent,                  "Continent")
  )}

# Lay-only block
lay_block <- combined_data %>%
  filter(cohort == "lay") %>%
  distinct(rater_id, .keep_all = TRUE) %>%
  {bind_rows(
    tab_by_cohort(., Sexual.Orientation,   "Sexual Orientation"),
    tab_by_cohort(., Racial.Identity,      "Racial Identity"),
    tab_by_cohort(., Country.of.Residence, "Country of Residence")
  )}

table1_df <- bind_rows(
  shared_block %>% mutate(Section = "Shared Demographics"),
  surg_block   %>% mutate(Section = "Surgeon Demographics"),
  lay_block    %>% mutate(Section = "Lay Person Demographics")
) %>%
  mutate(across(c("surgeon","lay"), ~ replace_na(.x, "--")))

table1_gt <- table1_df %>%
  dplyr::select(Section, Variable, Category, surgeon, lay) %>%
  gt(groupname_col = "Section", rowname_col = "Variable") %>%
  tab_header(title = md("**Table 1. Participant Demographics**")) %>%
  cols_label(Category = "Category",
             surgeon  = md("**Surgeon**"),
             lay      = md("**Lay Person**")) %>%
  gt_style(source_note = "\u00b9 n (%) reflects unique participants per group.")

save_gt_table(table1_gt, "table1_demographics.png")


# =============================================================================
# SECTION 2: SURVEY COMPLETION
# =============================================================================

message("Building survey completion summary...")

# combined_data has a Finished column — summarise once, group_by(cohort)
completion_summary <- combined_data %>%
  distinct(cohort, rater_id, .keep_all = TRUE) %>%
  mutate(Status = if_else(as.character(Finished) == "1", "Completed", "Incomplete")) %>%
  count(cohort, Status) %>%
  pivot_wider(names_from = Status, values_from = n, values_fill = 0L)

completion_gt <- completion_summary %>%
  gt() %>%
  tab_header(title    = md("**Survey Completion Status**"),
             subtitle = "Number of completed and incomplete surveys by group") %>%
  cols_label(cohort     = "Respondent Group",
             Completed  = "Completed",
             Incomplete = "Incomplete") %>%
  gt_style(source_note = "Counts reflect unique respondents per group.")

save_gt_table(completion_gt, "survey_completion.png")


# =============================================================================
# SECTION 3: SURVEY DURATION PLOT
# =============================================================================

duration_plot <- combined_data %>%
  distinct(cohort, rater_id, .keep_all = TRUE) %>%
  ggplot(aes(x = reorder(rater_id, Duration_min), y = Duration_min, fill = cohort)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  facet_wrap(~ cohort, scales = "free_x",
             labeller = as_labeller(c(surgeon = "Surgeon", lay = "Lay Person"))) +
  scale_fill_manual(values = c(surgeon = "steelblue", lay = "#e07b54"), guide = "none") +
  labs(title = "Survey Duration by Respondent",
       x = "Respondent ID", y = "Duration (minutes)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

save_figure(duration_plot, "survey_duration.png", width = 12, height = 4)


# =============================================================================
# SECTION 4: ITEM-LEVEL MISSINGNESS
# =============================================================================

message("Computing item-level missingness...")

#' Calculate per-item missingness for a given Qualtrics attribute suffix
#'
#' @param data Wide-format survey data frame.
#' @param suffix Numeric suffix string identifying the attribute (e.g., "1").
#' @param domain_label Character label for the domain.
#' @return Tidy data frame: Item, Pct_Missing, Domain.
get_missingness_by_item <- function(data, suffix, domain_label) {
  cols <- names(data) %>% str_subset(paste0("\\.", suffix, "$"))
  data %>%
    summarise(across(all_of(cols), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(everything(), names_to = "Item", values_to = "Pct_Missing") %>%
    mutate(Domain = domain_label)
}

# Helper: compute participant-level completion rate for a given suffix
calc_completion_rate <- function(data, suffix, label) {
  cols <- names(data) %>% str_subset(paste0("\\.", suffix, "$"))
  data %>%
    rowwise() %>%
    mutate(n_total    = length(cols),
           n_complete = sum(!is.na(c_across(all_of(cols)))),
           pct_rated  = 100 * n_complete / n_total) %>%
    ungroup() %>%
    summarise(Group           = label,
              Mean_Pct_Rated  = mean(pct_rated),
              SD_Pct_Rated    = sd(pct_rated),
              Min_Pct_Rated   = min(pct_rated),
              Max_Pct_Rated   = max(pct_rated))
}

# Both wide files if available
wide_files <- list(
  lay     = "data/processed/lay_survey_trimmed.xlsx",
  surgeon = "data/processed/surgeon_survey_trimmed.xlsx"
)

if (all(file.exists(unlist(wide_files)))) {

  lay_wide  <- read_excel(wide_files$lay)     %>% promote_header_row()
  surg_wide <- read_excel(wide_files$surgeon) %>% promote_header_row()

  # Item missingness (lay only — surgeon survey structure differs)
  item_miss <- bind_rows(
    get_missingness_by_item(lay_wide, "1", "Aesthetic"),
    get_missingness_by_item(lay_wide, "2", "Naturalness"),
    get_missingness_by_item(lay_wide, "3", "Shape"),
    get_missingness_by_item(lay_wide, "4", "Position")
  )

  miss_plot <- ggplot(item_miss,
                      aes(x = reorder(Item, Pct_Missing), y = Pct_Missing, fill = Domain)) +
    geom_col() + coord_flip() +
    facet_wrap(~ Domain, scales = "free_y") +
    labs(title = "Percent Missingness by Item (Lay Person Survey)",
         x = "Item", y = "% Missing") +
    theme_minimal()

  save_figure(miss_plot, "missingness_by_item.png", width = 12, height = 6)

  # Participant-level completion rates across both cohorts
  completion_rates <- bind_rows(
    calc_completion_rate(lay_wide,  "1", "Lay Person"),
    calc_completion_rate(surg_wide, "1", "Surgeon")
  )

  completion_rates_gt <- completion_rates %>%
    gt() %>%
    tab_header(title = md("**Participant-Level Survey Completion Rate by Group**")) %>%
    fmt_number(columns = where(is.numeric), decimals = 1) %>%
    cols_label(Group          = "Group",
               Mean_Pct_Rated = "Mean (%)",
               SD_Pct_Rated   = "SD (%)",
               Min_Pct_Rated  = "Min (%)",
               Max_Pct_Rated  = "Max (%)") %>%
    gt_style()

  save_gt_table(completion_rates_gt, "completion_rates.png")

} else {
  message("Skipping item missingness: trimmed survey files not found.")
}

# =============================================================
# SECTION 5: MAR CHECKS  (rater-level)
# =============================================================
# All 40 images were rated by all raters — image-level MAR is
# not applicable. MAR is assessed at the rater level: do raters
# who completed the full survey differ from those who did not?

mar_rater <- combined_data %>%
  distinct(cohort, rater_id, .keep_all = TRUE) %>%
  mutate(
    Completed = factor(
      if_else(as.numeric(as.character(Finished)) == 1,
              "Completed", "Incomplete"),
      levels = c("Completed", "Incomplete")
    )
  )

# Surgeon rater-level MAR
mar_surgeon_gt <- mar_rater %>%
  filter(cohort == "surgeon") %>%
  dplyr::select(Completed, Age, Gender, Continent,
                Surgeon.Length.of.Practice, Surgeon.Setting) %>%
  tbl_summary(
    by        = Completed,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"
  ) %>%
  add_p() %>% bold_labels() %>%
  modify_footnote(everything() ~ NA) %>%
  as_gt() %>%
  tab_header(title = md("**MAR: Surgeon Completion vs Demographic Characteristics**")) %>%
  gt_style(source_note = "Rater-level missingness. All 40 images were rated by every surgeon.")

# Lay rater-level MAR
mar_lay_gt <- mar_rater %>%
  filter(cohort == "lay") %>%
  dplyr::select(Completed, Age, Gender, Race,
                Sexual.Orientation, Country.of.Residence) %>%
  tbl_summary(
    by        = Completed,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing   = "no"
  ) %>%
  add_p() %>% bold_labels() %>%
  modify_footnote(everything() ~ NA) %>%
  as_gt() %>%
  tab_header(title = md("**MAR: Lay Person Completion vs Demographic Characteristics**")) %>%
  gt_style(source_note = "Rater-level missingness. All 40 images were rated by every lay rater.")

save_gt_table(mar_surgeon_gt, "mar_surgeon.png")
save_gt_table(mar_lay_gt,     "mar_layperson.png")


# =============================================================================
# SECTION 6: RATING FREQUENCY CHECKS
# =============================================================================

message("Rating frequency checks...")

# Rating counts per image, by cohort — one pipeline instead of two
rating_counts <- combined_data %>%
  count(cohort, PatientID, name = "TimesRated") %>%
  mutate(PatientID = as.numeric(as.character(PatientID)))

implant_rating_counts <- implant_data %>%
  left_join(rating_counts %>% filter(cohort == "surgeon") %>%
              dplyr::select(PatientID, surg_rated = TimesRated), by = "PatientID") %>%
  left_join(rating_counts %>% filter(cohort == "lay") %>%
              dplyr::select(PatientID, lay_rated = TimesRated),  by = "PatientID") %>%
  mutate(across(c(surg_rated, lay_rated), ~ replace_na(.x, 0L)))

kruskal_surg <- kruskal.test(Implant.size ~ surg_rated, data = implant_rating_counts)
kruskal_lay  <- kruskal.test(Implant.size ~ lay_rated,  data = implant_rating_counts)

message("Kruskal (implant size ~ surgeon rating count): p = ",
        round(kruskal_surg$p.value, 4))
message("Kruskal (implant size ~ lay rating count):     p = ",
        round(kruskal_lay$p.value,  4))

# Surgeon country distribution
surgeon_countries <- combined_data %>%
  filter(cohort == "surgeon") %>%
  distinct(rater_id, Country.of.Residence) %>%
  count(Country.of.Residence, sort = TRUE) %>%
  mutate(Pct = round(100 * n / sum(n), 1))

message("Surgeon country distribution:\n",
        paste(capture.output(print(surgeon_countries)), collapse = "\n"))

message("02_descriptives.R complete.")
