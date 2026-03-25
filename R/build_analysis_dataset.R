# =========================================================
# Build analysis-ready datasets for breast implant aesthetics project
# Inputs: raw survey Excel files and implant metadata
# Outputs: cleaned cohort-specific and combined datasets in data/processed/
# =========================================================

# -------------------------
# 1. Libraries
# -------------------------
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(readr)
library(purrr)

# -------------------------
# 2. File paths
# -------------------------
raw_dir <- "data/raw"
processed_dir <- "data/processed"

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

lay_raw_file <- file.path(raw_dir, "RAW_SURVEY_DATA_LAY_COHORT.xlsx")
surgeon_raw_file <- file.path(raw_dir, "RAW_SURVEY_DATA_SURGEON_COHORT.xlsx")
patient_file <- file.path(raw_dir, "Patient_Ratios_Malluci_Measurements_data.xlsx")

# -------------------------
# 3. Helper functions
# -------------------------

source("R/helpers_cleaning.R")

# -------------------------
# 4. Raw metadata columns to remove
# -------------------------
lay_drop_cols <- c(
  "StartDate", "EndDate", "Status", "RecordedDate", "RecipientLastName",
  "RecipientFirstName", "RecipientEmail", "ExternalReference",
  "LocationLatitude", "LocationLongitude", "DistributionChannel",
  "UserLanguage", "Q2_5_TEXT", "Q208_4_TEXT", "IPAddress", "ResponseId"
)

surgeon_drop_cols <- c(
  "StartDate", "EndDate", "Status", "RecordedDate", "RecipientLastName",
  "RecipientFirstName", "RecipientEmail", "ExternalReference",
  "LocationLatitude", "LocationLongitude", "DistributionChannel",
  "UserLanguage", "Q2_5_TEXT", "Q225_4_TEXT"
)

# -------------------------
# 5. Patient / patient-level data
# -------------------------
patient_data <- read_excel(patient_file)

# -------------------------
# 6. Clean lay survey
# -------------------------
lay_wide <- read_excel(lay_raw_file) %>%
  drop_metadata_cols(lay_drop_cols) %>%
  promote_first_row_to_header()

lay_long <- lay_wide %>%
  pivot_survey_wide_to_long() %>%
  rename(
    Duration = Duration..in.seconds.,
    Lay.SurveyID = MturkID,
    Age = What.is.your.age.,
    Gender = What.is.your.gender....Selected.Choice,
    Racial.Identity = What.is.your.racial.identity,
    Sexual.Orientation = What.is.your.sexual.orientation....Selected.Choice,
    Country.of.Residence = In.which.country.do.you.live.
  ) %>%
  mutate(
      Country.of.Residence = recode_country_surgeon(Country.of.Residence),
      Continent = assign_continent(Country.of.Residence),
    Sexual.Orientation = factor(
      Sexual.Orientation,
      levels = c("1.0", "2.0", "3.0", "4.0"),
      labels = c("Heterosexual", "Bisexual", "Gay/Lesbian", "Other")
    ),
    Racial.Identity = factor(
      Racial.Identity,
      levels = c("1", "2", "3", "4", "5", "6", "7"),
      labels = c(
        "African/Black", "Asian", "Caucasian/White",
        "Middle Eastern/North African", "Latino/Hispanic",
        "Pacific Islander", "Native American"
      )
    )
  ) %>%
  left_join(patient_data, by = "PatientID") %>%
  label_common_variables() %>%
  filter(
    Position != "I Don't Know",
    Shape != "I Don't Know",
    Gender != "Other",
    Sexual.Orientation != "Other"
  ) %>%
  mutate(
    Position = fct_drop(Position),
    Shape = fct_drop(Shape),
    Gender = fct_drop(Gender),
    Sexual.Orientation = fct_drop(Sexual.Orientation),
    Race = recode_race_binary(Racial.Identity),
    Cohort = "Layperson"
  ) %>%
  add_guess_variables() %>%
  mutate(
    Race = relevel(Race, ref = "Caucasian/White"),
    Age = relevel(Age, ref = "25-34"),
    Country.of.Residence = relevel(factor(Country.of.Residence), ref = "USA")
  )

# -------------------------
# 7. Clean surgeon survey
# -------------------------
surgeon_wide <- read_excel(surgeon_raw_file) %>%
  drop_metadata_cols(surgeon_drop_cols) %>%
  promote_first_row_to_header()

surgeon_long <- surgeon_wide %>%
  pivot_survey_wide_to_long() %>%
  rename(
    Duration = Duration..in.seconds.,
    Surgeon.SurveyID = Response.ID,
    Age = What.is.your.age.,
    Gender = What.is.your.gender....Selected.Choice,
    Surgeon.Sub.Specialization = What.is.your.sub.specialization.within.plastic.surgery...Select.all.that.apply.,
    Surgeon.Length.of.Practice = How.many.years.have.you.been.practicing.plastic.surgery.,
    Country.of.Residence = Current,
    Surgeon.Setting = In.what.type.of.setting.do.you.primarily.practice....Selected.Choice
  ) %>%
  mutate(
    Country.of.Residence = clean_country_surgeon(Country.of.Residence),
    Continent = assign_continent(Country.of.Residence)
  ) %>%
  left_join(patient_data, by = "PatientID") %>%
  label_common_variables() %>%
  mutate(
    Surgeon.Length.of.Practice = factor(
      Surgeon.Length.of.Practice,
      levels = c("1.0", "2.0", "3.0", "4.0", "5.0"),
      labels = c("1 year", "2 years", "3 years", "4 years", "5 years")
    ),
    Surgeon.Setting = factor(
      Surgeon.Setting,
      levels = c("1.0", "2.0", "3.0", "4.0"),
      labels = c("Private Practice", "Academic Medical Center", "Public Hospital", "Other")
    ),
    SubSpecialization_Count = case_when(
      is.na(Surgeon.Sub.Specialization) ~ 0,
      TRUE ~ str_count(Surgeon.Sub.Specialization, ",") + 1
    ),
    SubSpec_Category = case_when(
      is.na(Surgeon.Sub.Specialization) ~ "None",
      SubSpecialization_Count == 1 ~ "Single Specialty",
      SubSpecialization_Count > 1 ~ "Multiple Specialties"
    ),
    SubSpec_Category = factor(SubSpec_Category, levels = c("None", "Single Specialty", "Multiple Specialties")),
    Continent = factor(Continent, levels = c("Americas", "Europe", "Oceania", "Asia", "Africa", "Other"))
  ) %>%
  filter(
    Position != "I Don't Know",
    Shape != "I Don't Know",
    Gender != "Other",
    SubSpec_Category != "None"
  ) %>%
  mutate(
    Position = fct_drop(Position),
    Shape = fct_drop(Shape),
    Gender = fct_drop(Gender),
    SubSpec_Category = fct_drop(SubSpec_Category),
    Cohort = "Surgeon"
  ) %>%
  add_guess_variables() %>%
  mutate(
    Age = relevel(Age, ref = "34-44"),
    Continent = relevel(Continent, ref = "Oceania")
  )

# -------------------------
# 8. Align columns and combine cohorts
# -------------------------
common_vars <- union(names(lay_long), names(surgeon_long))

lay_clean <- lay_long %>%
  mutate(across(setdiff(common_vars, names(.)), ~ NA, .names = "{.col}")) %>%
  select(all_of(common_vars))

surgeon_clean <- surgeon_long %>%
  mutate(across(setdiff(common_vars, names(.)), ~ NA, .names = "{.col}")) %>%
  select(all_of(common_vars))

ratings_all <- bind_rows(lay_clean, surgeon_clean) %>%
  add_analysis_variables()

# -------------------------
# 9. Split back into cohort-specific datasets
# -------------------------
lay_analysis <- ratings_all %>% filter(Cohort == "Layperson")
surgeon_analysis <- ratings_all %>% filter(Cohort == "Surgeon")

# -------------------------
# 10. Save outputs
# -------------------------

# RDS files for analysis
saveRDS(lay_analysis, file.path(processed_dir, "lay_analysis.rds"))
saveRDS(surgeon_analysis, file.path(processed_dir, "surgeon_analysis.rds"))
saveRDS(ratings_all, file.path(processed_dir, "ratings_all.rds"))
saveRDS(patient_data, file.path(processed_dir, "patient_data.rds"))

# CSV files for portability
write_csv(lay_analysis, file.path(processed_dir, "lay_analysis.csv"))
write_csv(surgeon_analysis, file.path(processed_dir, "surgeon_analysis.csv"))
write_csv(ratings_all, file.path(processed_dir, "ratings_all.csv"))

# Excel files
write_xlsx(lay_analysis, file.path(processed_dir, "lay_analysis.xlsx"))
write_xlsx(surgeon_analysis, file.path(processed_dir, "surgeon_analysis.xlsx"))
write_xlsx(ratings_all, file.path(processed_dir, "ratings_all.xlsx"))

# -------------------------
# 11. Basic quality checks
# -------------------------
cat("\nSaved processed datasets to:", processed_dir, "\n")
cat("Lay rows:", nrow(lay_analysis), "\n")
cat("Surgeon rows:", nrow(surgeon_analysis), "\n")
cat("Combined rows:", nrow(ratings_all), "\n")

cat("\nUnique lay raters:", n_distinct(lay_analysis$Lay.SurveyID), "\n")
cat("Unique surgeon raters:", n_distinct(surgeon_analysis$Surgeon.SurveyID), "\n")
cat("Unique patients (combined):", n_distinct(ratings_all$PatientID), "\n")