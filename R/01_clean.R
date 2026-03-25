# =============================================================================
# 01_clean.R
# Data cleaning pipeline: raw survey exports -> analysis-ready datasets.
#
# Outputs (data/processed/):
#   surgeon_data.qs    — surgeon cohort, one row per surgeon-image rating
#   lay_data.qs        — lay person cohort, one row per rater-image rating
#   combined_data.qs   — both cohorts stacked with cohort + rater_id columns
#   implant_data.rds   — patient-level implant characteristics (N=40)
#   df_patients.qs     — patient-level summary with Mallucci metrics
#
# Run order: must run before any other pipeline script.
#
# One-time raw processing:
#   Sys.setenv(RUN_RAW = "TRUE")
#   source("R/01_clean.R")
# =============================================================================

source("R/utils.R")

suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(qs2)
})

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# SECTION 0: ONE-TIME RAW PROCESSING  (guarded — does not run by default)
# =============================================================================
# Reads original Qualtrics exports, strips admin columns, saves trimmed files.
# Only needs to run once. All downstream steps read from data/processed/.
# To trigger: Sys.setenv(RUN_RAW = "TRUE") before sourcing this script.

if (identical(Sys.getenv("RUN_RAW"), "TRUE")) {

  message("=== One-time raw processing ===")

  trim_survey(
    path     = "data/raw/RAW_SURVEY_DATA_LAY_COHORT.xlsx",
    new_path = "data/processed/lay_survey_trimmed.xlsx"
  )
  trim_survey(
    path     = "data/raw/RAW_SURVEY_DATA_SURGEON_COHORT.xlsx",
    new_path = "data/processed/surgeon_survey_trimmed.xlsx"
  )

  message("=== Raw processing complete ===")
}


# =============================================================================
# SECTION 1: IMPLANT / PATIENT-LEVEL DATA (implant characteristics + ratios)
# =============================================================================
message("Loading patient data and ratios...")

patient_data <- read_excel("data/raw/Patient_Ratios_Malluci_Measurements_data.xlsx")

# Keep implant_data as a clean patient-level reference (used in MAR checks)
implant_data <- patient_data %>%
  select(PatientID, Implant.size, Implant.size_scaled, Months.Post.Op, Method)

saveRDS(implant_data, "data/processed/implant_data.rds")

# =============================================================================
# SECTION 2: LAY PERSON SURVEY
# =============================================================================

message("Processing lay person survey...")

BA_wide_data <- read_excel("data/processed/lay_survey_trimmed.xlsx") %>%
  promote_header_row() %>%
  normalise_qualtrics_codes()

BA_long_data <- BA_wide_data %>%
  pivot_ratings_long() %>%
  rename(
    Duration             = "Duration..in.seconds.",
    Lay.SurveyID         = "MturkID",
    Age                  = "What.is.your.age.",
    Gender               = "What.is.your.gender....Selected.Choice",
    Racial.Identity      = "What.is.your.racial.identity",
    Sexual.Orientation   = "What.is.your.sexual.orientation....Selected.Choice",
    Country.of.Residence = "In.which.country.do.you.live."
  ) %>%
  mutate(Country.of.Residence = standardise_country_lay(Country.of.Residence)) %>%
  left_join(implant_data, by = "PatientID") %>%
  mutate(
    Finished             = factor(Finished),
    Progress             = as.numeric(Progress),
    Duration             = as.numeric(Duration),
    Duration_min         = as.numeric(Duration) / 60,
    Implant.size_scaled  = Implant.size / 10
  ) %>%
  label_rating_factors() %>%
  label_age() %>%
  label_gender() %>%
  mutate(
    Sexual.Orientation = factor(Sexual.Orientation,
      levels = c("1","2","3","4"),
      labels = c("Heterosexual","Bisexual","Gay/Lesbian","Other")),
    Racial.Identity = factor(Racial.Identity,
      levels = c("1","2","3","4","5","6","7"),
      labels = c("African/Black","Asian","Caucasian/White",
                 "Middle Eastern/North African","Latino/Hispanic",
                 "Pacific Islander","Native American")),
    Method = factor(Method, levels = c("Above the Muscle","Below the Muscle"))
  ) %>%
  # Exclusions: remove "I Don't Know" and "Other" responses
  filter(Position           != "I Don't Know") %>% mutate(Position           = fct_drop(Position)) %>%
  filter(Shape              != "I Don't Know") %>% mutate(Shape              = fct_drop(Shape)) %>%
  filter(Gender             != "Other")        %>% mutate(Gender             = fct_drop(Gender)) %>%
  filter(Sexual.Orientation != "Other")        %>% mutate(Sexual.Orientation = fct_drop(Sexual.Orientation)) %>%
  filter(!Country.of.Residence %in% c("Unknown")) %>%
  mutate(
    Country.of.Residence = fct_drop(factor(Country.of.Residence)),
    Race = if_else(Racial.Identity == "Caucasian/White", "Caucasian/White", "Non-White"),
    Race = fct_drop(factor(Race))
  ) %>%
  filter(Race != "Unknown") %>%
  add_binary_outcomes() %>%
  mutate(
    Aesthetic_3 = fct_collapse(Aesthetic,
      Low  = c("Very Unattractive","Unattractive"),
      Mid  = "Neutral",
      High = c("Attractive","Very Attractive"))
  )

# Reference level alignment
BA_long_data$Race               <- relevel(factor(BA_long_data$Race),               ref = "Caucasian/White")
BA_long_data$Age                <- relevel(factor(BA_long_data$Age),                ref = "25-34")
BA_long_data$Country.of.Residence <- relevel(factor(BA_long_data$Country.of.Residence), ref = "USA")

message("Lay: ", nrow(BA_long_data), " rows, ",
        length(unique(BA_long_data$Lay.SurveyID)), " raters")


# =============================================================================
# SECTION 3: SURGEON SURVEY
# =============================================================================

message("Processing surgeon survey...")

SurgeonSurvey_wide_data <- read_excel("data/processed/surgeon_survey_trimmed.xlsx") %>%
  promote_header_row()  %>%
  normalise_qualtrics_codes()

SurgeonSurvey_long_data <- SurgeonSurvey_wide_data %>%
  pivot_ratings_long() %>%
  rename(
    Duration                   = "Duration..in.seconds.",
    Surgeon.SurveyID           = "Response.ID",
    Age                        = "What.is.your.age.",
    Gender                     = "What.is.your.gender....Selected.Choice",
    Surgeon.Sub.Specialization = "What.is.your.sub.specialization.within.plastic.surgery...Select.all.that.apply.",
    Surgeon.Length.of.Practice = "How.many.years.have.you.been.practicing.plastic.surgery.",
    Country.of.Residence       = "Current",
    Surgeon.Setting            = "In.what.type.of.setting.do.you.primarily.practice....Selected.Choice"
  ) %>%
  mutate(
    Country.of.Residence = standardise_country_surgeon(Country.of.Residence),
    Country.of.Residence = if_else(
      Country.of.Residence %in% c("ASA","J","b","U","Aur"), NA_character_, Country.of.Residence),
    Continent = assign_continent(Country.of.Residence)
  ) %>%
  left_join(implant_data, by = "PatientID") %>%
  mutate(
    Finished     = factor(Finished),
    Progress     = as.numeric(Progress),
    Duration     = as.numeric(Duration),
    Duration_min = as.numeric(Duration) / 60
  ) %>%
  label_rating_factors() %>%
  label_age() %>%
  label_gender() %>%
  mutate(
    Surgeon.Length.of.Practice = factor(Surgeon.Length.of.Practice,
      levels = c("1","2","3","4","5"),
      labels = c("1 year","2 years","3 years","4 years","5 years")),
    Method = factor(Method, levels = c("Above the Muscle","Below the Muscle")),
    SubSpecialization_Count = case_when(
      is.na(Surgeon.Sub.Specialization) ~ 0L,
      TRUE ~ str_count(Surgeon.Sub.Specialization, ",") + 1L),
    SubSpec_Category = factor(case_when(
      is.na(Surgeon.Sub.Specialization)  ~ "None",
      SubSpecialization_Count == 1        ~ "Single Specialty",
      SubSpecialization_Count >  1        ~ "Multiple Specialties"),
      levels = c("None","Single Specialty","Multiple Specialties")),
    Country.of.Residence = factor(Country.of.Residence),
    Continent            = factor(Continent),
    Surgeon.Setting = factor(
      Surgeon.Setting,
      levels = c("1","2","3","4"),
      labels = c("Private Practice","Academic","Hospital","Other")
    ),
    UniqueID             = factor(UniqueID)
  ) %>%
  filter(Position      != "I Don't Know") %>% mutate(Position      = fct_drop(Position)) %>%
  filter(Shape         != "I Don't Know") %>% mutate(Shape         = fct_drop(Shape)) %>%
  filter(Gender        != "Other")        %>% mutate(Gender        = fct_drop(Gender)) %>%
  filter(SubSpec_Category != "None")      %>% mutate(SubSpec_Category = fct_drop(SubSpec_Category)) %>%
  add_binary_outcomes() %>%
  mutate(
    Aesthetic_3 = fct_collapse(Aesthetic,
      Low  = c("Very Unattractive","Unattractive"),
      Mid  = "Neutral",
      High = c("Attractive","Very Attractive"))
  )

SurgeonSurvey_long_data$Age       <- relevel(factor(SurgeonSurvey_long_data$Age),       ref = "34-44")
SurgeonSurvey_long_data$Continent <- relevel(factor(SurgeonSurvey_long_data$Continent), ref = "Oceania")

message("Surgeon: ", nrow(SurgeonSurvey_long_data), " rows, ",
        length(unique(SurgeonSurvey_long_data$Surgeon.SurveyID)), " surgeons")


# =============================================================================
# SECTION 4: MERGE RATIOS + COMPUTE FEATURES
# =============================================================================

message("Merging Mallucci ratios and computing features...")

# patient_data already contains both implant characteristics and ratio columns
# so we join once instead of joining implant_data and ratios separately

# Shared post-processing pipeline applied identically to both cohorts
add_derived_vars <- function(data) {
  data %>%
    mutate(
      AestheticScore_num     = as.numeric(Aesthetic),
      NaturalScore_num       = as.numeric(Naturalness),
      Months_PostOp_centered = Months.Post.Op - mean(Months.Post.Op, na.rm = TRUE),
      Method  = factor(Method, levels = c("Above the Muscle","Below the Muscle")),
      PatientID = factor(PatientID)
    ) %>%
    compute_ratio_features() %>%
    add_group_centred_covariates() %>%
    add_month_group() %>%
    make_mallucci()
}

surgeon_data <- SurgeonSurvey_long_data %>%
  # drop the implant columns that came in via the earlier left_join(implant_data)
  # to avoid .x/.y collisions when joining the full patient_data
  select(-any_of(c("Implant.size", "Implant.size_scaled", "Months.Post.Op",
                   "Method", "Implant.size_scaled"))) %>%
  left_join(patient_data, by = "PatientID") %>%
  mutate(Surgeon.SurveyID = factor(Surgeon.SurveyID)) %>%
  add_derived_vars()

lay_data <- BA_long_data %>%
  select(-any_of(c("Implant.size", "Implant.size_scaled", "Months.Post.Op",
                   "Method", "Implant.size_scaled.x"))) %>%
  left_join(patient_data, by = "PatientID") %>%
  mutate(Lay.SurveyID = factor(Lay.SurveyID)) %>%
  add_derived_vars()

# =============================================================================
# SECTION 5: COMBINED DATASET  (cohort variable + unified rater_id)
# =============================================================================
# Used for descriptive analyses, demographics, and EDA where both cohorts
# are examined together. The cohort variable enables group_by(cohort) in
# place of running the same code twice on separate objects.
#
# rater_id: unified column holding the respondent ID regardless of cohort,
# so iteration functions in 03_models.R and 04_outputs.R can reference a
# single column name when needed.

message("Building combined dataset...")

# Columns present in both cohorts (intersection, with shared naming)
shared_cols <- c(
  "PatientID", "Aesthetic", "Naturalness", "Shape", "Position",
  "Aesthetic_bin", "Naturalness_bin", "AestheticScore_num", "NaturalScore_num",
  "Aesthetic_3", "Pleasing", "Pleasing_natural",
  "Method", "Implant.size", "Implant.size_scaled",
  "Months.Post.Op", "Months_PostOp_centered", "MonthGroup3",
  "Age", "Gender", "UniqueID",
  "RATIO_Post_Op", "RATIO_Pre_Op", "RATIO DIFFERENCE",
  "RATIO_DIFFERENCE", "RATIO_DIFF_pct", "RATIO_DIFF_pct_centered",
  "RATIO_DIFF_pct_centered_sq", "RATIO_Post_Op_pct_centered",
  "RATIO_Post_Op_pct_centered_sq", "RATIO_Pre_Op_pct_centered",
  "RATIO_Pre_Op_pct_centered_sq",
  "upper_prop", "upper_prop_z", "upper_prop_z_sq", "upper_prop_mean",
  "Guess.Correctly", "Guess.Correctly_Num",
  "meets_upper", "meets_lower", "meets_both",
  "Months_PostOp_GroupCentered", "Implant_Size_GroupCentered"
)

all_cols <- union(names(surgeon_data), names(lay_data))

combined_data <- bind_rows(
  surgeon_data %>%
    mutate(cohort   = "surgeon",
           UniqueID = as.character(UniqueID),
           rater_id = as.character(Surgeon.SurveyID)),
  lay_data %>%
    mutate(cohort   = "lay",
           UniqueID = as.character(UniqueID),
           rater_id = as.character(Lay.SurveyID))
) %>%
  mutate(cohort = factor(cohort, levels = c("surgeon", "lay")))


# =============================================================================
# SECTION 6: PATIENT-LEVEL SUMMARY DATASET
# =============================================================================

df_patients <- patient_data %>%
  mutate(
    Method = factor(Method, levels = c("Above the Muscle", "Below the Muscle")),
    RATIO_Post_Op_pct_centered = (RATIO_Post_Op - mean(RATIO_Post_Op, na.rm = TRUE)) * 100,
    upper_prop_z               = as.numeric(scale(upper_prop))
  ) %>%
  make_mallucci()


# =============================================================================
# SECTION 7: SAVE
# =============================================================================

message("Saving processed datasets...")

qs_save(surgeon_data,  "data/processed/surgeon_data.qs")
qs_save(lay_data,      "data/processed/lay_data.qs")
qs_save(combined_data, "data/processed/combined_data.qs")
qs_save(df_patients,   "data/processed/df_patients.qs")

message("01_clean.R complete.")
message("  surgeon_data:  ", nrow(surgeon_data),  " rows")
message("  lay_data:      ", nrow(lay_data),      " rows")
message("  combined_data: ", nrow(combined_data), " rows")
message("  df_patients:   ", nrow(df_patients),   " rows")
