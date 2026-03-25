# -------------------------
# 3. Helper functions
# -------------------------

drop_metadata_cols <- function(data, cols_to_drop) {
  data %>%
    select(-any_of(cols_to_drop))
}

promote_first_row_to_header <- function(data) {
  colnames(data) <- as.character(unlist(data[1, ]))
  colnames(data) <- make.names(colnames(data), unique = TRUE)
  data[-1, ] %>%
    mutate(UniqueID = row_number())
}

pivot_survey_wide_to_long <- function(data) {
  data %>%
    pivot_longer(
      cols = matches("X\\d+\\.\\d+"),
      names_to = c("PatientID", "Attribute"),
      names_pattern = "X(\\d+)\\.(\\d+)",
      values_to = "Response"
    ) %>%
    pivot_wider(
      names_from = Attribute,
      values_from = Response,
      names_prefix = "Attribute_"
    ) %>%
    rename(
      Aesthetic = Attribute_1,
      Naturalness = Attribute_2,
      Shape = Attribute_3,
      Position = Attribute_4
    ) %>%
    mutate(PatientID = as.numeric(PatientID))
}

recode_country_lay <- function(x) {
  case_when(
    str_detect(x, regex("united states|usa|us|u\\.s\\.?a|u\\.s\\.?|america", ignore_case = TRUE)) ~ "USA",
    str_detect(x, regex("california|texas|new york|indiana|alabama|washington|sc|la|tx|alaska", ignore_case = TRUE)) ~ "USA",
    TRUE ~ "Other"
  )
}

recode_country_surgeon <- function(x) {
  case_when(
    str_detect(x, regex("united states|usa|u\\.s\\.a|u\\.s|america", ignore_case = TRUE)) ~ "USA",
    str_detect(x, regex("united kingdom|uk", ignore_case = TRUE)) ~ "United Kingdom",
    str_detect(x, regex("australia|aust|ausy", ignore_case = TRUE)) ~ "Australia",
    str_detect(x, regex("brasil|brazil", ignore_case = TRUE)) ~ "Brazil",
    str_detect(x, regex("mexico", ignore_case = TRUE)) ~ "Mexico",
    str_detect(x, regex("spain", ignore_case = TRUE)) ~ "Spain",
    str_detect(x, regex("argentina", ignore_case = TRUE)) ~ "Argentina",
    str_detect(x, regex("switzerland", ignore_case = TRUE)) ~ "Switzerland",
    str_detect(x, regex("poland", ignore_case = TRUE)) ~ "Poland",
    TRUE ~ x
  )
}

assign_continent <- function(country) {
  case_when(
    country %in% c("USA", "Mexico", "Brazil", "Argentina") ~ "Americas",
    country %in% c("United Kingdom", "Spain", "Switzerland", "Poland", "Belgium") ~ "Europe",
    country %in% c("Australia") ~ "Oceania",
    country %in% c("India", "Cyprus") ~ "Asia",
    country %in% c("Egypt", "Rwanda") ~ "Africa",
    TRUE ~ "Other"
  )
}

label_common_variables <- function(data) {
  data %>%
    mutate(
      Finished = factor(Finished),
      Progress = as.numeric(Progress),
      Duration = as.numeric(Duration),
      Duration_min = Duration / 60,
      
      Aesthetic = factor(
        Aesthetic,
        ordered = TRUE,
        levels = c("1.0", "2.0", "3.0", "4.0", "5.0"),
        labels = c("Very Unattractive", "Unattractive", "Neutral", "Attractive", "Very Attractive")
      ),
      
      Naturalness = factor(
        Naturalness,
        ordered = TRUE,
        levels = c("1.0", "2.0", "3.0", "4.0", "5.0"),
        labels = c("Very Unnatural", "Unnatural", "Neutral", "Natural", "Very Natural")
      ),
      
      Shape = factor(
        Shape,
        levels = c("1.0", "2.0", "3.0"),
        labels = c("Round", "Teardrop", "I Don't Know")
      ),
      
      Position = factor(
        Position,
        levels = c("1.0", "2.0", "3.0"),
        labels = c("Above the Muscle", "Below the Muscle", "I Don't Know")
      ),
      
      Age = factor(
        Age,
        levels = c("1.0", "2.0", "3.0", "4.0", "5.0"),
        labels = c("18-24", "25-34", "34-44", "45-54", "65+")
      ),
      
      Gender = factor(
        Gender,
        levels = c("1.0", "2.0", "3.0"),
        labels = c("Male", "Female", "Other")
      ),
      
      Method = factor(
        Method,
        levels = c("Above the Muscle", "Below the Muscle")
      )
    )
}

add_guess_variables <- function(data) {
  data %>%
    mutate(
      Guess.Correctly = case_when(
        is.na(Position) | is.na(Method) ~ NA_character_,
        as.character(Position) == as.character(Method) ~ "Correct",
        TRUE ~ "Incorrect"
      ),
      Guess.Correctly = factor(Guess.Correctly, levels = c("Incorrect", "Correct")),
      Guess.Correctly_Num = if_else(
        is.na(Guess.Correctly),
        NA_real_,
        as.numeric(Guess.Correctly) - 1
      )
    )
}

add_analysis_variables <- function(data) {
  data %>%
    mutate(
      Cohort = factor(Cohort, levels = c("Layperson", "Surgeon")),
      
      AestheticScore_num = as.numeric(Aesthetic),
      NaturalnessScore_num = as.numeric(Naturalness),
      
      Aesthetic_bin = if_else(AestheticScore_num >= 4, 1, 0),
      Naturalness_bin = if_else(NaturalnessScore_num >= 4, 1, 0),
      
      RATIO_Post_Op_pct_centered = (RATIO_Post_Op - mean(RATIO_Post_Op, na.rm = TRUE)) * 100,
      RATIO_Post_Op_pct_centered_sq = RATIO_Post_Op_pct_centered^2,
      
      RATIO_DIFF_pct_centered = (RATIO_DIFFERENCE - mean(RATIO_DIFFERENCE, na.rm = TRUE)) * 100,
      RATIO_DIFF_pct_centered_sq = RATIO_DIFF_pct_centered^2,
      
      RATIO_Pre_Op_pct_centered = (RATIO_Pre_Op - mean(RATIO_Pre_Op, na.rm = TRUE)) * 100,
      RATIO_Pre_Op_pct_centered_sq = RATIO_Pre_Op_pct_centered^2,
      
      upper_prop_z = as.numeric(scale(upper_prop, center = TRUE, scale = TRUE)),
      upper_prop_z_sq = upper_prop_z^2
    )
}

recode_race_binary <- function(racial_identity) {
  case_when(
    is.na(racial_identity) ~ NA_character_,
    as.character(racial_identity) == "Caucasian/White" ~ "Caucasian/White",
    TRUE ~ "Non-White"
  ) %>%
    factor(levels = c("Caucasian/White", "Non-White"))
}