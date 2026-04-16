# =============================================================================
# 04_outputs.R
# Generate all publication-ready tables and figures via model grid iteration.
#
# Inputs:
#   outputs/models/model_grid.rds   + all model .rds files
#   data/processed/surgeon_data.qs
#   data/processed/lay_data.qs
#   data/processed/combined_data.qs
#   data/processed/df_patients.qs
#
# Outputs:
#   outputs/tables/  — gt PNG tables
#   outputs/figures/ — ggplot PNG figures
# =============================================================================

source("R/utils.R")

suppressPackageStartupMessages({
  library(qs2)
  library(ordinal)
  library(lme4)
  library(gtsummary)
  library(gt)
  library(broom.mixed)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(gridExtra)
  library(grid)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(psych)
  library(irr)
  library(epitools)
  library(glue)
  library(tibble)
  library(stringr)
})

dir.create("outputs/tables",  showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# LOAD DATA AND MODELS
# =============================================================================

message("Loading data and model grid...")

surgeon_data  <- qs_read("data/processed/surgeon_data.qs")
lay_data      <- qs_read("data/processed/lay_data.qs")
combined_data <- qs_read("data/processed/combined_data.qs")
df_patients   <- qs_read("data/processed/df_patients.qs")
icc_results   <- readRDS("outputs/models/icc_results.rds")
model_grid    <- readRDS("outputs/models/model_grid.rds")

data_list <- list(surgeon = surgeon_data, lay = lay_data)

# Load all model objects into a named list keyed by filename
all_models <- setNames(
  map(model_grid$filename, function(fn) {
    path <- file.path("outputs", "models", fn)
    if (file.exists(path)) readRDS(path) else NULL
  }),
  model_grid$filename
)

n_loaded <- sum(!map_lgl(all_models, is.null))
message("Loaded ", n_loaded, " / ", nrow(model_grid), " models.")


# =============================================================================
# SECTION 1: MODEL RESULT TABLES
# =============================================================================
# For every pair (surgeon + lay) sharing the same outcome, predictor, and
# model_set, build a merged two-column gtsummary table and save it.
# This loop replaces ~20 copy-pasted merge_cohort_tables() calls.

message("Building model result tables...")

#' Create a gtsummary tbl_regression for a clmm or glmerMod model
#' @param model Fitted clmm or glmerMod object.
#' @param exponentiate Logical. Report odds ratios. Default TRUE.
#' @return A tbl_regression object.
# Variable labels applied consistently across all model tables

model_var_labels <- list(
  upper_prop_z                  = "Upper Proportion (standardized)",
  upper_prop_z_sq               = "Upper Proportion (standardized, squared)",
  RATIO_Post_Op_pct_centered    = "Post-Op Ratio (centered, %)",
  RATIO_Post_Op_pct_centered_sq = "Post-Op Ratio (centered, %, squared)",
  RATIO_DIFF_pct_centered       = "Ratio Difference (centered, %)",
  RATIO_DIFF_pct_centered_sq    = "Ratio Difference (centered, %, squared)",
  RATIO_Pre_Op_pct_z     = "Pre-Op Ratio (centered, %)",
  RATIO_Pre_Op_pct_z_sq  = "Pre-Op Ratio (centered, %, squared)",
  Months.Post.Op              = "Months Post-Op",
  Method                     = "Implant Method"
)
#' Create a gtsummary tbl_regression with clean variable labels
#' @param model Fitted clmm or glmerMod object.
#' @param exponentiate Logical. Report odds ratios. Default TRUE.
#' @return A tbl_regression object.
make_model_tbl <- function(model, exponentiate = TRUE) {
  tbl_regression(
    model,
    exponentiate = exponentiate,
    label        = model_var_labels
  ) %>%
    bold_p(t = 0.05) %>%
    bold_labels() %>%
    modify_footnote(everything() ~ NA)
}

#' Create a gtsummary tbl_regression with clean variable labels
#' @param model Fitted clmm or glmerMod object.
#' @param exponentiate Logical. Report odds ratios. Default TRUE.
#' @return A tbl_regression object.
make_model_tbl <- function(model, exponentiate = TRUE) {
  tbl_regression(
    model,
    exponentiate = exponentiate,
    label        = model_var_labels,
    show_single_row = "Method"
  ) %>%
    bold_p(t = 0.05) %>%
    bold_labels() %>%
    modify_footnote(everything() ~ NA)
  
}

#' Merge surgeon and lay person model tables side by side with gt styling
#' @param surg_mod,lay_mod Fitted model objects.
#' @param title Character table title.
#' @param source_note Optional source note.
#' @return A styled gt_tbl object.
merge_cohort_tbls <- function(surg_mod, lay_mod, title, source_note = NULL) {
  tbl_merge(
    tbls        = list(make_model_tbl(surg_mod), make_model_tbl(lay_mod)),
    tab_spanner = c("**Surgeon**", "**Lay Person**")
  ) %>%
    modify_footnote(everything() ~ NA) %>%
    as_gt() %>%
    tab_header(title = md(glue("**{title}**"))) %>%
    gt_style(source_note = source_note)
}

# Identify all unique (predictor_key, outcome, model_set, model_type) combos
# that have both a surgeon and lay row
table_specs <- model_grid %>%
  filter(model_set %in% c("unadjusted", "adjusted", "sensitivity")) %>%
  group_by(predictor_key, outcome, model_set, model_type) %>%
  filter(n() == 2, all(c("surgeon","lay") %in% cohort)) %>%
  summarise(
    surg_file = filename[cohort == "surgeon"],
    lay_file  = filename[cohort == "lay"],
    outcome_abbrev   = first(outcome_abbrev),
    type_prefix      = first(type_prefix),
    .groups = "drop"
  )

# Build and save every table in one loop
walk(seq_len(nrow(table_specs)), function(i) {
  spec  <- table_specs[i, ]
  surg_m <- all_models[[spec$surg_file]]
  lay_m  <- all_models[[spec$lay_file]]
  if (is.null(surg_m) || is.null(lay_m)) {
    message("Skipping table (model missing): ", spec$surg_file)
    return(invisible(NULL))
  }

  # Human-readable title
  pred_label <- switch(spec$predictor_key,
    upper    = "Upper Proportion",
    postop   = "Post-Op Ratio",
    ratiodiff = "Ratio Difference",
    preop    = "Pre-Op Ratio (Sensitivity)",
    spec$predictor_key)
  out_label  <- if (grepl("bin", spec$surg_file)) "Binary" else "Ordinal"
  set_label  <- tools::toTitleCase(spec$model_set)
  title      <- glue("{set_label} {out_label}: {pred_label} \u2014 {spec$outcome}")

  source_note <- switch(spec$outcome,
    Aesthetic   = "Cumulative OR (ordinal) or OR (binary) for Aesthetic Ratings.",
    Naturalness = "Cumulative OR (ordinal) or OR (binary) for Naturalness Ratings.",
    NULL)

  # Output filename mirrors model filename pattern
  out_fn <- str_replace(spec$surg_file, "^surg_", "tbl_") %>%
    str_replace("\\.rds$", ".png")

  tbl <- tryCatch(
    merge_cohort_tbls(surg_m, lay_m, title, source_note),
    error = function(e) {
      message("Table failed [", out_fn, "]: ", e$message)
      NULL
    }
  )
  if (!is.null(tbl)) save_gt_table(tbl, out_fn)
})

message("Model tables complete.")


# =============================================================================
# SECTION 2: PREDICTED PROBABILITY CURVES
# =============================================================================

message("Building predicted probability curves...")

#' Generate a population-level prediction grid for one predictor
#'
#' Varies the focal predictor across its observed range while holding all
#' covariates at reference / mean values (population-level prediction).
#'
#' @param data Analysis data frame.
#' @param focal_var Column name of the predictor to vary.
#' @param focal_sq Column name of the squared term, or NULL.
#' @param covariate_defaults Named list of fixed covariate values.
#' @param n Integer. Grid resolution (default 100).
#' @return Data frame ready for predict().
make_pred_grid <- function(data, focal_var, focal_sq = NULL,
                           covariate_defaults = list(), n = 100) {
  rng <- seq(min(data[[focal_var]], na.rm = TRUE),
             max(data[[focal_var]], na.rm = TRUE),
             length.out = n)
  grid <- tibble(!!sym(focal_var) := rng)
  if (!is.null(focal_sq)) grid[[focal_sq]] <- rng^2
  for (nm in names(covariate_defaults)) grid[[nm]] <- covariate_defaults[[nm]]
  grid
}

#' Plot predicted probability curves from a fitted clmm model
#'
#' @param model Fitted clmm object.
#' @param pred_grid Data frame from make_pred_grid().
#' @param focal_var Predictor column name (x-axis).
#' @param x_label Human-readable x-axis label.
#' @param title Plot title.
#' @return A ggplot object.
plot_pred_probs <- function(model, pred_grid, focal_var, x_label, title) {
  probs <- predict(model, newdata = pred_grid, type = "prob")$fit
  as.data.frame(probs) %>%
    mutate(x = pred_grid[[focal_var]]) %>%
    pivot_longer(-x, names_to = "Category", values_to = "Probability") %>%
    ggplot(aes(x = x, y = Probability, colour = Category)) +
    geom_line(linewidth = 0.9) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(x = x_label, y = "Predicted Probability", title = title) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title       = element_text(size = 10),
      axis.text        = element_text(size = 9),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}

# Covariate defaults (population-level: mean months, reference method)
covs <- list(
  surgeon = list(Months.Post.Op.x = mean(surgeon_data$Months.Post.Op.x, na.rm = TRUE),
                 Method.x         = "Below the Muscle"),
  lay     = list(Months.Post.Op.x = mean(lay_data$Months.Post.Op.x, na.rm = TRUE),
                 Method.x         = "Below the Muscle")
)

# Predictor metadata for grid construction
pred_meta <- tribble(
  ~predictor_key, ~focal_var,                    ~focal_sq,                       ~x_label,
  "upper",        "upper_prop_z",                "upper_prop_z_sq",               "Upper Proportion (z-scored)",
  "postop",       "RATIO_Post_Op_pct_centered",  "RATIO_Post_Op_pct_centered_sq", "Post-Op Ratio (centred, %)",
  "ratiodiff",    "RATIO_DIFF_pct_centered",     "RATIO_DIFF_pct_centered_sq",    "Ratio Difference (centred, %)"
)

# Build and save all predicted probability plots via iteration
pp_grid <- model_grid %>%
  filter(model_set == "adjusted", model_type == "ordinal") %>%
  left_join(pred_meta, by = "predictor_key")

walk(seq_len(nrow(pp_grid)), function(i) {
  spec  <- pp_grid[i, ]
  model <- all_models[[spec$filename]]
  if (is.null(model)) return(invisible(NULL))

  data      <- data_list[[spec$cohort]]
  cov_defaults <- covs[[spec$cohort]]

  grid <- make_pred_grid(data, spec$focal_var, spec$focal_sq, cov_defaults)

  cohort_label <- if (spec$cohort == "surgeon") "Surgeon" else "Lay Person"
  title <- glue("{cohort_label}: {spec$outcome} ~ {spec$x_label}")

  p <- tryCatch(
    plot_pred_probs(model, grid, spec$focal_var, spec$x_label, title),
    error = function(e) { message("PP plot failed: ", e$message); NULL }
  )
  if (!is.null(p)) {
    fn <- str_replace(spec$filename, "\\.rds$", "_pp.png")
    save_figure(p, fn, width = 7, height = 5)
  }
})

message("Predicted probability plots complete.")


# =============================================================================
# SECTION 3: DISTRIBUTION PLOTS
# =============================================================================

message("Building distribution plots...")

#' Density plot for a ratio variable with mean annotation
#' @param data Data frame.
#' @param var Column name.
#' @param x_label x-axis label.
#' @param title Plot title.
make_dist_plot <- function(data, var, x_label, title) {
  m <- mean(data[[var]], na.rm = TRUE)
  ggplot(data, aes(x = .data[[var]])) +
    geom_density(fill = "#b3cde3", alpha = 0.5, colour = "steelblue") +
    geom_vline(xintercept = m, linetype = "dashed", colour = "red", linewidth = 0.8) +
    annotate("text", x = m, y = Inf,
             label = sprintf("Mean = %.3f", m),
             vjust = 1.5, hjust = -0.05, size = 3.5) +
    labs(x = x_label, y = "Density", title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.title = element_text(size = 11), axis.text = element_text(size = 10),
          panel.grid.minor = element_blank())
}

pre_post_plot <- grid.arrange(
  make_dist_plot(df_patients, "RATIO_Pre_Op",  "Pre-Op Ratio",  "Pre-Operative Ratio"),
  make_dist_plot(df_patients, "RATIO_Post_Op", "Post-Op Ratio", "Post-Operative Ratio"),
  nrow = 1
)
save_figure(pre_post_plot, "pre_post_ratio_Distribution.png")


# =============================================================================
# SECTION 4: MALLUCCI PROPORTION PLOT AND TABLE
# =============================================================================

message("Building Mallucci plots...")

df_patients <- df_patients %>%
  mutate(
    meets_upper = abs(upper_prop - 0.45) <= 0.05,
    meets_lower = abs(lower_prop - 0.55) <= 0.05,
    meets_both  = meets_upper & meets_lower
  )

mallucci_plot <- ggplot(df_patients, aes(x = Method, fill = meets_both)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("TRUE" = "#66c2a5", "FALSE" = "#fc8d62")) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Implant Placement", y = "Proportion of Patients", fill = "Meets Both") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.title = element_text(size = 11), panel.grid.minor = element_blank())

mallucci_summary <- df_patients %>%
  group_by(Method) %>%
  summarise(`Total Patients`     = n(),
            `Meets Both`         = sum(meets_both),
            `Percent Meets Both` = percent(mean(meets_both), accuracy = 0.1),
            .groups = "drop")

mallucci_gt <- mallucci_summary %>%
  gt() %>%
  cols_label(Method = "Implant Method") %>%
  gt_style(source_note = "Upper (0.45 \u00b1 0.05) and Lower (0.55 \u00b1 0.05) thresholds.")

save_figure(mallucci_plot, "pop_mallucci_plot.png", width = 6, height = 5)
save_gt_table(mallucci_gt, "pop_mallucci_gt.png")


# =============================================================================
# SECTION 5: CONCORDANCE HEATMAPS
# =============================================================================

message("Building concordance heatmaps...")

#' Concordance heatmap: proportion of each rating category per image
#' @param data Analysis data frame.
#' @param score_var Column name of the numeric score (e.g. "AestheticScore_num").
#' @param title Plot title.
make_concordance_heatmap <- function(data, score_var, title) {
  data %>%
    mutate(.s = as.numeric(.data[[score_var]])) %>%
    count(PatientID, .s) %>%
    group_by(PatientID) %>%
    mutate(Prop = n / sum(n)) %>%
    ungroup() %>%
    ggplot(aes(x = factor(.s), y = factor(PatientID))) +
    geom_tile(aes(fill = Prop)) +
    scale_fill_viridis_c(option = "D") +
    labs(x = "Rating", y = "Image (PatientID)", title = title, fill = "Proportion") +
    theme_minimal(base_size = 14) +
    scale_y_discrete(breaks = function(x) x[as.numeric(x) %% 2 == 0]) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title  = element_text(hjust = 0.5))
}

# Build all four heatmaps via iteration over cohort x outcome
heatmap_specs <- tribble(
  ~cohort,    ~score_var,          ~label,
  "surgeon",  "AestheticScore_num", "Surgeon — Aesthetic",
  "lay",      "AestheticScore_num", "Lay Person — Aesthetic",
  "surgeon",  "NaturalScore_num",   "Surgeon — Naturalness",
  "lay",      "NaturalScore_num",   "Lay Person — Naturalness"
)

heatmaps <- heatmap_specs %>%
  pmap(function(cohort, score_var, label) {
    make_concordance_heatmap(data_list[[cohort]], score_var, label)
  })

# Save as paired panels
aes_heatmaps <- grid.arrange(heatmaps[[1]], heatmaps[[2]], nrow = 1)
nat_heatmaps <- grid.arrange(heatmaps[[3]], heatmaps[[4]], nrow = 1)
save_figure(aes_heatmaps, "concordance_heatmaps_aes.png")
save_figure(nat_heatmaps, "concordance_heatmaps_nat.png")


# =============================================================================
# SECTION 6: MEAN IMAGE RATING PLOTS
# =============================================================================

message("Building mean image rating plots...")

#' Mean rating per image, ranked, with cohort mean reference line
#' @param data Analysis data frame.
#' @param score_var Numeric score column name.
#' @param cohort_label Label for the plot title.
make_mean_image_plot <- function(data, score_var, cohort_label) {
  overall_mean <- mean(as.numeric(data[[score_var]]), na.rm = TRUE)
  data %>%
    group_by(PatientID) %>%
    summarise(mean_score = mean(as.numeric(.data[[score_var]]), na.rm = TRUE),
              .groups = "drop") %>%
    ggplot(aes(x = reorder(factor(PatientID), mean_score), y = mean_score)) +
    geom_point(size = 2, colour = "steelblue") +
    geom_hline(yintercept = overall_mean, linetype = "dashed", colour = "red") +
    labs(x = "Image (ranked by mean score)", y = "Mean Score",
         title = glue("{cohort_label}: Mean Rating per Image")) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          plot.title = element_text(hjust = 0.5))
}

# Iterate over 4 combinations
mean_plot_specs <- tribble(
  ~cohort,    ~score_var,          ~label,
  "surgeon",  "AestheticScore_num", "Surgeon",
  "lay",      "AestheticScore_num", "Lay Person",
  "surgeon",  "NaturalScore_num",   "Surgeon",
  "lay",      "NaturalScore_num",   "Lay Person"
)

mean_plots <- mean_plot_specs %>%
  pmap(function(cohort, score_var, label) {
    make_mean_image_plot(data_list[[cohort]], score_var, label) +
      theme(plot.title = element_blank())
  })

aes_mean_panel <- grid.arrange(mean_plots[[1]], mean_plots[[2]], nrow = 1,
  bottom = textGrob("Aesthetic Ratings: Surgeon (left) | Lay Person (right)",
                    gp = gpar(fontsize = 10)))
nat_mean_panel <- grid.arrange(mean_plots[[3]], mean_plots[[4]], nrow = 1,
  bottom = textGrob("Naturalness Ratings: Surgeon (left) | Lay Person (right)",
                    gp = gpar(fontsize = 10)))

save_figure(aes_mean_panel, "mean_image_plots_aes.png")
save_figure(nat_mean_panel, "mean_image_plots_nat.png")


# =============================================================================
# SECTION 7: ICC SUMMARY TABLE
# =============================================================================

icc_gt <- icc_results %>%
  mutate(across(c(image_ICC, rater_ICC), ~ round(.x, 3))) %>%
  gt() %>%
  tab_header(title = md("**ICC Summary: Image- and Rater-Level Clustering**")) %>%
  cols_label(cohort        = "Cohort",
             outcome       = "Outcome",
             predictor_key = "Predictor",
             image_ICC     = "Image ICC",
             rater_ICC     = "Rater ICC") %>%
  gt_style(source_note = "Adjusted ordinal models. Surgeon ICC up to 0.33; lay ICC near zero.")

save_gt_table(icc_gt, "icc_summary.png")


# =============================================================================
# SECTION 8: AESTHETIC ~ NATURALNESS CONCORDANCE
# =============================================================================

message("Building concordance tables...")

#' Concordance contingency table with odds ratio
#' @param data Data frame with binary aesthetic and naturalness columns.
#' @param aesthetic_var,naturalness_var Column names.
#' @param cohort_label Character label for the title.
#' @return Styled gt_tbl.
make_concordance_table <- function(data, aesthetic_var, naturalness_var, cohort_label) {
  tab    <- table(Aesthetic   = data[[aesthetic_var]],
                  Naturalness = data[[naturalness_var]])
  or_res <- epitools::oddsratio(tab)
  or_row <- rownames(or_res$measure)[2]
  OR     <- round(as.numeric(or_res$measure[or_row, "estimate"]), 2)
  CI_lo  <- round(as.numeric(or_res$measure[or_row, "lower"]),    2)
  CI_hi  <- round(as.numeric(or_res$measure[or_row, "upper"]),    2)
  pval   <- signif(as.numeric(or_res$p.value[or_row, "chi.square"]), 3)
  footer <- glue("OR: **{OR}** (95% CI: {CI_lo}\u2013{CI_hi}); *p* = {pval}")

  df <- as.data.frame.matrix(tab)
  df$Group             <- c("Low Aesthetic (<4)", "High Aesthetic (\u22654)")
  df$`Row Total`       <- rowSums(df[, 1:2])
  df$`% High Natural`  <- round(100 * df[, "1"] / df$`Row Total`, 1)
  colnames(df)[1:2]    <- c("Low Naturalness", "High Naturalness")

  df %>%
    dplyr::select(Group, everything()) %>%
    gt() %>%
    tab_header(title = md(glue("**{cohort_label}: Aesthetic vs Naturalness**"))) %>%
    fmt_percent(columns = `% High Natural`, scale_values = FALSE) %>%
    gt_style(source_note = md(footer))
}

# Iterate over both cohorts — no duplication
concordance_specs <- tribble(
  ~cohort,    ~label,
  "surgeon",  "Surgeon",
  "lay",      "Lay Person"
)

walk(seq_len(nrow(concordance_specs)), function(i) {
  spec <- concordance_specs[i, ]
  tbl  <- make_concordance_table(
    data_list[[spec$cohort]], "Aesthetic_bin", "Naturalness_bin", spec$label
  )
  save_gt_table(tbl, glue("{spec$cohort}_concordance_table.png"))
})

# Polychoric correlations (both cohorts in one map call)
poly_results <- concordance_specs %>%
  mutate(
    poly_r = map_dbl(cohort, function(ch) {
      d   <- data_list[[ch]]
      rho <- psych::polychoric(d[, c("AestheticScore_num", "NaturalScore_num")])$rho
      rho[1, 2]
    })
  )

message("Polychoric correlations (aesthetic ~ naturalness):")
print(poly_results)

message("04_outputs.R complete. All outputs saved.")
