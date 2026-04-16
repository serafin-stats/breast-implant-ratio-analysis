# =============================================================================
# 05_supplemental_outputs.R
# =============================================================================

source("R/utils.R")

suppressPackageStartupMessages({
  library(qs2)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(gridExtra)
  library(grid)
  library(gt)
  library(lme4)
  library(tibble)
  library(glue)
  library(scales)
})

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables",  showWarnings = FALSE, recursive = TRUE)

THESIS_MODELS <- "/Users/lserafin/Documents/PM 516 Stat Consulting/Models"

message("Loading data...")
surgeon_data <- qs_read("data/processed/surgeon_data.qs")
lay_data     <- qs_read("data/processed/lay_data.qs")


# =============================================================================
# SECTION 1: DAG
# =============================================================================

message("Building DAG...")

nodes <- data.frame(
  name  = c("Country","Surgeon","Method","BreastShape","Ratings","Rater"),
  x     = c(0, 0, 0, 0, 0, 2.8),
  y     = c(5, 3.5, 2, 0.5, -1, -1),
  label = c(
    "Country /\nCultural Norms",
    "Surgeon Skill /\nTechnique / Style",
    "Method\n(Sub-glandular vs\nSub-muscular)",
    "Postoperative\nBreast Shape\n(geometric metrics)",
    "Aesthetic /\nNaturalness Ratings",
    "Rater\n(Surgeon / Public)"
  ),
  color      = c("#990000","#990000","#990000","#FFCC00","#908C13","#908C13"),
  text_color = c("white","white","white","#333333","white","white"),
  stringsAsFactors = FALSE
)
edges <- data.frame(
  x    = c(0,    0,    0,    0,    2.8),
  y    = c(4.58, 3.08, 1.58, 0.08, -0.58),
  xend = c(0,    0,    0,    0,    0),
  yend = c(3.92, 2.42, 0.92, -0.58, -0.58)
)
legend_df <- data.frame(
  x     = c(-2.2, -2.2, -2.2),
  y     = c(0.5,  -0.2, -0.9),
  color = c("#990000","#FFCC00","#908C13"),
  label = c("Perfectly collinear block\n(confounder)",
            "Primary predictor", "Outcome / rater"),
  stringsAsFactors = FALSE
)

dag_plot <- ggplot() +
  annotate("rect",
           xmin = -1.4, xmax = 1.4, ymin = 1.2, ymax = 5.8,
           linetype = "dashed", color = "#990000", fill = "#990000",
           alpha = 0.04, linewidth = 0.7) +
  annotate("text", x = 1.45, y = 5.75,
           label = "Perfectly collinear\n(cannot be separated)",
           hjust = 0, vjust = 1, size = 3, color = "#990000", fontface = "italic") +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(9,"pt"), type = "closed"),
               color = "#444444", linewidth = 0.7) +
  geom_point(data = nodes, aes(x = x, y = y), color = nodes$color, size = 36) +
  geom_text(data  = nodes, aes(x = x, y = y, label = label),
            color = nodes$text_color, size = 2.8, fontface = "bold", lineheight = 0.88) +
  geom_point(data = legend_df, aes(x = x, y = y), color = legend_df$color, size = 5) +
  geom_text(data  = legend_df, aes(x = x + 0.25, y = y, label = label),
            hjust = 0, size = 2.7, color = "#333333", lineheight = 0.9) +
  coord_cartesian(xlim = c(-2.8, 5), ylim = c(-2.2, 6.8)) +
  labs(title = "Conceptual DAG \u2014 Unadjusted") +
  theme_void() +
  theme(plot.title  = element_text(size = 12, hjust = 0.5, face = "bold",
                                   margin = margin(b = 10)),
        plot.margin = margin(20, 20, 20, 20))

save_figure(dag_plot, "dag_unadjusted.png", width = 8, height = 10)
message("Saved: dag_unadjusted.png")


# =============================================================================
# SECTION 2: ICC BAR CHART
# =============================================================================

message("Building ICC bar chart...")

icc_values <- tibble(
  Cohort   = c("Surgeon","Surgeon","Surgeon","Surgeon",
               "Lay","Lay","Lay","Lay"),
  Outcome  = c("Aesthetic","Naturalness","Aesthetic","Naturalness",
               "Aesthetic","Naturalness","Aesthetic","Naturalness"),
  ICC_Type = c("Rater","Rater","Image","Image",
               "Rater","Rater","Image","Image"),
  ICC      = c(0.2683891, 0.2786646, 0.2826084, 0.3429616,
               0.52718,   0.4814837, 0.009356966, 0.005385191)
)

icc_df <- tidyr::expand_grid(
  Cohort   = c("Surgeon","Lay"),
  Outcome  = c("Aesthetic","Naturalness"),
  ICC_Type = c("Rater","Image")
) %>% left_join(icc_values, by = c("Cohort","Outcome","ICC_Type"))

icc_bar_chart <- ggplot(icc_df, aes(x = Outcome, y = ICC, fill = Cohort)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "darkgray") +
  annotate("text", x = 1.5, y = 0.42,
           label = "moderate threshold", hjust = 0, size = 4, color = "darkgray") +
  geom_text(aes(label = round(ICC, 2)),
            position = position_dodge(width = 0.7), vjust = -0.4, size = 3.5) +
  facet_wrap(~ ICC_Type) +
  scale_fill_manual(values = c("Surgeon" = "#619CFF", "Lay" = "#F8766D")) +
  labs(x = "Outcome", y = "ICC Value", fill = "Cohort",
       caption = "ICC = Intraclass Correlation Coefficient; values closer to 1 indicate greater agreement.") +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_text(size = 11), axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0, color = "gray40"),
        plot.caption.position = "plot")

save_figure(icc_bar_chart, "icc_bar_chart.png", width = 10, height = 5)
message("Saved: icc_bar_chart.png")


# =============================================================================
# SECTION 3: MEAN IMAGE SCATTER PLOTS  (Figure 13 style)
# =============================================================================

message("Building mean image scatter plots...")

make_mean_scatter <- function(score_var, x_label, y_label, title,
                              xlim = c(2.0, 4.3), ylim = c(2.0, 4.3)) {
  surg_m <- surgeon_data %>%
    group_by(PatientID) %>%
    summarise(mean_surg = mean(.data[[score_var]], na.rm = TRUE), .groups = "drop")
  lay_m <- lay_data %>%
    group_by(PatientID) %>%
    summarise(mean_lay = mean(.data[[score_var]], na.rm = TRUE), .groups = "drop")
  comb <- left_join(surg_m, lay_m, by = "PatientID")
  r    <- cor(comb$mean_surg, comb$mean_lay, use = "complete.obs")
  ggplot(comb, aes(x = mean_surg, y = mean_lay)) +
    geom_point(size = 3, alpha = 0.8, color = "black") +
    geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 1) +
    xlim(xlim[1], xlim[2]) + ylim(ylim[1], ylim[2]) +
    labs(title = title,
         subtitle = paste0("Pearson Correlation = ", round(r, 3)),
         x = x_label, y = y_label) +
    theme_minimal(base_size = 14) +
    theme(plot.title    = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 13, face = "italic"),
          axis.title    = element_text(size = 12), axis.text = element_text(size = 10),
          panel.grid.minor = element_blank())
}

aes_scatter <- make_mean_scatter("AestheticScore_num",
  "Mean Aesthetic Rating (Surgeons)", "Mean Aesthetic Rating (Laypersons)",
  "Mean Image Aesthetic Ratings: Surgeon vs Layperson")
nat_scatter <- make_mean_scatter("NaturalScore_num",
  "Mean Naturalness Rating (Surgeons)", "Mean Naturalness Rating (Laypersons)",
  "Mean Image Naturalness Ratings: Surgeon vs Layperson")
scatter_panel <- grid.arrange(aes_scatter, nat_scatter, nrow = 1,
  bottom = textGrob("Aesthetic (left) and Naturalness (right)",
                    gp = gpar(fontsize = 10, fontface = "italic")))

save_figure(scatter_panel, "mean_image_scatter_panel.png", width = 12, height = 5)
save_figure(aes_scatter,   "mean_image_scatter_aes.png",   width = 6,  height = 5)
save_figure(nat_scatter,   "mean_image_scatter_nat.png",   width = 6,  height = 5)
message("Saved: mean image scatter plots")


# =============================================================================
# SECTION 4: VIOLIN PLOTS
# =============================================================================

message("Building violin plots...")

make_violin <- function(data, rating_var, x_label, cohort_label) {
  ggplot(data, aes(x = .data[[rating_var]], y = RATIO_Post_Op)) +
    geom_violin(fill = "#c6dbef", color = "#6baed6", alpha = 0.8, trim = FALSE) +
    geom_boxplot(width = 0.15, outlier.alpha = 0.25, fill = "white") +
    stat_summary(fun = median, geom = "point", size = 2.5, color = "black") +
    labs(x = paste(x_label, "\u2014", cohort_label),
         y = "S\u2013IMF / S\u2013Cubital Ratio (Post-Op)") +
    theme_minimal(base_size = 12) +
    theme(axis.title = element_text(size = 10), axis.text = element_text(size = 9),
          axis.text.x = element_text(angle = 30, hjust = 1),
          panel.grid.minor = element_blank())
}

save_figure(
  grid.arrange(make_violin(surgeon_data,"Aesthetic","Aesthetic Rating","Surgeon"),
               make_violin(lay_data,    "Aesthetic","Aesthetic Rating","Lay Person"), nrow=1),
  "violin_postop_aes.png", width = 12, height = 5)
save_figure(
  grid.arrange(make_violin(surgeon_data,"Naturalness","Naturalness Rating","Surgeon"),
               make_violin(lay_data,    "Naturalness","Naturalness Rating","Lay Person"), nrow=1),
  "violin_postop_nat.png", width = 12, height = 5)
message("Saved: violin plots")


# =============================================================================
# SECTION 5: PREDICTED PROBABILITY CURVES
#
# Ported directly from make_predicted_probability_object() in the original QMD.
# Uses fixef() + vcov() for population-level predictions and delta method CIs.
# Variable names per cohort match the QMD exactly (lay uses _z suffix for
# ratio predictors; surgeon uses _centered suffix).
# =============================================================================

message("Building predicted probability curves...")

#' Load a model from the original thesis Models folder
load_thesis <- function(fn) {
  path <- file.path(THESIS_MODELS, fn)
  if (!file.exists(path)) { warning("Not found: ", path); return(NULL) }
  readRDS(path)
}

#' Compute PP object from a glmerMod binary model.
#' Matches make_predicted_probability_object() from the original QMD exactly.
#'
#' @param model        Fitted glmerMod (family = binomial).
#' @param data         Analysis data frame.
#' @param raw_var      Column name of raw predictor in data.
#' @param model_var    Column name of model-scale predictor (as in formula).
#' @param sq_var       Column name of squared term (as in formula).
#' @param method_level Reference level for Method.x. Default "Below the Muscle".
#' @param months_var   Months covariate name. Default "Months.Post.Op.x".
#' @param raw_to_model   Function: raw -> model scale.
#' @param model_to_raw   Function: model -> raw scale (for vertex).
#' @param raw_to_display Function: raw -> x-axis display scale.
#' @param n_points     Grid resolution. Default 200.
make_pp_glmer <- function(model, data,
                          raw_var, model_var, sq_var,
                          method_level = "Below the Muscle",
                          months_var   = "Months.Post.Op",
                          raw_to_model   = identity,
                          model_to_raw   = identity,
                          raw_to_display = identity,
                          n_points = 200) {

  raw_min  <- min(data[[raw_var]], na.rm = TRUE)
  raw_max  <- max(data[[raw_var]], na.rm = TRUE)
  raw_seq  <- seq(raw_min, raw_max, length.out = n_points)
  mod_seq  <- raw_to_model(raw_seq)
  disp_seq <- raw_to_display(raw_seq)

  newdata <- data.frame(
    raw_value     = raw_seq,
    display_value = disp_seq,
    stringsAsFactors = FALSE
  )
  newdata[[model_var]] <- mod_seq
  newdata[[sq_var]]    <- mod_seq^2

  # Only add covariates that are actually in the model formula
  model_terms <- tryCatch(
    attr(terms(lme4::nobars(formula(model))), "term.labels"),
    error = function(e) character(0)
  )
  if (months_var %in% model_terms) {
    newdata[[months_var]] <- mean(data[[months_var]], na.rm = TRUE)
  }
  if ("Method" %in% model_terms) {
    newdata[["Method"]] <- factor(method_level, levels = levels(data[["Method"]]))
  }

  # Fixed-effect design matrix
  fixed_form <- tryCatch({
    ff <- lme4::nobars(formula(model))
    stats::delete.response(terms(ff))
  }, error = function(e) NULL)

  beta <- lme4::fixef(model)
  V    <- as.matrix(vcov(model))

  X      <- model.matrix(fixed_form, newdata)
  eta    <- as.vector(X %*% beta)
  se_eta <- sqrt(pmax(0, diag(X %*% V %*% t(X))))

  newdata$pred_prob  <- plogis(eta)
  newdata$conf.low   <- plogis(eta - 1.96 * se_eta)
  newdata$conf.high  <- plogis(eta + 1.96 * se_eta)

  # Vertex / turning point
  beta1 <- tryCatch(unname(beta[model_var]), error = function(e) NA_real_)
  beta2 <- tryCatch(unname(beta[sq_var]),    error = function(e) NA_real_)

  vertex_model <- if (!is.na(beta1) && !is.na(beta2) && beta2 != 0)
    -beta1 / (2 * beta2) else NA_real_
  vertex_raw   <- if (!is.na(vertex_model)) model_to_raw(vertex_model) else NA_real_
  in_range     <- !is.na(vertex_raw) && is.finite(vertex_raw) &&
                  vertex_raw >= raw_min && vertex_raw <= raw_max
  shape        <- if (!is.na(beta2))
    ifelse(beta2 < 0, "Concave down (maximum)", "Concave up (minimum)")
  else NA_character_

  list(
    grid = newdata,
    summary = data.frame(
      predictor        = raw_var,
      beta_linear      = beta1,
      beta_quadratic   = beta2,
      shape            = shape,
      vertex_display   = if (!is.na(vertex_raw)) raw_to_display(vertex_raw) else NA_real_,
      within_obs_range = in_range,
      stringsAsFactors = FALSE
    )
  )
}


#' Plot a single PP panel
plot_pp <- function(pred_obj, x_label, y_label,
                    show_y_axis = TRUE, y_limits = NULL) {
  p <- ggplot(pred_obj$grid, aes(x = display_value, y = pred_prob)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                fill = "gray70", alpha = 0.25) +
    geom_line(linewidth = 1) +
    labs(x = x_label,
         y = if (show_y_axis) y_label else NULL,
         subtitle = "Gray shaded band represents 95% confidence intervals") +
    theme_minimal(base_size = 12) +
    theme(plot.subtitle    = element_text(size = 9, hjust = 0.5),
          axis.title       = element_text(size = 10),
          axis.text        = element_text(size = 9),
          panel.grid.minor = element_blank())
  if (!is.null(y_limits)) p <- p + coord_cartesian(ylim = y_limits)
  if (!show_y_axis)        p <- p + theme(axis.title.y = element_blank())
  p
}


#' Two-panel figure: A. Surgeon | B. Lay
make_two_panel_pp <- function(surg_pred, lay_pred, x_label, y_label,
                              y_limits = NULL) {
  p_surg <- plot_pp(surg_pred, x_label, y_label, show_y_axis = TRUE,  y_limits = y_limits)
  p_lay  <- plot_pp(lay_pred,  x_label, y_label, show_y_axis = FALSE, y_limits = y_limits)
  surg_grob <- arrangeGrob(
    textGrob("A. Surgeon Cohort", x = 0, hjust = 0,
             gp = gpar(fontsize = 11, fontface = "bold")),
    p_surg, ncol = 1, heights = c(0.08, 1))
  lay_grob <- arrangeGrob(
    textGrob("B. Lay Cohort", x = 0, hjust = 0,
             gp = gpar(fontsize = 11, fontface = "bold")),
    p_lay, ncol = 1, heights = c(0.08, 1))
  grid.arrange(surg_grob, lay_grob, ncol = 2)
}


# =============================================================================
# Scale parameters — match QMD exactly
# upper_prop:   surgeon_data mean/sd (same patients, identical to lay_data)
# ratio_postop: sd from pct_centered, mean from raw RATIO_Post_Op
# ratio_diff:   sd from pct_centered, mean_pct from RATIO_DIFFERENCE * 100
# =============================================================================

upper_prop_mean    <- mean(surgeon_data$upper_prop, na.rm = TRUE)
upper_prop_sd      <- sd(surgeon_data$upper_prop,   na.rm = TRUE)

ratio_postop_sd    <- sd(lay_data$RATIO_Post_Op_pct_centered, na.rm = TRUE)
ratio_postop_mean  <- mean(lay_data$RATIO_Post_Op, na.rm = TRUE)

ratio_diff_sd      <- sd(lay_data$RATIO_DIFF_pct_centered, na.rm = TRUE)
ratio_diff_mean_pct <- mean(lay_data$RATIO_DIFFERENCE * 100, na.rm = TRUE)


# =============================================================================
# Build all 12 PP objects
# Note asymmetric variable names between cohorts for ratio predictors
# =============================================================================

# ---- Upper Proportion (symmetric — both cohorts use _z) --------------------
surg_aes_upper_pred <- make_pp_glmer(
  load_thesis("surg_binary_uni_aes_upper.rds"), surgeon_data,
  raw_var = "upper_prop", model_var = "upper_prop_z", sq_var = "upper_prop_z_sq",
  raw_to_model   = function(x) (x - upper_prop_mean) / upper_prop_sd,
  model_to_raw   = function(z) z * upper_prop_sd + upper_prop_mean,
  raw_to_display = identity)

lay_aes_upper_pred <- make_pp_glmer(
  load_thesis("lay_binary_uni_aes_upper.rds"), lay_data,
  raw_var = "upper_prop", model_var = "upper_prop_z", sq_var = "upper_prop_z_sq",
  raw_to_model   = function(x) (x - upper_prop_mean) / upper_prop_sd,
  model_to_raw   = function(z) z * upper_prop_sd + upper_prop_mean,
  raw_to_display = identity)

surg_nat_upper_pred <- make_pp_glmer(
  load_thesis("surg_binary_uni_nat_upper.rds"), surgeon_data,
  raw_var = "upper_prop", model_var = "upper_prop_z", sq_var = "upper_prop_z_sq",
  raw_to_model   = function(x) (x - upper_prop_mean) / upper_prop_sd,
  model_to_raw   = function(z) z * upper_prop_sd + upper_prop_mean,
  raw_to_display = identity)

lay_nat_upper_pred <- make_pp_glmer(
  load_thesis("lay_binary_uni_nat_upper.rds"), lay_data,
  raw_var = "upper_prop", model_var = "upper_prop_z", sq_var = "upper_prop_z_sq",
  raw_to_model   = function(x) (x - upper_prop_mean) / upper_prop_sd,
  model_to_raw   = function(z) z * upper_prop_sd + upper_prop_mean,
  raw_to_display = identity)

# ---- Post-Op Ratio (asymmetric variable names) ------------------------------
# Surgeon: model uses RATIO_Post_Op_pct_centered (centred only, not standardised)
# Lay:     model uses RATIO_Post_Op_pct_z        (z-standardised)

surg_aes_ratio_pred <- make_pp_glmer(
  load_thesis("surg_binary_uni_aes_postop.rds"), surgeon_data,
  raw_var        = "RATIO_Post_Op_pct_centered",
  model_var      = "RATIO_Post_Op_pct_centered",
  sq_var         = "RATIO_Post_Op_pct_centered_sq",
  raw_to_model   = function(x) x / ratio_postop_sd,
  model_to_raw   = function(z) z * ratio_postop_sd,
  raw_to_display = function(x) (x / 100) + ratio_postop_mean)

lay_aes_ratio_pred <- make_pp_glmer(
  load_thesis("lay_binary_uni_aes_postop.rds"), lay_data,
  raw_var        = "RATIO_Post_Op_pct_centered",
  model_var      = "RATIO_Post_Op_pct_z",
  sq_var         = "RATIO_Post_Op_pct_z_sq",
  raw_to_model   = function(x) x / ratio_postop_sd,
  model_to_raw   = function(z) z * ratio_postop_sd,
  raw_to_display = function(x) (x / 100) + ratio_postop_mean)

surg_nat_ratio_pred <- make_pp_glmer(
  load_thesis("surg_binary_uni_nat_postop.rds"), surgeon_data,
  raw_var        = "RATIO_Post_Op_pct_centered",
  model_var      = "RATIO_Post_Op_pct_centered",
  sq_var         = "RATIO_Post_Op_pct_centered_sq",
  raw_to_model   = function(x) x / ratio_postop_sd,
  model_to_raw   = function(z) z * ratio_postop_sd,
  raw_to_display = function(x) (x / 100) + ratio_postop_mean)

lay_nat_ratio_pred <- make_pp_glmer(
  load_thesis("lay_binary_uni_nat_postop.rds"), lay_data,
  raw_var        = "RATIO_Post_Op_pct_centered",
  model_var      = "RATIO_Post_Op_pct_z",
  sq_var         = "RATIO_Post_Op_pct_z_sq",
  raw_to_model   = function(x) x / ratio_postop_sd,
  model_to_raw   = function(z) z * ratio_postop_sd,
  raw_to_display = function(x) (x / 100) + ratio_postop_mean)

# ---- Ratio Difference (asymmetric variable names) ---------------------------
# Surgeon: model uses RATIO_DIFF_pct_centered (centred only)
# Lay:     model uses RATIO_DIFF_pct_z        (z-standardised)

surg_aes_rdiff_pred <- make_pp_glmer(
  load_thesis("surg_binary_uni_aes_ratiodiff.rds"), surgeon_data,
  raw_var        = "RATIO_DIFF_pct_centered",
  model_var      = "RATIO_DIFF_pct_centered",
  sq_var         = "RATIO_DIFF_pct_centered_sq",
  raw_to_model   = function(x) x / ratio_diff_sd,
  model_to_raw   = function(z) z * ratio_diff_sd,
  raw_to_display = function(x) (x + ratio_diff_mean_pct) / 100)

lay_aes_rdiff_pred <- make_pp_glmer(
  load_thesis("lay_binary_uni_aes_ratiodiff.rds"), lay_data,
  raw_var        = "RATIO_DIFF_pct_centered",
  model_var      = "RATIO_DIFF_pct_z",
  sq_var         = "RATIO_DIFF_pct_z_sq",
  raw_to_model   = function(x) x / ratio_diff_sd,
  model_to_raw   = function(z) z * ratio_diff_sd,
  raw_to_display = function(x) (x + ratio_diff_mean_pct) / 100)

surg_nat_rdiff_pred <- make_pp_glmer(
  load_thesis("surg_binary_uni_nat_ratiodiff.rds"), surgeon_data,
  raw_var        = "RATIO_DIFF_pct_centered",
  model_var      = "RATIO_DIFF_pct_centered",
  sq_var         = "RATIO_DIFF_pct_centered_sq",
  raw_to_model   = function(x) x / ratio_diff_sd,
  model_to_raw   = function(z) z * ratio_diff_sd,
  raw_to_display = function(x) (x + ratio_diff_mean_pct) / 100)

lay_nat_rdiff_pred <- make_pp_glmer(
  load_thesis("lay_binary_uni_nat_ratiodiff.rds"), lay_data,
  raw_var        = "RATIO_DIFF_pct_centered",
  model_var      = "RATIO_DIFF_pct_z",
  sq_var         = "RATIO_DIFF_pct_z_sq",
  raw_to_model   = function(x) x / ratio_diff_sd,
  model_to_raw   = function(z) z * ratio_diff_sd,
  raw_to_display = function(x) (x + ratio_diff_mean_pct) / 100)


# =============================================================================
# Save all six two-panel PP figures — y-limits match QMD exactly
# =============================================================================

aes_y <- "Predicted Probability of High Aesthetic Score"
nat_y <- "Predicted Probability of High Naturalness Score"

pp_specs <- list(
  list(s=surg_aes_upper_pred, l=lay_aes_upper_pred,
       x="Upper Proportion",     y=aes_y, ylim=c(0.20,0.85), fn="pp_fig_upper_aes.png"),
  list(s=surg_nat_upper_pred, l=lay_nat_upper_pred,
       x="Upper Proportion",     y=nat_y, ylim=c(0.00,0.75), fn="pp_fig_upper_nat.png"),
  list(s=surg_aes_ratio_pred, l=lay_aes_ratio_pred,
       x="Post-Operative Ratio", y=aes_y, ylim=c(0.55,0.95), fn="pp_fig_ratio_aes.png"),
  list(s=surg_nat_ratio_pred, l=lay_nat_ratio_pred,
       x="Post-Operative Ratio", y=nat_y, ylim=c(0.05,0.75), fn="pp_fig_ratio_nat.png"),
  list(s=surg_aes_rdiff_pred, l=lay_aes_rdiff_pred,
       x="Ratio Difference",     y=aes_y, ylim=c(0.45,0.95), fn="pp_fig_rdiff_aes.png"),
  list(s=surg_nat_rdiff_pred, l=lay_nat_rdiff_pred,
       x="Ratio Difference",     y=nat_y, ylim=c(0.25,0.75), fn="pp_fig_rdiff_nat.png")
)

walk(pp_specs, function(sp) {
  fig <- tryCatch(
    make_two_panel_pp(sp$s, sp$l, sp$x, sp$y, sp$ylim),
    error = function(e) { message("Failed [", sp$fn, "]: ", e$message); NULL }
  )
  if (!is.null(fig)) {
    ggsave(file.path("outputs","figures", sp$fn),
           plot = fig, width = 10, height = 5, dpi = 300, bg = "white")
    message("Saved: outputs/figures/", sp$fn)
  }
})


# =============================================================================
# SECTION 6: ESTIMATED TURNING POINT TABLES
# =============================================================================

message("Building turning point tables...")

make_turning_point_table <- function(pred_list, title) {
  bind_rows(imap(pred_list, function(pred, label) {
    parts <- strsplit(label, " \u2014 ")[[1]]
    pred$summary %>% mutate(`Rater Group` = parts[1], Outcome = parts[2])
  })) %>%
    dplyr::select(`Rater Group`, Outcome, shape, vertex_display, within_obs_range) %>%
    rename(`Curve Shape`           = shape,
           `Turning Point`         = vertex_display,
           `Within Observed Range` = within_obs_range) %>%
    mutate(`Turning Point` = round(`Turning Point`, 3)) %>%
    gt() %>%
    tab_header(title = md(glue("**{title}**"))) %>%
    cols_label(`Rater Group` = "Rater Group", Outcome = "Outcome",
               `Curve Shape` = "Curve Shape", `Turning Point` = "Turning Point",
               `Within Observed Range` = "Within Observed Range") %>%
    gt_style(source_note = paste(
      "Turning point = x = -b/(2a) from binary unadjusted model.",
      "Values outside observed range are not interpreted as meaningful optima."
    ))
}

save_gt_table(make_turning_point_table(list(
  "Layperson \u2014 Aesthetic"   = lay_aes_ratio_pred,
  "Layperson \u2014 Naturalness" = lay_nat_ratio_pred,
  "Surgeon \u2014 Aesthetic"     = surg_aes_ratio_pred,
  "Surgeon \u2014 Naturalness"   = surg_nat_ratio_pred),
  "Estimated Turning Points for Post-Op Ratio"),
  "turning_point_postop.png")

save_gt_table(make_turning_point_table(list(
  "Layperson \u2014 Aesthetic"   = lay_aes_rdiff_pred,
  "Layperson \u2014 Naturalness" = lay_nat_rdiff_pred,
  "Surgeon \u2014 Aesthetic"     = surg_aes_rdiff_pred,
  "Surgeon \u2014 Naturalness"   = surg_nat_rdiff_pred),
  "Estimated Turning Points for Ratio Difference"),
  "turning_point_ratiodiff.png")

save_gt_table(make_turning_point_table(list(
  "Layperson \u2014 Aesthetic"   = lay_aes_upper_pred,
  "Layperson \u2014 Naturalness" = lay_nat_upper_pred,
  "Surgeon \u2014 Aesthetic"     = surg_aes_upper_pred,
  "Surgeon \u2014 Naturalness"   = surg_nat_upper_pred),
  "Estimated Turning Points for Upper Proportion"),
  "turning_point_upper.png")

message("05_supplemental_outputs.R complete.")
message("Figures -> outputs/figures/ | Tables -> outputs/tables/")
