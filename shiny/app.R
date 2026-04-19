# =============================================================================
# app.R — Breast Augmentation Ratio Analysis: Interactive Dashboard
# Keck School of Medicine, USC | Casandra Serafin
#
# Significant quadratic turning points (shown in UI):
#   Surgeon × Aesthetic   × Upper Proportion (p = 0.006)
#   Surgeon × Naturalness × Upper Proportion (p = 0.011)
#   All other predictors / cohort / outcome combos: NS — no turning point shown
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(qs2)

# =============================================================================
# USC COLORS
# =============================================================================
USC_CARDINAL <- "#990000"
USC_GOLD     <- "#FFCC00"
USC_BLUE     <- "#2B5597"
USC_CORAL    <- "#F26178"
USC_ORANGE   <- "#FF9015"
USC_OLIVE    <- "#908C13"
GRAY_30      <- "#CCCCCC"
GRAY_70      <- "#767676"
COHORT_COLORS <- c("Surgeon" = USC_BLUE, "Lay" = USC_CORAL)

# =============================================================================
# SIGNIFICANCE TABLE
# TRUE = quadratic term significant, turning point meaningful to report
# =============================================================================
SIGNIFICANT_QUADRATIC <- list(
  "Surgeon|Aesthetic|Upper Proportion"   = TRUE,
  "Surgeon|Naturalness|Upper Proportion" = TRUE
)

is_significant <- function(cohort, outcome, predictor) {
  key <- paste(cohort, outcome, predictor, sep = "|")
  isTRUE(SIGNIFICANT_QUADRATIC[[key]])
}

# =============================================================================
# CONFIG & DATA
# Paths use ../ because runApp('shiny') sets wd to shiny/
# =============================================================================

THESIS_MODELS <- "models"

# Replace the two qs_read lines with this:
surgeon_data <- tryCatch({
  qs2::qs_read("data/processed/surgeon_data.qs")
}, error = function(e) {
  message("qs2::qs_read failed for surgeon: ", e$message)
  tryCatch({
    qs::qread("data/processed/surgeon_data.qs")
  }, error = function(e2) {
    message("qs::qread also failed for surgeon: ", e2$message)
    NULL
  })
})

lay_data <- tryCatch({
  qs2::qs_read("data/processed/lay_data.qs")
}, error = function(e) {
  message("qs2::qs_read failed for lay: ", e$message)
  tryCatch({
    qs::qread("data/processed/lay_data.qs")
  }, error = function(e2) {
    message("qs::qread also failed for lay: ", e2$message)
    NULL
  })
})


# =============================================================================
# MODEL HELPERS
# =============================================================================

load_thesis_model <- function(fn) {
  if (is.null(THESIS_MODELS)) return(NULL)
  path <- file.path(THESIS_MODELS, fn)
  if (!file.exists(path)) {
    message("Model not found: ", path)
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(e) { message(e$message); NULL })
}

model_filename <- function(cohort, outcome, predictor) {
  paste0(
    if (cohort == "Surgeon") "surg" else "lay",
    "_binary_uni_",
    if (outcome == "Aesthetic") "aes" else "nat", "_",
    switch(predictor,
           "Upper Proportion" = "upper",
           "Post-Op Ratio"    = "postop",
           "Ratio Difference" = "ratiodiff"
    ), ".rds"
  )
}

# =============================================================================
# PP COMPUTATION
# =============================================================================

make_pp_glmer <- function(model, data, raw_var, model_var, sq_var,
                          raw_to_model = identity, model_to_raw = identity,
                          raw_to_display = identity, n_points = 200) {
  raw_min <- min(data[[raw_var]], na.rm = TRUE)
  raw_max <- max(data[[raw_var]], na.rm = TRUE)
  raw_seq <- seq(raw_min, raw_max, length.out = n_points)
  mod_seq <- raw_to_model(raw_seq)

  newdata <- data.frame(
    raw_value     = raw_seq,
    display_value = raw_to_display(raw_seq),
    stringsAsFactors = FALSE
  )
  newdata[[model_var]] <- mod_seq
  newdata[[sq_var]]    <- mod_seq^2

  if ("Months.Post.Op" %in% names(data))
    newdata[["Months.Post.Op"]] <- mean(data[["Months.Post.Op"]], na.rm = TRUE)
  if ("Method" %in% names(data))
    newdata[["Method"]] <- factor("Below the Muscle", levels = levels(data[["Method"]]))

  fixed_form <- tryCatch({
    ff <- lme4::nobars(formula(model))
    stats::delete.response(terms(ff))
  }, error = function(e) NULL)
  if (is.null(fixed_form)) return(NULL)

  beta <- lme4::fixef(model)
  V    <- as.matrix(vcov(model))
  X    <- tryCatch(model.matrix(fixed_form, newdata), error = function(e) NULL)
  if (is.null(X)) return(NULL)

  eta <- as.vector(X %*% beta)
  se  <- sqrt(pmax(0, diag(X %*% V %*% t(X))))

  newdata$pred_prob <- plogis(eta)
  newdata$conf_low  <- plogis(eta - 1.96 * se)
  newdata$conf_high <- plogis(eta + 1.96 * se)

  beta1 <- tryCatch(unname(beta[model_var]), error = function(e) NA_real_)
  beta2 <- tryCatch(unname(beta[sq_var]),    error = function(e) NA_real_)
  vertex_model <- if (!is.na(beta1) && !is.na(beta2) && beta2 != 0)
    -beta1 / (2 * beta2) else NA_real_
  vertex_raw     <- if (!is.na(vertex_model)) model_to_raw(vertex_model) else NA_real_
  vertex_display <- if (!is.na(vertex_raw))   raw_to_display(vertex_raw) else NA_real_
  in_range <- !is.na(vertex_raw) && is.finite(vertex_raw) &&
              vertex_raw >= raw_min && vertex_raw <= raw_max

  list(
    grid     = newdata,
    vertex   = vertex_display,
    in_range = in_range,
    shape    = if (!is.na(beta2)) ifelse(beta2 < 0, "maximum", "minimum") else NA_character_
  )
}

get_pp_params <- function(cohort, predictor, data_surg, data_lay) {
  upper_mean <- mean(data_surg$upper_prop, na.rm = TRUE)
  upper_sd   <- sd(data_surg$upper_prop,   na.rm = TRUE)
  rp_sd      <- sd(data_lay$RATIO_Post_Op_pct_centered, na.rm = TRUE)
  rp_mean    <- mean(data_lay$RATIO_Post_Op, na.rm = TRUE)
  rd_sd      <- sd(data_lay$RATIO_DIFF_pct_centered, na.rm = TRUE)
  rd_mean    <- mean(data_lay$RATIO_DIFFERENCE * 100, na.rm = TRUE)
  is_lay     <- cohort == "Lay"

  switch(predictor,
    "Upper Proportion" = list(
      data           = if (is_lay) data_lay else data_surg,
      raw_var        = "upper_prop",
      model_var      = "upper_prop_z",
      sq_var         = "upper_prop_z_sq",
      raw_to_model   = function(x) (x - upper_mean) / upper_sd,
      model_to_raw   = function(z) z * upper_sd + upper_mean,
      raw_to_display = identity,
      x_label        = "Upper Proportion (S\u2013IMF / S\u2013Cubital)"
    ),
    "Post-Op Ratio" = list(
      data           = if (is_lay) data_lay else data_surg,
      raw_var        = "RATIO_Post_Op_pct_centered",
      model_var      = if (is_lay) "RATIO_Post_Op_pct_z"    else "RATIO_Post_Op_pct_centered",
      sq_var         = if (is_lay) "RATIO_Post_Op_pct_z_sq" else "RATIO_Post_Op_pct_centered_sq",
      raw_to_model   = function(x) x / rp_sd,
      model_to_raw   = function(z) z * rp_sd,
      raw_to_display = function(x) (x / 100) + rp_mean,
      x_label        = "Post-Operative S\u2013IMF / S\u2013Cubital Ratio"
    ),
    "Ratio Difference" = list(
      data           = if (is_lay) data_lay else data_surg,
      raw_var        = "RATIO_DIFF_pct_centered",
      model_var      = if (is_lay) "RATIO_DIFF_pct_z"       else "RATIO_DIFF_pct_centered",
      sq_var         = if (is_lay) "RATIO_DIFF_pct_z_sq"    else "RATIO_DIFF_pct_centered_sq",
      raw_to_model   = function(x) x / rd_sd,
      model_to_raw   = function(z) z * rd_sd,
      raw_to_display = function(x) (x + rd_mean) / 100,
      x_label        = "Ratio Difference (Post-Op \u2212 Pre-Op)"
    )
  )
}

# =============================================================================
# INTERPRETATION TEXT
# Only references turning point when quadratic term is significant
# =============================================================================

pp_interpretation <- function(cohort, outcome, predictor, vertex, in_range, shape) {
  cohort_lbl  <- if (cohort == "Lay") "layperson" else "surgeon"
  outcome_lbl <- tolower(outcome)
  sig         <- is_significant(cohort, outcome, predictor)

  if (sig && in_range && !is.na(vertex)) {
    direction <- if (shape == "maximum") "peaks" else "reaches its minimum"
    paste0(
      "For the ", cohort_lbl, " cohort, the quadratic term was statistically significant. ",
      "The predicted probability of a high ", outcome_lbl, " rating ", direction,
      " at an upper proportion of approximately ", round(vertex, 3),
      " \u2014 within the observed range of patient measurements. ",
      "This suggests surgeons preference is centered near the average observed upper proportion ",
      "when evaluating ", outcome_lbl, ", rather than concentrated around a sharply defined optimal value."
    )
  } else if (!sig) {
    paste0(
      "For the ", cohort_lbl, " cohort, the quadratic term for ",
      tolower(predictor), " was not statistically significant. ",
      "The curve shows the model\u2019s estimated trajectory, but no meaningful ",
      "turning point should be inferred. ",
      "This is consistent with the overall finding that ",
      tolower(predictor), " did not consistently predict ",
      outcome_lbl, " perception in this cohort."
    )
  } else {
    paste0(
      "The estimated turning point falls outside the observed data range, ",
      "indicating no clinically meaningful optimum was identified for this combination."
    )
  }
}

# =============================================================================
# ICC & CONTINGENCY DATA
# =============================================================================

icc_df <- tidyr::expand_grid(
  Cohort = c("Surgeon","Lay"), Outcome = c("Aesthetic","Naturalness"),
  ICC_Type = c("Rater","Image")
) %>% left_join(tibble::tibble(
  Cohort   = c("Surgeon","Surgeon","Surgeon","Surgeon","Lay","Lay","Lay","Lay"),
  Outcome  = c("Aesthetic","Naturalness","Aesthetic","Naturalness",
               "Aesthetic","Naturalness","Aesthetic","Naturalness"),
  ICC_Type = c("Rater","Rater","Image","Image","Rater","Rater","Image","Image"),
  ICC      = c(0.268, 0.279, 0.283, 0.343, 0.527, 0.481, 0.009, 0.005)
), by = c("Cohort","Outcome","ICC_Type"))

# =============================================================================
# DAG
# =============================================================================

make_dag_plot <- function() {
  nodes <- data.frame(
    x = c(0,0,0,0,0,2.8), y = c(5,3.5,2,0.5,-1,-1),
    label = c("Country /\nCultural Norms","Surgeon Skill /\nTechnique / Style",
              "Method\n(Sub-glandular vs\nSub-muscular)",
              "Postoperative\nBreast Shape\n(geometric metrics)",
              "Aesthetic /\nNaturalness Ratings","Rater\n(Surgeon / Public)"),
    color      = c(USC_CARDINAL,USC_CARDINAL,USC_CARDINAL,USC_GOLD,USC_OLIVE,USC_OLIVE),
    text_color = c("white","white","white","#333333","white","white"),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    x=c(0,0,0,0,2.8), y=c(4.58,3.08,1.58,0.08,-0.58),
    xend=c(0,0,0,0,0), yend=c(3.92,2.42,0.92,-0.58,-0.58)
  )
  legend_df <- data.frame(
    x=c(-2.4,-2.4,-2.4), y=c(0.5,-0.2,-0.9),
    color=c(USC_CARDINAL,USC_GOLD,USC_OLIVE),
    label=c("Perfectly collinear\n(confounder)","Primary predictor","Outcome / rater"),
    stringsAsFactors = FALSE
  )
  ggplot() +
    annotate("rect", xmin=-1.35, xmax=1.35, ymin=1.25, ymax=5.75,
             linetype="dashed", color=USC_CARDINAL, fill=USC_CARDINAL,
             alpha=0.04, linewidth=0.7) +
    annotate("text", x=1.4, y=5.7, label="Perfectly collinear\n(cannot be separated)",
             hjust=0, vjust=1, size=2.8, color=USC_CARDINAL, fontface="italic") +
    geom_segment(data=edges, aes(x=x,y=y,xend=xend,yend=yend),
                 arrow=arrow(length=unit(8,"pt"),type="closed"),
                 color="#444444", linewidth=0.7) +
    geom_point(data=nodes, aes(x=x,y=y), color=nodes$color, size=28) +
    geom_text(data=nodes, aes(x=x,y=y,label=label),
              color=nodes$text_color, size=2.5, fontface="bold", lineheight=0.9) +
    geom_point(data=legend_df, aes(x=x,y=y), color=legend_df$color, size=4) +
    geom_text(data=legend_df, aes(x=x+0.2,y=y,label=label),
              hjust=0, size=2.5, color="#333333", lineheight=0.9) +
    coord_cartesian(xlim=c(-3,4.8), ylim=c(-2.2,6.8)) +
    theme_void() +
    theme(plot.margin=margin(10,10,10,10))
}

# =============================================================================
# UI HELPERS
# =============================================================================

card_style <- "background:white; border-radius:8px; padding:1.5rem;
               border:1px solid #e0e0e0; height:100%;"

section_header <- function(title, subtitle = NULL) {
  div(style = paste0("border-left:5px solid ", USC_CARDINAL,
                     "; padding-left:1.2rem; margin-bottom:1.5rem;"),
    h2(title, style = "margin-bottom:0.3rem;"),
    if (!is.null(subtitle))
      p(subtitle, style = paste0("color:", GRAY_70, "; font-size:1.05rem; margin:0;"))
  )
}

stat_box <- function(value, label, sublabel, bg) {
  div(class = "col-md-3 mb-3",
    div(style = paste0("background:", bg, "; color:white; border-radius:8px;
                        padding:1.2rem; text-align:center; height:100%;"),
      h2(style = "margin:0; color:white;", value),
      p(style = "margin:0.3rem 0 0 0; font-weight:600;", label),
      p(style = "margin:0; opacity:0.75; font-size:0.83rem;", sublabel)
    )
  )
}




# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = tags$span(
    tags$span("Breast Augmentation Outcomes", style = "font-weight:700;"),
    tags$span(" | Keck School of Medicine, USC",
              style = "color:#FFFFFF; font-weight:400; font-size:0.85em;")
  ),
  window_title = "BA Outcomes | USC Keck",
  theme = bs_theme(
    version=5, bg="#FFFFFF", fg="#000000",
    primary=USC_CARDINAL, secondary=GRAY_70,
    base_font    = font_google("Source Sans Pro", local = FALSE),
    heading_font = font_google("Source Serif Pro", local = FALSE),
    "navbar-bg"=USC_CARDINAL,
    "navbar-light-color"="#FFFFFF",
    "navbar-light-hover-color"=USC_GOLD,
    "navbar-light-active-color"=USC_GOLD
  ),

  # ── TAB 1 ─────────────────────────────────────────────────────────────────
  nav_panel(title="Quantifying Ratings", icon=icon("circle-question"),
    div(class="container-fluid py-4",

      section_header("Study Overview",
        "Understanding the data structure and study results."),

      # Study context
      div(class="row mb-4",
        div(class="col-12",
          div(style=card_style,
            h5("About This Study",
               style=paste0("color:",USC_CARDINAL,"; margin-bottom:1rem;")),
            div(class="row",
              div(class="col-md-8",
                p(style="line-height:1.75;",
                  "There is no universally accepted metric for guiding vertical implant
                   positioning across diverse body types. Surgeons rely on anatomical
                   heuristics that do not yet account for total torso proportions and fail
                   to generalize across individual variation."),
                p(style="line-height:1.75;",
                  "This study asks a direct clinical question: ",
                  tags$strong("can a postoperative vertical proportion metric predict
                   whether surgeons and laypersons perceive a breast augmentation outcome
                   as aesthetically pleasing or natural?"),
                  " Using standardised postoperative images from ",
                  tags$strong("40 patients"),
                  ", blinded ratings from surgeon and layperson cohorts, and
                   cross-classified multilevel models, we empirically tested whether
                   a reproducible ratio range aligns with human aesthetic perception —
                   and whether aesthetic appeal and naturalness are distinct constructs."),
                div(class="row mt-3",
                  div(class="col-md-3 text-center mb-2",
                    div(style=paste0("background:",USC_CARDINAL,"; color:white;
                                      border-radius:8px; padding:1rem;"),
                      h3(style="margin:0;color:white;","40"),
                      p(style="margin:0;font-size:0.85rem;","Patient images"))),
                  div(class="col-md-3 text-center mb-2",
                    div(style=paste0("background:",USC_BLUE,"; color:white;
                                      border-radius:8px; padding:1rem;"),
                      h3(style="margin:0;color:white;","78"),
                      p(style="margin:0;font-size:0.85rem;","Surgeon raters"))),
                  div(class="col-md-3 text-center mb-2",
                    div(style=paste0("background:",USC_CORAL,"; color:white;
                                      border-radius:8px; padding:1rem;"),
                      h3(style="margin:0;color:white;","243"),
                      p(style="margin:0;font-size:0.85rem;","Lay raters"))),
                  div(class="col-md-3 text-center mb-2",
                    div(style=paste0("background:",USC_OLIVE,"; color:white;
                                      border-radius:8px; padding:1rem;"),
                      h3(style="margin:0;color:white;","3"),
                      p(style="margin:0;font-size:0.85rem;","Ratio predictors")))
                )
              ),
              div(class="col-md-4",
                div(style="background:#f8f8f8; border-radius:8px; padding:1rem;",
                  h6("Key Findings",
                     style=paste0("color:",USC_CARDINAL,"; margin-bottom:0.8rem;")),
                  tags$ul(style="padding-left:1.1rem; margin:0; line-height:1.8;",
                    tags$li("Ratios did not consistently predict aesthetic or naturalness ratings"),
                    tags$li("Surgeons showed structured image-level agreement (ICC up to 0.33)"),
                    tags$li("Lay image-level ICC near zero — perception is individualised"),
                    tags$li("Aesthetic and naturalness are related but distinct constructs"),
                    tags$li("Submuscular placement group linked to higher surgeon ratings, but confounded by surgeon and country")
                  )
                )
              )
            )
          )
        )
      ),

      # Design table + can/can't
      div(class="row mb-4",
        div(class="col-md-5 mb-4",
          div(style=card_style,
            h5("Study Design at a Glance",
               style=paste0("color:",USC_CARDINAL,"; margin-bottom:1rem;")),
            tags$table(class="table table-bordered", style="font-size:0.95rem;",
              tags$thead(tags$tr(
                tags$th(""),
                tags$th(style=paste0("background:",USC_OLIVE,"; color:white; text-align:center;"),
                        "Surgeon 1"),
                tags$th(style=paste0("background:",USC_ORANGE,"; color:white; text-align:center;"),
                        "Surgeon 2")
              )),
              tags$tbody(
                tags$tr(tags$td(tags$strong("Country")),
                        tags$td(style="text-align:center;","Croatia"),
                        tags$td(style="text-align:center;","Australia")),
                tags$tr(tags$td(tags$strong("Method")),
                        tags$td(style="text-align:center;","Subglandular"),
                        tags$td(style="text-align:center;","Submuscular")),
                tags$tr(tags$td(tags$strong("Patients")),
                        tags$td(style="text-align:center;","20"),
                        tags$td(style="text-align:center;","20")),
                tags$tr(tags$td(tags$strong("Patient overlap")),
                        tags$td(colspan="2",
                                style="text-align:center; background:#fff3cd;",
                                tags$strong("None — zero patients in common"))),
                tags$tr(tags$td(tags$strong("Raters blinded?")),
                        tags$td(colspan="2", style="text-align:center;",
                                "Yes — method not disclosed to raters"))
              )
            ),
            p(style=paste0("color:",GRAY_70,"; font-size:0.85rem; margin:0.5rem 0 0 0;"),
              "Country, surgeon, and method are perfectly aliased —
               their individual effects cannot be statistically separated.")
          )
        ),
        div(class="col-md-7 mb-4",
          div(style=card_style,
            h5("What This Means for Interpretation",
               style=paste0("color:",USC_CARDINAL,"; margin-bottom:1rem;")),
            div(style=paste0("border-left:4px solid #28a745; padding:0.8rem 1rem;
                              background:#f8fff8; margin-bottom:1rem;
                              border-radius:0 6px 6px 0;"),
              tags$strong(style="color:#28a745;", "This means"),
              tags$ul(style="margin:0.5rem 0 0 0; padding-left:1.2rem; line-height:1.8;",
                tags$li("Whether postoperative shape metrics predict perceived aesthetics and/or naturalness"),
                tags$li("How much raters agree with each other within and across cohorts"),
                tags$li("Whether surgeons and laypersons perceive aesthetics differently"),
                tags$li("Whether aesthetic appeal and naturalness are the same construct")
              )
            ),
            div(style=paste0("border-left:4px solid ",USC_CARDINAL,"; padding:0.8rem 1rem;
                              background:#fff8f8; border-radius:0 6px 6px 0;"),
              tags$strong(style=paste0("color:",USC_CARDINAL,";"),
                           "Limitations"),
              tags$ul(style="margin:0.5rem 0 0 0; padding-left:1.2rem; line-height:1.8;",
                tags$li("Associations attributed to implant placement reflect between-group differences 
                within this dataset rather than causal effects of the surgical plane."), 
                tags$li("Surgical method effect cannot be separated from surgeon skill or country")
              )
            )
          )
        )
      ),

      # DAG + causal structure
      div(class="row",
        div(class="col-md-5 mb-4",
          div(style=card_style,
            h5("Conceptual DAG",
               style=paste0("color:",USC_CARDINAL,"; margin-bottom:0.5rem;")),
            p(style=paste0("color:",GRAY_70,"; font-size:0.82rem; margin-bottom:0.8rem;"),
              "Dashed box = variables that cannot be statistically separated."),
            plotOutput("dag_plot", height="340px")
          )
        ),
        div(class="col-md-7 mb-4",
          div(style=card_style,
            h5("The Causal Structure",
               style=paste0("color:",USC_CARDINAL,"; margin-bottom:1rem;")),
            p(style="line-height:1.75;",
              "Country of practice influences which surgeons participated. Surgeon identity
               determines surgical method because each surgeon consistently used one approach.
               Method determines postoperative breast shape, which influences aesthetic ratings.
               Because ", tags$strong("Method has no within-surgeon variation"),
              ", its effect cannot be statistically separated from all other surgeon-level
               influences: skill, patient selection, technique, and aesthetic training."),
            p(style="line-height:1.75;",
              "This is compounded by the fact that the two surgeons practice in different
               countries (Croatia and Australia), introducing country-level aesthetic norms and
               patient population differences as additional uncontrolled confounders."),
            div(style=paste0("background:#f8f8f8; border-radius:6px; padding:1rem;
                              border-left:4px solid ",GRAY_30,"; margin-top:1rem;"),
              p(style=paste0("color:",GRAY_70,"; font-size:0.88rem; margin:0; line-height:1.6;"),
                icon("circle-info"), " ",
                "This is a fundamental limitation of the observational design. 
                Models include Method as a covariate, but
                causal interpretation of the method coefficient is not possible.")
            )
          )
        )
      )
    )
  ),

  # ── TAB 2 ─────────────────────────────────────────────────────────────────
  nav_panel(title="Do Ratios Predict Ratings?", icon=icon("chart-line"),
    div(class="container-fluid py-4",

      section_header("Predicted Probability Curves",
        "Population-level predicted probability of a high rating across the observed ratio range. 95% CI shaded."),

      div(class="row",
        # Controls
        div(class="col-md-3",
          div(style=paste0("background:#f8f8f8; border-radius:8px; padding:1.5rem;
                            border-top:4px solid ",USC_CARDINAL,";"),
            h6("Controls", style=paste0("color:",USC_CARDINAL,
                                        "; font-weight:700; margin-bottom:1rem;")),
            selectInput("pp_cohort","Rater Cohort",
                        choices=c("Surgeon","Lay"), selected="Surgeon"),
            selectInput("pp_outcome","Outcome",
                        choices=c("Aesthetic","Naturalness"), selected="Aesthetic"),
            selectInput("pp_predictor","Predictor",
                        choices=c("Upper Proportion","Post-Op Ratio","Ratio Difference"),
                        selected="Upper Proportion"),
            hr(),
            # Turning point — only rendered when significant
            uiOutput("pp_vertex_box"),
            hr(),
            # Anatomical figure from www/
            div(style="text-align:center;",
              h6("The S\u2013IMF / S\u2013Cubital Ratio",
                 style=paste0("color:",USC_CARDINAL,
                              "; margin-bottom:0.5rem; font-size:0.88rem;")),
              tags$img(src="Ratio_Proxy_outline_version2.png",
                       style="width:100%; max-width:220px; border-radius:6px;",
                       alt="Anatomical diagram: S-IMF/S-Cubital ratio"),
              p(style=paste0("color:",GRAY_70,
                             "; font-size:0.78rem; margin:0.5rem 0 0 0; line-height:1.5;"),
                "S = Sternal Notch | IMF = Inframammary Fold |
                 Cubital = Antecubital Fossa")
            )
          )
        ),
        # Plot
        div(class="col-md-9",
          div(style=card_style,
            plotOutput("pp_plot", height="400px"),
            hr(style="margin:1rem 0;"),
            div(style=paste0("background:#f8f8f8; border-radius:6px; padding:1rem;
                              border-left:4px solid ",USC_BLUE,";"),
              p(style="margin:0; line-height:1.75;",
                textOutput("pp_interpretation", inline=TRUE))
            )
          )
        )
      )
    )
  ),

  # ── TAB 3 ─────────────────────────────────────────────────────────────────
  nav_panel(title="How Much Do Raters Agree?", icon=icon("users"),
    div(class="container-fluid py-4",

      section_header("Rater Agreement",
        "Intraclass correlation coefficients (ICC) and cross-cohort mean image rating consistency."),

      # Four ICC callouts
      div(class="row mb-4",
        stat_box("0.33","Surgeon image ICC","Naturalness | moderate agreement",USC_BLUE),
        stat_box("0.27","Surgeon rater ICC","Aesthetic | moderate self-consistency",USC_OLIVE),
        stat_box("\u22480","Lay image ICC","Near-zero — no shared image preferences",USC_CORAL),
        stat_box("0.53","Lay rater ICC","Aesthetic | self-consistent but not shared",USC_CARDINAL)
      ),

      # ICC chart + scatter
      div(class="row mb-4",
        div(class="col-md-7 mb-4",
          div(style=card_style,
            div(style="display:flex; justify-content:space-between;
                       align-items:center; margin-bottom:1rem;",
              h5("ICC by Outcome, Type, and Cohort", style="margin:0;"),
              selectInput("icc_type_filter",NULL,
                          choices=c("Both types"="both","Image-level"="Image",
                                    "Rater-level"="Rater"),
                          selected="both", width="160px")
            ),
            plotOutput("icc_plot", height="280px"),
            p(style=paste0("color:",GRAY_70,"; font-size:0.82rem; margin:0.5rem 0 0 0;"),
              "Image ICC: agreement on ", tags$em("which images"), " look better. ",
              "Rater ICC: consistency of individual raters ", tags$em("across"), " images.")
          )
        ),
        div(class="col-md-5 mb-4",
          div(style=card_style,
            div(style="display:flex; justify-content:space-between;
                       align-items:center; margin-bottom:1rem;",
              h5("Mean Image Ratings: Surgeon vs Lay", style="margin:0;"),
              selectInput("scatter_outcome",NULL,
                          choices=c("Aesthetic","Naturalness"),
                          selected="Aesthetic", width="140px")
            ),
            plotOutput("scatter_plot", height="280px"),
            p(style=paste0("color:",GRAY_70,"; font-size:0.82rem; margin:0.5rem 0 0 0;"),
              "Each point = one of the 40 patient images.")
          )
        )
      ),

      # Contingency: Aesthetic vs Naturalness
      div(class="row mb-4",
        div(class="col-12",
          div(style=card_style,
            h5("Are Aesthetic and Naturalness the Same Construct?",
               style=paste0("color:",USC_CARDINAL,"; margin-bottom:1rem;")),
            div(class="row",
              # Surgeon OR
              div(class="col-md-6 mb-3",
                div(style=paste0("border-left:5px solid ",USC_BLUE,
                                 "; padding:1rem 1.2rem; background:#f4f7fc;
                                  border-radius:0 8px 8px 0;"),
                  div(style="display:flex; align-items:center; margin-bottom:0.5rem;",
                    div(style=paste0("background:",USC_BLUE,"; color:white; border-radius:50%;
                                      width:48px; height:48px; display:flex;
                                      align-items:center; justify-content:center;
                                      font-size:1.3rem; font-weight:700;
                                      margin-right:0.8rem; flex-shrink:0;"),
                        "S"),
                    div(
                      h5(style="margin:0;", "Surgeon Cohort"),
                      p(style=paste0("color:",GRAY_70,"; margin:0; font-size:0.85rem;"),
                        "n = 1,688 ratings")
                    )
                  ),
                  div(style="text-align:center; margin:0.5rem 0;",
                    tags$span(style=paste0("font-size:2.2rem; font-weight:700;
                                            color:",USC_BLUE,";"), "OR = 11.84"),
                    p(style=paste0("color:",GRAY_70,"; font-size:0.85rem; margin:0;"),
                      "95% CI: 9.11\u201315.57 | p < 0.001")
                  ),
                  p(style="line-height:1.65; margin:0; font-size:0.9rem;",
                    "When a surgeon perceived an image as ", tags$strong("high aesthetic"),
                    ", they were ", tags$strong("~12x more likely"),
                    " to also rate it as high naturalness. ",
                    "For surgeons, these two constructs are tightly coupled.")
                )
              ),
              # Lay OR
              div(class="col-md-6 mb-3",
                div(style=paste0("border-left:5px solid ",USC_CORAL,
                                 "; padding:1rem 1.2rem; background:#fdf4f6;
                                  border-radius:0 8px 8px 0;"),
                  div(style="display:flex; align-items:center; margin-bottom:0.5rem;",
                    div(style=paste0("background:",USC_CORAL,"; color:white; border-radius:50%;
                                      width:48px; height:48px; display:flex;
                                      align-items:center; justify-content:center;
                                      font-size:1.3rem; font-weight:700;
                                      margin-right:0.8rem; flex-shrink:0;"),
                        "L"),
                    div(
                      h5(style="margin:0;", "Layperson Cohort"),
                      p(style=paste0("color:",GRAY_70,"; margin:0; font-size:0.85rem;"),
                        "n = 7,988 ratings")
                    )
                  ),
                  div(style="text-align:center; margin:0.5rem 0;",
                    tags$span(style=paste0("font-size:2.2rem; font-weight:700;
                                            color:",USC_CORAL,";"), "OR = 3.04"),
                    p(style=paste0("color:",GRAY_70,"; font-size:0.85rem; margin:0;"),
                      "95% CI: 2.76\u20133.34 | p < 0.001")
                  ),
                  p(style="line-height:1.65; margin:0; font-size:0.9rem;",
                    "When a layperson perceived an image as ", tags$strong("high aesthetic"),
                    ", they were ", tags$strong("~3x more likely"),
                    " to also rate it as high naturalness. ",
                    "The association exists, but aesthetic and naturalness are more
                     independent for laypersons")
                )
              )
            )
          )
        )
      ),

      # Interpretation
      div(class="row",
        div(class="col-12",
          div(style=paste0("background:#f8f8f8; border-radius:8px; padding:1.5rem;
                            border-left:4px solid ",USC_OLIVE,";"),
            h6("What Does This Mean?",
               style=paste0("color:",USC_CARDINAL,"; margin-bottom:0.8rem;")),
            p(style="line-height:1.75; margin:0;",
              "High rater-level ICC for laypersons (0.48\u20130.53) combined with
               near-zero image-level ICC reveals a dissociation: ",
              tags$strong("laypersons rate consistently, but don't consistently agree with each other."),
              " Each person applies their own stable aesthetic standard is not shared
               across individuals. Surgeons show moderate image-level agreement (up to 0.33),
               suggesting a partially shared perceptual framework likely shaped by clinical training. ",
              "The OR contrast (11.84 vs 3.04) further shows that ",
              tags$strong("surgeons treat aesthetics and naturalness as nearly inseparable,
               while laypersons experience them as more independent."),
              "Though it is important to note the survey did not provide detailed operational definitions of 
              “aesthetic” or “naturalness, leaving interpretation to respondent discretion. This approach captures 
              authentic perceptual judgment, but it may also introduce conceptual variability, particularly among lay raters. 
              These findings suggest that defining a universal aesthetic standard may be implausible for lay perception, 
              while surgeons demonstrate meaningful but imperfect consensus.")
          )
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(tags$a(href="https://github.com/serafin-stats/breast-implant-ratio-analysis",
    target="_blank", style="color:#FFCC00 !important; padding:0.5rem 1rem;",
    icon("github"), " GitHub")),
  nav_item(tags$a(href="https://www.linkedin.com/in/casandra-serafin/",
    target="_blank", style="color:#FFCC00 !important; padding:0.5rem 1rem;",
    icon("linkedin"), " LinkedIn"))
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  output$dag_plot <- renderPlot({ make_dag_plot() }, bg="transparent")

  # Model cache
  model_cache <- reactiveValues()
  get_model <- function(cohort, outcome, predictor) {
    fn <- model_filename(cohort, outcome, predictor)
    if (is.null(model_cache[[fn]])) {
      m <- load_thesis_model(fn)
      model_cache[[fn]] <- if (is.null(m)) NA else m
    }
    m <- model_cache[[fn]]
    if (identical(m, NA)) NULL else m
  }

  # PP reactive
  pp_result <- reactive({
    req(input$pp_cohort, input$pp_outcome, input$pp_predictor)
    req(!is.null(surgeon_data), !is.null(lay_data))
    
    
    model <- get_model(input$pp_cohort, input$pp_outcome, input$pp_predictor)
    if (is.null(model)) return(NULL)
    params <- get_pp_params(input$pp_cohort, input$pp_predictor, surgeon_data, lay_data)
    tryCatch(
      make_pp_glmer(model, params$data, params$raw_var, params$model_var,
                    params$sq_var, params$raw_to_model, params$model_to_raw,
                    params$raw_to_display),
      error = function(e) { message("PP error: ", e$message); NULL }
    )
  })
  
  

  # Turning point box — only shown when significant
  output$pp_vertex_box <- renderUI({
    sig <- is_significant(input$pp_cohort, input$pp_outcome, input$pp_predictor)
    if (!sig) return(NULL)
    res <- pp_result()
    if (is.null(res) || is.na(res$vertex)) return(NULL)
    vertex_txt <- paste0(
      round(res$vertex, 3),
      if (res$in_range) "  \u2713 within range" else "  \u2717 outside range"
    )
    div(style=paste0("background:white; border-radius:6px; padding:1rem;
                      border:1px solid ",GRAY_30,";"),
      h6("Estimated Turning Point",
         style="margin-bottom:0.4rem; font-size:0.88rem;"),
      tags$strong(vertex_txt),
      p(style=paste0("color:",GRAY_70,"; font-size:0.78rem; margin:0.5rem 0 0 0;"),
        "Shown only for statistically significant quadratic terms.",
        tags$br(),
        "x = \u2212b / (2a) from model coefficients")
    )
  })

  output$pp_plot <- renderPlot({
    res <- pp_result()
    if (is.null(res)) {
      fn <- model_filename(input$pp_cohort, input$pp_outcome, input$pp_predictor)
      return(ggplot() +
        annotate("text", x=0.5, y=0.5,
                 label=paste0("Model could not be loaded.\n\n",
                              "Check config.R and that\n", fn,
                              "\nexists in THESIS_MODELS."),
                 hjust=0.5, vjust=0.5, size=4.5, color=GRAY_70, lineheight=1.5) +
        theme_void())
    }
    cohort_color <- COHORT_COLORS[input$pp_cohort]
    params       <- get_pp_params(input$pp_cohort, input$pp_predictor,
                                  surgeon_data, lay_data)
    ggplot(res$grid, aes(x=display_value, y=pred_prob)) +
      geom_ribbon(aes(ymin=conf_low, ymax=conf_high),
                  fill=cohort_color, alpha=0.15) +
      geom_line(color=cohort_color, linewidth=1.5) +
      scale_y_continuous(labels=scales::percent_format(accuracy=1), limits=c(0,1)) +
      labs(x=params$x_label,
           y=paste0("P(High ", input$pp_outcome, ")"),
           title=paste0(input$pp_cohort, " Cohort \u2014 ",
                        input$pp_outcome, " \u00d7 ", input$pp_predictor),
           subtitle="Population-level prediction (RE = 0) | Covariates held at mean/reference | 95% CI shaded") +
      theme_minimal(base_size=13) +
      theme(plot.title=element_text(face="bold", color=USC_CARDINAL),
            plot.subtitle=element_text(color=GRAY_70, size=10),
            axis.title=element_text(color="#333333"),
            panel.grid.minor=element_blank())
  })

  output$pp_interpretation <- renderText({
    res <- pp_result()
    if (is.null(res)) return("Model not available for this combination.")
    pp_interpretation(input$pp_cohort, input$pp_outcome, input$pp_predictor,
                      res$vertex, res$in_range, res$shape)
  })

  # ICC plot
  output$icc_plot <- renderPlot({
    df <- if (input$icc_type_filter=="both") icc_df
          else filter(icc_df, ICC_Type==input$icc_type_filter)
    p <- ggplot(df, aes(x=Outcome, y=ICC, fill=Cohort)) +
      geom_col(position=position_dodge(width=0.7), width=0.6) +
      geom_hline(yintercept=0.4, linetype="dashed", color=GRAY_70, linewidth=0.7) +
      annotate("text", x=0.55, y=0.42, label="moderate threshold",
               hjust=0, size=3, color=GRAY_70, fontface="italic") +
      geom_text(aes(label=round(ICC,2)), position=position_dodge(width=0.7),
                vjust=-0.4, size=3.5, fontface="bold") +
      scale_fill_manual(values=COHORT_COLORS) +
      scale_y_continuous(limits=c(0,0.7)) +
      labs(x=NULL, y="ICC", fill="Cohort") +
      theme_minimal(base_size=12) +
      theme(axis.text=element_text(size=11), panel.grid.minor=element_blank(),
            legend.position="bottom")
    if (input$icc_type_filter=="both") p <- p + facet_wrap(~ICC_Type)
    p
  })

  # Scatter plot
  output$scatter_plot <- renderPlot({
    req(!is.null(surgeon_data), !is.null(lay_data))
    message("scatter: surgeon rows=", nrow(surgeon_data), " lay rows=", nrow(lay_data))
    message("AestheticScore_num exists: ", "AestheticScore_num" %in% names(surgeon_data))
    score_var <- if (input$scatter_outcome=="Aesthetic")
      "AestheticScore_num" else "NaturalScore_num"
    surg_m <- surgeon_data %>% group_by(PatientID) %>%
      summarise(mean_surg=mean(.data[[score_var]], na.rm=TRUE), .groups="drop")
    lay_m  <- lay_data %>% group_by(PatientID) %>%
      summarise(mean_lay=mean(.data[[score_var]], na.rm=TRUE), .groups="drop")
    comb <- left_join(surg_m, lay_m, by="PatientID")
    r    <- cor(comb$mean_surg, comb$mean_lay, use="complete.obs")
    ggplot(comb, aes(x=mean_surg, y=mean_lay)) +
      geom_point(size=3, alpha=0.8, color=USC_CARDINAL) +
      geom_smooth(method="lm", se=TRUE, color=USC_BLUE, fill=USC_BLUE,
                  alpha=0.1, linewidth=1.2) +
      xlim(2.0,4.3) + ylim(2.0,4.3) +
      labs(x=paste0("Mean ",input$scatter_outcome," (Surgeons)"),
           y=paste0("Mean ",input$scatter_outcome," (Lay)"),
           subtitle=paste0("Pearson r = ",round(r,3)," | n = 40 images")) +
      theme_minimal(base_size=12) +
      theme(plot.subtitle=element_text(color=GRAY_70, size=10),
            panel.grid.minor=element_blank())
  })
}

shinyApp(ui=ui, server=server)