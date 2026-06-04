# =============================================================================
#  HarmonizeR  —  Interactive Batch Diagnostics & Harmonization Shiny App
#  Supports both cross-sectional and longitudinal data structures
#  Data structure: list of m matrices (one per measurement/modality)
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(magrittr)
  library(purrr)
  library(broom)
  library(car)
  library(mgcv)
  library(lme4)
  library(MASS)
  library(MCMCpack)
  library(tibble)
  library(scales)
  library(MultiComBat)
  library(plotly)
  library(ggrepel)
  library(openxlsx)
  if (requireNamespace("patchwork", quietly = TRUE)) library(patchwork)
  if (requireNamespace("zip", quietly = TRUE)) library(zip)
  library(HarmonizeR)
})

# Allow moderately large diagnostic uploads.
options(shiny.maxRequestSize = 2 * 1024^3)

# --- Shortcut assignments for internal helpers ---
`%||%` <- HarmonizeR:::`%||%`
safe_positive_int <- HarmonizeR:::safe_positive_int
valid_positive_int <- HarmonizeR:::valid_positive_int
safe_m_value <- HarmonizeR:::safe_m_value
clean_batch_for_plot <- HarmonizeR:::clean_batch_for_plot
safe_discrete_palette <- HarmonizeR:::safe_discrete_palette

pca_equal_axes_default <- HarmonizeR:::pca_equal_axes_default
pca_ncol_facets <- HarmonizeR:::pca_ncol_facets
pca_legend_rows <- HarmonizeR:::pca_legend_rows
pca_plot_height_px <- HarmonizeR:::pca_plot_height_px
can_draw_ellipse_by_group <- HarmonizeR:::can_draw_ellipse_by_group
pca_plot_robust <- HarmonizeR:::pca_plot_robust

to_numeric_if_possible <- HarmonizeR:::to_numeric_if_possible
numeric_like_prop <- HarmonizeR:::numeric_like_prop
clean_uploaded_covar <- HarmonizeR:::clean_uploaded_covar

get_param_family <- HarmonizeR:::get_param_family
extract_stan_indices <- HarmonizeR:::extract_stan_indices
get_first_index <- HarmonizeR:::get_first_index
get_second_index <- HarmonizeR:::get_second_index

standardize_mcmc_summary <- HarmonizeR:::standardize_mcmc_summary
detect_mcmc_families <- HarmonizeR:::detect_mcmc_families
match_mcmc_param <- HarmonizeR:::match_mcmc_param
filter_mcmc_summary <- HarmonizeR:::filter_mcmc_summary

find_nested_component <- HarmonizeR:::find_nested_component
get_shrink_pair <- HarmonizeR:::get_shrink_pair
vectorize_shrink_object <- HarmonizeR:::vectorize_shrink_object

if (!exists("batch_matrix")) {
  r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  r_files <- r_files[!grepl("pkg-deps|stan_method", r_files)]
  invisible(lapply(sort(r_files), function(f)
    tryCatch(source(f, local = FALSE),
             error = function(e) message("Cannot source ", basename(f)))))
}

# =============================================================================
#  UI
# =============================================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$span(tags$b("HarmonizeR"),
                      tags$span(style = "font-size:11px;color:#aed6f1;margin-left:5px;",
                                "multi-modal ComBat")),
    titleWidth = 255
  ),
  
  dashboardSidebar(
    width = 240,
    sidebarMenu(id = "sidebar_menu",
                menuItem("Data Setup",         tabName = "data_setup",  icon = icon("database")),
                menuItem("Pre-Harmonization",  tabName = "pre_diag",    icon = icon("chart-bar")),
                menuItem("Harmonization",      tabName = "harmonize",   icon = icon("wand-magic-sparkles")),
                menuItem("Post-Harmonization", tabName = "post_diag",   icon = icon("chart-line")),
                menuItem("EB Diagnostics",     tabName = "eb_diag",     icon = icon("microscope")),
                menuItem("Covariance Diag",    tabName = "cov_diag",    icon = icon("table-cells")),
                menuItem("Bayesian MCMC Diag", tabName = "mcmc_diag",   icon = icon("atom"))
    ),
    tags$hr(),
    tags$div(style = "padding:10px 14px;font-size:11px;color:#aaa;",
             tags$div(style = "font-weight:600;color:#7fb3d3;margin-bottom:4px;", "Active Dataset"),
             uiOutput("sidebar_info")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
    body, .content-wrapper, .right-side { font-size: 16px; }
    .skin-blue .main-header .logo { background:#1a2b4a; font-size:20px; font-weight:700; }
    .skin-blue .main-header .navbar { background:#1a2b4a; }
    .skin-blue .main-sidebar { background:#111e35; font-size:16px; }
    .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a { font-size:16px; }
    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a { background:#2e4c8a; }
    .skin-blue .main-sidebar .sidebar .sidebar-menu>li:hover>a { background:#1e3566; }
    .content-wrapper { background:#f0f3f9; font-size:16px; }
    .box>.box-header { background:linear-gradient(135deg,#1a2b4a,#2e4c8a); color:#fff!important; border-radius:4px 4px 0 0; font-size:18px; }
    .box>.box-header .box-title { color:#fff!important; font-size:20px; font-weight:700; }
    .box-body { font-size:16px; }
    label, .control-label { font-size:16px; font-weight:600; }
    .form-control, .selectize-input, .selectize-dropdown, .shiny-input-container { font-size:16px; }
    .btn, .action-button { font-size:16px; font-weight:600; }
    .stat-card { background:#fff; border-radius:8px; padding:16px; box-shadow:0 2px 8px rgba(0,0,0,.08); text-align:center; margin-bottom:8px; }
    .stat-card .val { font-size:2.2rem; font-weight:700; color:#1a2b4a; line-height:1.1; }
    .stat-card .lbl { font-size:.9rem; color:#888; text-transform:uppercase; letter-spacing:.05em; }
    .stat-card .sub { font-size:.95rem; color:#3d7fc1; margin-top:3px; }
    .sec-hdr { font-size:18px; font-weight:700; color:#1a2b4a; margin:14px 0 6px; padding-left:8px; border-left:3px solid #3d7fc1; }
    .mod-tag { display:inline-block; background:#eaf2fb; color:#1a5276; border-radius:12px; padding:4px 12px; font-size:.9rem; margin:2px; font-weight:600; border:1px solid #aed6f1; }
    .long-badge { display:inline-block; background:#e8f5e9; color:#1b5e20; border-radius:12px; padding:4px 12px; font-size:.9rem; margin:2px 8px; font-weight:700; border:2px solid #66bb6a; }
    .cs-badge { display:inline-block; background:#e3f2fd; color:#0d47a1; border-radius:12px; padding:4px 12px; font-size:.9rem; margin:2px 8px; font-weight:700; border:2px solid #42a5f5; }
    .mcmc-badge { display:inline-block; background:#f3e5f5; color:#4a148c; border-radius:12px; padding:4px 12px; font-size:.9rem; margin:2px 8px; font-weight:700; border:2px solid #ce93d8; }
    .mcmc-info-box { background:linear-gradient(135deg,#1a2b4a,#2e4c8a); color:#fff; border-radius:10px; padding:16px 18px; margin-bottom:12px; }
    .mcmc-info-box h4 { color:#aed6f1; margin-top:0; font-size:16px; }
    .mcmc-info-box p  { margin:4px 0; font-size:14px; opacity:.9; }
    .mcmc-info-box code { background:rgba(255,255,255,.15); padding:2px 6px; border-radius:4px; }
    .rhat-ok   { color:#27ae60; font-weight:700; }
    .rhat-warn { color:#e67e22; font-weight:700; }
    .rhat-bad  { color:#c0392b; font-weight:700; }
    .content-wrapper { background:#f0f3f9; font-size:16px; }
    table.dataTable td, table.dataTable th { font-size:15px; }
    table.dataTable td.col-batch { background:#fff3cd!important; font-weight:600; }
    table.dataTable td.col-covar { background:#d4edda!important; }
    table.dataTable th.col-batch { background:#ffc107!important; color:#333!important; }
    table.dataTable th.col-covar { background:#28a745!important; color:#fff!important; }
    .legend-pill { display:inline-flex; align-items:center; gap:5px; margin-right:12px; font-size:.95rem; font-weight:600; }
    .legend-pill .swatch { width:14px; height:14px; border-radius:3px; display:inline-block; flex-shrink:0; }
    "))),
    
    tabItems(
      
      # ══════════════════════════════════════════════════════════════════════
      # TAB 1  DATA SETUP  (unchanged)
      # ══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "data_setup",
              fluidRow(
                box(title = "Data Source", width = 4, solidHeader = TRUE,
                    radioGroupButtons("data_source", "Source:",
                                      choices = c("Demo" = "demo", "Upload CSVs" = "upload"),
                                      selected = "demo", justified = TRUE),
                    conditionalPanel("input.data_source == 'demo'",
                                     tags$div(class = "sec-hdr", "Data type"),
                                     radioGroupButtons("demo_type", label = NULL,
                                                       choices = c("Cross-sectional" = "cs", "Longitudinal" = "long"),
                                                       selected = "cs", justified = TRUE),
                                     tags$div(class = "sec-hdr", "Simulation parameters"),
                                     conditionalPanel("input.demo_type == 'cs'",
                                                      sliderInput("demo_n", "Samples (n):", 30, 300, 90, step = 10)),
                                     conditionalPanel("input.demo_type == 'long'",
                                                      sliderInput("demo_nsubj", "Subjects:", 20, 200, 40, step = 10),
                                                      sliderInput("demo_maxv",  "Max visits:", 2, 8, 4, step = 1)),
                                     sliderInput("demo_G",  "Features (G):",  5, 100, 15, step = 5),
                                     sliderInput("demo_m",  "Imaging metrics / modalities (m):", 1, 6, 3),
                                     sliderInput("demo_nb", "Batches:", 2, 6, 3),
                                     actionBttn("gen_demo", "Generate Demo Data",
                                                style = "gradient", color = "primary", icon = icon("play"))
                    ),
                    conditionalPanel("input.data_source == 'upload'",
                                     tags$div(class = "sec-hdr", "Data type"),
                                     radioGroupButtons("upload_type", label = NULL,
                                                       choices = c("Cross-sectional" = "cs", "Longitudinal" = "long"),
                                                       selected = "cs", justified = TRUE),
                                     tags$div(class = "sec-hdr", "One CSV per modality"),
                                     tags$p(style = "font-size:11px;color:#777;",
                                            "Each CSV: rows=observations, cols=features.",
                                            " All CSVs must share identical column names and row order."),
                                     numericInput("upload_m", "Number of modalities:", 2, 1, 10),
                                     uiOutput("upload_ui"),
                                     tags$div(class = "sec-hdr", "Shared metadata"),
                                     fileInput("file_batch", "Batch labels CSV (1 column):"),
                                     fileInput("file_covar", "Covariates CSV (optional, shared):"),
                                     conditionalPanel("input.upload_type == 'long'",
                                                      tags$div(style = "background:#e8f5e9;border:1px solid #66bb6a;border-radius:8px;padding:10px 12px;margin:8px 0;",
                                                               tags$div(style = "font-weight:700;color:#1b5e20;margin-bottom:6px;",
                                                                        icon("timeline"), " Longitudinal Settings"),
                                                               tags$p(style = "font-size:12px;color:#2e7d32;margin:0 0 6px;",
                                                                      "The covariates CSV should contain columns for subject ID and visit number."),
                                                               textInput("re_col",    "Subject ID column name:", value = "subid"),
                                                               textInput("visit_col", "Visit column name:",      value = "visit")
                                                      )
                                     ),
                                     actionBttn("load_files", "Load Files",
                                                style = "gradient", color = "primary", icon = icon("upload"))
                    )
                ),
                box(title = "Dataset Summary", width = 8, solidHeader = TRUE,
                    uiOutput("data_type_badge"), tags$br(),
                    fluidRow(uiOutput("summary_cards")),
                    tags$div(class = "sec-hdr", "Modalities"),
                    uiOutput("mod_tags"),
                    tags$hr(),
                    tags$div(class = "sec-hdr", "Batch distribution"),
                    plotly::plotlyOutput("batch_bar", height = "160px"),
                    tags$hr(),
                    uiOutput("long_structure_ui"),
                    tags$div(class = "sec-hdr", uiOutput("covar_section_title")),
                    tags$p(style = "font-size:14px;color:#777;margin:0 0 6px;",
                           uiOutput("covar_section_hint")),
                    plotly::plotlyOutput("covar_dist_plot", height = "260px"),
                    uiOutput("timevar_covar_ui"),
                    tags$hr(),
                    tags$div(
                      style = "display:flex;align-items:center;gap:16px;flex-wrap:wrap;",
                      tags$div(class = "sec-hdr", style = "margin:0;", "Data preview"),
                      uiOutput("prev_mod_ui"),
                      tags$span(
                        tags$span(class = "legend-pill",
                                  tags$span(class = "swatch", style = "background:#ffc107;"), "Batch"),
                        tags$span(class = "legend-pill",
                                  tags$span(class = "swatch", style = "background:#28a745;"), "Covariate"),
                        tags$span(class = "legend-pill",
                                  tags$span(class = "swatch",
                                            style = "background:#ffffff;border:1px solid #ccc;"), "Feature")
                      )
                    ),
                    tags$div(style = "margin-top:8px;"),
                    DTOutput("data_prev")
                )
              ),
              fluidRow(
                box(title = "Model Configuration", width = 5, solidHeader = TRUE,
                    fluidRow(
                      column(6, selectInput("model_type", "Regression model:",
                                            choices = c("Linear (lm)" = "lm", "GAM (mgcv)" = "gam",
                                                        "Linear mixed (lmer)" = "lmer"), selected = "lm")),
                      column(6, uiOutput("formula_ui"))
                    ),
                    fluidRow(
                      column(6, uiOutput("ref_batch_ui")),
                      column(6,
                             uiOutput("re_spec_ui"),
                             checkboxInput("bat_adjust", "Include batch in diagnostic model", TRUE),
                             tags$br(),
                             actionBttn("confirm_config", "Confirm Configuration",
                                        style = "gradient", color = "success", icon = icon("check"))
                      )
                    ),
                    tags$div(style = "margin-top:16px;", uiOutput("config_status"))
                ),
                box(title = "Covariate–Feature Trend Explorer", width = 7, solidHeader = TRUE,
                    tags$p(style = "font-size:16px;color:#666;margin:0 0 8px;line-height:1.4;",
                           "Inspect linear vs non-linear relationships between a continuous covariate and a feature."),
                    fluidRow(
                      column(3, uiOutput("trend_covar_ui")),
                      column(3, uiOutput("trend_feat_ui")),
                      column(3, uiOutput("trend_mod_ui")),
                      column(3,
                             selectInput("trend_smooth", "Smooth method:",
                                         choices = c("lm (linear)" = "lm", "GAM (loess)" = "loess", "Both" = "both"),
                                         selected = "both"),
                             checkboxInput("trend_batch_color", "Color by batch", TRUE)
                      )
                    ),
                    plotOutput("trend_plot", height = "280px")
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════════════════════
      # TAB 2  PRE-HARMONIZATION  (unchanged)
      # ══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "pre_diag",
              fluidRow(
                box(width = 12, solidHeader = TRUE, title = "Pre-Harmonization Diagnostics",
                    fluidRow(
                      column(2, actionBttn("run_pre_diag", "Run Diagnostics",
                                           style = "gradient", color = "primary",
                                           icon = icon("play"), size = "sm")),
                      column(2, checkboxInput("pre_ellipse", "PCA ellipses", TRUE)),
                      column(2, sliderInput("pre_pc1", "PC x:", 1, 10, 1, step = 1)),
                      column(2, sliderInput("pre_pc2", "PC y:", 1, 10, 2, step = 1)),
                      column(2, uiOutput("pre_pca_type_ui"))
                    ),
                    tags$hr(style = "margin:8px 0;"),
                    fluidRow(
                      column(3, checkboxInput("pre_run_rf_auc", "Run RF batch prediction AUC", TRUE)),
                      column(3, numericInput("pre_rf_k", "CV folds:", value = 5, min = 2, max = 20, step = 1)),
                      column(3, numericInput("pre_rf_ntree", "RF trees:", value = 100, min = 50, max = 1000, step = 50)),
                      column(3,
                             tags$div(style = "font-size:12px;color:#777;margin-top:25px;",
                                      icon("circle-info"),
                                      " Out-of-sample AUC quantifies global batch detectability within each metric."))
                    )
                )
              ),
              fluidRow(
                tabBox(title = "Results", width = 12,
                       tabPanel("PCA", plotOutput("pre_pca", height = "auto")),
                       tabPanel("Residual Boxplots",
                                fluidRow(
                                  column(3, uiOutput("pre_feat_ui")),
                                  column(3, uiOutput("pre_mod_ui")),
                                  column(3,
                                         checkboxInput("pre_box_jitter", "Show jitter points", FALSE),
                                         checkboxGroupInput("pre_outlier_show", "Highlight outliers:",
                                                            choices = c("Regular (1.5x IQR)" = "regular",
                                                                        "Extreme (3x IQR)"   = "extreme"),
                                                            selected = c("regular","extreme"), inline = FALSE)
                                  ),
                                  column(3,
                                         tags$div(style = "font-size:13px;color:#555;margin-top:4px;",
                                                  tags$b("Outlier thresholds:"), tags$br(),
                                                  "Regular: value > Q3 + 1.5×IQR", tags$br(),
                                                  "or value < Q1 − 1.5×IQR", tags$br(),
                                                  "Extreme: value > Q3 + 3×IQR", tags$br(),
                                                  "or value < Q1 − 3×IQR")
                                  )
                                ),
                                tags$div(class = "sec-hdr", "Additive residuals: mean-shift by batch"),
                                plotly::plotlyOutput("pre_box_add", height = "600px"),
                                tags$br(),
                                tags$div(class = "sec-hdr", "Multiplicative residuals: variance-shift by batch"),
                                plotly::plotlyOutput("pre_box_mul", height = "600px"),
                                tags$br(),
                                tags$div(class = "sec-hdr", "Outlier summary — additive & multiplicative residuals"),
                                tags$p(style = "font-size:13px;color:#777;margin:0 0 6px;",
                                       "Combined outliers from both residual types for the selected feature and modality."),
                                fluidRow(
                                  column(10),
                                  column(2, downloadButton("dl_outlier_excel", "Export to Excel",
                                                           class = "btn btn-success btn-sm",
                                                           style = "width:100%;margin-bottom:6px;"))
                                ),
                                DTOutput("pre_outlier_tbl_combined")
                       ),
                       tabPanel("Univariate Tests",
                                tags$div(class = "sec-hdr",
                                         "% Bonferroni-significant features: rows = modalities, cols = tests"),
                                uiOutput("pre_uni_results_ui")
                       ),
                       tabPanel("P-value Explorer",
                                fluidRow(
                                  column(3, selectInput("pre_test", "Test:",
                                                        choices = c("ANOVA"="anova","Kruskal-Wallis"="kruskal",
                                                                    "Levene"="lv","Bartlett"="bl","Fligner-Killeen"="fk"),
                                                        selected = "anova")),
                                  column(3, uiOutput("pre_modsel_ui")),
                                  column(3, sliderInput("pre_alpha", HTML("&alpha;:"), 0.001, 0.2, 0.05, 0.005))
                                ),
                                plotly::plotlyOutput("pre_pval_bar", height = "500px"),
                                DTOutput("pre_pval_tbl")
                       ),
                       tabPanel("Multivariate Tests",
                                uiOutput("pre_mv_panel")
                       ),
                       tabPanel("Batch Prediction AUC",
                                tags$p(style = "font-size:15px;color:#666;margin:0 0 8px;",
                                       "Cross-validated random forest prediction of batch labels within each metric. Higher AUC indicates stronger global batch signal."),
                                plotOutput("pre_rf_auc_plot", height = "420px"),
                                tags$br(),
                                DTOutput("pre_rf_auc_tbl")
                       )
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════════════════════
      # TAB 3  HARMONIZATION  — extended with MCMC Script sub-tab
      # ══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "harmonize",
              fluidRow(
                box(title = "ComBat Settings", width = 4, solidHeader = TRUE,
                    tags$div(class = "sec-hdr", "Mode"),
                    uiOutput("harm_mode_ui"),
                    tags$div(class = "sec-hdr", "Options"),
                    checkboxInput("harm_eb",     "Empirical Bayes shrinkage",   TRUE),
                    checkboxInput("harm_robust", "Robust scale (biweight)",      FALSE),
                    uiOutput("harm_cov_ui"),
                    conditionalPanel("input.harm_mode == 'multi'",
                                     checkboxInput("harm_robcov",
                                                   "Robust covariance target (preserve shared Σ)", FALSE)
                    ),
                    tags$hr(),
                    tags$div(
                      style = paste0(
                        "background:linear-gradient(135deg,#1a2b4a 0%,#2e4c8a 50%,#1a6b8a 100%);",
                        "border-radius:10px;padding:2px;box-shadow:0 4px 15px rgba(46,76,138,.45);"
                      ),
                      tags$button(
                        id = "run_harm", class = "btn action-button",
                        style = paste0(
                          "width:100%;background:transparent;border:none;",
                          "color:#fff;font-size:15px;font-weight:700;",
                          "letter-spacing:.04em;padding:13px 10px;",
                          "display:flex;align-items:center;justify-content:center;gap:10px;cursor:pointer;"
                        ),
                        tags$span(
                          style = paste0(
                            "display:inline-flex;align-items:center;justify-content:center;",
                            "width:32px;height:32px;border-radius:50%;",
                            "background:rgba(255,255,255,.15);flex-shrink:0;"
                          ),
                          icon("atom", style = "font-size:16px;")
                        ),
                        tags$span("Run Harmonization (EB)")
                      )
                    ),
                    tags$br(),
                    uiOutput("harm_status_ui")
                ),
                
                box(title = "EB Shrinkage — Batch Parameter Estimates", width = 8, solidHeader = TRUE,
                    conditionalPanel(
                      "input.harm_eb",
                      withMathJax(
                        tags$p(style = "font-size:16px;color:#666;margin:0 0 6px;",
                               HTML(paste0(
                                 "Each segment connects the empirical estimate ",
                                 "\\((\\widehat{\\gamma}\\text{ or }\\widehat{\\delta})\\) to the EB-shrunken estimate ",
                                 "\\((\\gamma^{*}\\text{ or }\\delta^{*})\\). ",
                                 "Larger movement indicates stronger shrinkage toward the prior."
                               )))
                      ),
                      fluidRow(
                        column(4, selectInput("eb_shrink_parm", "Parameter:",
                                              choices = c("Mean shift γ" = "gamma",
                                                          "Batch scale δ (Frobenius norm)" = "delta"),
                                              selected = "gamma")),
                        column(4, uiOutput("eb_shrink_mod_ui")),
                        column(4, uiOutput("eb_shrink_range_ui"))
                      ),
                      fluidRow(
                        column(8, uiOutput("eb_shrink_batch_ui")),
                        column(4, selectInput("eb_shrink_sort", "Sort features by shrinkage:",
                                              choices = c("Original feature order" = "original",
                                                          "Largest shrinkage first" = "desc",
                                                          "Smallest shrinkage first" = "asc"),
                                              selected = "original"))
                      ),
                      plotOutput("eb_shrink_plot", height = "380px")
                    ),
                    conditionalPanel(
                      "!input.harm_eb",
                      tags$div(
                        style = paste0(
                          "background:#fff3cd;border-left:4px solid #f0ad4e;",
                          "padding:12px 14px;border-radius:6px;margin:8px 0 12px;",
                          "font-size:14px;color:#6b4f00;"
                        ),
                        tags$b(icon("circle-info"), " EB diagnostics disabled"),
                        tags$br(),
                        "Empirical Bayes shrinkage is not selected, so EB shrinkage diagnostics are not available for this run."
                      )
                    ),
                    tags$hr(),
                    fluidRow(
                      column(8, tags$div(class = "sec-hdr", style = "margin-bottom:4px;",
                                         "Before / After grand means")),
                      column(4, uiOutput("ba_mod_ui"))
                    ),
                    plotOutput("ba_plot", height = "220px")
                )
              ),
              
              # ── MCMC Script Generator box ──────────────────────────────────────
              fluidRow(
                box(
                  title = tags$span(icon("file-code"), " MCMC Script Generator — Full Bayesian ComBat"),
                  width = 12, solidHeader = TRUE,
                  style = "border-top: 3px solid #8e44ad;",
                  
                  tags$div(class = "mcmc-info-box",
                           tags$h4(icon("circle-info"), "  Why generate a script instead of running MCMC here?"),
                           tags$p("Stan MCMC can take minutes to hours depending on dataset size, number of features, ",
                                  "modalities, and chains. Running it inside a Shiny session risks timeouts and memory issues."),
                           tags$p("The downloaded project folder is self-contained: it includes a data/ folder, ",
                                  "an R/ folder with the runnable script, and a results/ folder for outputs."),
                           tags$p("After running ", tags$code('source("R/run_mcmc_harmonization.R")'),
                                  ", upload the lightweight ", tags$code("*_LIGHT_UPLOAD.rds"),
                                  " file(s) from ", tags$code("results/stan_fits/"), " to the ",
                                  tags$b("Bayesian MCMC Diag"), " tab for interactive diagnostics. ",
                                  "Keep ", tags$code("*_FULL_LOCAL.rds"),
                                  " files locally for reproducibility; do not upload them unless your server has enough memory.")
                  ),
                  
                  fluidRow(
                    # -- Left column: MCMC tuning parameters -----------------------
                    column(4,
                           tags$div(class = "sec-hdr", "MCMC Tuning"),
                           numericInput("mcmc_chains",     "Number of chains:",          value = 4, min = 1, max = 16),
                           numericInput("mcmc_parallel",   "Parallel chains:",           value = 4, min = 1, max = 16),
                           sliderInput("mcmc_adapt_delta", "adapt_delta (convergence):", min = 0.80, max = 0.999,
                                       value = 0.95, step = 0.005),
                           tags$div(style = "font-size:13px;color:#666;background:#f7f9fc;padding:8px 10px;border-radius:6px;",
                                    tags$b("Guidance:"), tags$br(),
                                    "• Start with 4 chains and adapt_delta = 0.95", tags$br(),
                                    "• Increase adapt_delta to 0.99 if you see divergent transitions", tags$br(),
                                    "• Use parallel_chains = number of available CPU cores"
                           )
                    ),
                    # -- Middle column: settings summary ---------------------------
                    column(4,
                           tags$div(class = "sec-hdr", "Settings to be embedded in script"),
                           uiOutput("mcmc_script_summary_ui")
                    ),
                    # -- Right column: generate + download -------------------------
                    column(4,
                           tags$div(class = "sec-hdr", "Generate & Download"),
                           tags$p(style = "font-size:14px;color:#555;",
                                  "Preview the script, then download a complete runnable project folder as a .zip file."),
                           actionBttn("preview_mcmc_script", "Preview Script",
                                      style = "gradient", color = "royal",
                                      icon = icon("eye"), size = "md"),
                           tags$br(), tags$br(),
                           uiOutput("mcmc_dl_ui"),
                           tags$br(),
                           tags$div(style = "background:#fff3cd;border-left:4px solid #f0ad4e;padding:10px 12px;border-radius:6px;font-size:13px;",
                                    tags$b("Requirements on your machine:"), tags$br(),
                                    tags$code("install.packages('cmdstanr',"),
                                    tags$br(),
                                    tags$code("  repos=c('https://mc-stan.org/r-packages/',getOption('repos')))"),
                                    tags$br(),
                                    tags$code("cmdstanr::install_cmdstan()"), tags$br(),
                                    tags$code("library(MultiComBat)")
                           )
                    )
                  ),
                  
                  # Script preview panel
                  conditionalPanel("input.preview_mcmc_script > 0",
                                   tags$hr(),
                                   tags$div(class = "sec-hdr", "Script Preview (first 120 lines)"),
                                   tags$div(
                                     style = paste0(
                                       "background:#1e1e2e;color:#cdd6f4;font-family:monospace;",
                                       "font-size:12.5px;line-height:1.6;padding:16px 18px;",
                                       "border-radius:8px;max-height:500px;overflow-y:auto;",
                                       "white-space:pre;border:1px solid #444;"
                                     ),
                                     uiOutput("mcmc_script_preview_ui")
                                   )
                  )
                )
              ),
              
              fluidRow(
                box(title = "Download Harmonized Data (EB)", width = 12, solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             tags$div(class = "sec-hdr", "Harmonized (Modality 1, 8 rows)"),
                             DTOutput("harm_prev")),
                      column(4,
                             tags$div(class = "sec-hdr", "Residuals (Modality 1, 8 rows)"),
                             DTOutput("resid_prev")),
                      column(4,
                             tags$div(class = "sec-hdr", "Download (one file per modality)"),
                             uiOutput("dl_ui"))
                    )
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════════════════════
      # TAB 4  POST-HARMONIZATION  (unchanged)
      # ══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "post_diag",
              fluidRow(
                box(width = 12, solidHeader = TRUE, title = "Post-Harmonization Diagnostics",
                    fluidRow(
                      column(2, actionBttn("run_post_diag", "Run Post Diagnostics",
                                           style = "gradient", color = "primary",
                                           icon = icon("play"), size = "sm")),
                      column(2, checkboxInput("post_ellipse", "PCA ellipses", TRUE)),
                      column(2, sliderInput("post_pc1", "PC x:", 1, 10, 1)),
                      column(2, sliderInput("post_pc2", "PC y:", 1, 10, 2)),
                      column(2, uiOutput("post_pca_type_ui"))
                    ),
                    tags$hr(style = "margin:8px 0;"),
                    fluidRow(
                      column(3, checkboxInput("post_run_rf_auc", "Run RF batch prediction AUC", TRUE)),
                      column(3, numericInput("post_rf_k", "CV folds:", value = 5, min = 2, max = 20, step = 1)),
                      column(3, numericInput("post_rf_ntree", "RF trees:", value = 100, min = 50, max = 1000, step = 50)),
                      column(3,
                             tags$div(style = "font-size:12px;color:#777;margin-top:25px;",
                                      icon("circle-info"),
                                      " Compares global batch detectability before and after harmonization."))
                    )
                )
              ),
              fluidRow(
                tabBox(title = "Post Results", width = 12,
                       tabPanel("PCA Comparison",
                                fluidRow(
                                  column(6, tags$div(class = "sec-hdr", "Before"),
                                         plotOutput("post_pca_before", height = "auto")),
                                  column(6, tags$div(class = "sec-hdr", "After"),
                                         plotOutput("post_pca_after", height = "auto"))
                                )
                       ),
                       tabPanel("Test Comparison",
                                fluidRow(
                                  column(6, tags$div(class = "sec-hdr", "Before harmonization"),
                                         plotOutput("post_heat_before", height = "500px")),
                                  column(6, tags$div(class = "sec-hdr", "After harmonization"),
                                         plotOutput("post_heat_after", height = "500px"))
                                ),
                                tags$br(),
                                tags$div(class = "sec-hdr", "Univariate Test Summary — before vs after"),
                                DTOutput("post_tbl"),
                                tags$br(),
                                tags$div(class = "sec-hdr", "Multivariate Test Summary — before vs after"),
                                DTOutput("post_tbl_mv")
                       ),
                       tabPanel("Batch Prediction AUC",
                                tags$p(style = "font-size:15px;color:#666;margin:0 0 8px;",
                                       "Before/after comparison of out-of-sample random forest batch-label prediction within each metric."),
                                plotOutput("post_rf_auc_plot", height = "420px"),
                                tags$br(),
                                DTOutput("post_rf_auc_tbl")
                       ),
                       tabPanel("Residual Boxplots",
                                fluidRow(
                                  column(4, uiOutput("post_feat_ui")),
                                  column(4, uiOutput("post_mod_ui")),
                                  column(4, checkboxInput("post_box_jitter", "Show jitter points", FALSE))
                                ),
                                tags$div(class = "sec-hdr", "Additive residuals — Before vs After"),
                                fluidRow(
                                  column(6, plotOutput("post_box_add_before", height = "410px")),
                                  column(6, plotOutput("post_box_add_after",  height = "410px"))
                                ),
                                tags$div(class = "sec-hdr", "Multiplicative residuals — Before vs After"),
                                fluidRow(
                                  column(6, plotOutput("post_box_mul_before", height = "410px")),
                                  column(6, plotOutput("post_box_mul_after",  height = "410px"))
                                )
                       )
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════════════════════
      # TAB 5  EB DIAGNOSTICS  (unchanged)
      # ══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "eb_diag",
              conditionalPanel(
                "input.harm_eb",
                fluidRow(
                  box(title = "Empirical Bayes Diagnostics", width = 12, solidHeader = TRUE,
                      tags$p("Compare estimated prior vs empirical batch parameter distributions. Harmonization must be run first."),
                      uiOutput("eb_controls_ui")
                  )
                ),
                fluidRow(
                  box(title = "Prior vs Empirical", width = 8, solidHeader = TRUE,
                      plotOutput("eb_plot", height = "720px")),
                  box(title = "EB Summary", width = 4, solidHeader = TRUE,
                      uiOutput("eb_summary_tables_ui"),
                      tags$hr(),
                      tags$div(class = "sec-hdr", "Hyperparameters"),
                      uiOutput("eb_hyper"))
                )
              ),
              conditionalPanel(
                "!input.harm_eb",
                fluidRow(
                  box(title = "Empirical Bayes Diagnostics", width = 12, solidHeader = TRUE,
                      tags$div(
                        style = paste0(
                          "background:#fff3cd;border-left:4px solid #f0ad4e;",
                          "padding:14px 16px;border-radius:6px;margin:8px 0;",
                          "font-size:15px;color:#6b4f00;"
                        ),
                        tags$b(icon("circle-info"), " EB diagnostics are disabled"),
                        tags$br(),
                        "Empirical Bayes shrinkage is currently unchecked in the Harmonization settings. ",
                        "Run harmonization with EB enabled to use this tab."
                      )
                  )
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════════════════════
      # TAB 6  COVARIANCE DIAGNOSTICS  (unchanged)
      # ══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "cov_diag",
              uiOutput("cov_diag_panel")
      ),
      
      # ══════════════════════════════════════════════════════════════════════
      # TAB 7  BAYESIAN MCMC DIAGNOSTICS  — NEW
      # ══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "mcmc_diag",
              fluidRow(
                box(title = tags$span(icon("atom"), " Bayesian MCMC Diagnostics"),
                    width = 12, solidHeader = TRUE,
                    style = "border-top: 3px solid #8e44ad;",
                    
                    tags$div(class = "mcmc-info-box",
                             tags$h4(icon("circle-info"), "  How to use this tab"),
                             tags$p("1. Run the downloaded MCMC script on your machine / HPC cluster."),
                             tags$p("2. The script saves two kinds of RDS files to ",
                                    tags$code("results/stan_fits/"), ": ",
                                    tags$code("*_FULL_LOCAL.rds"), " for local reproducibility and ",
                                    tags$code("*_LIGHT_UPLOAD.rds"), " for Shiny upload."),
                             tags$p("3. Upload only the ", tags$code("*_LIGHT_UPLOAD.rds"),
                                    " file(s) below. These contain harmonized data, MCMC summaries, and selected posterior draws."),
                             tags$p("4. Select which parameter family and diagnostic to inspect.")
                    ),
                    
                    fluidRow(
                      column(4,
                             tags$div(class = "sec-hdr", "Upload Stan Result"),
                             uiOutput("mcmc_upload_mode_ui"),
                             tags$hr(),
                             actionBttn("load_mcmc", "Load MCMC Results",
                                        style = "gradient", color = "primary",
                                        icon = icon("upload"), size = "md"),
                             tags$br(), tags$br(),
                             uiOutput("mcmc_load_status_ui")
                      ),
                      
                      column(8,
                             tags$div(class = "sec-hdr", "Loaded MCMC Summary"),
                             uiOutput("mcmc_overview_ui"),
                             uiOutput("mcmc_metric_filter_ui"),
                             tags$hr(),
                             # Convergence traffic-light cards
                             tags$div(class = "sec-hdr", "Convergence at a Glance"),
                             fluidRow(uiOutput("mcmc_conv_cards_ui"))
                      )
                    )
                )
              ),
              
              fluidRow(
                tabBox(title = "Diagnostic Panels", width = 12,
                       
                       # ── R-hat / ESS table ──────────────────────────────────────────
                       tabPanel("R-hat & ESS",
                                tags$p(style = "font-size:15px;color:#666;margin:0 0 8px;",
                                       "R-hat < 1.01 (excellent) / < 1.05 (acceptable) / ≥ 1.05 (poor). ",
                                       "ESS_bulk and ESS_tail should exceed 400 per chain."),
                                fluidRow(
                                  column(3, selectInput("rhat_filter", "Show:",
                                                        choices = c("All parameters" = "all",
                                                                    "R-hat ≥ 1.01 (warning)" = "warn",
                                                                    "R-hat ≥ 1.05 (poor)"   = "poor"),
                                                        selected = "all")),
                                  column(3, selectInput("rhat_param_filter", "Parameter family:",
                                                        choices = c("All" = "all"), selected = "all")),
                                  column(3, numericInput("rhat_top_n", "Show top N by R-hat:", 200, 10, 2000))
                                ),
                                plotly::plotlyOutput("rhat_plot", height = "420px"),
                                tags$br(),
                                DTOutput("rhat_tbl")
                       ),
                       
                       # ── Trace plots ────────────────────────────────────────────────
                       tabPanel("Trace Plots",
                                tags$p(style = "font-size:15px;color:#666;margin:0 0 8px;",
                                       "Trace plots show chain mixing. Good chains look like 'fuzzy caterpillars'."),
                                tags$div(
                                  style = "background:#fff3cd;border-left:4px solid #f0ad4e;padding:8px 12px;border-radius:6px;font-size:13px;margin-bottom:10px;",
                                  icon("triangle-exclamation"),
                                  " Trace plots are available only for parameter families saved in posterior_draws. ",
                                  "Light upload files may exclude very large families such as y_rep."
                                ),
                                fluidRow(
                                  column(4, uiOutput("trace_param_ui")),
                                  column(4, uiOutput("trace_metric_ui")),
                                  column(4,
                                         tags$div(style = "font-size:13px;color:#666;background:#f7f9fc;padding:8px 10px;border-radius:6px;margin-top:25px;",
                                                  "Trace plots use saved post-warmup draws from the uploaded RDS.")
                                  )
                                ),
                                plotOutput("trace_plot", height = "480px")
                       ),
                       
                       # ── Combined posterior summaries and evidence ───────────────────────
                       tabPanel("Posterior Summaries & Evidence",
                                tags$p(
                                  style = "font-size:15px;color:#666;margin:0 0 8px;",
                                  "Summarize posterior estimates, uncertainty, evidence strength, and MCMC convergence for selected Stan parameters. ",
                                  "The forest plot shows posterior means and credible intervals. The table reports posterior summaries, ",
                                  "posterior probabilities, signal-to-uncertainty ratios, R-hat, and effective sample sizes."
                                ),
                                fluidRow(
                                  column(
                                    3,
                                    uiOutput("postev_param_ui")
                                  ),
                                  column(3, uiOutput("postev_family_ui")),
                                  column(
                                    3,
                                    numericInput(
                                      "postev_top_n",
                                      "Top parameters shown:",
                                      value = 25,
                                      min = 5,
                                      max = 200
                                    )
                                  ),
                                  column(
                                    3,
                                    selectInput(
                                      "postev_rank_by",
                                      "Rank plot by:",
                                      choices = c(
                                        "Absolute posterior mean" = "abs_mean",
                                        "Signal-to-uncertainty" = "z_score",
                                        "Posterior probability away from zero" = "prob_away",
                                        "Interval width" = "ci_width"
                                      ),
                                      selected = "z_score"
                                    ),
                                    checkboxInput(
                                      "postev_sort_table",
                                      "Sort table by selected ranking",
                                      value = TRUE
                                    )
                                  )
                                ),
                                uiOutput("postev_entry_filter_ui"),
                                fluidRow(
                                  column(12, plotly::plotlyOutput("postev_forest_plot", height = "560px"))
                                ),
                                tags$hr(),
                                tags$div(class = "sec-hdr", "Full Posterior Summary and Evidence Table"),
                                DTOutput("postev_summary_tbl")
                       )
                )
              )
      )
    )
  )
)

# =============================================================================
#  SERVER
# =============================================================================
server <- function(input, output, session) {
  
  # ── Central reactive store ─────────────────────────────────────────────────
  rv <- reactiveValues(
    data_list    = NULL, bat_list = NULL, covar_list = NULL,
    feat_names   = NULL, batch_levels = NULL,
    m = NULL, n = NULL, G = NULL,
    is_longitudinal = FALSE,
    subid_vec = NULL, visit_vec = NULL, n_subjects = NULL, visits_per_subj = NULL,
    random_var = NULL, visit_col = NULL,
    config_ok   = FALSE, model_fn = NULL, formula_obj = NULL, ref_batch = NULL,
    pre_diag = NULL, pre_pca = NULL, pre_tests = NULL, mv_res = NULL,
    pre_rf_auc = NULL,
    harm = NULL,
    post_diag = NULL, post_pca = NULL, post_tests = NULL, post_mv = NULL,
    post_rf_auc = NULL,
    cov_plot = NULL,
    # MCMC specific
    mcmc_fit_list  = NULL,   # list of loaded RDS objects (length m for uni, length 1 for multi)
    mcmc_draws_list = NULL,   # list of stored posterior draws_df objects extracted from uploaded RDS
    mcmc_summary   = NULL,   # combined data.frame extracted from saved summaries or fit$summary()
    mcmc_mode      = NULL,   # "uni" | "multi"
    mcmc_m         = NULL,
    mcmc_harm_data = NULL,   # harmonized data extracted from uploaded Stan/MCMC RDS
    mcmc_script_txt = NULL   # generated script text
  )
  
  is_long <- reactive({ isTRUE(rv$is_longitudinal) })
  
  has_multi_metric <- reactive({
    !is.null(rv$m) && isTRUE(rv$m >= 2)
  })
  
  single_metric_notice <- function(section = "this section") {
    tags$div(
      style = paste0(
        "background:#fff3cd;border-left:4px solid #f0ad4e;",
        "padding:14px 16px;border-radius:6px;margin:8px 0;",
        "font-size:15px;color:#6b4f00;"
      ),
      tags$b(icon("circle-info"), " Single-metric dataset detected"),
      tags$br(),
      paste0(
        "Only one imaging metric/modality is available, so ", section,
        " is not applicable. For this dataset, use univariate ComBat ",
        "and univariate feature-level diagnostics."
      )
    )
  }
  
  reset_multimetric_results <- function() {
    rv$mv_res      <- NULL
    rv$post_mv     <- NULL
    rv$cov_plot    <- NULL
    rv$pre_rf_auc  <- NULL
    rv$post_rf_auc <- NULL
  }
  
  
  output$harm_mode_ui <- renderUI({
    if (!has_multi_metric()) {
      tagList(
        radioGroupButtons(
          "harm_mode", label = NULL,
          choices = c("Univariate ComBat" = "uni"),
          selected = "uni",
          justified = TRUE
        ),
        tags$div(
          style = "font-size:13px;color:#777;margin-top:6px;",
          icon("circle-info"),
          " Only one imaging metric is available, so multivariate joint harmonization is disabled."
        )
      )
    } else {
      radioGroupButtons(
        "harm_mode", label = NULL,
        choices = c("Univariate (per modality)" = "uni",
                    "Multivariate (joint)" = "multi"),
        selected = if (is.null(input$harm_mode) || !input$harm_mode %in% c("uni", "multi")) "multi" else input$harm_mode,
        justified = TRUE
      )
    }
  })
  
  output$harm_cov_ui <- renderUI({
    if (!has_multi_metric()) {
      tagList(
        checkboxInput("harm_cov", "CovBat covariance adjustment", FALSE),
        tags$script(HTML("$('#harm_cov').prop('disabled', true);")),
        tags$div(
          style = "font-size:13px;color:#777;margin-top:-6px;margin-bottom:8px;",
          icon("circle-info"),
          " Disabled for single-metric univariate ComBat."
        )
      )
    } else {
      tagList(
        checkboxInput("harm_cov", "CovBat covariance adjustment", FALSE),
        conditionalPanel(
          "input.harm_cov",
          sliderInput("harm_vt", "Variance threshold:", 0.5, 1.0, 0.95, 0.05),
          fluidRow(
            column(6, numericInput("harm_minr", "Min PC:", 1, 1, 50)),
            column(6, numericInput("harm_maxr", "Max PC:", 50, 1, 500))
          )
        )
      )
    }
  })
  
  observe({
    req(rv$m)
    if (rv$m < 2 && identical(input$harm_mode, "multi")) {
      updateRadioGroupButtons(session, "harm_mode", selected = "uni")
    }
  })
  
  # =============================================================================
  # MCMC diagnostic helper functions
  # These make the Bayesian diagnostic panels robust to different Stan parameter
  # naming conventions and to different RDS object structures.
  # =============================================================================
  
  has_cmdstan_summary <- function(x) {
    ok <- FALSE
    try(ok <- is.function(x$summary), silent = TRUE)
    isTRUE(ok)
  }
  
  extract_cmdstan_fit <- function(x, max_depth = 5) {
    if (max_depth < 0 || is.null(x)) return(NULL)
    if (has_cmdstan_summary(x)) return(x)
    if (is.list(x)) {
      candidates <- list(
        x$fit, x$stan_fit, x$stan_result, x$stan_result$fit,
        x$full_result, x$full_result$fit, x$full_result$stan_result,
        x$full_result$stan_result$fit
      )
      for (cand in candidates) {
        out <- extract_cmdstan_fit(cand, max_depth = max_depth - 1)
        if (!is.null(out)) return(out)
      }
      for (cand in x) {
        out <- extract_cmdstan_fit(cand, max_depth = max_depth - 1)
        if (!is.null(out)) return(out)
      }
    }
    NULL
  }
  
  extract_mcmc_summary <- function(x, max_depth = 5) {
    # Prefer summaries saved directly inside the RDS. This is more robust for
    # terminal/Rscript runs because CmdStan CSV files referenced by the fit object
    # may be temporary and unavailable after the R session exits.
    if (max_depth < 0 || is.null(x)) return(NULL)
    if (is.data.frame(x)) return(standardize_mcmc_summary(x))
    if (is.list(x)) {
      candidates <- list(
        x$mcmc_summary,
        x$summary,
        x$posterior_summary,
        x$stan_summary,
        x$full_result$mcmc_summary,
        x$stan_result$mcmc_summary
      )
      for (cand in candidates) {
        out <- extract_mcmc_summary(cand, max_depth = max_depth - 1)
        if (!is.null(out)) return(out)
      }
    }
    fit <- extract_cmdstan_fit(x)
    if (is.null(fit)) return(NULL)
    smry <- tryCatch(fit$summary(), error = function(e) NULL)
    standardize_mcmc_summary(smry)
  }
  
  extract_posterior_draws <- function(x, max_depth = 5) {
    # Prefer draws saved directly inside the RDS. This avoids relying on external
    # CmdStan CSV output files, which can be lost after running via Rscript.
    if (max_depth < 0 || is.null(x)) return(NULL)
    if (inherits(x, "draws_df") || is.data.frame(x)) return(as.data.frame(x, check.names = FALSE))
    if (is.list(x)) {
      candidates <- list(
        x$posterior_draws,
        x$draws,
        x$mcmc_draws,
        x$full_result$posterior_draws,
        x$stan_result$posterior_draws
      )
      for (cand in candidates) {
        out <- extract_posterior_draws(cand, max_depth = max_depth - 1)
        if (!is.null(out)) return(out)
      }
    }
    fit <- extract_cmdstan_fit(x)
    if (is.null(fit)) return(NULL)
    tryCatch(as.data.frame(fit$draws(format = "draws_df"), check.names = FALSE), error = function(e) NULL)
  }

  summarize_mcmc_draws_with_rhat_safe <- function(draws_df, chunk_size = 500) {
    HarmonizeR::summarize_draws_with_rhat_safe(draws_df, chunk_size = chunk_size)
  }

  repair_mcmc_summary_from_draws <- function(obj, smry = NULL, chunk_size = 500) {
    smry <- standardize_mcmc_summary(smry)
    if (!is.null(smry)) return(smry)

    draws_df <- extract_posterior_draws(obj)
    if (is.null(draws_df)) return(NULL)

    showNotification(
      "mcmc_summary was missing; reconstructing posterior summary, R-hat, and ESS from saved posterior_draws.",
      type = "message",
      duration = 6
    )

    standardize_mcmc_summary(
      summarize_mcmc_draws_with_rhat_safe(draws_df, chunk_size = chunk_size)
    )
  }
  
  extract_harm_data_from_mcmc_export <- function(x, max_depth = 5) {
    if (max_depth < 0 || is.null(x)) return(NULL)
    
    if (is.data.frame(x) || is.matrix(x)) {
      return(list(as.data.frame(x, check.names = FALSE)))
    }
    
    if (is.list(x)) {
      candidates <- list(
        x$harm_data,
        x$harmonized_data,
        x$harmonized,
        x$full_result$harm_data,
        x$full_result$harmonized_data,
        x$stan_result$harm_data
      )
      
      for (cand in candidates) {
        out <- extract_harm_data_from_mcmc_export(cand, max_depth = max_depth - 1)
        if (!is.null(out)) return(out)
      }
      
      # A plain list of modality-specific matrices/data frames.
      if (length(x) > 0 && all(vapply(x, function(z) is.data.frame(z) || is.matrix(z), logical(1)))) {
        return(lapply(x, as.data.frame, check.names = FALSE))
      }
    }
    
    NULL
  }
  
  validate_mcmc_harm_data <- function(harm_data, expected_m = rv$m, expected_n = rv$n, expected_G = rv$G) {
    if (is.null(harm_data) || !is.list(harm_data)) {
      stop("No harmonized data were found in the uploaded RDS. Make sure the RDS was generated by the HarmonizeR MCMC project script and contains `harm_data`.")
    }
    
    if (!is.null(expected_m) && length(harm_data) != expected_m) {
      stop(paste0("The uploaded harmonized data contain ", length(harm_data),
                  " modality/modalities, but the active dataset has ", expected_m, "."))
    }
    
    out <- lapply(seq_along(harm_data), function(i) {
      d <- as.data.frame(harm_data[[i]], check.names = FALSE)
      
      if (!is.null(expected_n) && nrow(d) != expected_n) {
        stop(paste0("Uploaded MCMC harmonized data for modality ", i, " have ",
                    nrow(d), " rows, but the active dataset has ", expected_n, " observations."))
      }
      
      if (!is.null(expected_G) && ncol(d) != expected_G) {
        stop(paste0("Uploaded MCMC harmonized data for modality ", i, " have ",
                    ncol(d), " columns, but the active dataset has ", expected_G, " features."))
      }
      
      if (!is.null(rv$feat_names) && length(rv$feat_names) == ncol(d)) {
        colnames(d) <- rv$feat_names
      }
      
      rownames(d) <- rownames(as.data.frame(rv$data_list[[i]]))
      d
    })
    
    out
  }
  
  use_mcmc_harm_data_for_post <- function(harm_data) {
    harm_data <- validate_mcmc_harm_data(harm_data)
    
    # Store it in rv$harm so the original Post-Harmonization code remains unchanged.
    # resid is only for preview/download compatibility; post diagnostics recompute residuals.
    resid_placeholder <- lapply(seq_along(harm_data), function(i) {
      as.data.frame(as.matrix(harm_data[[i]]) - as.matrix(as.data.frame(rv$data_list[[i]])),
                    check.names = FALSE)
    })
    
    rv$harm <- list(
      mode = "mcmc_uploaded",
      harm_data = harm_data,
      resid = resid_placeholder,
      source = "Uploaded Stan/MCMC RDS"
    )
    
    rv$post_diag <- NULL
    rv$post_pca <- NULL
    rv$post_tests <- NULL
    rv$post_mv <- NULL
    
    invisible(TRUE)
  }
  
  # ──────────────────────────────────────────────────────────────────────────
  # SIDEBAR INFO
  # ──────────────────────────────────────────────────────────────────────────
  output$sidebar_info <- renderUI({
    if (is.null(rv$data_list))
      return(tags$span(style = "color:#e67e22;", "No data loaded"))
    lines <- list(
      tags$div(paste0(rv$n, " obs  ×  ", rv$G, " features")),
      tags$div(paste0(rv$m, " modalities")),
      tags$div(paste0(length(rv$batch_levels), " batches: ",
                      paste(rv$batch_levels, collapse = ", ")))
    )
    if (is_long()) {
      lines <- c(lines, list(
        tags$div(paste0(rv$n_subjects, " subjects")),
        tags$div(paste0("Visit col: ", rv$visit_col %||% "visit"))
      ))
    }
    tags$div(lines)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # 1. DATA SETUP  (all unchanged from original)
  # ══════════════════════════════════════════════════════════════════════════
  
  observeEvent(input$gen_demo, {
    d <- if (input$demo_type == "long") {
      HarmonizeR::simulate_demo_longitudinal_data(n_subjects = input$demo_nsubj,
                                  G = input$demo_G, m = input$demo_m,
                                  n_batches = input$demo_nb, max_visits = input$demo_maxv)
    } else {
      HarmonizeR::simulate_demo_data(n = input$demo_n, G = input$demo_G,
                     m = input$demo_m, n_batches = input$demo_nb)
    }
    rv$data_list    <- d$data;  rv$bat_list  <- d$bat;  rv$covar_list <- d$covar
    rv$feat_names   <- d$feat_names;  rv$batch_levels <- d$batch_levels
    rv$m <- d$m;  rv$n <- d$n;  rv$G <- d$G
    rv$is_longitudinal <- isTRUE(d$is_longitudinal)
    rv$subid_vec <- d$subid_vec;  rv$visit_vec <- d$visit_vec
    rv$n_subjects <- d$n_subjects;  rv$visits_per_subj <- d$visits_per_subj
    rv$random_var <- if (rv$is_longitudinal) "subid" else NULL
    rv$visit_col  <- if (rv$is_longitudinal) "visit" else NULL
    rv$config_ok <- FALSE;  rv$harm <- NULL
    reset_multimetric_results()
    if (rv$m < 2) updateRadioGroupButtons(session, "harm_mode", selected = "uni")
    showNotification(
      paste0(if (rv$is_longitudinal) "Longitudinal" else "Cross-sectional",
             " demo: m=", d$m, " modalities, n=", d$n, " obs, G=", d$G, " features."),
      type = "message", duration = 4)
  })
  
  
  output$upload_ui <- renderUI({
    m <- safe_positive_int(input$upload_m, default = 1L, min_val = 1L, max_val = 10L)
    tagList(lapply(seq_len(m), function(i) {
      fileInput(paste0("fmod", i), paste0("Modality ", i, " CSV:"), accept = ".csv")
    }))
  })
  
  read_feature_csv <- function(path) {
    raw <- readLines(path, n = 2, warn = FALSE)
    if (length(raw) < 2) {
      stop("Feature CSV must contain a header row and at least one data row.")
    }
    df0 <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    df0 <- as.data.frame(df0, check.names = FALSE, stringsAsFactors = FALSE)
    names(df0) <- trimws(names(df0))
    if (ncol(df0) < 1) stop("Feature CSV has no columns.")
    header_starts_empty <- grepl("^,", raw[1])
    first_name <- names(df0)[1]
    first_col  <- df0[[1]]
    rest_cols  <- if (ncol(df0) >= 2) df0[-1] else NULL
    explicit_row_id <- first_name %in% c("", "X", "row.names", "Row.names", ".__row_id__", "row_id", "sample_id", "subject_visit")
    first_non_numeric <- numeric_like_prop(first_col) < 0.50
    rest_numeric_prop <- if (!is.null(rest_cols)) mean(vapply(rest_cols, numeric_like_prop, numeric(1)) >= 0.80) else 0
    first_unique <- length(unique(as.character(first_col))) == length(first_col)
    should_drop_first <- ncol(df0) >= 2 && rest_numeric_prop >= 0.80 &&
      (header_starts_empty || explicit_row_id || (first_non_numeric && first_unique))
    if (should_drop_first) {
      rownames(df0) <- make.unique(as.character(first_col))
      df0 <- df0[-1]
    }
    for (nm in names(df0)) {
      df0[[nm]] <- to_numeric_if_possible(df0[[nm]], min_prop = 0.95)
    }
    non_numeric <- names(df0)[!vapply(df0, is.numeric, logical(1))]
    if (length(non_numeric) > 0) {
      stop(
        "The following uploaded feature columns are not numeric after parsing: ",
        paste(head(non_numeric, 10), collapse = ", "),
        if (length(non_numeric) > 10) " ..." else "",
        ". If the first column is a row ID, make sure all remaining columns are numeric features."
      )
    }
    as.data.frame(df0, check.names = FALSE)
  }
  
  observeEvent(input$load_files, {
    req(input$file_batch)
    m <- safe_positive_int(input$upload_m, default = 1L, min_val = 1L, max_val = 10L)
    is_long_upload <- identical(input$upload_type, "long")
    tryCatch({
      bat_raw <- read.csv(input$file_batch$datapath, check.names = FALSE, stringsAsFactors = FALSE)
      if (ncol(bat_raw) < 1) stop("Batch labels CSV must contain at least one column.")
      bat_vec <- factor(bat_raw[[1]])
      data_list <- lapply(seq_len(m), function(i) {
        fi <- input[[paste0("fmod", i)]]
        req(fi)
        read_feature_csv(fi$datapath)
      })
      n_rows <- vapply(data_list, nrow, integer(1))
      if (length(unique(n_rows)) != 1) {
        stop(paste0("Modalities have different row counts: ", paste(n_rows, collapse = ", ")))
      }
      if (nrow(data_list[[1]]) != length(bat_vec)) {
        stop(paste0("Feature matrix has ", nrow(data_list[[1]]),
                    " rows but batch file has ", length(bat_vec), " rows."))
      }
      re_col_name    <- trimws(input$re_col %||% "subid")
      visit_col_name <- trimws(input$visit_col %||% "visit")
      covar_df <- if (!is.null(input$file_covar)) {
        raw_covar <- read.csv(input$file_covar$datapath, check.names = FALSE, stringsAsFactors = FALSE)
        clean_uploaded_covar(raw_covar, subject_id_cols = if (is_long_upload) re_col_name else character())
      } else {
        NULL
      }
      if (!is.null(covar_df) && nrow(covar_df) != nrow(data_list[[1]])) {
        stop(paste0("Covariate file has ", nrow(covar_df),
                    " rows but feature matrices have ", nrow(data_list[[1]]), " rows."))
      }
      subid_vec_up <- visit_vec_up <- n_subjects_up <- n_vis_per_subj_up <- NULL
      if (is_long_upload) {
        if (is.null(covar_df)) stop("Longitudinal upload requires a covariates CSV containing subject ID and visit columns.")
        if (!re_col_name %in% colnames(covar_df)) stop(paste0("Subject ID column '", re_col_name, "' not found."))
        if (!visit_col_name %in% colnames(covar_df)) stop(paste0("Visit column '", visit_col_name, "' not found."))
        subid_vec_up <- as.character(covar_df[[re_col_name]])
        visit_vec_up <- suppressWarnings(as.integer(covar_df[[visit_col_name]]))
        if (any(is.na(visit_vec_up))) {
          stop(paste0("Visit column '", visit_col_name, "' contains values that cannot be converted to integer."))
        }
        n_subjects_up <- length(unique(subid_vec_up))
        n_vis_per_subj_up <- as.integer(table(subid_vec_up))
      }
      rv$data_list    <- data_list
      rv$bat_list     <- replicate(m, bat_vec, simplify = FALSE)
      rv$covar_list   <- replicate(m, covar_df, simplify = FALSE)
      rv$feat_names   <- colnames(data_list[[1]])
      rv$batch_levels <- levels(bat_vec)
      rv$m <- m
      rv$n <- nrow(data_list[[1]])
      rv$G <- ncol(data_list[[1]])
      rv$is_longitudinal  <- is_long_upload
      rv$subid_vec        <- subid_vec_up
      rv$visit_vec        <- visit_vec_up
      rv$n_subjects       <- n_subjects_up
      rv$visits_per_subj  <- n_vis_per_subj_up
      rv$random_var       <- if (is_long_upload) re_col_name else NULL
      rv$visit_col        <- if (is_long_upload) visit_col_name else NULL
      rv$config_ok <- FALSE
      rv$harm <- NULL
      reset_multimetric_results()
      if (rv$m < 2) updateRadioGroupButtons(session, "harm_mode", selected = "uni")
      showNotification(
        paste0(if (is_long_upload) "Longitudinal" else "Cross-sectional",
               " data: ", m, " modalities, ", rv$n, " obs × ", rv$G, " features."),
        type = "message", duration = 5)
    }, error = function(e) showNotification(paste("Load error:", e$message), type = "error"))
  })
  
  output$data_type_badge <- renderUI({
    req(rv$data_list)
    if (is_long()) tags$span(class = "long-badge", icon("timeline"), " Longitudinal dataset")
    else           tags$span(class = "cs-badge",   icon("table"),    " Cross-sectional dataset")
  })
  
  sc <- function(v, l, s = NULL) tags$div(class = "stat-card",
                                          tags$div(class = "val", v), tags$div(class = "lbl", l),
                                          if (!is.null(s)) tags$div(class = "sub", s))
  
  output$summary_cards <- renderUI({
    req(rv$data_list)
    if (is_long()) {
      med_vis <- if (!is.null(rv$visits_per_subj)) median(rv$visits_per_subj) else "?"
      tagList(
        column(2, sc(rv$n_subjects, "Subjects")),
        column(2, sc(rv$n, "Observations")),
        column(2, sc(rv$G, "Features")),
        column(2, sc(rv$m, "Modalities")),
        column(2, sc(length(rv$batch_levels), "Batches",
                     paste(rv$batch_levels, collapse = " · "))),
        column(2, sc(med_vis, "Median Visits/Subj"))
      )
    } else {
      tagList(
        column(3, sc(rv$n, "Samples")),
        column(3, sc(rv$G, "Features")),
        column(3, sc(rv$m, "Modalities")),
        column(3, sc(length(rv$batch_levels), "Batches",
                     paste(rv$batch_levels, collapse = " · ")))
      )
    }
  })
  
  output$mod_tags <- renderUI({
    m <- safe_m_value(rv$m)
    req(!is.null(m))
    tags$div(lapply(seq_len(m), function(i)
      tags$span(class = "mod-tag", paste0("M", i))))
  })
  
  output$long_structure_ui <- renderUI({
    if (!is_long()) return(NULL)
    tagList(
      tags$div(class = "sec-hdr", "Longitudinal Structure"),
      fluidRow(
        column(4, tags$div(class = "sec-hdr", style = "font-size:14px;",
                           "Observations by batch × visit"),
               plotOutput("long_batch_visit_heat", height = "200px")),
        column(4, tags$div(class = "sec-hdr", style = "font-size:14px;",
                           "Visits per subject"),
               plotOutput("long_visits_hist", height = "200px")),
        column(4, tags$div(class = "sec-hdr", style = "font-size:14px;",
                           "Retention / dropout by batch"),
               plotOutput("long_retention", height = "200px"))
      ),
      tags$hr()
    )
  })
  
  bold_theme <- function(base_size = 12) {
    theme_minimal(base_size = base_size) %+replace% theme(
      axis.title = element_text(face = "bold", size = base_size),
      axis.text  = element_text(face = "bold", size = base_size * 0.85),
      axis.ticks = element_line(linewidth = 0.6, colour = "#555"),
      axis.ticks.length = unit(4, "pt"),
      legend.title = element_text(face = "bold", size = base_size * 0.95),
      legend.text  = element_text(size = base_size * 0.90),
      strip.text   = element_text(face = "bold", size = base_size),
      plot.title   = element_text(face = "bold", hjust = 0, size = base_size * 1.1),
      plot.subtitle = element_text(size = base_size * 0.90, color = "#555"),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank()
    )
  }
  
  output$long_batch_visit_heat <- renderPlot({
    req(is_long(), rv$bat_list, rv$visit_vec)
    df <- data.frame(Batch = rv$bat_list[[1]], Visit = factor(rv$visit_vec)) %>%
      group_by(Batch, Visit) %>% summarise(n = n(), .groups = "drop")
    ggplot(df, aes(x = Visit, y = Batch, fill = n)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = n), fontface = "bold", size = 3.5) +
      scale_fill_gradient(low = "#eaf2fb", high = "#1a5276", name = "Count") +
      labs(x = "Visit", y = "Batch", title = "Obs count") +
      bold_theme(11) +
      theme(panel.grid = element_blank(), plot.background = element_rect(fill="white",color=NA))
  }, bg = "white")
  
  output$long_visits_hist <- renderPlot({
    req(is_long(), rv$visits_per_subj)
    ggplot(data.frame(n_visits = rv$visits_per_subj), aes(x = factor(n_visits))) +
      geom_bar(fill = "#2e7d32", color = "white", width = 0.6, alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)),
                vjust = -0.35, size = 3.4, fontface = "bold") +
      scale_y_continuous(expand = expansion(mult = c(0, .18))) +
      labs(x = "Visits completed", y = "# Subjects", title = "Visit distribution") +
      bold_theme(11) +
      theme(panel.grid.major.x = element_blank(), plot.background = element_rect(fill="white",color=NA))
  }, bg = "white")
  
  output$long_retention <- renderPlot({
    req(is_long(), rv$bat_list, rv$visit_vec, rv$subid_vec)
    max_v <- max(rv$visit_vec)
    df_ret <- do.call(rbind, lapply(rv$batch_levels, function(b) {
      idx_b  <- which(rv$bat_list[[1]] == b)
      subjs_b <- unique(rv$subid_vec[idx_b])
      n_total <- length(subjs_b)
      do.call(rbind, lapply(seq_len(max_v), function(v) {
        subjs_v <- unique(rv$subid_vec[idx_b][rv$visit_vec[idx_b] >= v])
        data.frame(Batch = b, Visit = v, PropRetained = length(subjs_v)/max(n_total,1))
      }))
    }))
    df_ret$Batch <- clean_batch_for_plot(df_ret$Batch)
    batch_cols <- safe_discrete_palette(levels(df_ret$Batch))
    ggplot(df_ret, aes(x = Visit, y = PropRetained, color = Batch, group = Batch)) +
      geom_line(linewidth = 1.1) + geom_point(size = 2.5) +
      scale_color_manual(values = batch_cols) +
      scale_y_continuous(labels = percent_format(), limits = c(0, 1.05)) +
      scale_x_continuous(breaks = seq_len(max_v)) +
      labs(x = "Visit", y = "Proportion retained", title = "Retention") +
      bold_theme(11) +
      theme(plot.background = element_rect(fill = "white", color = NA))
  }, bg = "white")
  
  output$covar_section_title <- renderUI({
    if (is_long()) "Baseline / Subject-level Covariates by Batch"
    else "Covariate Distributions by Batch"
  })
  output$covar_section_hint <- renderUI({
    if (is_long()) "Baseline-only (one row per subject). Continuous → boxplot | Categorical → count bar"
    else "Continuous → boxplot  |  Factor / character → count bar"
  })
  
  output$batch_bar <- plotly::renderPlotly({
    req(rv$bat_list, length(rv$bat_list) >= 1L, !is.null(rv$bat_list[[1]]))
    bat <- clean_batch_for_plot(rv$bat_list[[1]])
    bat <- bat[!is.na(bat)]
    validate(need(length(bat) > 0, "No valid batch labels are available."))
    df    <- as.data.frame(table(Batch = bat), stringsAsFactors = FALSE)
    n_bat <- nrow(df)
    pal   <- safe_discrete_palette(as.character(df$Batch))
    
    plotly::plot_ly(
      data = df,
      x = ~Batch, y = ~Freq,
      type = "bar",
      marker = list(color = unname(pal), line = list(color = "white", width = 1.2)),
      text = ~Freq, textposition = "inside",
      hovertemplate = "<b>%{x}</b><br>Count: %{y}<extra></extra>"
    ) %>%
      plotly::layout(
        xaxis = list(
          title          = "",
          showticklabels = FALSE,
          showgrid       = FALSE,
          zeroline       = FALSE
        ),
        yaxis = list(
          title          = "",
          showticklabels = FALSE,
          showgrid       = FALSE,
          zeroline       = FALSE
        ),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        showlegend    = FALSE,
        margin        = list(t = 30, b = 10, l = 10, r = 10)
      )
  })
  
  output$covar_dist_plot <- plotly::renderPlotly({
    req(rv$covar_list, rv$bat_list)
    cov <- rv$covar_list[[1]]
    bat <- rv$bat_list[[1]]
    
    if (is.null(cov) || ncol(cov) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = list(text = "No covariates loaded"),
                              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }
    
    exclude_cols <- c(rv$random_var, rv$visit_col)
    if (is_long() && !is.null(rv$subid_vec)) {
      subid_col   <- rv$random_var
      all_cols    <- setdiff(colnames(cov), exclude_cols)
      static_cols <- if (!is.null(subid_col) && subid_col %in% colnames(cov)) {
        Filter(function(v) {
          !any(tapply(cov[[v]], cov[[subid_col]], function(x) length(unique(x)) > 1))
        }, all_cols)
      } else all_cols
      first_idx  <- !duplicated(rv$subid_vec)
      cov_plot   <- cov[first_idx, static_cols, drop = FALSE]
      bat_plot   <- bat[first_idx]
    } else {
      static_cols <- setdiff(colnames(cov), exclude_cols)
      cov_plot    <- cov[, static_cols, drop = FALSE]
      bat_plot    <- bat
    }
    
    if (length(static_cols) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = list(text = "No baseline covariates to display"),
                              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }
    
    bat_plot <- clean_batch_for_plot(bat_plot)
    batches <- levels(droplevels(bat_plot))
    n_bat   <- length(batches)
    pal     <- safe_discrete_palette(batches)
    
    # Shared clean axis: no titles, no tick labels, no grid lines
    clean_axis <- list(
      title          = "",
      showticklabels = FALSE,
      showgrid       = FALSE,
      zeroline       = FALSE,
      ticks          = ""
    )
    
    subplot_list <- lapply(seq_along(static_cols), function(j) {
      vname <- static_cols[j]
      v     <- cov_plot[[vname]]
      
      if (is.numeric(v)) {
        # Single plot_ly + add_boxplot() per batch — one axis, clean_axis applies fully
        p <- plotly::plot_ly()
        for (bi in seq_along(batches)) {
          b    <- batches[bi]
          vals <- v[as.character(bat_plot) == b]
          p <- p %>% plotly::add_boxplot(
            x             = rep(b, length(vals)),
            y             = vals,
            name          = b,
            marker        = list(color = pal[bi], opacity = 0.6),
            line          = list(color = "#333333"),
            fillcolor     = pal[bi],
            opacity       = 0.75,
            showlegend    = (j == 1),
            legendgroup   = b,
            hovertemplate = paste0("<b>", b, "</b><br>", vname, ": %{y:.3f}<extra></extra>")
          )
        }
        p %>% plotly::layout(
          xaxis = clean_axis,
          yaxis = clean_axis,
          annotations = list(list(
            text      = vname, x = 0.5, y = 1.25,
            xref      = "paper", yref = "paper",
            showarrow = FALSE,
            font      = list(size = 12, color = "#1a2b4a"),
            xanchor   = "center"
          ))
        )
        
      } else {
        # Percentage stacked bar — each bar sums to 100%
        v_chr   <- as.character(v)
        cats    <- sort(unique(v_chr[!is.na(v_chr)]))
        cat_pal <- safe_discrete_palette(cats, palette = "Pastel 1")
        
        # Pre-compute counts and totals per batch for percentage calculation
        count_mat <- sapply(batches, function(b)
          sapply(cats, function(cat_i)
            sum(v_chr[as.character(bat_plot) == b] == cat_i, na.rm = TRUE)))
        # count_mat: rows = cats, cols = batches
        totals <- colSums(count_mat)
        
        p <- plotly::plot_ly()
        for (ci in seq_along(cats)) {
          cat_i   <- cats[ci]
          counts  <- count_mat[ci, ]
          pcts    <- ifelse(totals > 0, round(100 * counts / totals, 1), 0)
          p <- p %>% plotly::add_bars(
            x             = batches,
            y             = pcts,
            name          = cat_i,
            marker        = list(color = cat_pal[ci],
                                 line  = list(color = "white", width = 0.8)),
            showlegend    = (j == 1),
            legendgroup   = paste0("cat_", cat_i),
            customdata    = counts,
            hovertemplate = paste0("<b>%{x}</b><br>", vname, " = ", cat_i,
                                   "<br>%{y}%  (n = %{customdata})<extra></extra>")
          )
        }
        p %>% plotly::layout(
          barmode = "stack",
          xaxis   = clean_axis,
          yaxis   = modifyList(clean_axis, list(range = c(0, 100))),
          annotations = list(list(
            text      = vname, x = 0.5, y = 1.25,
            xref      = "paper", yref = "paper",
            showarrow = FALSE,
            font      = list(size = 12, color = "#1a2b4a"),
            xanchor   = "center"
          ))
        )
      }
    })
    
    if (length(subplot_list) == 1) {
      final_p <- subplot_list[[1]]
    } else {
      final_p <- plotly::subplot(
        subplot_list,
        nrows  = 1,
        shareX = FALSE,
        shareY = FALSE,
        titleX = FALSE,
        titleY = FALSE,
        margin = 0.06
      )
    }
    
    final_p %>% plotly::layout(
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      legend        = list(orientation = "h", x = 0, y = -0.15),
      margin        = list(t = 40, b = 50, l = 10, r = 10)
    )
  })
  
  output$timevar_covar_ui <- renderUI({
    if (!is_long()) return(NULL)
    cov <- rv$covar_list[[1]]; if (is.null(cov)) return(NULL)
    exclude_cols <- c(rv$random_var, rv$visit_col)
    numeric_cols <- setdiff(names(which(sapply(cov, is.numeric))), exclude_cols)
    if (length(numeric_cols) == 0) return(NULL)
    subid_col  <- rv$random_var
    is_varying <- if (!is.null(subid_col) && subid_col %in% colnames(cov)) {
      sapply(numeric_cols, function(v)
        any(tapply(cov[[v]], cov[[subid_col]], function(x) length(unique(x)) > 1)))
    } else setNames(rep(TRUE, length(numeric_cols)), numeric_cols)
    tv_cols <- numeric_cols[is_varying]; st_cols <- numeric_cols[!is_varying]
    all_choices <- c(setNames(tv_cols, tv_cols),
                     setNames(st_cols, paste0(st_cols, "  (static)")))
    default_sel <- if (length(tv_cols) > 0) tv_cols[1] else numeric_cols[1]
    tagList(
      tags$div(class = "sec-hdr", "Numeric Covariates by Visit & Batch"),
      tags$p(style = "font-size:14px;color:#777;margin:0 0 6px;",
             "Mean ± SE profile across visits, stratified by batch."),
      fluidRow(
        column(4, selectInput("tv_covar_sel", "Covariate:", choices = all_choices,
                              selected = default_sel)),
        column(4, selectInput("tv_plot_type", "Plot type:",
                              choices = c("Mean profile (line)" = "line",
                                          "Boxplot by visit" = "box",
                                          "Spaghetti (individual)" = "spaghetti"),
                              selected = "line"))
      ),
      plotOutput("timevar_covar_plot", height = "240px"),
      tags$hr()
    )
  })
  
  output$timevar_covar_plot <- renderPlot({
    req(is_long(), rv$covar_list, rv$bat_list, rv$visit_vec,
        input$tv_covar_sel, input$tv_plot_type)
    cov <- rv$covar_list[[1]]; bat <- rv$bat_list[[1]]
    vname <- input$tv_covar_sel
    if (!vname %in% colnames(cov)) return(NULL)
    df <- data.frame(value = suppressWarnings(as.numeric(cov[[vname]])),
                     Visit = factor(rv$visit_vec), Batch = clean_batch_for_plot(bat),
                     Subject = rv$subid_vec %||% paste0("S", seq_len(rv$n)))
    df <- df[is.finite(df$value) & !is.na(df$Batch), , drop = FALSE]
    validate(need(nrow(df) > 0, "No valid observations are available for this covariate plot."))
    batch_cols <- safe_discrete_palette(levels(droplevels(df$Batch)))
    if (input$tv_plot_type == "line") {
      sum_df <- df %>% group_by(Visit, Batch) %>%
        summarise(mean_val = mean(value, na.rm=TRUE),
                  se = sd(value, na.rm=TRUE)/sqrt(n()), .groups="drop")
      ggplot(sum_df, aes(x=Visit, y=mean_val, color=Batch, group=Batch)) +
        geom_line(linewidth=1.1) + geom_point(size=2.5) +
        geom_errorbar(aes(ymin=mean_val-se, ymax=mean_val+se), width=0.15, linewidth=0.7) +
        scale_color_manual(values=batch_cols) +
        labs(x="Visit", y=paste("Mean", vname), title=paste0(vname," — mean profile")) +
        bold_theme(12)
    } else if (input$tv_plot_type == "box") {
      ggplot(df, aes(x=Visit, y=value, fill=Batch)) +
        geom_boxplot(outlier.size=0.6, width=0.6, linewidth=0.4, position=position_dodge(0.8)) +
        scale_fill_manual(values=batch_cols) +
        labs(x="Visit", y=vname, title=paste0(vname," distribution by visit")) +
        bold_theme(12)
    } else {
      sum_df <- df %>% group_by(Visit, Batch) %>%
        summarise(mean_val = mean(value, na.rm=TRUE), .groups="drop")
      ggplot(df, aes(x=Visit, y=value, group=Subject, color=Batch)) +
        geom_line(alpha=0.15, linewidth=0.5) +
        geom_line(data=sum_df, aes(x=Visit, y=mean_val, color=Batch, group=Batch),
                  linewidth=2, inherit.aes=FALSE) +
        scale_color_manual(values=batch_cols) +
        labs(x="Visit", y=vname, title=paste0(vname," — spaghetti + batch mean"),
             subtitle="Thin = individual, thick = batch mean") +
        bold_theme(12)
    }
  }, bg = "white")
  
  output$prev_mod_ui <- renderUI({
    m <- safe_m_value(rv$m)
    req(!is.null(m))
    selectInput("prev_mod", NULL,
                choices = setNames(seq_len(m), paste0("Modality ", seq_len(m))),
                selected = 1, width = "140px")
  })
  
  output$data_prev <- renderDT({
    req(rv$data_list, rv$bat_list, input$prev_mod)
    i      <- as.integer(input$prev_mod)
    feat   <- rv$data_list[[i]];  bat <- rv$bat_list[[i]];  cov <- rv$covar_list[[i]]
    n_show <- min(10, nrow(feat)); g_show <- min(12, ncol(feat))
    feat_df <- as.data.frame(
      lapply(feat[1:n_show, 1:g_show, drop = FALSE],
             function(x) if (is.numeric(x)) round(x, 3) else as.character(x)),
      stringsAsFactors = FALSE)
    batch_col <- data.frame(batch = as.character(bat[1:n_show]), stringsAsFactors = FALSE)
    if (!is.null(cov)) {
      cov_sub <- cov[1:n_show, , drop = FALSE]
      cov_df  <- as.data.frame(
        lapply(cov_sub, function(x) {
          if (is.factor(x)) as.character(x)
          else if (is.numeric(x)) round(x, 3)
          else as.character(x)
        }), stringsAsFactors = FALSE)
      cov_cols_show <- colnames(cov_df)
      df_full <- data.frame(batch_col, cov_df, feat_df, check.names=FALSE, stringsAsFactors=FALSE)
    } else {
      cov_cols_show <- character(0)
      df_full <- data.frame(batch_col, feat_df, check.names=FALSE, stringsAsFactors=FALSE)
    }
    dt <- datatable(df_full, rownames=TRUE,
                    options=list(dom="t", scrollX=TRUE, pageLength=n_show),
                    class="compact stripe") %>%
      formatStyle("batch", backgroundColor="#fff3cd", fontWeight="bold")
    if (length(cov_cols_show) > 0)
      dt <- formatStyle(dt, columns=cov_cols_show, backgroundColor="#d4edda")
    dt
  })
  
  cont_covars <- reactive({
    req(rv$covar_list)
    cov <- rv$covar_list[[1]]
    if (is.null(cov) || ncol(cov) == 0) return(character(0))
    exclude_cols <- c(rv$random_var, rv$visit_col)
    exclude_cols <- exclude_cols[!is.na(exclude_cols) & nzchar(exclude_cols)]
    numeric_cols <- names(cov)[vapply(cov, is.numeric, logical(1))]
    setdiff(numeric_cols, exclude_cols)
  })
  
  output$trend_covar_ui <- renderUI({
    cc <- cont_covars()
    if (length(cc) == 0) {
      return(tags$p(
        style = "font-size:12px;color:#c0392b;",
        "No continuous covariates detected. For uploaded data, check that numeric covariates were not read as character."
      ))
    }
    selectInput("trend_covar", "Continuous covariate:", choices = cc, selected = cc[1])
  })
  output$trend_feat_ui <- renderUI({
    req(rv$feat_names)
    if (length(rv$feat_names) == 0) return(NULL)
    selectInput("trend_feat", "Feature (y-axis):", choices = rv$feat_names, selected = rv$feat_names[1])
  })
  output$trend_mod_ui <- renderUI({
    m <- safe_m_value(rv$m)
    req(!is.null(m))
    selectInput("trend_mod", "Modality:",
                choices = setNames(seq_len(m), paste0("M", seq_len(m))), selected = 1)
  })
  
  output$trend_plot <- renderPlot({
    req(rv$data_list, rv$covar_list, rv$bat_list,
        input$trend_covar, input$trend_feat, input$trend_mod)
    cc <- cont_covars()
    validate(
      need(length(cc) > 0, "No continuous covariates detected."),
      need(input$trend_covar %in% cc, "Selected covariate is not available."),
      need(input$trend_feat %in% rv$feat_names, "Selected feature is not available.")
    )
    i <- safe_positive_int(input$trend_mod, default = 1L, min_val = 1L, max_val = length(rv$data_list))
    feat <- input$trend_feat
    cvar <- input$trend_covar
    dat  <- rv$data_list[[i]]
    cov  <- rv$covar_list[[i]]
    bat  <- rv$bat_list[[i]]
    validate(
      need(!is.null(dat) && feat %in% colnames(dat), "Feature not found in the selected modality."),
      need(!is.null(cov) && cvar %in% colnames(cov), "Covariate not found in uploaded covariate file."),
      need(nrow(dat) == nrow(cov), "Feature and covariate files have different numbers of rows."),
      need(nrow(dat) == length(bat), "Feature and batch files have different numbers of rows.")
    )
    df <- data.frame(x = cov[[cvar]], y = dat[[feat]], Batch = bat, stringsAsFactors = FALSE)
    df$x <- suppressWarnings(as.numeric(df$x))
    df$y <- suppressWarnings(as.numeric(df$y))
    df$Batch <- clean_batch_for_plot(df$Batch)
    df <- df[is.finite(df$x) & is.finite(df$y) & !is.na(df$Batch), , drop = FALSE]
    df$Batch <- droplevels(df$Batch)
    validate(
      need(nrow(df) >= 3, "Not enough complete observations to draw the trend plot."),
      need(length(unique(df$x)) >= 2, "The selected covariate has too little variation for a trend plot."),
      need(length(unique(df$y)) >= 2, "The selected feature has too little variation for a trend plot.")
    )
    
    batch_cols <- safe_discrete_palette(levels(df$Batch))
    
    p <- ggplot(df, aes(x = x, y = y))
    if (isTRUE(input$trend_batch_color)) {
      p <- p +
        geom_point(aes(color = Batch), alpha = 0.55, size = 1.8) +
        scale_color_manual(values = batch_cols, name = "Batch")
    } else {
      p <- p + geom_point(alpha = 0.45, size = 1.8, color = "#2e4c8a")
    }
    
    # The smooth layers intentionally use group = 1. This shows the overall
    # covariate-feature trend and prevents batch coloring from being inherited
    # as separate smoothing groups in some ggplot/Shiny environments.
    if (input$trend_smooth == "lm") {
      p <- p + geom_smooth(aes(group = 1), method = "lm", formula = y ~ x,
                           color = "#c0392b", linewidth = 1.1,
                           se = TRUE, fill = "#f5b7b1", alpha = 0.25)
    } else if (input$trend_smooth == "loess") {
      p <- p + geom_smooth(aes(group = 1), method = "loess", formula = y ~ x,
                           color = "#1a5276", linewidth = 1.1,
                           se = TRUE, fill = "#aed6f1", alpha = 0.25)
    } else {
      p <- p +
        geom_smooth(aes(group = 1, linetype = "lm"), method = "lm", formula = y ~ x,
                    color = "#c0392b", linewidth = 1.0, se = TRUE,
                    fill = "#f5b7b1", alpha = 0.18) +
        geom_smooth(aes(group = 1, linetype = "loess"), method = "loess", formula = y ~ x,
                    color = "#1a5276", linewidth = 1.0, se = TRUE,
                    fill = "#aed6f1", alpha = 0.18) +
        scale_linetype_manual(name = "Smooth",
                              values = c("lm" = "solid", "loess" = "dashed"),
                              labels = c("lm (linear)", "GAM / loess"))
    }
    p + labs(x = cvar, y = feat, title = paste0(feat, " ~ ", cvar, "  [M", i, "]")) + bold_theme(12)
  }, bg = "white")
  
  output$formula_ui <- renderUI({
    req(rv$covar_list)
    cov <- rv$covar_list[[1]]; if (is.null(cov)) return(NULL)
    exclude_cols <- c(rv$random_var, rv$visit_col)
    fixed_cols   <- setdiff(colnames(cov), exclude_cols)
    default_f    <- if (length(fixed_cols) > 0)
      paste0("y ~ ", paste(fixed_cols, collapse=" + ")) else "y ~ 1"
    textInput("formula_txt", "Fixed-effects formula:", value = default_f)
  })
  
  output$ref_batch_ui <- renderUI({
    req(rv$batch_levels)
    selectInput("ref_batch", "Reference batch:",
                choices = c("None" = "", rv$batch_levels), selected = "")
  })
  
  output$re_spec_ui <- renderUI({
    if (!is_long()) return(NULL)
    tags$div(
      style = "background:#e8f5e9;border:1px solid #66bb6a;border-radius:6px;padding:8px 10px;margin-bottom:8px;",
      tags$div(style = "font-weight:700;color:#1b5e20;margin-bottom:4px;font-size:13px;",
               icon("random"), " Random effect"),
      tags$p(style = "font-size:12px;color:#2e7d32;margin:0 0 4px;",
             paste0("Subject ID column: '", rv$random_var %||% "subid", "'")),
      tags$p(style = "font-size:12px;color:#2e7d32;margin:0;",
             "Formula: (1 | subid) added automatically for lmer")
    )
  })
  
  observeEvent(input$confirm_config, {
    req(rv$data_list)
    model_type <- input$model_type
    if (is_long() && model_type != "lmer") {
      showNotification("Longitudinal data detected: switching model to lmer.", type="warning", duration=4)
      updateSelectInput(session, "model_type", selected = "lmer")
      model_type <- "lmer"
    }
    rv$model_fn <- switch(model_type,
                          "lm"   = stats::lm,
                          "gam"  = mgcv::gam,
                          "lmer" = lme4::lmer,
                          stats::lm)
    base_f <- if (!is.null(input$formula_txt) && nchar(trimws(input$formula_txt)) > 0)
      tryCatch(as.formula(input$formula_txt), error = function(e) NULL)
    else NULL
    if (model_type == "lmer" && is_long() && !is.null(rv$random_var)) {
      re_term <- paste0("(1 | ", rv$random_var, ")")
      if (!is.null(base_f)) {
        f_str <- paste(deparse(base_f), collapse = "")
        if (!grepl("\\|", f_str)) base_f <- as.formula(paste(f_str, "+", re_term))
      } else {
        base_f <- as.formula(paste("y ~ 1 +", re_term))
      }
    }
    rv$formula_obj <- base_f
    rv$random_var  <- if (is_long()) rv$random_var else NULL
    rv$ref_batch   <- if (!is.null(input$ref_batch) && input$ref_batch != "")
      input$ref_batch else NULL
    rv$config_ok <- TRUE
    showNotification("Configuration confirmed.", type = "message", duration = 2)
  })
  
  output$config_status <- renderUI({
    if (!rv$config_ok) {
      return(tags$div(
        style = "background:#fff3cd;color:#7a5a00;border-left:4px solid #f0ad4e;padding:12px 14px;border-radius:8px;font-weight:600;",
        icon("triangle-exclamation"), " Press 'Confirm Configuration' after loading data."))
    }
    model_lbl   <- if (!is.null(rv$random_var)) "Linear mixed-effects model (lmer)"
    else if (identical(input$model_type,"gam")) "Generalized additive model (gam)"
    else "Linear model (lm)"
    formula_lbl <- if (is.null(rv$formula_obj)) "y ~ 1"
    else paste(deparse(rv$formula_obj), collapse="")
    ref_lbl <- if (is.null(rv$ref_batch)) "None" else rv$ref_batch
    re_lbl  <- if (is.null(rv$random_var)) "None" else rv$random_var
    tags$div(
      style = "background:#ffffff;border:1px solid #d9e2f2;border-radius:10px;padding:14px 16px;box-shadow:0 2px 6px rgba(0,0,0,.06);",
      tags$div(style = "font-weight:700;color:#1a2b4a;margin-bottom:10px;font-size:15px;",
               icon("circle-check", style="color:#27ae60;margin-right:6px;"), "Configuration confirmed"),
      tags$div(
        style = "display:grid;grid-template-columns:160px 1fr;row-gap:8px;column-gap:10px;",
        tags$div(style="font-weight:600;color:#4a5568;","Model"),
        tags$div(style="color:#1a2b4a;", model_lbl),
        tags$div(style="font-weight:600;color:#4a5568;","Formula"),
        tags$div(style="color:#1a2b4a;font-family:monospace;background:#f7f9fc;padding:4px 8px;border-radius:6px;display:inline-block;", formula_lbl),
        tags$div(style="font-weight:600;color:#4a5568;","Reference batch"),
        tags$div(style="color:#1a2b4a;", ref_lbl),
        tags$div(style="font-weight:600;color:#4a5568;","Random effect"),
        tags$div(style="color:#1a2b4a;", re_lbl),
        tags$div(style="font-weight:600;color:#4a5568;","Modalities"),
        tags$div(style="color:#1a2b4a;", paste0("m = ", rv$m))
      )
    )
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # SHARED DIAGNOSTIC HELPERS  (unchanged)
  # ══════════════════════════════════════════════════════════════════════════
  diag_test_param <- reactive({
    if (!is.null(rv$random_var)) list(random = rv$random_var) else list()
  })
  
  run_diag <- function(data_list, bat_list, covar_list) {
    tp <- diag_test_param()
    lapply(seq_along(data_list), function(i)
      do.call(diag_model_summary,
              c(list(diag_model = diag_model_gen(
                bat = bat_list[[i]], data = data_list[[i]],
                covar = covar_list[[i]], model = rv$model_fn,
                formula = rv$formula_obj, ref.batch = rv$ref_batch,
                bat_adjust = input$bat_adjust)), tp))
    )
  }
  
  run_uni <- function(diag_list, bat_list) {
    list(
      anova   = lapply(seq_along(diag_list), function(i) anova_test(diag_list[[i]]$resid_add, bat_list[[i]])),
      kruskal = lapply(seq_along(diag_list), function(i) kruskal_test(diag_list[[i]]$resid_add, bat_list[[i]])),
      lv      = lapply(seq_along(diag_list), function(i) lv_test(diag_list[[i]]$resid_mul, bat_list[[i]])),
      bl      = lapply(seq_along(diag_list), function(i) bl_test(diag_list[[i]]$resid_mul, bat_list[[i]])),
      fk      = lapply(seq_along(diag_list), function(i) fk_test(diag_list[[i]]$resid_mul, bat_list[[i]]))
    )
  }
  
  tests_to_df <- function(tests) {
    m <- length(tests$anova)
    data.frame(
      anova_result   = sapply(seq_len(m), function(i) tests$anova[[i]]$perc.sig),
      kruskal_result = sapply(seq_len(m), function(i) tests$kruskal[[i]]$perc.sig),
      lv_result      = sapply(seq_len(m), function(i) tests$lv[[i]]$perc.sig),
      bl_result      = sapply(seq_len(m), function(i) tests$bl[[i]]$perc.sig),
      fk_result      = sapply(seq_len(m), function(i) tests$fk[[i]]$perc.sig)
    )
  }
  
  get_test_tbl <- function(tests, name, i) {
    switch(name,
           anova = tests$anova[[i]]$test_table, kruskal = tests$kruskal[[i]]$test_table,
           lv    = tests$lv[[i]]$test_table,    bl      = tests$bl[[i]]$test_table,
           fk    = tests$fk[[i]]$test_table)
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # 2. PRE-HARMONIZATION  (unchanged from original)
  # ══════════════════════════════════════════════════════════════════════════
  observeEvent(input$run_pre_diag, {
    req(rv$data_list, rv$bat_list, rv$config_ok)
    withProgress(message = "Pre-harmonization diagnostics…", {
      tryCatch({
        setProgress(0.15, "Fitting models…")
        rv$pre_diag  <- run_diag(rv$data_list, rv$bat_list, rv$covar_list)
        setProgress(0.45, "PCA…")
        rv$pre_pca   <- pca_prep(bat = rv$bat_list, data = rv$data_list,
                                 covar = rv$covar_list, model = rv$model_fn,
                                 formula = rv$formula_obj, ref.batch = rv$ref_batch,
                                 bat_adjust = input$bat_adjust, test_param = diag_test_param())
        setProgress(0.70, "Univariate tests…")
        rv$pre_tests <- run_uni(rv$pre_diag, rv$bat_list)
        if (isTRUE(input$pre_run_rf_auc)) {
          setProgress(0.88, "Random forest batch AUC...")
          rv$pre_rf_auc <- tryCatch(
            HarmonizeR::batch_auc_cal_cv(
              rv$data_list,
              rv$bat_list,
              k = input$pre_rf_k %||% 5,
              ntree = input$pre_rf_ntree %||% 100
            ),
            error = function(e) {
              showNotification(paste("RF batch AUC skipped:", e$message), type = "warning", duration = 8)
              NULL
            }
          )
        } else {
          rv$pre_rf_auc <- NULL
        }
        setProgress(1)
        showNotification("Pre-diagnostics done.", type = "message")
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
    })
  })
  
  # ---- PCA view controls -------------------------------------------------
  # For a single-metric dataset, shared-space PCA is not meaningful.
  # Hide/disable the shared-space option and use within-modality PCA internally.
  pca_type_use <- function(x) {
    if (!is.null(rv$m) && rv$m <= 1) "within" else (x %||% "within")
  }
  
  output$pre_pca_type_ui <- renderUI({
    req(rv$m)
    if (rv$m <= 1) {
      tagList(
        selectInput("pre_pca_type", "PCA view:",
                    choices = c("Within-modality" = "within"),
                    selected = "within"),
        tags$script(HTML("$('#pre_pca_type').prop('disabled', true);")),
        tags$div(
          style = "font-size:12px;color:#777;margin-top:-4px;",
          icon("circle-info"),
          " Shared-space PCA is disabled for one metric."
        )
      )
    } else {
      selectInput("pre_pca_type", "PCA view:",
                  choices = c("Within-modality" = "within",
                              "Shared space" = "shared"),
                  selected = input$pre_pca_type %||% "within")
    }
  })
  
  output$post_pca_type_ui <- renderUI({
    req(rv$m)
    if (rv$m <= 1) {
      tagList(
        selectInput("post_pca_type", "PCA view:",
                    choices = c("Within-modality" = "within"),
                    selected = "within"),
        tags$script(HTML("$('#post_pca_type').prop('disabled', true);")),
        tags$div(
          style = "font-size:12px;color:#777;margin-top:-4px;",
          icon("circle-info"),
          " Shared-space PCA is disabled for one metric."
        )
      )
    } else {
      selectInput("post_pca_type", "PCA view:",
                  choices = c("Within-modality" = "within",
                              "Shared space" = "shared"),
                  selected = input$post_pca_type %||% "within")
    }
  })
  
  output$pre_pca <- renderPlot({
    req(rv$pre_pca)
    pca_plot_robust(rv$pre_pca, type = pca_type_use(input$pre_pca_type),
                    pc_1 = input$pre_pc1, pc_2 = input$pre_pc2,
                    ellipse = input$pre_ellipse) +
      labs(title = "PCA – Pre-Harmonization") + bold_theme(12) +
      theme(plot.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(8, 8, 8, 8))
  }, height = function() {
    req(rv$pre_pca)
    pca_plot_height_px(rv$pre_pca, type = pca_type_use(input$pre_pca_type), compare_mode = FALSE)
  }, bg = "white")
  
  output$pre_feat_ui   <- renderUI({ req(rv$feat_names); selectInput("pre_feat", "Feature:", choices = rv$feat_names, selected = rv$feat_names[1]) })
  output$pre_mod_ui    <- renderUI({ m <- safe_m_value(rv$m); req(!is.null(m)); selectInput("pre_mod", "Modality:", choices = setNames(seq_len(m), paste0("M",seq_len(m))), selected = 1) })
  output$pre_modsel_ui <- renderUI({ m <- safe_m_value(rv$m); req(!is.null(m)); selectInput("pre_modsel", "Modality:", choices = setNames(seq_len(m), paste0("M",seq_len(m))), selected = 1) })
  
  detect_outliers <- function(resid_mat, bat, feat) {
    # Detect regular and extreme Tukey outliers within each batch.
    #
    # Important plotting note:
    #   The outlier rule here is batch-specific and value-based:
    #     regular: value < Q1 - 1.5*IQR or value > Q3 + 1.5*IQR
    #     extreme: value < Q1 - 3.0*IQR or value > Q3 + 3.0*IQR
    #   It is NOT based on |residual|.
    if (!feat %in% colnames(resid_mat)) return(NULL)
    
    sids <- rownames(resid_mat) %||% paste0("S", seq_len(nrow(resid_mat)))
    batch_levels <- unique(as.character(bat))
    
    df <- data.frame(
      SampleID = sids,
      Batch    = factor(as.character(bat), levels = batch_levels),
      Value    = as.numeric(resid_mat[, feat]),
      OutType  = NA_character_,
      Q1       = NA_real_,
      Q3       = NA_real_,
      IQR      = NA_real_,
      Lower15  = NA_real_,
      Upper15  = NA_real_,
      Lower30  = NA_real_,
      Upper30  = NA_real_,
      stringsAsFactors = FALSE
    )
    
    for (b in levels(df$Batch)) {
      idx <- which(df$Batch == b & is.finite(df$Value))
      if (length(idx) < 4L) next
      
      v  <- df$Value[idx]
      q  <- stats::quantile(v, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
      iq <- q[2] - q[1]
      
      lower15 <- q[1] - 1.5 * iq
      upper15 <- q[2] + 1.5 * iq
      lower30 <- q[1] - 3.0 * iq
      upper30 <- q[2] + 3.0 * iq
      
      is_ext <- v < lower30 | v > upper30
      is_reg <- (v < lower15 | v > upper15) & !is_ext
      
      df$Q1[idx]      <- q[1]
      df$Q3[idx]      <- q[2]
      df$IQR[idx]     <- iq
      df$Lower15[idx] <- lower15
      df$Upper15[idx] <- upper15
      df$Lower30[idx] <- lower30
      df$Upper30[idx] <- upper30
      
      df$OutType[idx[is_ext]] <- "extreme"
      df$OutType[idx[is_reg]] <- "regular"
    }
    
    df
  }
  
  brewer_hex <- function(palette, n) {
    # Kept for backward compatibility with internal plotting helpers, but make it
    # safe for n > Brewer palette limits and for environments that handle Brewer
    # warnings/errors differently.
    pal <- switch(palette,
                  "Pastel1" = "Pastel 1",
                  "Pastel2" = "Pastel 2",
                  "Set2" = "Dark 3",
                  "Dark 3")
    safe_discrete_palette(as.integer(n), palette = pal)
  }
  
  make_resid_boxplot_plotly <- function(
    resid_mat, bat, feat, title_str,
    palette = "Set2",
    jitter = FALSE,
    show_outliers = c("regular", "extreme")
  ) {
    if (!feat %in% colnames(resid_mat)) return(NULL)
    
    # character(0) = user unchecked everything = show no outliers
    show_outliers <- intersect(as.character(show_outliers), c("regular", "extreme"))
    show_regular  <- "regular" %in% show_outliers
    show_extreme  <- "extreme" %in% show_outliers
    
    df <- detect_outliers(resid_mat, bat, feat)
    if (is.null(df)) return(NULL)
    
    df <- df[!is.na(df$Value), , drop = FALSE]
    df$Batch <- factor(df$Batch, levels = unique(as.character(bat)))
    
    batches <- levels(df$Batch)
    x_map   <- setNames(seq_along(batches), batches)
    df$xpos <- unname(x_map[as.character(df$Batch)])
    
    cols <- brewer_hex(palette, length(batches))
    names(cols) <- batches
    
    box_stats <- lapply(batches, function(b) {
      sub <- df[df$Batch == b, , drop = FALSE]
      v   <- sub$Value
      
      q1  <- stats::quantile(v, 0.25, na.rm = TRUE, names = FALSE)
      med <- stats::median(v, na.rm = TRUE)
      q3  <- stats::quantile(v, 0.75, na.rm = TRUE, names = FALSE)
      iq  <- q3 - q1
      
      lower15 <- q1 - 1.5 * iq
      upper15 <- q3 + 1.5 * iq
      
      lower_whisker <- suppressWarnings(min(v[v >= lower15], na.rm = TRUE))
      upper_whisker <- suppressWarnings(max(v[v <= upper15], na.rm = TRUE))
      
      if (!is.finite(lower_whisker)) lower_whisker <- min(v, na.rm = TRUE)
      if (!is.finite(upper_whisker)) upper_whisker <- max(v, na.rm = TRUE)
      
      data.frame(
        Batch         = b,
        xpos          = x_map[[b]],
        q1            = q1,
        med           = med,
        q3            = q3,
        iqr           = iq,
        lower15       = lower15,
        upper15       = upper15,
        lower_whisker = lower_whisker,
        upper_whisker = upper_whisker,
        stringsAsFactors = FALSE
      )
    }) %>% dplyr::bind_rows()
    
    p <- plotly::plot_ly()
    
    box_width <- 0.55
    cap_width <- 0.28
    
    for (i in seq_len(nrow(box_stats))) {
      st <- box_stats[i, ]
      b  <- st$Batch
      x  <- st$xpos
      
      hover_box <- paste0(
        "Batch: ", b,
        "<br>Median: ", round(st$med, 4),
        "<br>Q1: ", round(st$q1, 4),
        "<br>Q3: ", round(st$q3, 4),
        "<br>IQR: ", round(st$iqr, 4),
        "<br>Lower whisker: ", round(st$lower_whisker, 4),
        "<br>Upper whisker: ", round(st$upper_whisker, 4),
        "<br>1.5×IQR fence: [", round(st$lower15, 4), ", ", round(st$upper15, 4), "]"
      )
      
      # Box rectangle
      p <- p %>%
        plotly::add_trace(
          type      = "scatter", mode = "lines",
          x         = c(x - box_width/2, x + box_width/2,
                        x + box_width/2, x - box_width/2,
                        x - box_width/2),
          y         = c(st$q1, st$q1, st$q3, st$q3, st$q1),
          fill      = "toself",
          fillcolor = cols[[b]],
          line      = list(color = "#333333", width = 1),
          opacity   = 0.65,
          name      = b,
          legendgroup = b,
          text      = hover_box,
          hoverinfo = "text",
          showlegend = TRUE
        )
      
      # Median line
      p <- p %>%
        plotly::add_segments(
          x = x - box_width/2, xend = x + box_width/2,
          y = st$med, yend = st$med,
          line = list(color = "#333333", width = 2),
          hoverinfo = "skip", showlegend = FALSE
        )
      
      # Lower whisker
      p <- p %>%
        plotly::add_segments(
          x = x, xend = x,
          y = st$lower_whisker, yend = st$q1,
          line = list(color = "#333333", width = 1),
          hoverinfo = "skip", showlegend = FALSE
        )
      
      # Upper whisker
      p <- p %>%
        plotly::add_segments(
          x = x, xend = x,
          y = st$q3, yend = st$upper_whisker,
          line = list(color = "#333333", width = 1),
          hoverinfo = "skip", showlegend = FALSE
        )
      
      # Lower cap
      p <- p %>%
        plotly::add_segments(
          x = x - cap_width/2, xend = x + cap_width/2,
          y = st$lower_whisker, yend = st$lower_whisker,
          line = list(color = "#333333", width = 1),
          hoverinfo = "skip", showlegend = FALSE
        )
      
      # Upper cap
      p <- p %>%
        plotly::add_segments(
          x = x - cap_width/2, xend = x + cap_width/2,
          y = st$upper_whisker, yend = st$upper_whisker,
          line = list(color = "#333333", width = 1),
          hoverinfo = "skip", showlegend = FALSE
        )
    }
    
    # Optional jittered raw points — exclude unchecked outlier types
    if (isTRUE(jitter)) {
      set.seed(1)
      df_jit <- df[
        is.na(df$OutType) |
          (df$OutType == "regular" & show_regular) |
          (df$OutType == "extreme" & show_extreme),
        , drop = FALSE
      ]
      if (nrow(df_jit) > 0) {
        df_jit$xjit <- df_jit$xpos + stats::runif(nrow(df_jit), -0.08, 0.08)
        p <- p %>%
          plotly::add_markers(
            data      = df_jit,
            x         = ~xjit,
            y         = ~Value,
            name      = "Raw residuals",
            marker    = list(size = 5, color = "rgba(80,80,80,0.35)"),
            text      = ~paste0("Sample: ", SampleID,
                                "<br>Batch: ", Batch,
                                "<br>Value: ", round(Value, 4)),
            hoverinfo = "text",
            showlegend = FALSE,
            inherit   = FALSE
          )
      }
    }
    
    # Highlighted outliers — only the checked types
    out_df <- df[
      !is.na(df$OutType) & (
        (df$OutType == "regular" & show_regular) |
          (df$OutType == "extreme" & show_extreme)
      ),
      , drop = FALSE
    ]
    
    if (nrow(out_df) > 0) {
      out_df$OutLabel <- dplyr::case_when(
        out_df$OutType == "extreme" ~ "Extreme outlier (3×IQR)",
        out_df$OutType == "regular" ~ "Regular outlier (1.5×IQR)",
        TRUE ~ NA_character_
      )
      out_df$xout <- out_df$xpos
      
      hover_out <- paste0(
        "Sample: ", out_df$SampleID,
        "<br>Batch: ", out_df$Batch,
        "<br>Value: ", round(out_df$Value, 4),
        "<br>Type: ", out_df$OutLabel,
        "<br>Q1: ", round(out_df$Q1, 4),
        "<br>Q3: ", round(out_df$Q3, 4),
        "<br>IQR: ", round(out_df$IQR, 4),
        "<br>1.5×IQR fence: [", round(out_df$Lower15, 4), ", ", round(out_df$Upper15, 4), "]",
        "<br>3×IQR fence: [",  round(out_df$Lower30, 4), ", ", round(out_df$Upper30, 4), "]"
      )
      
      reg_df <- out_df[out_df$OutType == "regular", , drop = FALSE]
      ext_df <- out_df[out_df$OutType == "extreme", , drop = FALSE]
      
      if (nrow(reg_df) > 0) {
        p <- p %>%
          plotly::add_markers(
            data   = reg_df,
            x      = ~xout, y = ~Value,
            name   = "Regular outlier",
            marker = list(symbol = "circle-open", size = 10,
                          color  = "#e67e22",
                          line   = list(width = 2, color = "#e67e22")),
            text      = hover_out[out_df$OutType == "regular"],
            hoverinfo = "text",
            showlegend = TRUE,
            inherit   = FALSE
          )
      }
      
      if (nrow(ext_df) > 0) {
        p <- p %>%
          plotly::add_markers(
            data   = ext_df,
            x      = ~xout, y = ~Value,
            name   = "Extreme outlier",
            marker = list(symbol = "diamond-open", size = 12,
                          color  = "#c0392b",
                          line   = list(width = 2.5, color = "#c0392b")),
            text      = hover_out[out_df$OutType == "extreme"],
            hoverinfo = "text",
            showlegend = TRUE,
            inherit   = FALSE
          )
      }
    }
    
    p %>%
      plotly::layout(
        title = list(text = title_str),
        xaxis = list(
          title    = "Batch",
          tickmode = "array",
          tickvals = seq_along(batches),
          ticktext = batches,
          range    = c(0.5, length(batches) + 0.5)
        ),
        yaxis = list(title = "Residual value", zeroline = TRUE),
        hovermode     = "closest",
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        legend        = list(orientation = "h", x = 0, y = -0.18),
        margin        = list(t = 60, b = 90)
      )
  }
  
  make_outlier_tbl_combined <- function(resid_add, resid_mul, bat, feat, show_types) {
    build_side <- function(mat, label) {
      df <- detect_outliers(mat, bat, feat); if (is.null(df)) return(NULL)
      out <- df[!is.na(df$OutType) & df$OutType %in% show_types, , drop=FALSE]
      if (nrow(out) == 0) return(NULL)
      out$ResidType <- label; out
    }
    combined <- rbind(build_side(resid_add,"Additive"), build_side(resid_mul,"Multiplicative"))
    if (is.null(combined) || nrow(combined) == 0) return(NULL)
    combined$Value   <- round(combined$Value, 4)
    combined$OutType <- ifelse(combined$OutType == "extreme",
                               "⚠ Extreme (3×IQR)", "● Regular (1.5×IQR)")
    combined[order(combined$OutType, combined$ResidType, combined$Batch),
             c("SampleID","Batch","ResidType","Value","OutType")]
  }
  
  output$pre_box_add <- plotly::renderPlotly({
    req(rv$pre_diag, input$pre_feat, input$pre_mod)
    i        <- as.integer(input$pre_mod)
    show_out <- if (is.null(input$pre_outlier_show) || length(input$pre_outlier_show) == 0)
      character(0)
    else
      input$pre_outlier_show
    make_resid_boxplot_plotly(
      rv$pre_diag[[i]]$resid_add, rv$bat_list[[i]], input$pre_feat,
      paste0("M", i, " – ", input$pre_feat, " (additive residuals)"),
      palette = "Set2", jitter = isTRUE(input$pre_box_jitter),
      show_outliers = show_out
    )
  })
  output$pre_box_mul <- plotly::renderPlotly({
    req(rv$pre_diag, input$pre_feat, input$pre_mod)
    i        <- as.integer(input$pre_mod)
    show_out <- if (is.null(input$pre_outlier_show) || length(input$pre_outlier_show) == 0)
      character(0)
    else
      input$pre_outlier_show
    make_resid_boxplot_plotly(
      rv$pre_diag[[i]]$resid_mul, rv$bat_list[[i]], input$pre_feat,
      paste0("M", i, " – ", input$pre_feat, " (multiplicative residuals)"),
      palette = "Pastel2", jitter = isTRUE(input$pre_box_jitter),
      show_outliers = show_out
    )
  })
  
  output$pre_outlier_tbl_combined <- renderDT({
    req(rv$pre_diag, input$pre_feat, input$pre_mod)
    i        <- as.integer(input$pre_mod)
    show_out <- if (is.null(input$pre_outlier_show) || length(input$pre_outlier_show) == 0)
      character(0)
    else
      input$pre_outlier_show
    tbl <- make_outlier_tbl_combined(rv$pre_diag[[i]]$resid_add, rv$pre_diag[[i]]$resid_mul,
                                     rv$bat_list[[i]], input$pre_feat, show_out)
    if (is.null(tbl))
      return(datatable(data.frame(Note = "No outliers detected."), rownames = FALSE,
                       colnames = "", options = list(dom = "t"), class = "compact"))
    datatable(tbl, rownames = FALSE, escape = FALSE, class = "compact stripe hover",
              options = list(dom = "tip", pageLength = 10, scrollX = TRUE,
                             columnDefs = list(list(className = "dt-center", targets = c(1,2,3,4))))) %>%
      formatStyle("OutType", color = styleEqual(c("⚠ Extreme (3×IQR)", "● Regular (1.5×IQR)"),
                                                c("#c0392b", "#e67e22")), fontWeight = "bold") %>%
      formatStyle("ResidType", color = styleEqual(c("Additive", "Multiplicative"),
                                                  c("#1a5276", "#6c3483")), fontWeight = "600")
  })
  
  output$dl_outlier_excel <- downloadHandler(
    filename = function() paste0("outlier_summary_", input$pre_feat, "_M",
                                 input$pre_mod %||% "1", "_",
                                 format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content  = function(file) {
      req(rv$pre_diag, input$pre_feat, input$pre_mod)
      i        <- as.integer(input$pre_mod %||% 1)
      show_out <- input$pre_outlier_show %||% c("regular","extreme")
      bat      <- rv$bat_list[[i]]; feat <- input$pre_feat; all_feats <- rv$feat_names
      build_all <- function(mat, label) {
        do.call(rbind, lapply(all_feats, function(f) {
          df_out <- detect_outliers(mat, bat, f); if (is.null(df_out)) return(NULL)
          out <- df_out[!is.na(df_out$OutType) & df_out$OutType %in% show_out, , drop=FALSE]
          if (nrow(out) == 0) return(NULL)
          out$Feature <- f; out$ResidType <- label; out
        }))
      }
      combined <- rbind(build_all(as.matrix(rv$pre_diag[[i]]$resid_add),"Additive"),
                        build_all(as.matrix(rv$pre_diag[[i]]$resid_mul),"Multiplicative"))
      wb <- openxlsx::createWorkbook()
      if (is.null(combined) || nrow(combined) == 0) {
        openxlsx::addWorksheet(wb, "All Features")
        openxlsx::writeData(wb, "All Features", data.frame(Note="No outliers detected."))
        openxlsx::saveWorkbook(wb, file, overwrite=TRUE); return(invisible(NULL))
      }
      combined$Value   <- round(combined$Value, 4)
      combined$OutType <- ifelse(combined$OutType == "extreme","Extreme (3xIQR)","Regular (1.5xIQR)")
      combined <- combined[order(combined$Feature, combined$ResidType, combined$OutType, combined$Batch),
                           c("Feature","SampleID","Batch","ResidType","Value","OutType")]
      cur_feat <- combined[combined$Feature == feat, , drop=FALSE]
      openxlsx::addWorksheet(wb, paste0("Feature_", feat))
      openxlsx::writeData(wb, 1, cur_feat)
      ext_rows <- which(cur_feat$OutType == "Extreme (3xIQR)") + 1L
      reg_rows <- which(cur_feat$OutType == "Regular (1.5xIQR)") + 1L
      if (length(ext_rows) > 0)
        openxlsx::addStyle(wb, 1, openxlsx::createStyle(fontColour="#c0392b",textDecoration="bold"),
                           rows=ext_rows, cols=seq_len(ncol(cur_feat)), gridExpand=TRUE)
      if (length(reg_rows) > 0)
        openxlsx::addStyle(wb, 1, openxlsx::createStyle(fontColour="#e67e22",textDecoration="bold"),
                           rows=reg_rows, cols=seq_len(ncol(cur_feat)), gridExpand=TRUE)
      openxlsx::addWorksheet(wb, "All_Features")
      openxlsx::writeData(wb, 2, combined)
      ext_rows2 <- which(combined$OutType == "Extreme (3xIQR)") + 1L
      reg_rows2 <- which(combined$OutType == "Regular (1.5xIQR)") + 1L
      if (length(ext_rows2) > 0)
        openxlsx::addStyle(wb, 2, openxlsx::createStyle(fontColour="#c0392b",textDecoration="bold"),
                           rows=ext_rows2, cols=seq_len(ncol(combined)), gridExpand=TRUE)
      if (length(reg_rows2) > 0)
        openxlsx::addStyle(wb, 2, openxlsx::createStyle(fontColour="#e67e22",textDecoration="bold"),
                           rows=reg_rows2, cols=seq_len(ncol(combined)), gridExpand=TRUE)
      openxlsx::saveWorkbook(wb, file, overwrite=TRUE)
    }
  )
  
  render_uni_heat <- function(df, ms, title_str) {
    validate(
      need(!is.null(df) && nrow(df) > 0, "No univariate test results available."),
      need(length(ms) == nrow(df), "Number of modality labels does not match test results.")
    )
    
    test_cols <- c("anova_result", "kruskal_result", "lv_result", "bl_result", "fk_result")
    test_labs <- c("ANOVA", "Kruskal", "Levene", "Bartlett", "Fligner")
    
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    
    # Be defensive: tests_to_df() should return the internal names above, but after
    # some table-rendering steps the same results may have display names such as
    # ANOVA/Kruskal/Levene/Bartlett/Fligner.  If the expected names are not present,
    # use the first five columns and reset them to the internal names.  This prevents
    # a one-metric heatmap from producing a single x-axis level called NA.
    if (!all(test_cols %in% names(df))) {
      validate(need(ncol(df) >= length(test_cols), "Univariate test table has fewer than five test columns."))
      df <- df[, seq_along(test_cols), drop = FALSE]
      names(df) <- test_cols
    } else {
      df <- df[, test_cols, drop = FALSE]
    }
    
    parse_pct <- function(x) {
      x <- as.character(x)
      x <- trimws(gsub("%", "", x, fixed = TRUE))
      suppressWarnings(as.numeric(x))
    }
    
    # Important: do not use apply() here. With one modality, apply() can
    # simplify the one-row data frame into a vector and break the heatmap shape.
    num_df <- as.data.frame(lapply(df, parse_pct), check.names = FALSE)
    names(num_df) <- test_cols
    
    long_df <- num_df %>%
      mutate(Modality = factor(ms, levels = rev(ms))) %>%
      pivot_longer(
        cols = all_of(test_cols),
        names_to = "Test",
        values_to = "pct"
      ) %>%
      mutate(
        Test = factor(Test, levels = test_cols, labels = test_labs),
        label = ifelse(is.na(pct), "NA", paste0(round(pct, 1), "%"))
      )
    
    one_metric <- length(ms) == 1L
    
    p <- ggplot(long_df, aes(x = Test, y = Modality, fill = pct)) +
      geom_tile(color = "white", linewidth = 0.6, width = 0.96, height = 0.78) +
      geom_text(aes(label = label, color = !is.na(pct) & pct > 50),
                size = if (one_metric) 4.3 else 3.8,
                fontface = "bold") +
      scale_x_discrete(drop = FALSE, expand = expansion(add = if (one_metric) 0.15 else 0.05)) +
      scale_y_discrete(drop = FALSE, expand = expansion(add = if (one_metric) 0.25 else 0.05)) +
      scale_fill_gradientn(colours = c("#eaf6fb", "#7bccc4", "#2171b5", "#08306b"),
                           limits = c(0, 100), name = "% Sig.", na.value = "grey90") +
      scale_color_manual(values = c("FALSE" = "#1a1a1a", "TRUE" = "white"), guide = "none") +
      labs(x = "Test", y = "Modality", title = title_str) +
      bold_theme(11) +
      theme(panel.grid = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            axis.text.x = element_text(angle = if (one_metric) 0 else 30,
                                       hjust = if (one_metric) 0.5 else 1,
                                       size = if (one_metric) 10 else 9),
            axis.text.y = element_text(size = if (one_metric) 11 else 9),
            legend.margin = margin(l = 8),
            plot.margin = margin(t = 16, r = 20, b = 16, l = 20))
    
    # For multiple metrics, fixed coordinates make square heatmap cells easier to read.
    # For one metric, fixed coordinates make a single row look visually disconnected
    # from the summary table, so use a free rectangular layout.
    if (!one_metric) {
      p <- p + coord_fixed(ratio = 0.9)
    }
    
    p
  }
  
  output$pre_uni_results_ui <- renderUI({
    one_metric <- !is.null(rv$m) && rv$m == 1
    plot_h  <- if (one_metric) "310px" else "650px"
    table_h <- if (one_metric) "310px" else "650px"
    
    fluidRow(
      style = "margin-top:28px;",
      column(
        width = 7,
        div(
          style = "padding-top:12px;",
          plotOutput("pre_uni_heat", height = plot_h)
        )
      ),
      column(
        width = 5,
        div(
          style = paste0(
            "height:", table_h, ";",
            "display:flex;",
            "align-items:center;",
            "justify-content:center;",
            "padding-left:45px;",
            "padding-right:20px;"
          ),
          DTOutput("pre_uni_tbl")
        )
      )
    )
  })
  
  output$pre_uni_heat <- renderPlot({
    req(rv$pre_tests)
    m <- safe_m_value(rv$m); req(!is.null(m))
    print(render_uni_heat(tests_to_df(rv$pre_tests), paste0("M",seq_len(m)),
                          "Pre-harmonization: % significant features \n"))
  }, bg = "white")
  
  output$pre_uni_tbl <- renderDT({
    req(rv$pre_tests)
    m <- safe_m_value(rv$m); req(!is.null(m))
    df <- tests_to_df(rv$pre_tests)
    df <- cbind(Modality = paste0("M", seq_len(m)), df)
    names(df) <- c("Modality","ANOVA","Kruskal","Levene","Bartlett","Fligner")
    datatable(df, rownames=FALSE, options=list(dom="t"), class="compact stripe")
  })

  summarize_rf_auc <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df %>%
      dplyr::group_by(measurement) %>%
      dplyr::summarise(
        mean_auc = mean(auc, na.rm = TRUE),
        sd_auc = stats::sd(auc, na.rm = TRUE),
        median_auc = stats::median(auc, na.rm = TRUE),
        min_auc = min(auc, na.rm = TRUE),
        max_auc = max(auc, na.rm = TRUE),
        valid_folds = sum(is.finite(auc)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        dplyr::across(where(is.numeric), ~ round(.x, 3))
      )
  }
  
  rf_auc_placeholder <- function(stage = "pre") {
    msg <- if (stage == "pre") {
      "Random forest batch-prediction AUC was not run. Check 'Run RF batch prediction AUC' and rerun pre-harmonization diagnostics."
    } else {
      "Random forest batch-prediction AUC was not run. Check 'Run RF batch prediction AUC' and rerun post-harmonization diagnostics."
    }
    ggplot() +
      annotate("text", x = 0, y = 0, label = msg, size = 4.5, color = "#666666") +
      xlim(-1, 1) + ylim(-1, 1) +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", color = NA))
  }
  
  output$pre_rf_auc_plot <- renderPlot({
    if (is.null(rv$pre_rf_auc)) {
      print(rf_auc_placeholder("pre"))
      return(invisible(NULL))
    }
    df <- rv$pre_rf_auc
    validate(need(nrow(df) > 0, "No RF batch-prediction AUC results available."))
    metric_levels <- unique(as.character(df$measurement))
    metric_palette <- setNames(
      grDevices::hcl.colors(length(metric_levels), palette = "TealGrn"),
      metric_levels
    )
    p <- ggplot(df, aes(x = measurement, y = auc, fill = measurement, color = measurement)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.45, ymax = 0.60,
               fill = "#f7f7f7", alpha = 0.65) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "#6b7280", linewidth = 0.55) +
      geom_boxplot(width = 0.48, outlier.shape = NA, alpha = 0.72,
                   linewidth = 0.65, color = "#34495e") +
      geom_jitter(width = 0.10, height = 0, alpha = 0.82, size = 2.1,
                  shape = 21, stroke = 0.25, color = "#263238") +
      scale_fill_manual(values = metric_palette, guide = "none") +
      scale_color_manual(values = metric_palette, guide = "none") +
      coord_cartesian(ylim = c(0.45, 1.0)) +
      labs(
        x = "Metric",
        y = "Cross-validated macro AUC",
        title = "Pre-harmonization global batch prediction",
        subtitle = "Random forest batch-label prediction within each metric; dashed line indicates chance-level AUC = 0.5."
      ) +
      bold_theme(12) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#fbfcfe", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#e5e7eb", linewidth = 0.35)
      )
    print(p)
  }, bg = "white")
  
  output$pre_rf_auc_tbl <- renderDT({
    if (is.null(rv$pre_rf_auc)) {
      return(datatable(
        data.frame(Message = "RF batch-prediction AUC has not been run."),
        rownames = FALSE, options = list(dom = "t"), class = "compact stripe"
      ))
    }
    datatable(summarize_rf_auc(rv$pre_rf_auc), rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE),
              class = "compact stripe") %>%
      formatStyle("mean_auc", background = styleInterval(c(0.60, 0.80), c("#f0fdf4", "#fef3c7", "#fee2e2")),
                  fontWeight = "bold")
  })
  
  output$pre_pval_bar <- plotly::renderPlotly({
    req(rv$pre_tests, input$pre_test, input$pre_modsel)
    i   <- as.integer(input$pre_modsel)
    tbl <- get_test_tbl(rv$pre_tests, input$pre_test, i)
    validate(need(!is.null(tbl),"No test table."), need(nrow(tbl)>0,"Empty table."))
    feature_col <- intersect(c("feature","Feature","roi","ROI"), names(tbl))[1]
    validate(need(length(feature_col)==1,"Cannot find feature column."))
    tbl$p_value <- as.numeric(tbl$p_value)
    tbl$feature_name <- as.character(tbl[[feature_col]])
    tbl <- tbl[order(tbl$p_value),,drop=FALSE]
    tbl$rank <- seq_len(nrow(tbl)); tbl$sig <- tbl$p_value < input$pre_alpha
    tbl$logp <- -log10(pmax(tbl$p_value,1e-12)); tbl$status <- ifelse(tbl$sig,"Significant","n.s.")
    alpha_line <- -log10(input$pre_alpha)
    plotly::plot_ly(data=tbl, x=~rank, y=~logp, type="scatter", mode="markers",
                    color=~status, colors=c("Significant"="#c0392b","n.s."="#7f8c8d"),
                    text=~paste0("Feature: ",feature_name,"<br>p: ",signif(p_value,3)),
                    hoverinfo="text", marker=list(size=7,opacity=0.95)) %>%
      plotly::add_segments(x=~rank,xend=~rank,y=0,yend=~logp,inherit=FALSE,
                           line=list(width=1),color=~status,
                           colors=c("Significant"="#c0392b","n.s."="#7f8c8d"),
                           showlegend=FALSE,hoverinfo="none") %>%
      plotly::add_segments(x=min(tbl$rank),xend=max(tbl$rank),
                           y=alpha_line,yend=alpha_line,inherit=FALSE,
                           line=list(color="#c0392b",dash="dash",width=1.2),
                           showlegend=FALSE,hoverinfo="none") %>%
      plotly::layout(
        title=list(text=paste0(toupper(input$pre_test)," – M",input$pre_modsel)),
        xaxis=list(title="Ranked features",showticklabels=FALSE,zeroline=FALSE),
        yaxis=list(title="-log10(adjusted p-value)",rangemode="tozero"),
        legend=list(orientation="h",x=0.72,y=1.08))
  })
  
  output$pre_pval_tbl <- renderDT({
    req(rv$pre_tests, input$pre_test, input$pre_modsel)
    i   <- as.integer(input$pre_modsel)
    tbl <- get_test_tbl(rv$pre_tests, input$pre_test, i)
    tbl$p_value <- as.numeric(tbl$p_value)
    tbl <- tbl[order(tbl$p_value),,drop=FALSE]
    tbl_show <- data.frame(Feature = tbl$feature,
                           `Adjusted p-value` = signif(tbl$p_value,3),
                           Status = ifelse(tbl$p_value < input$pre_alpha,"Significant","OK"),
                           check.names=FALSE)
    datatable(
      tbl_show,
      rownames = FALSE,
      class = "compact stripe hover",
      width = "100%",
      options = list(
        pageLength = 8,
        lengthMenu = c(8, 15, 25, 50, 100),
        scrollX = TRUE,
        dom = "lfrtip"
      )
    ) %>%
      formatStyle(
        "Status",
        fontWeight = "bold",
        fontSize = "17px",
        color = styleEqual(
          c("Significant", "OK"),
          c("#c0392b", "#1e8449")
        ),
        backgroundColor = styleEqual(
          c("Significant", "OK"),
          c("#fdecea", "#eafaf1")
        )
      )
  })
  
  output$pre_mv_panel <- renderUI({
    if (!has_multi_metric()) {
      return(single_metric_notice("multivariate mean/covariance testing"))
    }
    
    tagList(
      tags$p("MANOVA (mean) and Box's M (covariance) across batches, pooled across modalities."),
      actionBttn("run_mv", "Run Multivariate Tests",
                 style = "gradient", color = "royal",
                 icon = icon("play"), size = "sm"),
      tags$br(), tags$br(),
      fluidRow(
        column(
          4,
          div(
            style = "height:480px;display:flex;align-items:center;",
            div(
              style = "width:100%;padding:0 12px;",
              tags$div(class = "sec-hdr", style = "text-align:center;margin-bottom:12px;",
                       "% Significant (Bonferroni)"),
              div(style = "display:flex;justify-content:center;",
                  div(style = "width:85%;", DTOutput("pre_mv_sum")))
            )
          )
        ),
        column(
          8,
          fluidRow(
            column(6,
                   tags$div(class = "sec-hdr", "Feature-level adj. p-values"),
                   selectInput("mv_bar_test", "Test to display:",
                               choices = c("MANOVA (mean shift)" = "manova",
                                           "Box's M (covariance shift)" = "boxm"),
                               selected = "manova")),
            column(6, sliderInput("mv_alpha", "α threshold:", 0.001, 0.2, 0.05, 0.005))
          ),
          plotly::plotlyOutput("pre_mv_bar", height = "480px")
        )
      )
    )
  })
  
  observeEvent(input$run_mv, {
    req(rv$data_list, rv$bat_list, rv$config_ok)
    if (!has_multi_metric()) {
      showNotification(
        "Multivariate tests are disabled because only one imaging metric/modality is available.",
        type = "warning"
      )
      return(NULL)
    }
    withProgress(message="Multivariate tests...", {
      tryCatch({
        rv$mv_res <- mul_test(rv$bat_list, rv$data_list, rv$covar_list,
                              model=rv$model_fn, formula=rv$formula_obj,
                              test_param=diag_test_param())
        setProgress(1); showNotification("Multivariate tests done.", type="message")
      }, error = function(e) showNotification(paste("Error:", e$message), type="error"))
    })
  })
  
  output$pre_mv_sum <- renderDT({
    if (!has_multi_metric()) return(datatable(data.frame(Message="Not applicable for a single metric."), rownames=FALSE, options=list(dom="t")))
    req(rv$mv_res)
    df <- data.frame(Test=c("MANOVA","Box's M"),
                     `% Significant`=c(rv$mv_res$manova_result[1],rv$mv_res$boxM_result[1]),
                     check.names=FALSE)
    datatable(df, rownames=FALSE, options=list(dom="t"), class="compact stripe")
  })
  
  output$pre_mv_bar <- plotly::renderPlotly({
    if (!has_multi_metric()) {
      return(plotly::plot_ly() %>% plotly::layout(title = list(text = "Not applicable for a single metric."),
                                                  xaxis = list(visible = FALSE),
                                                  yaxis = list(visible = FALSE)))
    }
    req(rv$pre_diag, rv$bat_list)
    tryCatch({
      multi_r    <- multi_test_reshape(rv$pre_diag)
      alpha_use  <- input$mv_alpha %||% 0.05
      if (!is.null(input$mv_bar_test) && input$mv_bar_test == "boxm") {
        res <- boxM_test(multi_r$resid_mul, rv$bat_list[[1]])
        tbl <- res$test_table; tbl$p.value <- as.numeric(tbl$p.value)
        plot_title <- paste0("Box's M — covariance shift (", res$perc.sig, " sig.)")
      } else {
        res <- manova_test(multi_r$resid_add, rv$bat_list[[1]])
        tbl <- res$test_table; tbl$p.value <- as.numeric(tbl$p.value)
        plot_title <- paste0("MANOVA — mean shift (", res$perc.sig, " sig.)")
      }
      validate(need(!is.null(tbl),"No MV table."), need(nrow(tbl)>0,"Empty."))
      feature_col <- intersect(c("feature","Feature","roi","ROI"), names(tbl))[1]
      validate(need(length(feature_col)==1,"Cannot find feature column."))
      tbl$feature_name <- as.character(tbl[[feature_col]])
      tbl <- tbl[order(tbl$p.value),,drop=FALSE]
      tbl$rank <- seq_len(nrow(tbl)); tbl$sig <- tbl$p.value < alpha_use
      tbl$logp <- -log10(pmax(tbl$p.value,1e-12)); tbl$status <- ifelse(tbl$sig,"Significant","n.s.")
      alpha_line <- -log10(alpha_use)
      plotly::plot_ly(data=tbl, x=~rank, y=~logp, type="scatter", mode="markers",
                      color=~status, colors=c("Significant"="#c0392b","n.s."="#7f8c8d"),
                      text=~paste0("Feature: ",feature_name,"<br>p: ",signif(p.value,3)),
                      hoverinfo="text", marker=list(size=7,opacity=0.95)) %>%
        plotly::add_segments(x=~rank,xend=~rank,y=0,yend=~logp,inherit=FALSE,
                             line=list(width=1),color=~status,
                             colors=c("Significant"="#c0392b","n.s."="#7f8c8d"),
                             showlegend=FALSE,hoverinfo=FALSE) %>%
        plotly::add_segments(x=min(tbl$rank),xend=max(tbl$rank),
                             y=alpha_line,yend=alpha_line,inherit=FALSE,
                             line=list(color="#c0392b",dash="dash",width=1.2),
                             showlegend=FALSE,hoverinfo=FALSE) %>%
        plotly::layout(title=list(text=plot_title),
                       xaxis=list(title="Ranked features",showticklabels=FALSE,zeroline=FALSE),
                       yaxis=list(title="-log10(adjusted p-value)",rangemode="tozero"),
                       legend=list(orientation="h",x=0.72,y=1.08))
    }, error = function(e) {
      plotly::plot_ly() %>%
        plotly::layout(title=list(text=paste("Run multivariate tests first.", e$message)),
                       xaxis=list(visible=FALSE),yaxis=list(visible=FALSE))
    })
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # 3. HARMONIZATION — EB run + MCMC Script Generation  (NEW section added)
  # ══════════════════════════════════════════════════════════════════════════
  observeEvent(input$run_harm, {
    req(rv$data_list, rv$bat_list, rv$config_ok)
    harm_mode_use <- if (!has_multi_metric()) "uni" else (input$harm_mode %||% "multi")
    harm_cov_use  <- if (!has_multi_metric()) FALSE else isTRUE(input$harm_cov)
    withProgress(message="Running harmonization…", {
      setProgress(0.2)
      tryCatch({
        if (harm_mode_use == "uni") {
          m <- safe_m_value(rv$m); req(!is.null(m))
          parts <- lapply(seq_len(m), function(i)
            com_harm(bat=rv$bat_list[[i]], data=rv$data_list[[i]],
                     covar=rv$covar_list[[i]], model=rv$model_fn,
                     formula=rv$formula_obj, ref.batch=rv$ref_batch,
                     eb=input$harm_eb, robust.LS=input$harm_robust,
                     cov=harm_cov_use, var_thresh=input$harm_vt,
                     min_rblock=input$harm_minr, max_rblock=input$harm_maxr))
          rv$harm <- list(harm_data=lapply(parts,`[[`,"harm_data"),
                          resid=lapply(parts,`[[`,"resid"),
                          eb_result=lapply(parts,`[[`,"eb_result"),
                          mode="uni",
                          eb_enabled=isTRUE(input$harm_eb))
        } else {
          res <- com_harm.multivariate(
            bat=rv$bat_list, data=rv$data_list, covar=rv$covar_list,
            model=rv$model_fn, formula=rv$formula_obj, ref.batch=rv$ref_batch,
            eb=input$harm_eb, robust.LS=input$harm_robust, cov=harm_cov_use,
            var_thresh=input$harm_vt, min_rblock=input$harm_minr, max_rblock=input$harm_maxr,
            robust_cov=input$harm_robcov)
          res$mode <- "multi"
          res$eb_enabled <- isTRUE(input$harm_eb)
          rv$harm <- res
        }
        setProgress(1)
        showNotification(paste0("Harmonization done (", harm_mode_use, " mode)."), type="message")
      }, error = function(e) showNotification(paste("Error:", e$message), type="error"))
    })
  })
  
  output$harm_status_ui <- renderUI({
    if (is.null(rv$harm))
      tags$span(style="color:#e67e22;font-weight:600;", "Not run yet")
    else
      tags$span(style="color:#27ae60;font-weight:600;",
                icon("check-circle"),
                paste0(" Done (", rv$harm$mode, " mode; EB ",
                       if (isTRUE(rv$harm$eb_enabled)) "on" else "off", ")"))
  })
  
  # ── MCMC script settings summary ──────────────────────────────────────────
  mcmc_effective_formula <- reactive({
    base_txt   <- if (!is.null(input$formula_txt) && nchar(trimws(input$formula_txt)) > 0)
      trimws(input$formula_txt) else "y ~ 1"
    model_type <- input$model_type %||% "lm"
    re_var     <- rv$random_var
    if (model_type == "lmer" && isTRUE(rv$is_longitudinal) &&
        !is.null(re_var) && nzchar(re_var)) {
      re_term <- paste0("(1 | ", re_var, ")")
      if (!grepl("\\|", base_txt)) {
        return(paste(base_txt, "+", re_term))
      }
    }
    base_txt
  })
  
  output$mcmc_script_summary_ui <- renderUI({
    grid_row <- function(label, val)
      tags$div(style="display:grid;grid-template-columns:160px 1fr;column-gap:8px;margin-bottom:4px;",
               tags$div(style="font-weight:600;color:#4a5568;font-size:13px;", label),
               tags$div(style="color:#1a2b4a;font-size:13px;", val))
    if (is.null(rv$data_list)) {
      return(tags$div(style="color:#e67e22;font-size:14px;",
                      icon("triangle-exclamation"), " Load data first."))
    }
    tags$div(
      style = "background:#f7f9fc;border:1px solid #d9e2f2;border-radius:8px;padding:12px 14px;",
      grid_row("Mode",       if (!has_multi_metric() || input$harm_mode == "uni") "Univariate" else "Multivariate"),
      grid_row("Modalities", paste0("m = ", rv$m %||% "?")),
      grid_row("Obs / Features", paste0("n=", rv$n %||% "?", "  G=", rv$G %||% "?")),
      grid_row("Batches",    paste(rv$batch_levels %||% "?", collapse=", ")),
      grid_row("Data type",  if (is_long()) "Longitudinal" else "Cross-sectional"),
      grid_row("Model",      input$model_type %||% "lm"),
      grid_row("Formula",    mcmc_effective_formula()), 
      grid_row("Ref. batch", if (is.null(input$ref_batch)||input$ref_batch=="") "None"
               else input$ref_batch),
      #grid_row("EB shrinkage",  if (isTRUE(input$harm_eb))  "Yes" else "No"),
      grid_row("Robust LS",     if (isTRUE(input$harm_robust)) "Yes" else "No"),
      grid_row("CovBat",        if (has_multi_metric() && isTRUE(input$harm_cov))  "Yes" else "No"),
      grid_row("Chains",        as.character(input$mcmc_chains    %||% 4)),
      grid_row("Parallel",      as.character(input$mcmc_parallel  %||% 4)),
      grid_row("adapt_delta",   as.character(input$mcmc_adapt_delta %||% 0.95))
    )
  })
  
  # ── Generate script (reactive, triggered by button or settings change) ────
  mcmc_script_reactive <- eventReactive(input$preview_mcmc_script, {
    harm_mode_use <- if (!has_multi_metric()) "uni" else (input$harm_mode %||% "multi")
    harm_cov_use  <- if (!has_multi_metric()) FALSE else isTRUE(input$harm_cov)
    validate(
      need(!is.null(rv$data_list),  "Please load data first (Data Setup tab)."),
      need(isTRUE(rv$config_ok),    "Please confirm the model configuration first.")
    )
    HarmonizeR::build_mcmc_script(
      m               = rv$m,
      G               = rv$G,
      n               = rv$n,
      batch_levels    = rv$batch_levels,
      feat_names      = rv$feat_names,
      harm_mode       = harm_mode_use,
      formula_txt     = input$formula_txt,
      ref_batch       = if (is.null(input$ref_batch)||input$ref_batch=="") NULL
      else input$ref_batch,
      is_longitudinal = isTRUE(rv$is_longitudinal),
      random_var      = rv$random_var,
      visit_col       = rv$visit_col,
      model_type      = input$model_type,
      harm_eb         = isTRUE(input$harm_eb),
      harm_robust     = isTRUE(input$harm_robust),
      harm_cov        = harm_cov_use,
      harm_vt         = input$harm_vt   %||% 0.95,
      harm_minr       = input$harm_minr %||% 1,
      harm_maxr       = input$harm_maxr %||% 50,
      harm_robcov     = isTRUE(input$harm_robcov),
      chains          = input$mcmc_chains        %||% 4,
      parallel_chains = input$mcmc_parallel      %||% 4,
      adapt_delta     = input$mcmc_adapt_delta   %||% 0.95
    )
  }, ignoreNULL = TRUE)
  
  # Preview (first 120 lines, syntax highlighted monospace)
  output$mcmc_script_preview_ui <- renderUI({
    txt <- tryCatch(mcmc_script_reactive(), error = function(e)
      paste("Error generating script:", e$message))
    lines <- strsplit(txt, "\n")[[1]]
    preview <- paste(head(lines, 120), collapse = "\n")
    if (length(lines) > 120)
      preview <- paste0(preview, "\n\n# ... (", length(lines)-120, " more lines — download to see full script) ...")
    tags$pre(style = "margin:0;color:#cdd6f4;", preview)
  })
  
  # Download button — only shown after first preview click
  output$mcmc_dl_ui <- renderUI({
    if (input$preview_mcmc_script == 0) return(NULL)
    downloadButton("dl_mcmc_project", "Download MCMC Project Folder (.zip)",
                   class = "btn btn-success", style = "width:100%;")
  })
  
  output$dl_mcmc_project <- downloadHandler(
    filename = function() {
      paste0("HarmonizeR_MCMC_project_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(rv$data_list, rv$bat_list)
      validate(
        need(!is.null(rv$data_list), "Please load data first (Data Setup tab)."),
        need(isTRUE(rv$config_ok), "Please confirm the model configuration first.")
      )
      
      project_name <- paste0("HarmonizeR_MCMC_project_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      project_dir  <- file.path(tempdir(), project_name)
      if (dir.exists(project_dir)) unlink(project_dir, recursive = TRUE, force = TRUE)
      
      dir.create(file.path(project_dir, "data"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(project_dir, "R"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(project_dir, "results", "harmonized"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(project_dir, "results", "stan_fits"), recursive = TRUE, showWarnings = FALSE)
      
      # Save one feature matrix per modality to data/
      m <- safe_m_value(rv$m); req(!is.null(m))
      for (i in seq_len(m)) {
        write.csv(
          rv$data_list[[i]],
          file = file.path(project_dir, "data", paste0("modality_", i, ".csv")),
          row.names = TRUE
        )
      }
      
      # Save shared batch labels to data/
      write.csv(
        data.frame(batch = as.character(rv$bat_list[[1]])),
        file = file.path(project_dir, "data", "batch_labels.csv"),
        row.names = FALSE
      )
      
      # Save shared covariates to data/. If missing, create an intercept-only placeholder.
      covar_to_write <- if (!is.null(rv$covar_list) && !is.null(rv$covar_list[[1]])) {
        rv$covar_list[[1]]
      } else {
        data.frame(intercept_only = rep(1, rv$n))
      }
      write.csv(
        covar_to_write,
        file = file.path(project_dir, "data", "covariates.csv"),
        row.names = FALSE
      )
      
      # Save the generated MCMC script to R/
      txt <- tryCatch(mcmc_script_reactive(), error = function(e)
        paste("# Error generating script:", e$message))
      writeLines(txt, file.path(project_dir, "R", "run_mcmc_harmonization.R"))
      
      # Add a README so users know how to run the project
      readme_txt <- c(
        "# HarmonizeR MCMC Project",
        "",
        "This folder was generated by the HarmonizeR Shiny app.",
        "",
        "## Folder structure",
        "",
        "- data/: input data exported from the Shiny app",
        "- R/: runnable MCMC harmonization script",
        "- results/harmonized/: harmonized CSV outputs generated by the script",
        "- results/stan_fits/: Stan fit .rds files generated by the script",
        "",
        "## How to run",
        "",
        "1. Open R or RStudio.",
        "2. Set the working directory to this project folder.",
        "3. Run:",
        "",
        "```r",
        'source("R/run_mcmc_harmonization.R")',
        "```",
        "",
        "## After running",
        "",
        "Upload the .rds file(s) from results/stan_fits/ back to the Bayesian MCMC Diag tab for interactive diagnostics. In univariate mode, upload one RDS per metric/modality."
      )
      writeLines(readme_txt, file.path(project_dir, "README.md"))
      
      # Zip the whole project folder. Prefer zip::zipr(); fall back to utils::zip().
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(dirname(project_dir))
      
      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zipr(zipfile = file, files = basename(project_dir))
      } else {
        utils::zip(zipfile = file, files = basename(project_dir), flags = "-r9X")
      }
    },
    contentType = "application/zip"
  )
  
  # ── EB availability guard ─────────────────────────────────────────────
  eb_available <- reactive({
    if (is.null(rv$harm) || is.null(rv$harm$eb_result)) return(FALSE)
    
    # New harmonization runs store whether EB was enabled.
    # If an older object lacks this flag, fall back to checking eb_result.
    if (!is.null(rv$harm$eb_enabled) && !isTRUE(rv$harm$eb_enabled)) {
      return(FALSE)
    }
    
    eb <- rv$harm$eb_result
    if (rv$harm$mode == "uni") {
      return(is.list(eb) && length(eb) > 0 && any(!vapply(eb, is.null, logical(1))))
    }
    
    !is.null(eb)
  })
  
  # ── EB shrinkage plot and BA plot (unchanged) ─────────────────────────────
  output$eb_shrink_mod_ui <- renderUI({
    req(eb_available())
    m <- safe_m_value(rv$m); req(!is.null(m))
    if (rv$harm$mode == "uni" && m > 1)
      selectInput("eb_shrink_mod", "Modality:",
                  choices=setNames(seq_len(m),paste0("M",seq_len(m))), selected=1)
  })
  output$eb_shrink_range_ui <- renderUI({
    req(eb_available(), rv$G)
    sliderInput("eb_shrink_range","Feature index range:",
                min=1, max=rv$G, value=c(1,min(15L,rv$G)), step=1)
  })

  output$eb_shrink_batch_ui <- renderUI({
    req(eb_available(), rv$batch_levels)
    batches <- as.character(rv$batch_levels)
    ref <- rv$ref_batch %||% NULL
    if (!is.null(ref) && !identical(ref, "") && !is.na(ref)) {
      batches <- setdiff(batches, as.character(ref))
    }
    if (length(batches) == 0) {
      return(tags$div(style = "color:#777;margin-top:4px;",
                      "No non-reference batches available for display."))
    }
    shinyWidgets::pickerInput(
      inputId = "eb_shrink_batches",
      label = "Batch levels to display:",
      choices = batches,
      selected = batches,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        selectedTextFormat = "count > 2"
      )
    )
  })
  
  output$eb_shrink_plot <- renderPlot({
    req(eb_available(), input$eb_shrink_range)
    tryCatch({
      parm <- input$eb_shrink_parm %||% "gamma"
      fi   <- input$eb_shrink_range
      feat_all <- rv$feat_names
      lo <- max(1L, fi[1]); hi <- min(length(feat_all), fi[2])
      feat_sel <- feat_all[lo:hi]
      ref <- rv$ref_batch %||% NULL
      if (!is.null(ref) && !identical(ref, "") && !is.na(ref)) ref <- as.character(ref) else ref <- NULL

      if (rv$harm$mode == "uni") {
        i   <- as.integer(input$eb_shrink_mod %||% 1)
        eb  <- rv$harm$eb_result[[i]]

        batches <- rownames(eb$gamma_hat)
        if (is.null(batches) || length(batches) == 0) batches <- as.character(rv$batch_levels)
        if (!is.null(ref)) batches <- setdiff(batches, ref)

        if (parm == "gamma") {
          hat_mat <- eb$gamma_hat[, feat_sel, drop = FALSE]
          star_mat <- eb$gamma_star[, feat_sel, drop = FALSE]
          x_lbl <- expression(gamma[g]); ttl <- expression("EB shrinkage of " ~ gamma[g])
          size_lbl <- expression(abs(hat(gamma)[g]-gamma[g]^"*"))
        } else {
          hat_mat <- eb$delta_hat[, feat_sel, drop = FALSE]
          star_mat <- eb$delta_star[, feat_sel, drop = FALSE]
          x_lbl <- expression(delta[g]); ttl <- expression("EB shrinkage of " ~ delta[g])
          size_lbl <- expression(abs(hat(delta)[g]-delta[g]^"*"))
        }
      } else {
        eb <- rv$harm$eb_result
        batches <- names(eb$gamma_hat)
        if (is.null(batches) || length(batches) == 0) batches <- as.character(rv$batch_levels)
        if (!is.null(ref)) batches <- setdiff(batches, ref)

        if (parm == "gamma") {
          hat_mat  <- do.call(rbind, lapply(batches, function(b) colMeans(eb$gamma_hat[[b]])[feat_sel]))
          star_mat <- do.call(rbind, lapply(batches, function(b) colMeans(eb$gamma_star[[b]])[feat_sel]))
          rownames(hat_mat) <- rownames(star_mat) <- batches
          x_lbl <- expression(bar(gamma)[g]); ttl <- expression("EB shrinkage of " ~ bar(gamma)[g])
          size_lbl <- expression(abs(bar(hat(gamma))[g]-bar(gamma)[g]^"*"))
        } else {
          feat_idx <- match(feat_sel, feat_all)
          hat_mat  <- do.call(rbind, lapply(batches, function(b)
            sapply(feat_idx, function(gi) sqrt(sum(eb$delta_hat[[b]][[gi]]^2)))))
          star_mat <- do.call(rbind, lapply(batches, function(b)
            sapply(feat_idx, function(gi) sqrt(sum(eb$delta_star[[b]][[gi]]^2)))))
          rownames(hat_mat) <- rownames(star_mat) <- batches
          colnames(hat_mat) <- colnames(star_mat) <- feat_sel
          x_lbl <- expression(group("||",Sigma[g],"||")[F])
          ttl    <- expression("EB shrinkage of " ~ group("||",Sigma[g],"||")[F])
          size_lbl <- expression(abs(group("||",hat(Sigma)[g],"||")[F]-group("||",Sigma[g]^"*","||")[F]))
        }
      }

      selected_batches <- input$eb_shrink_batches
      if (!is.null(selected_batches) && length(selected_batches) > 0) {
        batches <- intersect(batches, as.character(selected_batches))
      }
      validate(need(length(batches) > 0, "Please select at least one batch level to display."))

      long_df <- do.call(rbind, lapply(batches, function(b) {
        data.frame(
          Feature = feat_sel,
          Batch = b,
          hat = as.numeric(hat_mat[b, feat_sel]),
          star = as.numeric(star_mat[b, feat_sel]),
          shrink = abs(as.numeric(hat_mat[b, feat_sel]) - as.numeric(star_mat[b, feat_sel])),
          stringsAsFactors = FALSE
        )
      }))

      sort_opt <- input$eb_shrink_sort %||% "original"
      if (sort_opt %in% c("desc", "asc")) {
        shrink_by_feature <- tapply(long_df$shrink, long_df$Feature, mean, na.rm = TRUE)
        feat_levels <- names(sort(shrink_by_feature, decreasing = identical(sort_opt, "desc")))
      } else {
        feat_levels <- feat_sel
      }
      long_df$Feature <- factor(long_df$Feature, levels = rev(feat_levels))

      long_df$Batch <- clean_batch_for_plot(long_df$Batch)
      batch_cols <- safe_discrete_palette(levels(droplevels(long_df$Batch)))
      ggplot(long_df) +
        geom_segment(aes(x=hat,xend=star,y=Feature,yend=Feature,color=Batch),
                     linewidth=1.1,alpha=0.65,
                     arrow=arrow(length=unit(0.13,"cm"),type="closed",ends="last")) +
        geom_point(aes(x=hat,y=Feature,color=Batch),shape=21,fill="white",size=3.0,stroke=1.3) +
        geom_point(aes(x=star,y=Feature,color=Batch,size=shrink),shape=16,alpha=0.88) +
        geom_vline(xintercept=0,linetype="dashed",color="#999",linewidth=0.5) +
        facet_wrap(~Batch,nrow=1) +
        scale_color_manual(values=batch_cols, guide="none") +
        scale_size_continuous(name=size_lbl,range=c(1.5,6.5),
                              guide=guide_legend(override.aes=list(color="#2e4c8a"))) +
        labs(x=x_lbl,y="Feature",title=ttl) + bold_theme(11) +
        theme(strip.text=element_text(face="bold",size=16),
              panel.spacing=unit(6,"pt"),axis.text.y=element_text(size=8))
    }, error = function(e) {
      ggplot() + labs(title=paste("Run harmonization first.", e$message)) + bold_theme(12)
    })
  })
  
  output$ba_mod_ui <- renderUI({
    req(rv$harm)
    m <- safe_m_value(rv$m); req(!is.null(m))
    selectInput("ba_mod", label=NULL,
                choices=setNames(seq_len(m),paste0("M",seq_len(m))),
                selected=1, width="90px")
  })
  
  output$ba_plot <- renderPlot({
    req(rv$harm, rv$data_list)
    i       <- as.integer(input$ba_mod %||% 1)
    bat     <- droplevels(rv$bat_list[[i]]); batches <- levels(bat)
    raw_mat <- as.matrix(rv$data_list[[i]])
    hrm_mat <- as.matrix(as.data.frame(rv$harm$harm_data[[i]]))
    df <- do.call(rbind, lapply(batches, function(b) {
      idx <- which(bat == b)
      data.frame(Feature=rep(rv$feat_names,2), Batch=b,
                 Stage=rep(c("Before","After"),each=rv$G),
                 Mean=c(colMeans(raw_mat[idx,,drop=FALSE]),
                        colMeans(hrm_mat[idx,,drop=FALSE])),
                 stringsAsFactors=FALSE)
    }))
    df$Stage   <- factor(df$Stage, levels=c("Before","After"))
    df$Feature <- factor(df$Feature, levels=rv$feat_names)
    df$Batch <- clean_batch_for_plot(df$Batch)
    df$IsRef   <- !is.null(rv$ref_batch) & as.character(df$Batch) == (rv$ref_batch %||% "")
    batch_cols <- safe_discrete_palette(levels(droplevels(df$Batch)))
    ggplot(df, aes(x=Feature,y=Mean,color=Batch,group=Batch)) +
      geom_line(aes(linetype=IsRef,alpha=IsRef),linewidth=0.65) +
      facet_wrap(~Stage,ncol=2) +
      scale_color_manual(values=batch_cols,name="Batch") +
      scale_linetype_manual(values=c("FALSE"="solid","TRUE"="dashed"),guide="none") +
      scale_alpha_manual(values=c("FALSE"=0.72,"TRUE"=1.0),guide="none") +
      labs(x="Feature",y="Batch mean",
           title=paste0("M",i," — per-feature batch means before vs after"),
           subtitle=if(!is.null(rv$ref_batch))
             paste0("Reference batch: ",rv$ref_batch," (dashed) stays flat; others converge toward it")
           else "No reference batch — lines should converge to the pooled mean") +
      bold_theme(10) +
      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
            panel.spacing=unit(8,"pt"),strip.text=element_text(face="bold",size=11))
  }, bg = "white")
  
  output$harm_prev <- renderDT({
    req(rv$harm)
    d <- as.data.frame(rv$harm$harm_data[[1]])
    datatable(round(d[1:min(8,nrow(d)),1:min(6,ncol(d))],3),
              options=list(dom="t",scrollX=TRUE), class="compact stripe")
  })
  output$resid_prev <- renderDT({
    req(rv$harm)
    d <- as.data.frame(rv$harm$resid[[1]])
    datatable(round(d[1:min(8,nrow(d)),1:min(6,ncol(d))],3),
              options=list(dom="t",scrollX=TRUE), class="compact stripe")
  })
  
  output$dl_ui <- renderUI({
    req(rv$harm)
    m <- safe_m_value(rv$m); req(!is.null(m))
    tagList(lapply(seq_len(m), function(i) tags$div(
      style="margin-bottom:4px;",
      downloadButton(paste0("dlh",i), paste0("M",i," harm_data.csv"),
                     class="btn btn-primary btn-sm", style="width:100%;margin-bottom:3px;"),
      downloadButton(paste0("dlr",i), paste0("M",i," residuals.csv"),
                     class="btn btn-warning btn-sm", style="width:100%;")
    )))
  })
  
  observe({
    req(rv$harm)
    m <- safe_m_value(rv$m); req(!is.null(m))
    lapply(seq_len(m), function(i) local({
      ii <- i
      output[[paste0("dlh",ii)]] <- downloadHandler(
        filename=paste0("harm_data_M",ii,".csv"),
        content=function(f) write.csv(rv$harm$harm_data[[ii]],f))
      output[[paste0("dlr",ii)]] <- downloadHandler(
        filename=paste0("residuals_M",ii,".csv"),
        content=function(f) write.csv(rv$harm$resid[[ii]],f))
    }))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # 4. POST-HARMONIZATION  (unchanged)
  # ══════════════════════════════════════════════════════════════════════════
  observeEvent(input$run_post_diag, {
    req(rv$harm, rv$config_ok)
    withProgress(message="Post-harmonization diagnostics…", {
      tryCatch({
        harm_data    <- rv$harm$harm_data; setProgress(0.2)
        rv$post_diag <- run_diag(harm_data, rv$bat_list, rv$covar_list); setProgress(0.5)
        rv$post_pca  <- pca_prep(bat=rv$bat_list, data=harm_data,
                                 covar=rv$covar_list, model=rv$model_fn,
                                 formula=rv$formula_obj, ref.batch=rv$ref_batch,
                                 bat_adjust=input$bat_adjust, test_param=diag_test_param())
        setProgress(0.75)
        rv$post_tests <- run_uni(rv$post_diag, rv$bat_list)
        if (isTRUE(input$post_run_rf_auc)) {
          setProgress(0.82, "Random forest batch AUC...")
          rv$pre_rf_auc <- rv$pre_rf_auc %||% tryCatch(
            HarmonizeR::batch_auc_cal_cv(
              rv$data_list,
              rv$bat_list,
              k = input$post_rf_k %||% 5,
              ntree = input$post_rf_ntree %||% 100
            ),
            error = function(e) NULL
          )
          rv$post_rf_auc <- tryCatch(
            HarmonizeR::batch_auc_cal_cv(
              harm_data,
              rv$bat_list,
              k = input$post_rf_k %||% 5,
              ntree = input$post_rf_ntree %||% 100
            ),
            error = function(e) {
              showNotification(paste("Post RF batch AUC skipped:", e$message), type = "warning", duration = 8)
              NULL
            }
          )
        } else {
          rv$post_rf_auc <- NULL
        }
        
        # MANOVA / Box's M require multiple response variables. When the active
        # dataset has only one imaging metric/modality, skip multivariate testing
        # rather than calling mul_test(), which otherwise raises:
        # "need multiple responses".
        if (has_multi_metric()) {
          rv$post_mv <- mul_test(rv$bat_list, harm_data, rv$covar_list,
                                 model=rv$model_fn, formula=rv$formula_obj,
                                 test_param=diag_test_param())
        } else {
          rv$post_mv <- NULL
        }
        
        setProgress(1); showNotification("Post-diagnostics done.", type="message")
      }, error = function(e) showNotification(paste("Error:", e$message), type="error"))
    })
  })
  
  pca_with_matched_axes <- function(pca_obj, type, pc1, pc2, ellipse, title_lbl) {
    p_before <- pca_plot_robust(rv$pre_pca,  type = type, pc_1 = pc1, pc_2 = pc2,
                                ellipse = ellipse, equal_axes = FALSE)
    p_after  <- pca_plot_robust(rv$post_pca, type = type, pc_1 = pc1, pc_2 = pc2,
                                ellipse = ellipse, equal_axes = FALSE)
    b1 <- ggplot_build(p_before); b2 <- ggplot_build(p_after)
    x_all <- c(unlist(lapply(b1$data, `[[`, "x")), unlist(lapply(b2$data, `[[`, "x")))
    y_all <- c(unlist(lapply(b1$data, `[[`, "y")), unlist(lapply(b2$data, `[[`, "y")))
    x_rng <- range(x_all, na.rm = TRUE); y_rng <- range(y_all, na.rm = TRUE)
    p <- if (title_lbl == "Before") p_before else p_after
    use_equal_axes <- pca_equal_axes_default(pca_obj, type)
    p <- p + labs(title = title_lbl) + bold_theme(12) +
      theme(plot.background = element_rect(fill = "white", color = NA),
            legend.position = "bottom", plot.margin = margin(8, 8, 8, 8))
    if (isTRUE(use_equal_axes)) {
      p + coord_equal(xlim = x_rng, ylim = y_rng)
    } else {
      p + coord_cartesian(xlim = x_rng, ylim = y_rng)
    }
  }
  
  output$post_pca_before <- renderPlot({
    req(rv$pre_pca, rv$post_pca)
    print(pca_with_matched_axes(rv$pre_pca, pca_type_use(input$post_pca_type),
                                input$post_pc1, input$post_pc2, input$post_ellipse, "Before"))
  }, height = function() {
    req(rv$pre_pca)
    pca_plot_height_px(rv$pre_pca, type = pca_type_use(input$post_pca_type), compare_mode = TRUE)
  }, bg = "white")
  output$post_pca_after <- renderPlot({
    req(rv$pre_pca, rv$post_pca)
    print(pca_with_matched_axes(rv$post_pca, pca_type_use(input$post_pca_type),
                                input$post_pc1, input$post_pc2, input$post_ellipse, "After"))
  }, height = function() {
    req(rv$post_pca)
    pca_plot_height_px(rv$post_pca, type = pca_type_use(input$post_pca_type), compare_mode = TRUE)
  }, bg = "white")
  output$post_heat_before <- renderPlot({
    req(rv$pre_tests)
    m <- safe_m_value(rv$m); req(!is.null(m))
    print(render_uni_heat(tests_to_df(rv$pre_tests),paste0("M",seq_len(m)),"Before harmonization \n"))
  }, bg="white")
  output$post_heat_after <- renderPlot({
    req(rv$post_tests)
    m <- safe_m_value(rv$m); req(!is.null(m))
    print(render_uni_heat(tests_to_df(rv$post_tests),paste0("M",seq_len(m)),"After harmonization \n"))
  }, bg="white")
  
  output$post_tbl <- renderDT({
    req(rv$pre_tests, rv$post_tests)
    m <- safe_m_value(rv$m); req(!is.null(m))
    pre_df <- tests_to_df(rv$pre_tests); pst_df <- tests_to_df(rv$post_tests)
    ms <- paste0("M",seq_len(m))
    test_labels <- c("ANOVA","Kruskal","Levene","Bartlett","Fligner")
    test_cols   <- c("anova_result","kruskal_result","lv_result","bl_result","fk_result")
    df <- data.frame(Modality=ms,
                     setNames(lapply(test_cols,function(tc) pre_df[[tc]]),paste0(test_labels," (Before)")),
                     setNames(lapply(test_cols,function(tc) pst_df[[tc]]),paste0(test_labels," (After)")),
                     check.names=FALSE)
    interleaved <- as.vector(rbind(paste0(test_labels," (Before)"),paste0(test_labels," (After)")))
    df <- df[,c("Modality",interleaved)]
    datatable(df,rownames=FALSE,options=list(dom="t",scrollX=TRUE),class="compact stripe") %>%
      formatStyle(columns=paste0(test_labels," (Before)"),backgroundColor="#fef9e7") %>%
      formatStyle(columns=paste0(test_labels," (After)"), backgroundColor="#eafaf1")
  })
  
  output$post_tbl_mv <- renderDT({
    if (!has_multi_metric()) {
      df <- data.frame(
        Message = "Multivariate test comparison is not applicable because only one imaging metric/modality is available."
      )
      return(datatable(df, rownames = FALSE, options = list(dom = "t"), class = "compact stripe"))
    }
    
    req(rv$mv_res, rv$post_mv)
    mv_df <- data.frame("MANOVA (Before)"=rv$mv_res$manova_result[1],
                        "MANOVA (After)"=rv$post_mv$manova_result[1],
                        "Box's M (Before)"=rv$mv_res$boxM_result[1],
                        "Box's M (After)"=rv$post_mv$boxM_result[1], check.names=FALSE)
    datatable(mv_df, rownames=FALSE, options=list(dom="t",scrollX=TRUE,ordering=FALSE),
              class="compact stripe") %>%
      formatStyle(columns=c("MANOVA (Before)","Box's M (Before)"),backgroundColor="#fef9e7") %>%
      formatStyle(columns=c("MANOVA (After)", "Box's M (After)"), backgroundColor="#eafaf1")
  })

  output$post_rf_auc_plot <- renderPlot({
    if (is.null(rv$post_rf_auc)) {
      print(rf_auc_placeholder("post"))
      return(invisible(NULL))
    }
    before <- rv$pre_rf_auc
    after  <- rv$post_rf_auc
    if (is.null(before)) {
      before <- data.frame(measurement = character(), fold = integer(), auc = numeric())
    }
    before$stage <- "Before"
    after$stage <- "After"
    df <- rbind(before, after)
    validate(need(nrow(df) > 0, "No RF batch-prediction AUC results available."))
    df$stage <- factor(df$stage, levels = c("Before", "After"))
    stage_palette <- c("Before" = "#d97706", "After" = "#0f766e")
    p <- ggplot(df, aes(x = measurement, y = auc, fill = stage, color = stage)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.45, ymax = 0.60,
               fill = "#f7f7f7", alpha = 0.65) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "#6b7280", linewidth = 0.55) +
      geom_boxplot(position = position_dodge(width = 0.72), width = 0.56,
                   outlier.shape = NA, alpha = 0.68, linewidth = 0.65) +
      geom_point(aes(group = stage),
                 position = position_jitterdodge(jitter.width = 0.08, dodge.width = 0.72),
                 alpha = 0.78, size = 1.8, shape = 21, stroke = 0.25) +
      scale_fill_manual(values = stage_palette) +
      scale_color_manual(values = stage_palette) +
      coord_cartesian(ylim = c(0.45, 1.0)) +
      labs(
        x = "Metric",
        y = "Cross-validated macro AUC",
        fill = NULL,
        color = NULL,
        title = "Before vs after global batch prediction",
        subtitle = "Lower post-harmonization AUC indicates weaker out-of-sample batch detectability."
      ) +
      bold_theme(12) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#fbfcfe", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#e5e7eb", linewidth = 0.35),
        legend.position = "top",
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA)
      )
    print(p)
  }, bg = "white")
  
  output$post_rf_auc_tbl <- renderDT({
    if (is.null(rv$post_rf_auc)) {
      return(datatable(
        data.frame(Message = "Post-harmonization RF batch-prediction AUC has not been run."),
        rownames = FALSE, options = list(dom = "t"), class = "compact stripe"
      ))
    }
    pre_sum <- summarize_rf_auc(rv$pre_rf_auc)
    post_sum <- summarize_rf_auc(rv$post_rf_auc)
    if (is.null(pre_sum)) {
      df <- post_sum
      names(df)[names(df) == "mean_auc"] <- "mean_auc_after"
    } else {
      df <- dplyr::full_join(
        pre_sum %>% dplyr::select(measurement, mean_auc_before = mean_auc, sd_auc_before = sd_auc, valid_folds_before = valid_folds),
        post_sum %>% dplyr::select(measurement, mean_auc_after = mean_auc, sd_auc_after = sd_auc, valid_folds_after = valid_folds),
        by = "measurement"
      ) %>%
        dplyr::mutate(delta_auc = round(mean_auc_after - mean_auc_before, 3))
    }
    datatable(df, rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE),
              class = "compact stripe") %>%
      formatStyle("mean_auc_after", background = styleInterval(c(0.60, 0.80), c("#f0fdf4", "#fef3c7", "#fee2e2")),
                  fontWeight = "bold") %>%
      formatStyle("delta_auc", background = styleInterval(c(-0.05, 0.05), c("#d1fae5", "#fef9c3", "#fee2e2")),
                  fontWeight = "bold")
  })
  
  output$post_feat_ui <- renderUI({ req(rv$feat_names); selectInput("post_feat","Feature:",choices=rv$feat_names,selected=rv$feat_names[1]) })
  output$post_mod_ui  <- renderUI({ m <- safe_m_value(rv$m); req(!is.null(m)); selectInput("post_mod","Modality:",choices=setNames(seq_len(m),paste0("M",seq_len(m))),selected=1) })
  
  post_box <- function(resid_mat, bat, feat, stage_label, fill_pal="Set2", jitter=FALSE) {
    if (is.null(resid_mat) || !feat %in% colnames(resid_mat)) return(NULL)
    df <- data.frame(value=suppressWarnings(as.numeric(resid_mat[,feat])), Batch=clean_batch_for_plot(bat))
    df <- df[is.finite(df$value) & !is.na(df$Batch), , drop = FALSE]
    validate(need(nrow(df) > 0, "No valid residual values are available."))
    fill_cols <- safe_discrete_palette(levels(droplevels(df$Batch)))
    p <- ggplot(df, aes(x=Batch,y=value,fill=Batch)) +
      geom_boxplot(outlier.size=0.7,width=.55,linewidth=.4,alpha=.85,color="#333") +
      scale_fill_manual(values=fill_cols) +
      labs(x="Batch",y="Residual",title=stage_label) + bold_theme(11) +
      theme(legend.position="none",panel.grid.major.x=element_blank())
    if (jitter) p <- p + geom_jitter(width=.18,size=.5,alpha=.3,color="#333")
    p
  }
  
  output$post_box_add_before <- renderPlot({ req(rv$pre_diag,input$post_feat,input$post_mod); i<-as.integer(input$post_mod); print(post_box(rv$pre_diag[[i]]$resid_add,rv$bat_list[[i]],input$post_feat,"Before",jitter=isTRUE(input$post_box_jitter))) }, bg="white")
  output$post_box_add_after  <- renderPlot({ req(rv$post_diag,input$post_feat,input$post_mod); i<-as.integer(input$post_mod); print(post_box(rv$post_diag[[i]]$resid_add,rv$bat_list[[i]],input$post_feat,"After",jitter=isTRUE(input$post_box_jitter))) }, bg="white")
  output$post_box_mul_before <- renderPlot({ req(rv$pre_diag,input$post_feat,input$post_mod); i<-as.integer(input$post_mod); print(post_box(rv$pre_diag[[i]]$resid_mul,rv$bat_list[[i]],input$post_feat,"Before",fill_pal="Pastel2",jitter=isTRUE(input$post_box_jitter))) }, bg="white")
  output$post_box_mul_after  <- renderPlot({ req(rv$post_diag,input$post_feat,input$post_mod); i<-as.integer(input$post_mod); print(post_box(rv$post_diag[[i]]$resid_mul,rv$bat_list[[i]],input$post_feat,"After",fill_pal="Pastel2",jitter=isTRUE(input$post_box_jitter))) }, bg="white")
  
  # ══════════════════════════════════════════════════════════════════════════
  # 5. EB DIAGNOSTICS
  #    If a reference batch is used, hide that batch from EB diagnostic display.
  #
  #    Important implementation detail:
  #    Do NOT remove the reference batch before calling eb_check()/eb_plot().
  #    Some EB plotting internals label batches positionally. If we drop batch A
  #    from the EB object first, the original A values can be relabeled as B.
  #    Therefore we always run eb_check()/eb_plot() on the original fitted EB
  #    object, then filter the plotted data and summary table by the true batch
  #    label afterward.
  # ══════════════════════════════════════════════════════════════════════════
  
  # ---- Helper: current reference batch -----------------------------------
  eb_ref_batch <- reactive({
    ref <- rv$ref_batch %||% NULL
    if (is.null(ref) || identical(ref, "") || is.na(ref)) NULL else as.character(ref)
  })
  
  # ---- Helper: display batches = all batches except reference batch --------
  eb_display_batches <- reactive({
    req(rv$batch_levels)
    batches <- as.character(rv$batch_levels)
    ref <- eb_ref_batch()
    
    if (!is.null(ref)) {
      batches <- setdiff(batches, ref)
    }
    
    batches
  })
  
  # ---- Helper: get original EB object -------------------------------------
  # This returns the EB object exactly as fitted, without dropping/relabeling.
  eb_selected_mod_index <- reactive({
    req(rv$m)
    
    # Single-metric data always use M1 internally; no selector is displayed.
    if (rv$m <= 1) {
      return(1L)
    }
    
    as.integer(input$eb_mod %||% 1L)
  })
  
  eb_original <- reactive({
    req(rv$harm)
    
    if (rv$harm$mode == "uni") {
      rv$harm$eb_result[[eb_selected_mod_index()]]
    } else {
      rv$harm$eb_result
    }
  })
  
  # ---- Helper: attach batch labels when EB objects are unlabeled -----------
  # This is only for table calculations. It prevents accidental positional
  # relabeling after a reference batch is removed.
  add_batch_labels_if_needed <- function(x, batch_levels) {
    if (is.null(x)) return(x)
    batch_levels <- as.character(batch_levels)
    
    if (is.matrix(x) || is.data.frame(x)) {
      if (nrow(x) == length(batch_levels)) {
        rn <- rownames(x)
        if (is.null(rn) || anyNA(rn) || all(rn == as.character(seq_len(nrow(x))))) {
          rownames(x) <- batch_levels
        }
      }
      return(x)
    }
    
    if (is.list(x) && length(x) == length(batch_levels)) {
      nm <- names(x)
      if (is.null(nm) || anyNA(nm) || any(nm == "")) {
        names(x) <- batch_levels
      }
      return(x)
    }
    
    if (is.atomic(x) && length(x) == length(batch_levels)) {
      nm <- names(x)
      if (is.null(nm) || anyNA(nm) || any(nm == "")) {
        names(x) <- batch_levels
      }
      return(x)
    }
    
    x
  }
  
  # ---- Helper: drop reference batch from a labeled object ------------------
  drop_ref_by_label <- function(x, ref, batch_levels = NULL) {
    if (is.null(x) || is.null(ref)) return(x)
    
    if (!is.null(batch_levels)) {
      x <- add_batch_labels_if_needed(x, batch_levels)
    }
    
    if ((is.matrix(x) || is.data.frame(x)) && !is.null(rownames(x))) {
      return(x[rownames(x) != ref, , drop = FALSE])
    }
    
    if (is.list(x) && !is.null(names(x))) {
      return(x[setdiff(names(x), ref)])
    }
    
    if (is.atomic(x) && !is.null(names(x))) {
      return(x[setdiff(names(x), ref)])
    }
    
    # If no reliable label exists, do not drop by position.
    # Dropping by position is exactly what can cause the A-as-B problem.
    x
  }
  
  # ---- Helper: filter a ggplot object by the true reference batch label ----
  # Works after eb_plot() creates the plot from the original EB object.
  remove_ref_from_plot_data <- function(p, ref) {
    if (is.null(p) || is.null(ref)) return(p)
    
    filter_df <- function(d) {
      if (!is.data.frame(d) || nrow(d) == 0) return(d)
      
      batch_cols <- intersect(
        c("Batch", "batch", "bat", "BatchID", "batch_id", "site", "Site"),
        names(d)
      )
      
      for (cc in batch_cols) {
        vals <- as.character(d[[cc]])
        if (ref %in% vals) {
          d <- d[vals != ref, , drop = FALSE]
          if (is.factor(d[[cc]])) d[[cc]] <- droplevels(d[[cc]])
        }
      }
      
      d
    }
    
    p$data <- filter_df(p$data)
    
    if (!is.null(p$layers) && length(p$layers) > 0) {
      for (ii in seq_along(p$layers)) {
        if (!is.null(p$layers[[ii]]$data) && is.data.frame(p$layers[[ii]]$data)) {
          p$layers[[ii]]$data <- filter_df(p$layers[[ii]]$data)
        }
      }
    }
    
    p
  }
  
  filter_eb_plot_to_selected_param <- function(p, parm_use) {
    if (is.null(p) || is.null(parm_use)) return(p)
    
    keep_param <- function(d) {
      if (!is.data.frame(d) || nrow(d) == 0) return(d)
      parm_col <- intersect(c("parm", "param", "Parameter", "parameter"), names(d))
      if (length(parm_col) == 0) return(d)
      cc <- parm_col[1]
      d[as.character(d[[cc]]) == parm_use, , drop = FALSE]
    }
    
    p$data <- keep_param(p$data)
    
    if (!is.null(p$layers) && length(p$layers) > 0) {
      for (ii in seq_along(p$layers)) {
        if (!is.null(p$layers[[ii]]$data) && is.data.frame(p$layers[[ii]]$data)) {
          p$layers[[ii]]$data <- keep_param(p$layers[[ii]]$data)
        }
      }
    }
    
    p
  }
  
  # ---- Batch selector: only show/use it for δ view ------------------------
  # Important: the γ plot should not depend on input$eb_batch. If this UI is
  # always shown and output$eb_plot reads input$eb_batch, changing the δ batch
  # selector will also redraw/change the γ plot. Therefore this selector is
  # hidden for γ and used only when input$eb_parm == "delta".
  output$eb_controls_ui <- renderUI({
    req(eb_available())
    
    # In the single-metric/univariate case, the package eb_plot.univariate()
    # already displays both gamma and delta facets, and the δ density is not
    # controlled by a batch argument. Therefore parameter and batch selectors
    # are hidden to avoid implying that they change the plot.
    if (!is.null(rv$m) && rv$m <= 1 && identical(rv$harm$mode, "uni")) {
      return(
        fluidRow(
          column(9,
                 tags$div(
                   style = paste0(
                     "background:#eef6ff;border-left:4px solid #3d7fc1;",
                     "padding:11px 13px;border-radius:6px;margin-top:6px;",
                     "font-size:14px;color:#24476b;line-height:1.35;"
                   ),
                   tags$b(icon("circle-info"), " Single-metric EB diagnostic"),
                   tags$br(),
                   "The prior-vs-empirical plot shows both mean-shift ",
                   tags$b("γ"), " and variance ", tags$b("δ"),
                   " facets together. Parameter and batch selectors are hidden because they do not change the single-metric EB density plot."
                 )),
          column(3, tags$br(),
                 actionBttn("run_eb", "Run EB Check",
                            style = "gradient", color = "primary",
                            icon = icon("play"), size = "sm"))
        )
      )
    }
    
    fluidRow(
      column(3, selectInput("eb_parm", "Parameter:",
                            choices = c("Mean shift (γ)" = "gamma",
                                        "Variance / covariance (δ)" = "delta"),
                            selected = "gamma")),
      column(3, uiOutput("eb_batch_ui")),
      column(3, uiOutput("eb_mod_control_ui")),
      column(3, tags$br(),
             actionBttn("run_eb", "Run EB Check",
                        style = "gradient", color = "primary",
                        icon = icon("play"), size = "sm"))
    )
  })
  
  output$eb_batch_ui <- renderUI({
    req(eb_available(), input$eb_parm)
    
    if ((input$eb_parm %||% "gamma") == "gamma") {
      msg <- if (!is.null(eb_ref_batch())) {
        paste0("Mean-shift γ view shows all non-reference batches; reference ",
               eb_ref_batch(), " is hidden.")
      } else {
        "Mean-shift γ view shows all batches."
      }
      
      return(tags$div(
        style = "color:#666;margin-top:30px;font-size:14px;line-height:1.35;",
        msg
      ))
    }
    
    batches <- eb_display_batches()
    
    if (length(batches) == 0) {
      return(tags$div(
        style = "color:#B22222;font-weight:600;margin-top:8px;",
        "No non-reference batches available."
      ))
    }
    
    label_txt <- if (!is.null(eb_ref_batch())) {
      paste0("Batch for δ view (reference ", eb_ref_batch(), " hidden):")
    } else {
      "Batch for δ view:"
    }
    
    selectInput(
      "eb_batch",
      label_txt,
      choices  = batches,
      selected = batches[1]
    )
  })
  
  output$eb_mod_control_ui <- renderUI({
    req(eb_available(), rv$m)
    
    # For one metric, M1 is used internally and the selector is unnecessary.
    if (rv$m <= 1) {
      return(NULL)
    }
    
    # Only show the modality selector for univariate harmonization with >1 metrics.
    if (!identical(input$harm_mode, "uni")) {
      return(NULL)
    }
    
    uiOutput("eb_mod_ui")
  })
  
  output$eb_mod_ui <- renderUI({
    req(eb_available())
    m <- safe_m_value(rv$m); req(!is.null(m))
    
    if (m <= 1) {
      return(NULL)
    }
    
    selectInput(
      "eb_mod",
      "Modality:",
      choices  = setNames(seq_len(m), paste0("M", seq_len(m))),
      selected = 1
    )
  })
  
  observeEvent(input$run_eb, {
    req(eb_available())
    
    tryCatch({
      eb <- eb_original()
      
      rv$eb_obj <- list(
        chk  = eb_check(eb),
        eb   = eb,
        mode = rv$harm$mode,
        ref_excluded = eb_ref_batch()
      )
      
      msg <- if (!is.null(eb_ref_batch())) {
        paste0("EB check done. Reference batch ", eb_ref_batch(), " hidden from display.")
      } else {
        "EB check done."
      }
      
      showNotification(msg, type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("EB error:", e$message), type = "error")
    })
  })
  
  output$eb_plot <- renderPlot({
    req(eb_available())
    
    tryCatch({
      eb  <- eb_original()
      chk <- eb_check(eb)
      ref <- eb_ref_batch()
      
      # Single-metric/univariate eb_plot() from MultiComBat always facets both
      # gamma and delta from eb_df$eb_df. Do not pass parameter/batch controls here.
      if (!is.null(rv$m) && rv$m <= 1 && identical(rv$harm$mode, "uni")) {
        p <- eb_plot(chk)
        p <- remove_ref_from_plot_data(p, ref)
      } else {
        req(input$eb_parm)
        parm_use <- input$eb_parm %||% "gamma"
        
        if (parm_use == "delta") {
          batches <- eb_display_batches()
          validate(need(length(batches) > 0,
                        "No non-reference batches available for EB plot."))
          
          req(input$eb_batch)
          bat_use <- input$eb_batch
          
          if (is.null(bat_use) || !bat_use %in% batches) {
            bat_use <- batches[1]
          }
          
          p <- eb_plot(chk, param = "delta", bat = bat_use)
          p <- filter_eb_plot_to_selected_param(p, "delta")
          
        } else {
          p <- eb_plot(chk, param = "gamma")
          p <- remove_ref_from_plot_data(p, ref)
          p <- filter_eb_plot_to_selected_param(p, "gamma")
        }
      }
      
      subtitle_txt <- if (!is.null(ref)) {
        paste0("Reference batch ", ref, " hidden from EB diagnostic display.")
      } else {
        NULL
      }
      
      print(
        p +
          labs(subtitle = subtitle_txt) +
          bold_theme(13) +
          theme(plot.background = element_rect(fill = "white", color = NA))
      )
      
    }, error = function(e) {
      ggplot() +
        labs(title = paste("EB unavailable:", e$message)) +
        bold_theme(13)
    })
  })
  
  output$eb_hyper <- renderUI({
    req(eb_available())
    
    eb <- eb_original()
    mom <- eb$mom
    if (is.null(mom)) return(tags$pre("Hyperparameters not available."))
    
    num_style <- "color:#2E6BE6;font-weight:600;"
    ref_note <- if (!is.null(eb_ref_batch())) {
      paste0(
        "<br/><span style='color:#777;font-size:12px;'>",
        "Reference batch ", eb_ref_batch(),
        " is hidden from plots and shrinkage table; hyperparameters are shown from the fitted EB object.",
        "</span>"
      )
    } else {
      ""
    }
    
    if (rv$harm$mode == "uni") {
      tags$div(
        style = "font-size:14px;line-height:1.6;",
        HTML(paste0(
          "<b>g_bar</b>: <span style='", num_style, "'>", round(mean(sapply(mom, `[[`, "g_bar")), 4), "</span><br/>",
          "<b>g_var</b>: <span style='", num_style, "'>", round(mean(sapply(mom, `[[`, "g_var")), 4), "</span><br/>",
          "<b>d_a</b>:   <span style='", num_style, "'>", round(mean(sapply(mom, `[[`, "d_a")), 4), "</span><br/>",
          "<b>d_b</b>:   <span style='", num_style, "'>", round(mean(sapply(mom, `[[`, "d_b")), 4), "</span>",
          ref_note
        ))
      )
    } else {
      tags$div(
        style = "font-size:14px;line-height:1.6;",
        HTML(paste0(
          "<b>g_bar mean</b>:  <span style='", num_style, "'>", round(mean(sapply(mom, function(x) mean(x$g_bar))), 4), "</span><br/>",
          "<b>g_var trace</b>: <span style='", num_style, "'>", round(mean(sapply(mom, function(x) sum(diag(as.matrix(x$g_var))))), 4), "</span><br/>",
          "<b>d_a (IW df)</b>: <span style='", num_style, "'>", round(mean(sapply(mom, `[[`, "d_a")), 4), "</span><br/>",
          "<b>d_b trace</b>:   <span style='", num_style, "'>", round(mean(sapply(mom, function(x) sum(diag(as.matrix(x$d_b))))), 4), "</span>",
          ref_note
        ))
      )
    }
  })
  
  make_eb_shrink_dt <- function(parm = c("gamma", "delta")) {
    parm <- match.arg(parm)
    
    tryCatch({
      eb <- eb_original()
      ref <- eb_ref_batch()
      batch_levels <- rv$batch_levels
      
      if (parm == "gamma") {
        gh <- drop_ref_by_label(eb$gamma_hat,  ref, batch_levels)
        gs <- drop_ref_by_label(eb$gamma_star, ref, batch_levels)
        
        if (rv$harm$mode == "uni") {
          validate(need(!is.null(gh) && nrow(gh) > 0,
                        "No non-reference batches available for γ shrinkage table."))
          
          df <- data.frame(
            Batch     = rownames(gh),
            hat_mean  = rowMeans(abs(gh)),
            star_mean = rowMeans(abs(gs)),
            check.names = FALSE
          )
        } else {
          batches <- names(gh)
          validate(need(length(batches) > 0,
                        "No non-reference batches available for γ shrinkage table."))
          
          df <- do.call(rbind, lapply(batches, function(b) {
            data.frame(
              Batch     = b,
              hat_mean  = mean(abs(gh[[b]])),
              star_mean = mean(abs(gs[[b]])),
              check.names = FALSE
            )
          }))
        }
        
      } else {
        dh <- drop_ref_by_label(eb$delta_hat,  ref, batch_levels)
        ds <- drop_ref_by_label(eb$delta_star, ref, batch_levels)
        
        if (rv$harm$mode == "uni") {
          validate(need(!is.null(dh) && nrow(dh) > 0,
                        "No non-reference batches available for δ shrinkage table."))
          
          df <- data.frame(
            Batch     = rownames(dh),
            hat_mean  = rowMeans(abs(dh)),
            star_mean = rowMeans(abs(ds)),
            check.names = FALSE
          )
        } else {
          batches <- names(dh)
          validate(need(length(batches) > 0,
                        "No non-reference batches available for δ shrinkage table."))
          
          df <- do.call(rbind, lapply(batches, function(b) {
            data.frame(
              Batch = b,
              hat_mean = mean(sapply(dh[[b]], function(m) sqrt(sum(m^2)))),
              star_mean = mean(sapply(ds[[b]], function(m) sqrt(sum(m^2)))),
              check.names = FALSE
            )
          }))
        }
      }
      
      # Final safety filter by label only.
      if (!is.null(ref) && "Batch" %in% names(df)) {
        df <- df[df$Batch != ref, , drop = FALSE]
      }
      
      validate(need(nrow(df) > 0, "No non-reference batches available."))
      
      df$shrinkage <- 1 - df$star_mean / (df$hat_mean + 1e-9)
      df$strength <- ifelse(
        df$shrinkage > 0.50, "Strong",
        ifelse(df$shrinkage > 0.20, "Moderate", "Light")
      )
      
      df$hat_mean   <- round(df$hat_mean, 4)
      df$star_mean  <- round(df$star_mean, 4)
      df$shrink_pct <- paste0(round(100 * df$shrinkage, 1), "%")
      
      df_show <- df[, c("Batch", "hat_mean", "star_mean", "shrink_pct", "strength")]
      
      cap_txt <- if (!is.null(ref)) {
        paste0("Reference batch ", ref, " hidden from display.")
      } else {
        NULL
      }
      
      datatable(
        df_show,
        rownames = FALSE,
        class = "compact stripe hover",
        caption = if (!is.null(cap_txt)) {
          htmltools::tags$caption(
            style = "caption-side: top; text-align: left; color:#777;",
            cap_txt
          )
        } else {
          NULL
        },
        options = list(
          dom = "t",
          pageLength = nrow(df_show),
          autoWidth = TRUE
        )
      ) %>%
        formatStyle(
          "shrink_pct",
          color = styleInterval(c(20, 50), c("#B22222", "#C98600", "#2E8B57")),
          fontWeight = "bold"
        ) %>%
        formatStyle("strength", fontWeight = "bold")
      
    }, error = function(e) {
      datatable(
        data.frame(Note = paste("EB shrinkage table unavailable:", e$message)),
        rownames = FALSE,
        options = list(dom = "t")
      )
    })
  }
  
  output$eb_summary_tables_ui <- renderUI({
    req(eb_available())
    
    if (!is.null(rv$m) && rv$m <= 1 && identical(rv$harm$mode, "uni")) {
      tagList(
        tags$div(class = "sec-hdr", "Mean-shift γ shrinkage by batch"),
        DTOutput("eb_shrink_gamma"),
        tags$hr(style = "margin:10px 0;"),
        tags$div(class = "sec-hdr", "Variance δ shrinkage by batch"),
        DTOutput("eb_shrink_delta")
      )
    } else {
      tagList(
        tags$div(class = "sec-hdr", "Shrinkage by batch"),
        DTOutput("eb_shrink")
      )
    }
  })
  
  output$eb_shrink_gamma <- renderDT({
    req(eb_available())
    make_eb_shrink_dt("gamma")
  })
  
  output$eb_shrink_delta <- renderDT({
    req(eb_available())
    make_eb_shrink_dt("delta")
  })
  
  output$eb_shrink <- renderDT({
    req(eb_available())
    parm <- input$eb_parm %||% "gamma"
    make_eb_shrink_dt(parm)
  })
  

  # ══════════════════════════════════════════════════════════════════════════
  # 6. COVARIANCE DIAGNOSTICS  (unchanged)
  # ══════════════════════════════════════════════════════════════════════════
  output$cov_diag_panel <- renderUI({
    if (!has_multi_metric()) {
      return(
        fluidRow(
          box(
            title = "Cross-Modality Covariance Diagnostics",
            width = 12,
            solidHeader = TRUE,
            single_metric_notice("cross-modality covariance diagnostics")
          )
        )
      )
    }
    
    tagList(
      fluidRow(
        box(
          title = "Cross-Modality Covariance Diagnostics",
          width = 12,
          solidHeader = TRUE,
          tags$p("Visualizes average cross-modality covariance/correlation after removing fixed + batch effects."),
          fluidRow(
            column(3, selectInput("cov_type", "Metric:",
                                  choices = c("Correlation" = "TRUE", "Covariance" = "FALSE"),
                                  selected = "TRUE")),
            column(3, selectInput("cov_view", "View:",
                                  choices = c("Pooled only" = "pooled",
                                              "Pooled + per-batch" = "batch"),
                                  selected = "batch")),
            column(3, numericInput("cov_feat", "Feature index (NA = average all):", NA, min = 1)),
            column(3, tags$br(),
                   actionBttn("run_cov", "Plot",
                              style = "gradient", color = "primary",
                              icon = icon("play"), size = "sm"))
          )
        )
      ),
      fluidRow(
        box(title = "Heatmap", width = 8, solidHeader = TRUE,
            plotOutput("cov_heat", height = "440px")),
        box(title = "Recovery Metrics vs Pooled Reference", width = 4, solidHeader = TRUE,
            DTOutput("cov_metrics"),
            tags$hr(),
            tags$ul(style = "font-size:12px;padding-left:18px;",
                    tags$li(tags$b("Frobenius:"), " Total entry-wise deviation"),
                    tags$li(tags$b("MSE:"),       " Mean squared entry error"),
                    tags$li(tags$b("Spectral:"),  " Largest eigenvalue error"),
                    tags$li(tags$b("EigenError:")," Mean abs eigenvalue error"),
                    tags$li(tags$b("KL:"),        " KL divergence (zero-mean Gaussians)")))
      ),
      fluidRow(
        box(title = "Recovery Metrics Visualisation", width = 12, solidHeader = TRUE,
            fluidRow(
              column(3, selectInput("cov_metric_sel", "Metric to highlight:",
                                    choices = c("Frobenius","MSE","Spectral","EigenError","KL"),
                                    selected = "Frobenius"))
            ),
            fluidRow(
              column(6,
                     tags$div(class = "sec-hdr", "Bar chart — selected metric by batch"),
                     plotOutput("cov_metric_bar", height = "260px")),
              column(6,
                     tags$div(class = "sec-hdr", "All metrics — parallel-coordinates plot by batch"),
                     plotOutput("cov_metric_parallel", height = "260px"))
            )
        )
      )
    )
  })
  
  observeEvent(input$run_cov, {
    req(rv$data_list, rv$bat_list, rv$config_ok)
    if (!has_multi_metric()) {
      showNotification(
        "Covariance diagnostics are disabled because only one imaging metric/modality is available.",
        type = "warning"
      )
      return(NULL)
    }
    tryCatch({
      use_cor <- as.logical(input$cov_type)
      feat_i  <- if (is.na(input$cov_feat)) NULL else as.integer(input$cov_feat)
      p <- if (input$cov_view=="pooled")
        diag_plot_pooled_cov(rv$data_list,rv$bat_list,rv$covar_list,
                             model=rv$model_fn,formula=rv$formula_obj,
                             ref.batch=rv$ref_batch,use_correlation=use_cor)
      else
        diag_plot_batch_cov(rv$data_list,rv$bat_list,rv$covar_list,
                            model=rv$model_fn,formula=rv$formula_obj,
                            ref.batch=rv$ref_batch,use_correlation=use_cor,feature_idx=feat_i)
      rv$cov_plot <- p
      showNotification("Covariance plot ready.", type="message", duration=2)
    }, error=function(e) showNotification(paste("Error:", e$message), type="error"))
  })
  
  output$cov_heat <- renderPlot({
    req(rv$cov_plot)
    print(rv$cov_plot + bold_theme(12) + theme(plot.background=element_rect(fill="white",color=NA)))
  }, bg="white")
  
  cov_metrics_df <- reactive({
    if (!has_multi_metric()) return(NULL)
    req(rv$pre_diag, rv$bat_list)
    tryCatch({
      resid_list <- lapply(rv$pre_diag, function(d) as.matrix(d$resid_mul))
      G          <- ncol(resid_list[[1]])
      S_pool <- Reduce(`+`, lapply(seq_len(G), function(g) {
        mat <- do.call(cbind, lapply(resid_list, function(D) D[,g]))
        cov(mat)
      })) / G
      do.call(rbind, lapply(rv$batch_levels, function(b) {
        idx   <- which(rv$bat_list[[1]] == b)
        S_bat <- Reduce(`+`, lapply(seq_len(G), function(g) {
          mat <- do.call(cbind, lapply(resid_list, function(D) D[idx,g,drop=FALSE]))
          cov(mat)
        })) / G
        r <- evaluate_cov_recovery(S_pool, S_bat)
        data.frame(Batch=b,Frobenius=r$Frobenius,MSE=r$MSE,
                   Spectral=r$Spectral,EigenError=r$EigenError,KL=r$KL,stringsAsFactors=FALSE)
      }))
    }, error=function(e) NULL)
  })
  
  output$cov_metrics <- renderDT({
    df <- cov_metrics_df()
    if (is.null(df)) return(datatable(data.frame(Note="Run pre-diagnostics first."),rownames=FALSE))
    df_rnd <- df; df_rnd[,-1] <- lapply(df_rnd[,-1],function(x) signif(x,4))
    datatable(df_rnd,rownames=FALSE,options=list(dom="t"),class="compact stripe")
  })
  
  output$cov_metric_bar <- renderPlot({
    df <- cov_metrics_df(); req(!is.null(df), input$cov_metric_sel)
    metric <- input$cov_metric_sel
    df$Batch <- factor(clean_batch_for_plot(df$Batch), levels=rv$batch_levels)
    fill_cols <- safe_discrete_palette(levels(droplevels(df$Batch)))
    ggplot(df, aes(x=Batch,y=.data[[metric]],fill=Batch)) +
      geom_col(width=.55,color="white",alpha=.88) +
      geom_text(aes(label=signif(.data[[metric]],3)),vjust=-0.4,size=3.4,fontface="bold") +
      scale_fill_manual(values=fill_cols) +
      scale_y_continuous(expand=expansion(mult=c(0,.18))) +
      labs(x="Batch",y=metric,title=paste0(metric," deviation from pooled covariance")) +
      bold_theme(12) + theme(legend.position="none",panel.grid.major.x=element_blank())
  }, bg="white")
  
  output$cov_metric_parallel <- renderPlot({
    df <- cov_metrics_df(); req(!is.null(df))
    metric_cols <- c("Frobenius","MSE","Spectral","EigenError","KL")
    metric_sel  <- input$cov_metric_sel %||% "Frobenius"
    norm_df <- df
    for (mc in metric_cols) {
      rng <- range(norm_df[[mc]],na.rm=TRUE)
      if (diff(rng)<1e-12) norm_df[[mc]] <- 0.5
      else norm_df[[mc]] <- (norm_df[[mc]]-rng[1])/diff(rng)
    }
    long_df <- norm_df %>%
      pivot_longer(all_of(metric_cols),names_to="Metric",values_to="NormValue") %>%
      mutate(Metric=factor(Metric,levels=metric_cols), Highlight=Metric==metric_sel)
    long_df$Batch <- clean_batch_for_plot(long_df$Batch)
    color_cols <- safe_discrete_palette(levels(droplevels(long_df$Batch)))
    ggplot(long_df, aes(x=Metric,y=NormValue,group=Batch,color=Batch)) +
      geom_line(linewidth=.9,alpha=.75) +
      geom_point(aes(size=Highlight,alpha=Highlight),shape=16) +
      annotate("rect",
               xmin=as.numeric(factor(metric_sel,levels=metric_cols))-.35,
               xmax=as.numeric(factor(metric_sel,levels=metric_cols))+.35,
               ymin=-Inf,ymax=Inf,fill="#f0e6fb",alpha=.45) +
      scale_color_manual(values=color_cols,name="Batch") +
      scale_size_manual(values=c("FALSE"=2.2,"TRUE"=4.5),guide="none") +
      scale_alpha_manual(values=c("FALSE"=.65,"TRUE"=1),guide="none") +
      scale_y_continuous(limits=c(0,1),labels=scales::label_percent(),
                         name="Normalised deviation (0=min, 1=max)") +
      labs(x="Metric",title="All recovery metrics — normalised parallel coordinates",
           subtitle=paste0("Highlighted: ",metric_sel,"  |  Each batch = one line")) +
      bold_theme(11) +
      theme(panel.grid.major.x=element_line(color="#ddd",linewidth=.4),
            panel.grid.major.y=element_line(color="#eee",linewidth=.3))
  }, bg="white")
  
  # ══════════════════════════════════════════════════════════════════════════
  # 7. BAYESIAN MCMC DIAGNOSTICS  — NEW SERVER CODE
  # ══════════════════════════════════════════════════════════════════════════
  
  # ── Dynamic upload UI for multiple univariate RDS files ──────────────────
  output$mcmc_upload_mode_ui <- renderUI({
    current_m <- rv$m %||% 1L
    current_m <- as.integer(current_m)
    if (is.na(current_m) || current_m < 1) current_m <- 1L
    
    if (current_m == 1L) {
      tagList(
        radioGroupButtons(
          "mcmc_upload_mode", "Stan fit type:",
          choices = c("Univariate: one metric RDS" = "uni"),
          selected = "uni",
          justified = TRUE
        ),
        numericInput(
          "mcmc_upload_m", "Number of metrics/modalities:",
          value = 1, min = 1, max = 1
        ),
        tags$script(HTML("$('#mcmc_upload_m').prop('disabled', true);")),
        uiOutput("mcmc_upload_ui_uni"),
        tags$div(
          style = paste0(
            "background:#fff3cd;border-left:4px solid #f0ad4e;",
            "padding:10px 12px;border-radius:6px;margin-top:8px;",
            "font-size:13px;color:#6b4f00;"
          ),
          tags$b(icon("circle-info"), " Single-metric dataset detected"),
          tags$br(),
          "Only univariate MCMC upload is allowed. Multivariate MCMC diagnostics are disabled because there is only one imaging metric."
        )
      )
    } else {
      tagList(
        radioGroupButtons(
          "mcmc_upload_mode", "Stan fit type:",
          choices = c("Univariate (RDS per modality)" = "uni",
                      "Multivariate (single RDS)" = "multi"),
          selected = input$mcmc_upload_mode %||% "uni",
          justified = TRUE
        ),
        conditionalPanel(
          "input.mcmc_upload_mode == 'uni'",
          numericInput(
            "mcmc_upload_m", "Number of modalities:",
            value = current_m, min = 1, max = 10
          ),
          uiOutput("mcmc_upload_ui_uni")
        ),
        conditionalPanel(
          "input.mcmc_upload_mode == 'multi'",
          fileInput(
            "mcmc_rds_multi", "Upload multivariate RDS:",
            accept = ".rds", multiple = FALSE
          )
        )
      )
    }
  })
  
  # ── Dynamic upload UI for univariate RDS files ───────────────────────────
  output$mcmc_upload_ui_uni <- renderUI({
    active_m <- rv$m %||% input$mcmc_upload_m %||% 1L
    active_m <- as.integer(active_m)
    if (is.na(active_m) || active_m < 1) active_m <- 1L
    
    m <- as.integer(input$mcmc_upload_m %||% active_m)
    if (is.na(m) || m < 1) m <- active_m
    
    # If the active dataset has only one metric, force exactly one upload box.
    if (!is.null(rv$m) && rv$m == 1L) m <- 1L
    
    tagList(
      lapply(seq_len(m), function(i) {
        fileInput(
          paste0("mcmc_rds_uni_", i),
          paste0("Metric/Modality ", i, " univariate RDS:"),
          accept = ".rds", multiple = FALSE
        )
      })
    )
  })
  
  warn_if_full_local_upload <- function(obj) {
    if (is.list(obj) && identical(obj$export_type, "full_local")) {
      showNotification(
        paste(
          "You uploaded a FULL_LOCAL object. It may work, but it can be very large.",
          "For Shiny upload, please use the corresponding *_LIGHT_UPLOAD.rds file."
        ),
        type = "warning",
        duration = 8
      )
    }
    invisible(NULL)
  }
  
  # ── Load MCMC results ─────────────────────────────────────────────────────
  observeEvent(input$load_mcmc, {
    withProgress(message = "Loading MCMC results…", {
      tryCatch({
        active_m <- rv$m %||% 1L
        active_m <- as.integer(active_m)
        if (is.na(active_m) || active_m < 1) active_m <- 1L
        
        upload_mode_use <- input$mcmc_upload_mode %||% "uni"
        
        # Critical guard: if the active dataset has only one metric,
        # never allow multivariate upload mode even if a stale UI value remains.
        if (active_m == 1L) upload_mode_use <- "uni"
        
        if (upload_mode_use == "multi") {
          req(input$mcmc_rds_multi)
          setProgress(0.4, "Reading multivariate RDS…")
          obj <- readRDS(input$mcmc_rds_multi$datapath)
          warn_if_full_local_upload(obj)
          rv$mcmc_fit_list   <- list(obj)
          rv$mcmc_draws_list <- list(extract_posterior_draws(obj))
          rv$mcmc_mode       <- "multi"
          rv$mcmc_m          <- 1L
          
          # Prefer summaries saved directly inside the RDS; if missing, reconstruct from posterior_draws.
          smry <- extract_mcmc_summary(obj)
          smry <- repair_mcmc_summary_from_draws(obj, smry)
          rv$mcmc_summary <- smry
          
          harm_data <- extract_harm_data_from_mcmc_export(obj)
          if (!is.null(harm_data)) {
            rv$mcmc_harm_data <- validate_mcmc_harm_data(harm_data)
            use_mcmc_harm_data_for_post(rv$mcmc_harm_data)
          } else {
            rv$mcmc_harm_data <- NULL
          }
        } else {
          m <- if (active_m == 1L) 1L else as.integer(input$mcmc_upload_m %||% active_m)
          if (is.na(m) || m < 1) m <- 1L
          
          fit_list <- lapply(seq_len(m), function(i) {
            fi <- input[[paste0("mcmc_rds_uni_", i)]]
            if (is.null(fi)) stop(paste0("Metric/Modality ", i, " RDS not uploaded."))
            setProgress(0.2 + 0.6 * (i - 1) / m, paste0("Reading metric/modality ", i, "…"))
            obj_i <- readRDS(fi$datapath)
            warn_if_full_local_upload(obj_i)
            
            obj_mode <- tolower(as.character(obj_i$mode %||% ""))
            if (active_m == 1L && grepl("multi", obj_mode)) {
              stop(
                "The uploaded RDS appears to be a multivariate MCMC object, ",
                "but the active dataset has only one imaging metric. ",
                "Please upload the univariate *_LIGHT_UPLOAD.rds file for Metric/Modality 1."
              )
            }
            
            obj_i
          })
          
          rv$mcmc_fit_list   <- fit_list
          rv$mcmc_draws_list <- lapply(fit_list, extract_posterior_draws)
          rv$mcmc_mode       <- "uni"
          rv$mcmc_m          <- m
          
          # Build combined summary across modalities. If missing, reconstruct from posterior_draws.
          all_smry <- lapply(seq_len(m), function(i) {
            smry_i <- extract_mcmc_summary(fit_list[[i]])
            smry_i <- repair_mcmc_summary_from_draws(fit_list[[i]], smry_i)
            if (!is.null(smry_i)) smry_i$modality <- paste0("M", i)
            smry_i
          })
          rv$mcmc_summary <- do.call(rbind, Filter(Negate(is.null), all_smry))
          
          # For univariate uploads, each RDS should contain one modality-specific harm_data.
          harm_list <- lapply(seq_len(m), function(i) {
            h_i <- extract_harm_data_from_mcmc_export(fit_list[[i]])
            if (is.null(h_i) || length(h_i) < 1) {
              stop(paste0(
                "No harm_data found in Metric/Modality ", i,
                " RDS. Re-run the downloaded MCMC project script so it saves `harm_data` in each RDS."
              ))
            }
            
            # Robust handling:
            # univariate export may save harm_data as a data.frame/matrix,
            # while multivariate export saves harm_data as a list.
            if (is.data.frame(h_i) || is.matrix(h_i)) {
              as.data.frame(h_i, check.names = FALSE)
            } else if (is.list(h_i) && length(h_i) == 1) {
              as.data.frame(h_i[[1]], check.names = FALSE)
            } else {
              stop(paste0(
                "Metric/Modality ", i,
                " RDS does not look like a single-metric univariate harmonized dataset."
              ))
            }
          })
          
          rv$mcmc_harm_data <- validate_mcmc_harm_data(harm_list)
          use_mcmc_harm_data_for_post(rv$mcmc_harm_data)
        }
        
        setProgress(1)
        showNotification(
          "MCMC results loaded. The uploaded MCMC harmonized data are now active for Post-Harmonization diagnostics; go to Post-Harmonization and click Run Post Diagnostics.",
          type = "message", duration = 6
        )
      }, error = function(e) {
        showNotification(paste("Load error:", e$message), type = "error")
      })
    })
  })
  
  # ── Metric-specific view for univariate MCMC output ─────────────────────
  output$mcmc_metric_filter_ui <- renderUI({
    if (is.null(rv$mcmc_summary) || !identical(rv$mcmc_mode, "uni") || is.null(rv$mcmc_m) || rv$mcmc_m <= 1) {
      return(NULL)
    }
    choices <- c("All metrics" = "All", setNames(paste0("M", seq_len(rv$mcmc_m)), paste0("Metric/Modality ", seq_len(rv$mcmc_m))))
    tags$div(
      style = "margin-top:10px;background:#f7f9fc;border:1px solid #d9e2ef;border-radius:8px;padding:10px 12px;",
      selectInput(
        "mcmc_metric_focus",
        "Metric/modality to inspect:",
        choices = choices,
        selected = "All"
      ),
      tags$div(
        style = "font-size:12px;color:#666;margin-top:-6px;",
        "For univariate MCMC, diagnostics can be viewed across all uploaded metrics or one metric at a time."
      )
    )
  })
  
  mcmc_active_summary <- reactive({
    smry <- standardize_mcmc_summary(rv$mcmc_summary)
    if (is.null(smry)) return(NULL)
    if (identical(rv$mcmc_mode, "uni") && "modality" %in% colnames(smry)) {
      metric_focus <- input$mcmc_metric_focus %||% "All"
      if (!identical(metric_focus, "All")) {
        smry <- smry[smry$modality == metric_focus, , drop = FALSE]
      }
    }
    smry
  })
  
  # ── Load status badge ─────────────────────────────────────────────────────
  output$mcmc_load_status_ui <- renderUI({
    if (is.null(rv$mcmc_summary) && is.null(rv$mcmc_fit_list))
      return(tags$span(style = "color:#e67e22;font-weight:600;", "No results loaded yet."))
    n_params <- if (!is.null(rv$mcmc_summary)) nrow(rv$mcmc_summary) else "?"
    tags$div(
      style = "background:#e8f5e9;border:1px solid #66bb6a;border-radius:8px;padding:10px 12px;",
      tags$div(style = "font-weight:700;color:#1b5e20;margin-bottom:4px;",
               icon("circle-check"), " Loaded"),
      tags$div(style = "font-size:13px;color:#2e7d32;",
               paste0("Mode: ", rv$mcmc_mode %||% "?"), tags$br(),
               paste0("Uploaded RDS files: ", rv$mcmc_m %||% "?"), tags$br(),
               paste0("Parameters in summary: ", n_params),
               tags$span(" | Full fit object: ✓")
      )
    )
  })
  
  # ── Overview cards ────────────────────────────────────────────────────────
  output$mcmc_overview_ui <- renderUI({
    smry <- mcmc_active_summary()
    if (is.null(smry)) return(tags$p(style = "color:#888;", "Load results to see overview."))
    n_params <- nrow(smry)
    has_rhat  <- "rhat" %in% colnames(smry)
    has_ess   <- "ess_bulk" %in% colnames(smry)
    n_bad_rhat <- if (has_rhat) sum(!is.na(smry$rhat) & smry$rhat >= 1.05, na.rm = TRUE) else NA
    n_low_ess  <- if (has_ess)  sum(!is.na(smry$ess_bulk) & smry$ess_bulk < 400, na.rm = TRUE) else NA
    metric_label <- if (identical(rv$mcmc_mode, "uni")) input$mcmc_metric_focus %||% "All" else "Multivariate"
    tags$div(
      style = "display:grid;grid-template-columns:repeat(4,1fr);gap:10px;",
      tags$div(class = "stat-card", tags$div(class = "val", n_params),
               tags$div(class = "lbl", "Parameters"),
               tags$div(class = "sub", paste0("View: ", metric_label))),
      tags$div(class = "stat-card", tags$div(class = "val", rv$mcmc_m %||% "?"),
               tags$div(class = "lbl", "Uploaded RDS files")),
      tags$div(class = "stat-card",
               tags$div(class = "val", style = if (!is.na(n_bad_rhat) && n_bad_rhat > 0) "color:#c0392b" else "color:#27ae60;",
                        if (is.na(n_bad_rhat)) "N/A" else n_bad_rhat),
               tags$div(class = "lbl", "R-hat ≥ 1.05")),
      tags$div(class = "stat-card",
               tags$div(class = "val", style = if (!is.na(n_low_ess) && n_low_ess > 0) "color:#e67e22" else "color:#27ae60;",
                        if (is.na(n_low_ess)) "N/A" else n_low_ess),
               tags$div(class = "lbl", "ESS_bulk < 400"))
    )
  })
  
  # ── Convergence traffic-light cards ──────────────────────────────────────
  output$mcmc_conv_cards_ui <- renderUI({
    smry <- mcmc_active_summary()
    if (is.null(smry) || !"rhat" %in% colnames(smry))
      return(tags$p(style="color:#888;","Load MCMC results with R-hat column to see convergence summary."))
    rhat_vals <- smry$rhat[!is.na(smry$rhat)]
    n_total   <- length(rhat_vals)
    n_ok      <- sum(rhat_vals < 1.01)
    n_warn    <- sum(rhat_vals >= 1.01 & rhat_vals < 1.05)
    n_bad     <- sum(rhat_vals >= 1.05)
    make_card <- function(count, total, label, col_bg, col_txt, icon_nm) {
      pct <- if (total > 0) round(100*count/total, 2) else 0
      column(4, tags$div(
        style=paste0("background:",col_bg,";border-radius:10px;padding:14px 16px;text-align:center;"),
        icon(icon_nm, style=paste0("font-size:24px;color:",col_txt,";")),
        tags$div(style=paste0("font-size:2rem;font-weight:700;color:",col_txt,";"), count),
        tags$div(style=paste0("font-size:1.55rem;color:",col_txt,";font-weight:600;"), label),
        tags$div(style=paste0("font-size:1.3rem;color:",col_txt,";opacity:.8;"),
                 paste0(pct,"% of ",total," params"))
      ))
    }
    fluidRow(
      make_card(n_ok,   n_total, "R-hat < 1.01 (Excellent)", "#e8f5e9","#1b5e20","circle-check"),
      make_card(n_warn, n_total, "R-hat 1.01–1.05 (Warn)",   "#fff3e0","#e65100","triangle-exclamation"),
      make_card(n_bad,  n_total, "R-hat ≥ 1.05 (Poor)",      "#fce4ec","#b71c1c","circle-xmark")
    )
  })
  
  # ── Dynamically update parameter-family filter choices ────────────────────
  observe({
    smry <- mcmc_active_summary(); req(!is.null(smry), "variable" %in% colnames(smry))
    families <- unique(sub("\\[.*","", smry$variable))
    families <- sort(unique(c("All", families)))
    updateSelectInput(session, "rhat_param_filter", choices = families, selected = "All")
  })
  
  # ── R-hat plot ─────────────────────────────────────────────────────────────
  output$rhat_plot <- plotly::renderPlotly({
    smry <- mcmc_active_summary(); req(!is.null(smry))
    if (!"rhat" %in% colnames(smry)) {
      return(plotly::plot_ly() %>%
               plotly::layout(title=list(text="R-hat column not found in summary."),
                              xaxis=list(visible=FALSE), yaxis=list(visible=FALSE)))
    }
    df <- smry[!is.na(smry$rhat), ]
    if (input$rhat_param_filter != "All") {
      df <- df[grepl(paste0("^",input$rhat_param_filter,"(\\[|$)"), df$variable), ]
    }
    df <- df[order(-df$rhat), ]
    df <- head(df, input$rhat_top_n %||% 200)
    df$status <- ifelse(df$rhat >= 1.05, "Poor (≥1.05)",
                        ifelse(df$rhat >= 1.01, "Warn (1.01–1.05)", "OK (<1.01)"))
    if (input$rhat_filter == "warn") df <- df[df$rhat >= 1.01, ]
    if (input$rhat_filter == "poor") df <- df[df$rhat >= 1.05, ]
    df$rank <- seq_len(nrow(df))
    plotly::plot_ly(df, x=~rank, y=~rhat, type="scatter", mode="markers",
                    color=~status, colors=c("OK (<1.01)"="#27ae60",
                                            "Warn (1.01–1.05)"="#e67e22",
                                            "Poor (≥1.05)"="#c0392b"),
                    text=~paste0("Variable: ",variable,"<br>R-hat: ",round(rhat,4)),
                    hoverinfo="text", marker=list(size=7,opacity=0.9)) %>%
      plotly::add_segments(x=min(df$rank),xend=max(df$rank),y=1.05,yend=1.05,
                           inherit=FALSE,line=list(color="#c0392b",dash="dash",width=1.2),
                           showlegend=FALSE,hoverinfo="none") %>%
      plotly::add_segments(x=min(df$rank),xend=max(df$rank),y=1.01,yend=1.01,
                           inherit=FALSE,line=list(color="#e67e22",dash="dot",width=1),
                           showlegend=FALSE,hoverinfo="none") %>%
      plotly::layout(
        title=list(text="R-hat by parameter (sorted descending)"),
        xaxis=list(title="Parameter rank",showticklabels=FALSE,zeroline=FALSE),
        yaxis=list(title="R-hat",zeroline=FALSE),
        legend=list(orientation="h",x=0,y=-0.15))
  })
  
  output$rhat_tbl <- renderDT({
    smry <- mcmc_active_summary(); req(!is.null(smry))
    cols_show <- intersect(c("variable","modality","mean","median","sd","q5","q95",
                             "rhat","ess_bulk","ess_tail"), colnames(smry))
    df <- smry[, cols_show, drop=FALSE]
    if ("rhat" %in% cols_show && input$rhat_filter != "all") {
      threshold <- if (input$rhat_filter == "poor") 1.05 else 1.01
      df <- df[!is.na(df$rhat) & df$rhat >= threshold, ]
    }
    if (input$rhat_param_filter != "All")
      df <- df[grepl(paste0("^",input$rhat_param_filter,"(\\[|$)"), df$variable), ]
    df <- df[order(if("rhat"%in%colnames(df)) -df$rhat else seq_len(nrow(df))), ]
    df <- head(df, input$rhat_top_n %||% 200)
    for (nc in intersect(c("mean","median","sd","q5","q95","rhat"), colnames(df)))
      df[[nc]] <- round(df[[nc]], 4)
    for (nc in intersect(c("ess_bulk","ess_tail"), colnames(df)))
      df[[nc]] <- round(df[[nc]])
    dt <- datatable(df, rownames=FALSE, class="compact stripe hover",
                    options=list(pageLength=15, scrollX=TRUE, dom="tip"))
    if ("rhat" %in% colnames(df)) {
      dt <- dt %>%
        formatStyle("rhat",
                    color=styleInterval(c(1.01,1.05),c("#27ae60","#e67e22","#c0392b")),
                    fontWeight="bold")
    }
    dt
  })
  
  
  # ── Trace plot helper: skip deterministic / constrained entries ─────────────
  filter_informative_draw_columns <- function(draws_df, param_cols, min_sd = 1e-8) {
    if (is.null(draws_df) || length(param_cols) == 0) return(character(0))
    
    param_cols <- intersect(param_cols, names(draws_df))
    if (length(param_cols) == 0) return(character(0))
    
    sds <- sapply(param_cols, function(v) {
      x <- suppressWarnings(as.numeric(draws_df[[v]]))
      stats::sd(x, na.rm = TRUE)
    })
    
    param_cols[is.finite(sds) & sds > min_sd]
  }
  
  keep_free_cholesky_entries <- function(param_cols) {
    if (length(param_cols) == 0) return(character(0))
    
    keep <- sapply(param_cols, function(v) {
      idx <- extract_stan_indices(v)
      
      # For L_R_ig[i, g, row, col], row and col are the last two indices.
      # Free Cholesky-correlation entries satisfy row > col.
      if (length(idx) < 2) return(TRUE)
      
      row_idx <- idx[length(idx) - 1]
      col_idx <- idx[length(idx)]
      
      is.finite(row_idx) && is.finite(col_idx) && row_idx > col_idx
    })
    
    param_cols[keep]
  }
  
  get_matrix_entry_type <- function(vars) {
    # Classify Stan matrix-like variables by the last two indices.
    # For L_R_ig[i,g,row,col] and L_Sigma_ig[i,g,row,col], the last two
    # indices are the Cholesky row and column positions.
    sapply(vars, function(v) {
      idx <- extract_stan_indices(v)
      if (length(idx) < 2) return("unknown")
      row_idx <- idx[length(idx) - 1]
      col_idx <- idx[length(idx)]
      if (!is.finite(row_idx) || !is.finite(col_idx)) return("unknown")
      if (row_idx > col_idx) return("off_diagonal")
      if (row_idx == col_idx) return("diagonal")
      "upper_fixed"
    }, USE.NAMES = FALSE)
  }
  
  filter_posterior_summary_entries <- function(df, family, entry_filter = "auto", min_sd = 1e-8) {
    # Avoid posterior evidence plots being dominated by deterministic entries
    # such as fixed Cholesky zeros/ones. This is especially important for
    # L_R_ig, where L_R_ig[...,1,1] is structurally 1 and upper-triangular
    # entries are fixed to 0.
    if (is.null(df) || nrow(df) == 0 || !"variable" %in% colnames(df)) return(df)
    
    out <- df
    
    if ("sd" %in% colnames(out)) {
      out <- out[is.na(out$sd) | !is.finite(out$sd) | out$sd > min_sd, , drop = FALSE]
    }
    
    if (nrow(out) == 0) return(out)
    
    if (grepl("^L_R", family)) {
      entry_type <- get_matrix_entry_type(out$variable)
      out <- out[entry_type == "off_diagonal", , drop = FALSE]
    } else if (grepl("^L_Sigma", family)) {
      entry_type <- get_matrix_entry_type(out$variable)
      out$entry_type <- entry_type
      # L_Sigma_ig is a transformed Cholesky covariance factor. By default,
      # show off-diagonal entries because these are the covariance/correlation
      # structure components; diagonal entries mostly reflect marginal scale.
      if (identical(entry_filter, "diag")) {
        out <- out[entry_type == "diagonal", , drop = FALSE]
      } else if (identical(entry_filter, "all_lower")) {
        out <- out[entry_type %in% c("diagonal", "off_diagonal"), , drop = FALSE]
      } else {
        out <- out[entry_type == "off_diagonal", , drop = FALSE]
      }
    }
    
    out
  }
  
  posterior_family_filter_label <- function(family, entry_filter = "auto") {
    if (is.null(family) || is.na(family)) return("")
    if (grepl("^L_R", family)) {
      return("Showing only free lower-triangular Cholesky-correlation entries (row > column); fixed 0/1 entries are removed.")
    }
    if (grepl("^L_Sigma", family)) {
      if (identical(entry_filter, "diag")) {
        return("Showing diagonal entries of L_Sigma_ig. These mainly reflect marginal scale/standard-deviation structure.")
      }
      if (identical(entry_filter, "all_lower")) {
        return("Showing all informative lower-triangular entries of L_Sigma_ig; fixed upper-triangular entries are removed.")
      }
      return("Showing off-diagonal lower-triangular entries of L_Sigma_ig. These are more directly related to covariance/correlation structure.")
    }
    ""
  }
  
  # ── Trace plots ─────────────────────────────────────────────────────────────
  output$trace_param_ui <- renderUI({
    smry <- mcmc_active_summary()
    if (is.null(smry)) return(NULL)
    
    # Prefer parameter families that actually exist in saved posterior_draws.
    # In lightweight uploads, mcmc_summary may contain large generated quantities
    # such as y_rep, but posterior_draws intentionally excludes them to keep
    # the upload file small. Showing only draw-backed families avoids confusing
    # "No saved posterior draws found" messages in the Trace Plot tab.
    draws_obj <- NULL
    
    if (identical(rv$mcmc_mode, "uni")) {
      mod_idx <- suppressWarnings(as.integer(input$trace_metric %||% input$mcmc_metric_select %||% 1))
      if (is.na(mod_idx) || mod_idx < 1) mod_idx <- 1L
      if (!is.null(rv$mcmc_draws_list) && length(rv$mcmc_draws_list) >= mod_idx) {
        draws_obj <- rv$mcmc_draws_list[[mod_idx]]
      }
    } else {
      if (!is.null(rv$mcmc_draws_list) && length(rv$mcmc_draws_list) >= 1) {
        draws_obj <- rv$mcmc_draws_list[[1]]
      }
    }
    
    if (!is.null(draws_obj)) {
      draw_names <- names(as.data.frame(draws_obj, check.names = FALSE))
      draw_names <- setdiff(draw_names, c(".chain", ".iteration", ".draw"))
      all_vars <- unique(get_param_family(draw_names))
      all_vars <- all_vars[!is.na(all_vars) & nzchar(all_vars)]
    } else {
      # Fallback for full uploads or legacy objects without saved posterior_draws.
      all_vars <- sort(unique(sub("\\[.*", "", smry$variable)))
    }
    
    all_vars <- sort(unique(all_vars))
    all_vars <- all_vars[!all_vars %in% c("lp__")]
    
    if (length(all_vars) == 0) {
      return(tags$div(
        style = "color:#c0392b;font-weight:700;margin-top:6px;",
        "No posterior draw families available for trace plots."
      ))
    }
    
    selected <- input$trace_param
    if (is.null(selected) || !selected %in% all_vars) selected <- all_vars[1]
    
    selectInput(
      "trace_param",
      "Parameter family:",
      choices = all_vars,
      selected = selected
    )
  })
  
  output$trace_metric_ui <- renderUI({
    if (is.null(rv$mcmc_fit_list)) return(NULL)
    if (!identical(rv$mcmc_mode, "uni") || is.null(rv$mcmc_m) || rv$mcmc_m <= 1) {
      return(tags$div(style = "font-size:13px;color:#666;margin-top:28px;", "Multivariate RDS selected."))
    }
    choices <- setNames(seq_len(rv$mcmc_m), paste0("Metric/Modality ", seq_len(rv$mcmc_m)))
    selected <- input$mcmc_metric_focus %||% "M1"
    selected_idx <- suppressWarnings(as.integer(gsub("[^0-9]", "", selected)))
    if (is.na(selected_idx) || selected_idx < 1 || selected_idx > rv$mcmc_m) selected_idx <- 1L
    selectInput("trace_metric", "Metric/modality for trace:", choices = choices, selected = selected_idx)
  })
  
  observeEvent(input$mcmc_metric_focus, {
    if (!identical(rv$mcmc_mode, "uni") || is.null(rv$mcmc_m) || rv$mcmc_m <= 1) return(NULL)
    metric_focus <- input$mcmc_metric_focus %||% "All"
    metric_idx <- suppressWarnings(as.integer(gsub("[^0-9]", "", metric_focus)))
    if (!is.na(metric_idx) && metric_idx >= 1 && metric_idx <= rv$mcmc_m) {
      updateSelectInput(session, "trace_metric", selected = metric_idx)
    }
  }, ignoreInit = TRUE)
  
  output$trace_plot <- renderPlot({
    req(rv$mcmc_fit_list, input$trace_param)
    tryCatch({
      mod_idx <- if (identical(rv$mcmc_mode, "uni")) as.integer(input$trace_metric %||% 1) else 1L
      if (is.na(mod_idx) || mod_idx < 1 || mod_idx > length(rv$mcmc_fit_list)) mod_idx <- 1L
      
      draws_df <- NULL
      
      # Prefer posterior draws saved directly inside the uploaded RDS. This is
      # robust to terminal/Rscript runs where CmdStan CSV files may have been
      # written to a temporary directory that no longer exists.
      if (!is.null(rv$mcmc_draws_list) && length(rv$mcmc_draws_list) >= mod_idx) {
        draws_df <- rv$mcmc_draws_list[[mod_idx]]
      }
      
      if (!is.null(draws_df)) {
        draws_df <- as.data.frame(draws_df, check.names = FALSE)
        param_cols <- setdiff(colnames(draws_df), c(".chain", ".iteration", ".draw"))
        param_cols <- param_cols[grepl(paste0("^", input$trace_param, "(\\[|$)"), param_cols)]
        
        if (length(param_cols) == 0) {
          stop("No saved posterior draws found for selected parameter family: ", input$trace_param)
        }
        
        # Cholesky correlation factors contain many deterministic entries
        # (e.g., structural zeros / ones). For L_R_ig, keep only free
        # lower-triangular entries before checking posterior variation.
        if (identical(input$trace_param, "L_R_ig") || grepl("^L_R", input$trace_param)) {
          param_cols <- keep_free_cholesky_entries(param_cols)
        }
        
        # Remove deterministic or near-deterministic entries before selecting
        # the first few columns. This prevents fixed Cholesky entries from
        # producing a misleading "not informative" message.
        param_cols <- filter_informative_draw_columns(draws_df, param_cols)
        
        if (length(param_cols) == 0) {
          stop("No informative posterior draws found for selected parameter family: ", input$trace_param,
               ". All saved entries are fixed, deterministic, or have near-zero variation.")
        }
        
        param_cols <- head(param_cols, 6)
        draws_long <- draws_df[, c(".chain", ".iteration", ".draw", param_cols), drop = FALSE] %>%
          pivot_longer(cols = all_of(param_cols), names_to = "Parameter", values_to = "Value") %>%
          mutate(Chain = factor(.chain), Iteration = .iteration)
      } else {
        fit_obj_raw <- rv$mcmc_fit_list[[mod_idx]]
        fit_obj     <- extract_cmdstan_fit(fit_obj_raw)
        if (is.null(fit_obj)) stop("No saved posterior draws or usable CmdStanR fit object found inside the uploaded RDS.")
        draws_raw <- fit_obj$draws(
          variables = input$trace_param,
          format    = "draws_df"
        )
        draws_df2 <- as.data.frame(draws_raw, check.names = FALSE)
        param_cols <- setdiff(names(draws_df2), c(".chain", ".iteration", ".draw"))
        if (identical(input$trace_param, "L_R_ig") || grepl("^L_R", input$trace_param)) {
          param_cols <- keep_free_cholesky_entries(param_cols)
        }
        param_cols <- filter_informative_draw_columns(draws_df2, param_cols)
        if (length(param_cols) == 0) {
          stop("No informative posterior draws found for selected parameter family: ", input$trace_param,
               ". All saved entries are fixed, deterministic, or have near-zero variation.")
        }
        param_cols <- head(param_cols, 6)
        draws_long <- draws_df2[, c(".chain", ".iteration", ".draw", param_cols), drop = FALSE] %>%
          pivot_longer(cols = all_of(param_cols), names_to = "Parameter", values_to = "Value") %>%
          mutate(Chain = factor(.chain), Iteration = .iteration)
      }
      
      if (nrow(draws_long) == 0) stop("No posterior draws available for the selected parameter family.")
      
      draw_sds <- draws_long %>%
        group_by(Parameter) %>%
        summarise(sd_value = sd(Value, na.rm = TRUE), .groups = "drop")
      if (all(!is.finite(draw_sds$sd_value) | draw_sds$sd_value < 1e-8)) {
        return(
          ggplot() +
            labs(
              title = paste0("Trace plot not informative for ", input$trace_param),
              subtitle = "The selected parameter family has essentially no posterior variation. It may be deterministic, fixed, or constrained."
            ) +
            bold_theme(12)
        )
      }
      
      draws_long$Chain <- clean_batch_for_plot(draws_long$Chain)
      chain_cols <- safe_discrete_palette(levels(droplevels(draws_long$Chain)))
      ggplot(draws_long, aes(x = Iteration, y = Value, color = Chain, group = Chain)) +
        geom_line(alpha = 0.6, linewidth = 0.4) +
        facet_wrap(~Parameter, scales = "free_y", ncol = 2) +
        scale_color_manual(values = chain_cols) +
        labs(x = "Iteration", y = "Value",
             title = paste0("Trace plots — ", input$trace_param),
             subtitle = if (identical(rv$mcmc_mode, "uni")) paste0("Metric/Modality ", mod_idx) else "Multivariate fit") +
        bold_theme(11) +
        theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 9))
    }, error = function(e) {
      ggplot() + labs(title = paste("Trace plots unavailable:", e$message),
                      subtitle = "The uploaded RDS should contain saved `posterior_draws`; otherwise a usable CmdStanR fit object is required.") +
        bold_theme(12)
    })
  }, bg = "white")
  
  # ── Posterior Evidence ─────────────────────────────────────────────────────
  # This replaces the earlier prior-vs-posterior density overlay. It is more useful
  # for diagnostics because it ranks selected parameters by posterior evidence,
  # uncertainty, and convergence diagnostics.
  guess_mcmc_family <- function(smry, semantic = c("gamma", "delta", "other")) {
    semantic <- match.arg(semantic)
    families <- detect_mcmc_families(smry)
    families <- families[!families %in% c("lp__")]
    if (length(families) == 0) return(NA_character_)
    
    if (semantic == "gamma") {
      candidate_patterns <- c(
        "^gamma", "^gamma_star", "^gamma_post", "^gamma_posterior",
        "^gamma_base", "^mu_ig", "^mu_gamma", "^g_"
      )
    } else if (semantic == "delta") {
      candidate_patterns <- c(
        "^delta", "^delta_star", "^delta_post", "^delta_posterior",
        "^sigma", "^Sigma_ig", "^Sigma", "^tau", "^d_"
      )
    } else {
      return(families[1])
    }
    
    for (pat in candidate_patterns) {
      hit <- families[grepl(pat, families, ignore.case = TRUE)]
      if (length(hit) > 0) return(hit[1])
    }
    
    families[1]
  }
  
  get_ci_cols <- function(df) {
    if (all(c("q5", "q95") %in% colnames(df))) return(c("q5", "q95"))
    if (all(c("q2.5", "q97.5") %in% colnames(df))) return(c("q2.5", "q97.5"))
    if (all(c("5%", "95%") %in% colnames(df))) return(c("5%", "95%"))
    if (all(c("2.5%", "97.5%") %in% colnames(df))) return(c("2.5%", "97.5%"))
    c(NA_character_, NA_character_)
  }
  
  compute_posterior_evidence <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(data.frame())
    if (!"mean" %in% colnames(df)) return(data.frame())
    
    ci_cols <- get_ci_cols(df)
    ci_low_col <- ci_cols[1]
    ci_high_col <- ci_cols[2]
    
    out <- df
    if (!"sd" %in% colnames(out)) out$sd <- NA_real_
    
    out$abs_mean <- abs(out$mean)
    out$z_score  <- abs(out$mean) / pmax(out$sd, 1e-8)
    
    if (!is.na(ci_low_col) && !is.na(ci_high_col)) {
      out$ci_low  <- out[[ci_low_col]]
      out$ci_high <- out[[ci_high_col]]
    } else {
      out$ci_low  <- out$mean - 1.96 * out$sd
      out$ci_high <- out$mean + 1.96 * out$sd
    }
    
    out$ci_width <- out$ci_high - out$ci_low
    out$ci_excludes_zero <- out$ci_low > 0 | out$ci_high < 0
    
    # Approximate posterior probabilities using Normal(mean, sd) summaries.
    # If full draws are exposed later, this can be upgraded to empirical draw-based probabilities.
    out$prob_gt_0 <- ifelse(
      is.na(out$sd) | out$sd <= 0,
      NA_real_,
      1 - pnorm(0, mean = out$mean, sd = out$sd)
    )
    out$prob_lt_0 <- ifelse(
      is.na(out$sd) | out$sd <= 0,
      NA_real_,
      pnorm(0, mean = out$mean, sd = out$sd)
    )
    out$prob_away <- pmax(out$prob_gt_0, out$prob_lt_0)
    
    out$evidence_label <- dplyr::case_when(
      is.na(out$prob_away) ~ "Unknown",
      out$prob_away >= 0.995 ~ "Very strong",
      out$prob_away >= 0.975 ~ "Strong",
      out$prob_away >= 0.950 ~ "Moderate",
      TRUE ~ "Weak"
    )
    
    out
  }
  
  output$pe_family_ui <- renderUI({
    smry <- mcmc_active_summary()
    if (is.null(smry)) return(NULL)
    
    families <- detect_mcmc_families(smry)
    families <- families[!families %in% c("lp__")]
    
    if (length(families) == 0) {
      return(tags$div(
        style = "font-size:13px;color:#c0392b;",
        "No Stan parameter families detected."
      ))
    }
    
    selected_family <- guess_mcmc_family(smry, input$pe_param %||% "gamma")
    if (is.na(selected_family) || !selected_family %in% families) selected_family <- families[1]
    
    selectInput(
      "pe_family",
      "Detected Stan family:",
      choices = families,
      selected = selected_family
    )
  })
  
  observeEvent(input$pe_param, {
    smry <- mcmc_active_summary()
    if (is.null(smry)) return(NULL)
    families <- detect_mcmc_families(smry)
    families <- families[!families %in% c("lp__")]
    if (length(families) == 0) return(NULL)
    selected_family <- guess_mcmc_family(smry, input$pe_param %||% "gamma")
    if (!is.na(selected_family) && selected_family %in% families) {
      updateSelectInput(session, "pe_family", selected = selected_family)
    }
  }, ignoreInit = TRUE)
  
  pe_filtered_df <- reactive({
    smry <- standardize_mcmc_summary(rv$mcmc_summary)
    req(!is.null(smry), input$pe_family)
    
    df <- smry[
      grepl(paste0("^", input$pe_family, "(\\[|$)"), smry$variable) &
        !is.na(smry$mean),
      ,
      drop = FALSE
    ]
    
    compute_posterior_evidence(df)
  })
  
  pe_ranked_df <- reactive({
    df <- pe_filtered_df()
    if (nrow(df) == 0) return(df)
    
    rank_by <- input$pe_rank_by %||% "z_score"
    if (!rank_by %in% colnames(df)) rank_by <- "z_score"
    
    if (rank_by == "ci_width") {
      df <- df[order(df[[rank_by]], decreasing = FALSE, na.last = TRUE), , drop = FALSE]
    } else {
      df <- df[order(df[[rank_by]], decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    }
    
    n_show <- input$pe_top_n %||% 25
    head(df, n_show)
  })
  
  output$pe_forest_plot <- plotly::renderPlotly({
    df <- pe_ranked_df()
    
    if (nrow(df) == 0) {
      return(
        plotly::plot_ly() %>%
          plotly::layout(
            title = list(text = paste0("No matching informative posterior parameters found",
                                       if (!is.null(input$postev_family)) paste0(" for ", input$postev_family) else "",
                                       ".")),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }
    
    df <- df[order(df$mean), , drop = FALSE]
    df$idx <- seq_len(nrow(df))
    
    hover_txt <- paste0(
      df$variable,
      "<br>Mean: ", round(df$mean, 4),
      "<br>CI: [", round(df$ci_low, 4), ", ", round(df$ci_high, 4), "]",
      "<br>SD: ", round(df$sd, 4),
      "<br>|mean| / SD: ", round(df$z_score, 3),
      "<br>P(theta > 0 | y): ", round(df$prob_gt_0, 4),
      "<br>P(theta < 0 | y): ", round(df$prob_lt_0, 4),
      "<br>Evidence: ", df$evidence_label,
      if ("rhat" %in% colnames(df)) paste0("<br>R-hat: ", round(df$rhat, 4)) else "",
      if ("ess_bulk" %in% colnames(df)) paste0("<br>ESS bulk: ", round(df$ess_bulk)) else ""
    )
    
    fig <- plotly::plot_ly(
      df,
      y = ~idx,
      x = ~mean,
      type = "scatter",
      mode = "markers",
      text = hover_txt,
      hoverinfo = "text",
      marker = list(
        size = 8,
        opacity = 0.9,
        color = ~z_score,
        colorscale = "Viridis",
        showscale = TRUE,
        colorbar = list(title = "|mean| / SD")
      )
    ) %>%
      plotly::add_segments(
        x = df$ci_low,
        xend = df$ci_high,
        y = df$idx,
        yend = df$idx,
        inherit = FALSE,
        showlegend = FALSE,
        hoverinfo = "none",
        line = list(width = 2)
      ) %>%
      plotly::add_segments(
        x = 0,
        xend = 0,
        y = 0,
        yend = nrow(df) + 1,
        inherit = FALSE,
        showlegend = FALSE,
        hoverinfo = "none",
        line = list(dash = "dash", width = 1)
      )
    
    fig %>%
      plotly::layout(
        title = list(text = paste0("Posterior evidence for ", input$pe_family, " parameters")),
        xaxis = list(title = "Posterior mean with credible interval", zeroline = FALSE),
        yaxis = list(
          title = "",
          ticktext = df$variable,
          tickvals = df$idx,
          autorange = TRUE,
          showgrid = FALSE
        ),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  output$pe_summary_tbl <- renderDT({
    df <- pe_filtered_df()
    
    if (nrow(df) == 0) {
      return(
        datatable(
          data.frame(Message = "No matching posterior parameters found."),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }
    
    cols_show <- intersect(
      c(
        "variable", "mean", "sd", "ci_low", "ci_high", "ci_width",
        "prob_gt_0", "prob_lt_0", "prob_away", "z_score",
        "ci_excludes_zero", "evidence_label", "rhat", "ess_bulk", "ess_tail"
      ),
      colnames(df)
    )
    
    df_show <- df[, cols_show, drop = FALSE]
    
    for (nc in intersect(
      c("mean", "sd", "ci_low", "ci_high", "ci_width", "prob_gt_0", "prob_lt_0", "prob_away", "z_score", "rhat"),
      colnames(df_show)
    )) {
      df_show[[nc]] <- round(df_show[[nc]], 4)
    }
    
    for (nc in intersect(c("ess_bulk", "ess_tail"), colnames(df_show))) {
      df_show[[nc]] <- round(df_show[[nc]])
    }
    
    datatable(
      df_show,
      rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })
  
  # ── Shrinkage diagnostics ─────────────────────────────────────────────────
  output$shrink_mod_ui <- renderUI({
    req(rv$mcmc_m)
    if (rv$mcmc_m == 1) return(NULL)
    selectInput("shrink_mod", "Modality:",
                choices = setNames(seq_len(rv$mcmc_m), paste0("M", seq_len(rv$mcmc_m))),
                selected = 1)
  })
  
  output$shrink_batch_ui <- renderUI({
    fit_list <- rv$mcmc_fit_list
    if (is.null(fit_list)) return(NULL)
    mod_idx <- as.integer(input$shrink_mod %||% 1)
    obj <- fit_list[[mod_idx]]
    pair <- get_shrink_pair(obj, input$shrink_param %||% "gamma")
    emp <- pair$empirical
    batches <- "All"
    if (!is.null(emp)) {
      if (is.data.frame(emp)) emp <- as.matrix(emp)
      if (is.matrix(emp)) {
        batches <- rownames(emp) %||% paste0("B", seq_len(nrow(emp)))
        batches <- c("All", batches)
      } else if (is.list(emp) && !is.null(names(emp))) {
        batches <- c("All", names(emp))
      }
    }
    selectInput("shrink_batch", "Batch:", choices = batches, selected = batches[1])
  })
  
  output$shrink_plot <- renderPlot({
    fit_list <- rv$mcmc_fit_list
    req(!is.null(fit_list))
    mod_idx <- as.integer(input$shrink_mod %||% 1)
    obj     <- fit_list[[mod_idx]]
    parm    <- input$shrink_param %||% "gamma"
    b       <- input$shrink_batch %||% "All"
    n_feat  <- input$shrink_n_feat %||% 20
    pair <- get_shrink_pair(obj, parm)
    if (is.null(pair$empirical) || is.null(pair$shrunken)) {
      return(ggplot() +
               labs(title = paste0("Shrinkage diagnostics unavailable for ", parm),
                    subtitle = paste0("The uploaded RDS does not contain empirical and shrunken fields. Expected something like ",
                                      paste(pair$empirical_names, collapse = " / "), " and ",
                                      paste(pair$shrunken_names, collapse = " / "), ".")) +
               bold_theme(12))
    }
    h_vec <- vectorize_shrink_object(pair$empirical, batch = b)
    s_vec <- vectorize_shrink_object(pair$shrunken, batch = b)
    len <- min(length(h_vec), length(s_vec))
    if (len == 0) {
      return(ggplot() + labs(title = "Shrinkage diagnostics unavailable",
                             subtitle = "The detected empirical/shrunken objects are empty.") + bold_theme(12))
    }
    n_use <- min(n_feat, len)
    df <- data.frame(
      Feature = factor(paste0("feature_", seq_len(n_use)), levels = rev(paste0("feature_", seq_len(n_use)))),
      Empirical = as.numeric(h_vec[seq_len(n_use)]),
      Posterior = as.numeric(s_vec[seq_len(n_use)])
    )
    df$Shrinkage <- abs(df$Posterior - df$Empirical)
    ggplot(df) +
      geom_segment(aes(x = Empirical, xend = Posterior, y = Feature, yend = Feature),
                   linewidth = 1.0, alpha = 0.7,
                   arrow = arrow(length = unit(0.12, "cm"), type = "closed", ends = "last")) +
      geom_point(aes(x = Empirical, y = Feature), shape = 21, fill = "white", size = 3.0, stroke = 1.2) +
      geom_point(aes(x = Posterior, y = Feature, size = Shrinkage), shape = 16, alpha = 0.85) +
      geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
      scale_size_continuous(name = "Shrinkage\n|hat - star|", range = c(1.5, 6.5)) +
      labs(x = if (parm == "gamma") "gamma estimate" else "delta estimate",
           y = "Feature / parameter index",
           title = paste0(parm, " shrinkage diagnostics"),
           subtitle = "Open circle = empirical estimate; filled circle = shrunken/posterior estimate.") +
      bold_theme(11)
  }, bg = "white")
  
  output$shrink_tbl <- renderDT({
    fit_list <- rv$mcmc_fit_list
    req(!is.null(fit_list))
    mod_idx <- as.integer(input$shrink_mod %||% 1)
    obj     <- fit_list[[mod_idx]]
    parm    <- input$shrink_param %||% "gamma"
    b       <- input$shrink_batch %||% "All"
    pair <- get_shrink_pair(obj, parm)
    if (is.null(pair$empirical) || is.null(pair$shrunken)) {
      return(datatable(
        data.frame(Message = paste0("No empirical/shrunken shrinkage fields found for ", parm,
                                    ". This RDS likely contains only Stan posterior draws/summaries.")),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    h_vec <- vectorize_shrink_object(pair$empirical, batch = b)
    s_vec <- vectorize_shrink_object(pair$shrunken, batch = b)
    len <- min(length(h_vec), length(s_vec))
    df <- data.frame(
      index = seq_len(len),
      empirical = as.numeric(h_vec[seq_len(len)]),
      shrunken = as.numeric(s_vec[seq_len(len)])
    )
    df$absolute_change <- abs(df$shrunken - df$empirical)
    df$relative_change <- df$absolute_change / (abs(df$empirical) + 1e-8)
    datatable(df, rownames = FALSE, class = "compact stripe hover",
              options = list(pageLength = 20, scrollX = TRUE, dom = "tip"))
  })
  # ── Posterior summaries ────────────────────────────────────────────────────
  # The first dropdown is semantic (gamma vs delta), and the second dropdown shows
  # the detected Stan parameter family that will actually be plotted.
  # When the semantic dropdown changes, the detected-family dropdown updates too.
  guess_postsumm_family <- function(smry, semantic = c("gamma", "delta", "other")) {
    semantic <- match.arg(semantic)
    families <- detect_mcmc_families(smry)
    families <- families[!families %in% c("lp__")]
    if (length(families) == 0) return(NA_character_)
    
    if (semantic == "gamma") {
      candidate_patterns <- c(
        "^gamma", "^gamma_star", "^gamma_post", "^gamma_posterior",
        "^mu_ig", "^mu", "^g_"
      )
    } else if (semantic == "delta") {
      candidate_patterns <- c(
        "^delta", "^delta_star", "^delta_post", "^delta_posterior",
        "^sigma", "^Sigma_ig", "^Sigma", "^tau", "^d_", "^lp__"
      )
    } else {
      return(families[1])
    }
    
    for (pat in candidate_patterns) {
      hit <- families[grepl(pat, families, ignore.case = TRUE)]
      if (length(hit) > 0) return(hit[1])
    }
    
    NA_character_
  }
  
  output$postsumm_family_ui <- renderUI({
    smry <- mcmc_active_summary()
    if (is.null(smry)) return(NULL)
    
    families <- detect_mcmc_families(smry)
    families <- families[!families %in% c("lp__")]
    if (length(families) == 0) {
      return(tags$div(
        style = "font-size:13px;color:#c0392b;",
        "No Stan parameter families detected."
      ))
    }
    
    selected_family <- guess_postsumm_family(smry, input$postsumm_param %||% "gamma")
    if (is.na(selected_family) || !selected_family %in% families) {
      selected_family <- families[1]
    }
    
    selectInput(
      "postsumm_family",
      "Detected Stan parameter family:",
      choices = families,
      selected = selected_family
    )
  })
  
  observeEvent(input$postsumm_param, {
    smry <- mcmc_active_summary()
    if (is.null(smry)) return(NULL)
    families <- detect_mcmc_families(smry)
    families <- families[!families %in% c("lp__")]
    if (length(families) == 0) return(NULL)
    selected_family <- guess_postsumm_family(smry, input$postsumm_param %||% "gamma")
    if (is.na(selected_family) || !selected_family %in% families) return(NULL)
    updateSelectInput(session, "postsumm_family", selected = selected_family)
  }, ignoreInit = TRUE)
  
  postsumm_filtered_df <- reactive({
    smry <- standardize_mcmc_summary(rv$mcmc_summary)
    req(!is.null(smry), input$postsumm_family)
    smry[
      grepl(paste0("^", input$postsumm_family, "(\\[|$)"), smry$variable) &
        !is.na(smry$mean),
      ,
      drop = FALSE
    ]
  })
  
  output$postsumm_plot <- plotly::renderPlotly({
    df <- postsumm_filtered_df()
    if (nrow(df) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(
                 title = list(text = paste0("No parameters match: ", input$postsumm_family)),
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE)
               ))
    }
    
    n_show <- input$postsumm_n_feat %||% 20
    if (isTRUE(input$postsumm_sort)) df <- df[order(df$mean), , drop = FALSE]
    df <- head(df, n_show)
    df$idx <- seq_len(nrow(df))
    
    ci_low <- ci_high <- NULL
    if (all(c("q5", "q95") %in% colnames(df))) {
      ci_low <- "q5"; ci_high <- "q95"
    } else if (all(c("q2.5", "q97.5") %in% colnames(df))) {
      ci_low <- "q2.5"; ci_high <- "q97.5"
    } else if (all(c("5%", "95%") %in% colnames(df))) {
      ci_low <- "5%"; ci_high <- "95%"
    }
    has_ci <- !is.null(ci_low) && !is.null(ci_high)
    
    df$hover <- paste0(
      df$variable,
      "<br>Mean: ", round(df$mean, 4),
      if (has_ci) paste0("<br>Credible interval: [", round(df[[ci_low]], 4), ", ", round(df[[ci_high]], 4), "]") else "",
      if ("rhat" %in% colnames(df)) paste0("<br>R-hat: ", round(df$rhat, 4)) else "",
      if ("ess_bulk" %in% colnames(df)) paste0("<br>ESS bulk: ", round(df$ess_bulk)) else "",
      if ("ess_tail" %in% colnames(df)) paste0("<br>ESS tail: ", round(df$ess_tail)) else ""
    )
    
    fig <- plotly::plot_ly(
      df,
      y = ~idx,
      x = ~mean,
      type = "scatter",
      mode = "markers",
      text = ~hover,
      hoverinfo = "text",
      marker = list(size = 8, opacity = 0.85)
    )
    
    if (has_ci) {
      fig <- fig %>%
        plotly::add_segments(
          x = df[[ci_low]],
          xend = df[[ci_high]],
          y = df$idx,
          yend = df$idx,
          inherit = FALSE,
          showlegend = FALSE,
          hoverinfo = "none"
        )
    }
    
    fig %>%
      plotly::layout(
        title = list(
          text = paste0(
            "Posterior means",
            if (has_ci) " ± credible intervals" else "",
            " — ",
            input$postsumm_family
          )
        ),
        xaxis = list(title = "Posterior mean", zeroline = TRUE),
        yaxis = list(
          title = "",
          ticktext = df$variable,
          tickvals = df$idx,
          autorange = TRUE,
          showgrid = FALSE
        ),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  output$postsumm_tbl <- renderDT({
    df <- postsumm_filtered_df()
    cols_show <- intersect(
      c(
        "variable", "modality", "mean", "median", "sd", "mad",
        "q5", "q95", "q2.5", "q97.5", "5%", "95%",
        "rhat", "ess_bulk", "ess_tail"
      ),
      colnames(df)
    )
    df_show <- df[, cols_show, drop = FALSE]
    if (isTRUE(input$postsumm_sort) && "mean" %in% colnames(df_show)) {
      df_show <- df_show[order(df_show$mean), , drop = FALSE]
    }
    for (nc in intersect(c("mean", "median", "sd", "mad", "q5", "q95", "q2.5", "q97.5", "5%", "95%", "rhat"), colnames(df_show))) {
      df_show[[nc]] <- round(df_show[[nc]], 4)
    }
    for (nc in intersect(c("ess_bulk", "ess_tail"), colnames(df_show))) {
      df_show[[nc]] <- round(df_show[[nc]])
    }
    datatable(
      df_show,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(pageLength = 20, scrollX = TRUE, dom = "tip")
    )
  })
  
  # ── Combined posterior summaries and evidence ──────────────────────────────
  # Family categories used by the "Parameter type" selector. This keeps the
  # semantic category and the Stan-family dropdown aligned: for example,
  # "Other" will no longer show gamma_base or mu_ig.
  postev_family_category <- function(family) {
    family <- as.character(family)
    
    if (grepl("^(gamma_base|gamma_i|mu_ig|mu|alpha|beta|theta|intercept|g_)", family, ignore.case = TRUE)) {
      return("mean")
    }
    
    if (grepl("^(sigma_ig|sigma_psi|sigma_tau|sigma_delta|sigma|scale_sigma|tau|delta|nu_sigma|nu_psi|nu_tau|nu_delta|A_psi|A_tau|A_delta|sd_)", family, ignore.case = TRUE)) {
      return("scale")
    }
    
    if (grepl("^(L_R_ig|L_R|L_corr|corr|Omega|rho|L_Sigma_ig|L_Sigma|Sigma_ig_out|Sigma_ig|Sigma)", family, ignore.case = TRUE)) {
      return("cov")
    }
    
    if (grepl("^(y_rep|.*_rep$|.*_out$|log_lik|posterior_predictive|ppc)", family, ignore.case = TRUE)) {
      return("generated")
    }
    
    "other"
  }
  
  postev_families_by_type <- function(smry, param_type = "mean") {
    families <- detect_mcmc_families(smry)
    families <- families[!families %in% c("lp__")]
    families <- families[!is.na(families) & nzchar(families)]
    if (length(families) == 0) return(character(0))
    
    param_type <- param_type %||% "mean"
    cats <- vapply(families, postev_family_category, character(1))
    out <- families[cats == param_type]
    
    # Put common Stan families first within each category.
    preferred_order <- switch(
      param_type,
      mean = c("gamma_base", "gamma_i", "mu_ig", "mu", "beta", "alpha"),
      scale = c("sigma_ig", "sigma_delta", "sigma_tau", "sigma_psi", "scale_sigma", "nu_sigma", "tau", "delta"),
      cov = c("L_R_ig", "L_R", "L_corr", "rho", "Omega", "L_Sigma_ig", "Sigma_ig", "Sigma_ig_out"),
      generated = c("y_rep", "log_lik", "Sigma_ig_out"),
      other = character(0),
      character(0)
    )
    
    ordered <- character(0)
    for (pref in preferred_order) {
      hit <- out[tolower(out) == tolower(pref)]
      if (length(hit) > 0) ordered <- c(ordered, hit)
    }
    c(unique(ordered), setdiff(sort(unique(out)), ordered))
  }
  
  postev_type_label <- function(param_type) {
    switch(
      param_type %||% "mean",
      mean = "mean / location",
      scale = "scale / variance",
      cov = "correlation / covariance",
      generated = "generated / transformed",
      other = "other / uncategorized",
      "selected"
    )
  }
  
  postev_type_choices <- function(smry) {
    all_choices <- c(
      "Mean / location parameters" = "mean",
      "Scale / variance parameters" = "scale",
      "Correlation / covariance parameters" = "cov",
      "Generated / transformed quantities" = "generated",
      "Other / uncategorized parameters" = "other"
    )
    
    smry <- standardize_mcmc_summary(smry)
    if (is.null(smry) || nrow(smry) == 0) return(all_choices[FALSE])
    
    keep <- vapply(
      unname(all_choices),
      function(tp) length(postev_families_by_type(smry, tp)) > 0,
      logical(1)
    )
    
    all_choices[keep]
  }
  
  active_postev_type <- function(smry) {
    choices <- postev_type_choices(smry)
    if (length(choices) == 0) return(NA_character_)
    
    current <- input$postev_param
    if (!is.null(current) && current %in% unname(choices)) {
      current
    } else {
      unname(choices)[1]
    }
  }
  
  output$postev_param_ui <- renderUI({
    smry <- mcmc_active_summary()
    choices <- postev_type_choices(smry)
    
    if (length(choices) == 0) {
      return(tags$div(
        style = "font-size:13px;color:#c0392b;padding-top:28px;",
        "No posterior parameter categories with detected Stan families are available for the current upload/metric."
      ))
    }
    
    selected <- active_postev_type(smry)
    
    selectInput(
      "postev_param",
      "Parameter type:",
      choices = choices,
      selected = selected
    )
  })
  
  observe({
    smry <- mcmc_active_summary()
    choices <- postev_type_choices(smry)
    if (length(choices) == 0) return(NULL)
    
    selected <- active_postev_type(smry)
    updateSelectInput(
      session,
      "postev_param",
      choices = choices,
      selected = selected
    )
  })
  
  guess_postev_family <- function(smry, param_type = "mean") {
    families <- postev_families_by_type(smry, param_type)
    if (length(families) == 0) return(NA_character_)
    families[1]
  }
  
  output$postev_family_ui <- renderUI({
    smry <- mcmc_active_summary()
    if (is.null(smry)) return(NULL)
    
    param_type <- active_postev_type(smry)
    if (is.na(param_type)) {
      return(tags$div(
        style = "font-size:13px;color:#c0392b;padding-top:28px;",
        "No detected Stan families are available for posterior summaries."
      ))
    }
    
    families <- postev_families_by_type(smry, param_type)
    
    if (length(families) == 0) {
      return(NULL)
    }
    
    selected_family <- guess_postev_family(smry, param_type)
    if (is.na(selected_family) || !selected_family %in% families) selected_family <- families[1]
    
    selectInput(
      "postev_family",
      "Detected Stan family:",
      choices = families,
      selected = selected_family
    )
  })
  
  observeEvent(input$postev_param, {
    smry <- mcmc_active_summary()
    if (is.null(smry)) return(NULL)
    
    param_type <- active_postev_type(smry)
    if (is.na(param_type)) return(NULL)
    
    families <- postev_families_by_type(smry, param_type)
    if (length(families) == 0) return(NULL)
    
    selected_family <- guess_postev_family(smry, param_type)
    if (is.na(selected_family) || !selected_family %in% families) selected_family <- families[1]
    
    updateSelectInput(
      session,
      "postev_family",
      choices = families,
      selected = selected_family
    )
  }, ignoreInit = TRUE)
  
  output$postev_entry_filter_ui <- renderUI({
    family <- input$postev_family
    if (is.null(family) || is.na(family)) return(NULL)
    
    if (grepl("^L_R", family)) {
      return(tags$div(
        style = "background:#eef7ff;border-left:4px solid #3d7fc1;padding:8px 12px;border-radius:6px;font-size:13px;margin:6px 0 10px;",
        icon("circle-info"),
        " For ", tags$code(family),
        ", the posterior summary automatically removes fixed Cholesky entries and shows only free lower-triangular correlation components."
      ))
    }
    
    if (grepl("^L_Sigma", family)) {
      return(tags$div(
        style = "background:#eef7ff;border-left:4px solid #3d7fc1;padding:8px 12px;border-radius:6px;font-size:13px;margin:6px 0 10px;",
        fluidRow(
          column(
            4,
            selectInput(
              "postev_matrix_entry",
              "L_Sigma_ig entries to summarize:",
              choices = c(
                "Off-diagonal covariance/correlation entries" = "offdiag",
                "Diagonal scale entries" = "diag",
                "All informative lower-triangular entries" = "all_lower"
              ),
              selected = input$postev_matrix_entry %||% "offdiag"
            )
          ),
          column(
            8,
            tags$div(
              style = "padding-top:26px;color:#555;",
              icon("circle-info"),
              " ", tags$code("L_Sigma_ig"),
              " is transformed from ", tags$code("sigma_ig"),
              " and ", tags$code("L_R_ig"),
              ". Off-diagonal entries are usually more relevant for covariance/correlation structure; diagonal entries mostly reflect marginal scale."
            )
          )
        )
      ))
    }
    
    NULL
  })
  
  postev_filtered_df <- reactive({
    smry <- mcmc_active_summary()
    req(!is.null(smry), input$postev_family)
    
    # Guard against stale input values when the semantic category changes.
    # The selected family must belong to the currently selected parameter type.
    param_type <- active_postev_type(smry)
    if (is.na(param_type)) {
      return(compute_posterior_evidence(smry[0, , drop = FALSE]))
    }
    
    valid_families <- postev_families_by_type(smry, param_type)
    if (length(valid_families) == 0 || !input$postev_family %in% valid_families) {
      return(compute_posterior_evidence(smry[0, , drop = FALSE]))
    }
    
    df <- smry[
      grepl(paste0("^", input$postev_family, "(\\[|$)"), smry$variable) &
        !is.na(smry$mean),
      ,
      drop = FALSE
    ]
    
    df <- filter_posterior_summary_entries(
      df,
      family = input$postev_family,
      entry_filter = input$postev_matrix_entry %||% "offdiag"
    )
    
    compute_posterior_evidence(df)
  })
  
  postev_ranked_df <- reactive({
    df <- postev_filtered_df()
    if (nrow(df) == 0) return(df)
    
    rank_by <- input$postev_rank_by %||% "z_score"
    if (!rank_by %in% colnames(df)) rank_by <- "z_score"
    
    if (rank_by == "ci_width") {
      df <- df[order(df[[rank_by]], decreasing = FALSE, na.last = TRUE), , drop = FALSE]
    } else {
      df <- df[order(df[[rank_by]], decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    }
    
    n_show <- input$postev_top_n %||% 25
    head(df, n_show)
  })
  
  output$postev_forest_plot <- plotly::renderPlotly({
    df <- postev_ranked_df()
    
    if (nrow(df) == 0) {
      return(
        plotly::plot_ly() %>%
          plotly::layout(
            title = list(text = "No matching posterior parameters found."),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }
    
    df <- df[order(df$mean), , drop = FALSE]
    df$idx <- seq_len(nrow(df))
    
    hover_txt <- paste0(
      df$variable,
      "<br>Mean: ", round(df$mean, 4),
      if ("median" %in% names(df)) paste0("<br>Median: ", round(df$median, 4)) else "",
      "<br>CI: [", round(df$ci_low, 4), ", ", round(df$ci_high, 4), "]",
      "<br>SD: ", round(df$sd, 4),
      "<br>|mean| / SD: ", round(df$z_score, 3),
      "<br>P(theta > 0 | y): ", round(df$prob_gt_0, 4),
      "<br>P(theta < 0 | y): ", round(df$prob_lt_0, 4),
      "<br>Evidence: ", df$evidence_label,
      if ("rhat" %in% colnames(df)) paste0("<br>R-hat: ", round(df$rhat, 4)) else "",
      if ("ess_bulk" %in% colnames(df)) paste0("<br>ESS bulk: ", round(df$ess_bulk)) else "",
      if ("ess_tail" %in% colnames(df)) paste0("<br>ESS tail: ", round(df$ess_tail)) else ""
    )
    
    fig <- plotly::plot_ly(
      df,
      y = ~idx,
      x = ~mean,
      type = "scatter",
      mode = "markers",
      text = hover_txt,
      hoverinfo = "text",
      marker = list(
        size = 8,
        opacity = 0.9,
        color = ~z_score,
        colorscale = "Viridis",
        showscale = TRUE,
        colorbar = list(title = "|mean| / SD")
      )
    ) %>%
      plotly::add_segments(
        x = df$ci_low,
        xend = df$ci_high,
        y = df$idx,
        yend = df$idx,
        inherit = FALSE,
        showlegend = FALSE,
        hoverinfo = "none",
        line = list(width = 2)
      ) %>%
      plotly::add_segments(
        x = 0,
        xend = 0,
        y = 0,
        yend = nrow(df) + 1,
        inherit = FALSE,
        showlegend = FALSE,
        hoverinfo = "none",
        line = list(dash = "dash", width = 1)
      )
    
    fig %>%
      plotly::layout(
        title = list(text = paste0(
          "Posterior summaries and evidence for ", input$postev_family, " parameters",
          if (nzchar(posterior_family_filter_label(input$postev_family, input$postev_matrix_entry %||% "offdiag")))
            paste0("<br><sup>", posterior_family_filter_label(input$postev_family, input$postev_matrix_entry %||% "offdiag"), "</sup>") else ""
        )),
        xaxis = list(title = "Posterior mean with credible interval", zeroline = FALSE),
        yaxis = list(
          title = "",
          ticktext = df$variable,
          tickvals = df$idx,
          autorange = TRUE,
          showgrid = FALSE
        ),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  output$postev_summary_tbl <- renderDT({
    df <- postev_filtered_df()
    
    if (nrow(df) == 0) {
      return(
        datatable(
          data.frame(Message = "No matching posterior parameters found."),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }
    
    rank_by <- input$postev_rank_by %||% "z_score"
    if (isTRUE(input$postev_sort_table) && rank_by %in% colnames(df)) {
      if (rank_by == "ci_width") {
        df <- df[order(df[[rank_by]], decreasing = FALSE, na.last = TRUE), , drop = FALSE]
      } else {
        df <- df[order(df[[rank_by]], decreasing = TRUE, na.last = TRUE), , drop = FALSE]
      }
    }
    
    cols_show <- intersect(
      c(
        "variable", "mean", "median", "sd", "mad",
        "ci_low", "ci_high", "ci_width",
        "prob_gt_0", "prob_lt_0", "prob_away", "z_score",
        "ci_excludes_zero", "evidence_label",
        "rhat", "ess_bulk", "ess_tail"
      ),
      colnames(df)
    )
    
    df_show <- df[, cols_show, drop = FALSE]
    
    for (nc in intersect(
      c("mean", "median", "sd", "mad", "ci_low", "ci_high", "ci_width", "prob_gt_0", "prob_lt_0", "prob_away", "z_score", "rhat"),
      colnames(df_show)
    )) {
      df_show[[nc]] <- round(df_show[[nc]], 4)
    }
    
    for (nc in intersect(c("ess_bulk", "ess_tail"), colnames(df_show))) {
      df_show[[nc]] <- round(df_show[[nc]])
    }
    
    datatable(
      df_show,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(pageLength = 20, scrollX = TRUE, dom = "tip")
    )
  })
  
}  # end server

shinyApp(ui, server)
