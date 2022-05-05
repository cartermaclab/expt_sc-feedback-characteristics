# -------------------------------------------
# CHOICE AND FEEDBACK CHARACTERISTICS PROJECT
# -- St. Germain, McKay, Poskus, Williams, Leshchyshen, Feldman, Cashaback,
# and Carter
#
# Manuscript - Analyses
#
# Authors:
#   Laura St. Germain
#   Brad McKay
#   Mike Carter
#
# Last update: March 30 2022
#
# Website: https://www.cartermaclab.org
# -------------------------------------------

# SCRIPT SETUP ----
#
# Required files and tibbles
source("scripts/main_wrangle.R")

# DATA SETUP ----
#
# Create tibbles to work with
#
# PRETEST, RETENTION, TRANSFER
#
# EXPERIMENT 1
#
# Spatial goal
#
# Omnibus test
expt1_prt_deg_lm <- afex::aov_ez(
  "id", "p_mean_e_deg", expt1_performance_prt_p,
  between = c("choice_id", "fb_id"),
  within = "block_id"
)
expt1_prt_deg_lm

# Post hoc for main effect of Test
expt1_prt_deg_me_test <- emmeans::emmeans(
  expt1_prt_deg_lm, ~block_id
)
pairs(expt1_prt_deg_me_test, adjust = "holm")

# Timing goal
#
# Omnibus test
expt1_prt_ms_lm <- afex::aov_ez(
  "id", "p_mean_e_ms", expt1_performance_prt_p,
  between = c("choice_id", "fb_id"),
  within = "block_id"
)
expt1_prt_ms_lm

# Post hoc for main effect of Test
expt1_prt_ms_me_test <- emmeans::emmeans(
  expt1_prt_ms_lm, ~block_id
)
pairs(expt1_prt_ms_me_test, adjust = "holm")


# EXPERIMENT 2
#
# Spatial goal
#
# Omnibus test
expt2_prt_deg_lm <- afex::aov_ez(
  "id", "p_mean_e_deg", expt2_performance_prt_p,
  between = "group_id",
  within = "block_id"
)
expt2_prt_deg_lm

# Timing goal
#
# Omnibus test
expt2_prt_ms_lm <- afex::aov_ez(
  "id", "p_mean_e_ms", expt2_performance_prt_p,
  between = "group_id",
  within = "block_id"
)
expt2_prt_ms_lm


# ACQUISITION
#
# EXPERIMENT 1
#
# Spatial goal
#
# Omnibus test
expt1_acq_deg_lm <- afex::aov_ez(
  "id", "p_mean_e_deg", expt1_performance_acq_p,
  between = c("choice_id", "fb_id"),
  within = "block_id"
)
expt1_acq_deg_lm

# Post hoc main effect of Block
expt1_acq_deg_me_block <- emmeans::emmeans(
  expt1_acq_deg_lm, ~block_id
)
pairs(expt1_acq_deg_me_block, adjust = "holm")

# Timing goal
#
# Omnibus test
expt1_acq_ms_lm <- afex::aov_ez(
  "id", "p_mean_e_ms", expt1_performance_acq_p,
  between = c("choice_id", "fb_id"),
  within = "block_id"
)
expt1_acq_ms_lm

# Post hoc Feedback x Block interaction
emmeans::emmip(expt1_acq_ms_lm, fb_id ~ block_id) # Visualize interaction

expt1_acq_ms_fb_x_block <- emmeans::emmeans(
  expt1_acq_ms_lm, ~block_id|fb_id
)
pairs(expt1_acq_ms_fb_x_block, adjust = "holm")


# EXPERIMENT 2
#
# Spatial goal
#
# Omnibus test
expt2_acq_deg_lm <- afex::aov_ez(
  "id", "p_mean_e_deg", expt2_performance_acq_p,
  between = "group_id",
  within = "block_id"
)
expt2_acq_deg_lm

# Timing goal
expt2_acq_ms_lm <- afex::aov_ez(
  "id", "p_mean_e_deg", expt2_performance_acq_p,
  between = "group_id",
  within = "block_id"
)
expt2_acq_ms_lm


# PSYCHOLOGICAL VARIABLES
#
# EXPERIMENT 1
#
# Perceived autonomy
#
# Omnibus test
expt1_pa_lm <- afex::aov_ez(
  "id", "score", expt1_qaire_pa_p,
  between = c("choice_id", "fb_id"),
  within = "time_id"
)
expt1_pa_lm

# Post hoc main effect of time
expt1_pa_me_time <- emmeans::emmeans(
  expt1_pa_lm, ~time_id
)
pairs(expt1_pa_me_time, adjust = "holm")

# Perceived competence
#
# Omnibus test
expt1_pc_lm <- afex::aov_ez(
  "id", "score", expt1_qaire_pc_p,
  between = c("choice_id", "fb_id"),
  within = "time_id"
)
expt1_pc_lm

# Post hoc interaction of feedback and time
expt1_pc_fb_x_time <- emmeans::emmeans(
  expt1_pc_lm, "fb_id", by = "time_id"
)
pairs(expt1_pc_fb_x_time, adjust = "holm")

# Intrinsic motivation
#
# Omnibus test
expt1_im_lm <- afex::aov_ez(
  "id", "score", expt1_qaire_im_p,
  between = c("choice_id", "fb_id"),
  within = "time_id"
)
expt1_im_lm

# Post hoc main effect of time
expt1_im_me_time <- emmeans::emmeans(
  expt1_im_lm, ~time_id
)
pairs(expt1_im_me_time, adjust = "holm")


# EXPERIMENT 2
#
# Perceived autonomy
#
# Omnibus test
expt2_pa_lm <- afex::aov_ez(
  "id", "score", expt2_qaire_pa_p,
  between = "group_id",
  within = "time_id"
)
expt2_pa_lm

# Perceived competence
#
# Omnibus test
expt2_pc_lm <- afex::aov_ez(
  "id", "score", expt2_qaire_pc_p,
  between = "group_id",
  within = "time_id"
)
expt2_pc_lm

# Post hoc main effect of time
expt2_pc_me_time <- emmeans::emmeans(
  expt2_pc_lm, ~time_id
)
pairs(expt2_pc_me_time, adjust = "holm")

# Intrinsic motivation
#
# Omnibus test
expt2_im_lm <- afex::aov_ez(
  "id", "score", expt2_qaire_im_p,
  between = "group_id",
  within = "time_id"
)
expt2_im_lm

# Post hoc main effect of time
expt2_im_me_time <- emmeans::emmeans(
  expt2_im_lm, ~time_id
)
pairs(expt2_im_me_time, adjust = "holm")


# CRONBACH'S ALPHA
#
# EXPERIMENT 1
#
# After pre-test
expt1_cronbach_pa_pre <- Cronbach::cronbach(expt1_pa_pre_tib)
expt1_cronbach_pc_pre <- Cronbach::cronbach(expt1_pc_pre_tib)
expt1_cronbach_im_pre <- Cronbach::cronbach(expt1_im_pre_tib)

# After trial 12 (block 1)
expt1_cronbach_pa_b1 <- Cronbach::cronbach(expt1_pa_b1_tib)
expt1_cronbach_pc_b1 <- Cronbach::cronbach(expt1_pc_b1_tib)
expt1_cronbach_im_b1 <- Cronbach::cronbach(expt1_im_b1_tib)

# After trial 72 (block 6)
expt1_cronbach_pa_b6 <- Cronbach::cronbach(expt1_pa_b6_tib)
expt1_cronbach_pc_b6 <- Cronbach::cronbach(expt1_pc_b6_tib)
expt1_cronbach_im_b6 <- Cronbach::cronbach(expt1_im_b6_tib)

# Before 24-h retention
expt1_cronbach_pa_ret <- Cronbach::cronbach(expt1_pa_ret_tib)
expt1_cronbach_pc_ret <- Cronbach::cronbach(expt1_pc_ret_tib)
expt1_cronbach_im_ret <- Cronbach::cronbach(expt1_im_ret_tib)

# EXPERIMENT 2
#
# After pre-test
expt2_cronbach_pa_pre <- Cronbach::cronbach(expt2_pa_pre_tib)
expt2_cronbach_pc_pre <- Cronbach::cronbach(expt2_pc_pre_tib)
expt2_cronbach_im_pre <- Cronbach::cronbach(expt2_im_pre_tib)

# After trial 12 (block 1)
expt2_cronbach_pa_b1 <- Cronbach::cronbach(expt2_pa_b1_tib)
expt2_cronbach_pc_b1 <- Cronbach::cronbach(expt2_pc_b1_tib)
expt2_cronbach_im_b1 <- Cronbach::cronbach(expt2_im_b1_tib)

# After trial 72 (block 6)
expt2_cronbach_pa_b6 <- Cronbach::cronbach(expt2_pa_b6_tib)
expt2_cronbach_pc_b6 <- Cronbach::cronbach(expt2_pc_b6_tib)
expt2_cronbach_im_b6 <- Cronbach::cronbach(expt2_im_b6_tib)

# Before 24-h retention
expt2_cronbach_pa_ret <- Cronbach::cronbach(expt2_pa_ret_tib)
expt2_cronbach_pc_ret <- Cronbach::cronbach(expt2_pc_ret_tib)
expt2_cronbach_im_ret <- Cronbach::cronbach(expt2_im_ret_tib)


# EQUIVALENCE ANALYSIS
#
# EXPERIMENT 1
#
# Calculate hedges g and 90% confidence intervals
#
# Spatial goal
expt1_deg_es <- compute.es::mes(
  m.1 = expt1_choice_g_ret$mean_deg[2],
  m.2 = expt1_choice_g_ret$mean_deg[1],
  sd.1 = expt1_choice_g_ret$sd_deg[2],
  sd.2 = expt1_choice_g_ret$sd_deg[1],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# Timing goal
expt1_ms_es <- compute.es::mes(
  m.1 = expt1_choice_g_ret$mean_ms[2],
  m.2 = expt1_choice_g_ret$mean_ms[1],
  sd.1 = expt1_choice_g_ret$sd_ms[2],
  sd.2 = expt1_choice_g_ret$sd_ms[1],
  n.1 = 76,
  n.2 = 76,
  level = 90
)


# EXPERIMENT 2
#
# Calculate hedges g and 90% confidence intervals
#
# Spatial goal
expt2_deg_es <- compute.es::mes(
  m.1 = expt2_choice_g_ret$mean_deg[2],
  m.2 = expt2_choice_g_ret$mean_deg[1],
  sd.1 = expt2_choice_g_ret$sd_deg[2],
  sd.2 = expt2_choice_g_ret$sd_deg[1],
  n.1 = 38,
  n.2 = 38,
  level = 90
)

# Timing goal
expt2_ms_es <- compute.es::mes(
  m.1 = expt2_choice_g_ret$mean_ms[2],
  m.2 = expt2_choice_g_ret$mean_ms[1],
  sd.1 = expt2_choice_g_ret$sd_ms[2],
  sd.2 = expt2_choice_g_ret$sd_ms[1],
  n.1 = 38,
  n.2 = 38,
  level = 90
)


# MINI META-ANALYSIS OF EXPERIMENTS 1 AND 2
#
# Spatial goal for both experiment
expt1_spatial <- dplyr::bind_cols(
  expt1_deg_es$g, expt1_deg_es$var.g
) %>%
  dplyr::rename(g = 1) %>%
  dplyr::rename(var = 2)

expt2_spatial <- dplyr::bind_cols(
  expt2_deg_es$g, expt2_deg_es$var.g
) %>%
  dplyr::rename(g = 1) %>%
  dplyr::rename(var = 2)

meta_spatial <- dplyr::bind_rows(expt1_spatial, expt2_spatial) %>%
  dplyr::mutate(study = c("expt1", "expt2"))

# Timing goal for both experiment
expt1_timing <- dplyr::bind_cols(
  expt1_ms_es$g, expt1_ms_es$var.g
) %>%
  dplyr::rename(g = 1) %>%
  dplyr::rename(var = 2)

expt2_timing <- dplyr::bind_cols(
  expt2_ms_es$g, expt2_ms_es$var.g
) %>%
  dplyr::rename(g = 1) %>%
  dplyr::rename(var = 2)

meta_timing <- dplyr::bind_rows(expt1_timing, expt2_timing) %>%
  dplyr::mutate(study = c("expt1", "expt2"))

# Merge effect sizes for both goals in a single tibble
meta_combined <- dplyr::bind_rows(
  meta_spatial, meta_timing
) %>%
  dplyr::mutate(n1 = c(76,38,76,38)) %>%
  dplyr::mutate(n2 = c(76,38,76,38))

meta_dat <- metafor::escalc(
  measure = "SMD",
  yi = g,
  vi = var,
  n1i = n1,
  n2i = n2,
  data = meta_combined
)

# Convert to wide format for correlations
#
expt1_wide_prt_deg_p <- expt1_performance_prt_p %>%
  dplyr::select(-c(n, p_mean_e_ms)) %>%
  tidyr::pivot_wider(
    names_from = block_id,
    values_from = p_mean_e_deg
  ) %>%
  # rename phases based on column number
  dplyr::rename(pre = 5) %>%
  dplyr::rename(ret = 6) %>%
  dplyr::rename(trn = 7)

expt1_wide_prt_ms_p <- expt1_performance_prt_p %>%
  dplyr::select(-c(n, p_mean_e_deg)) %>%
  tidyr::pivot_wider(
    names_from = block_id,
    values_from = p_mean_e_ms
  ) %>%
  # rename phases based on column number
  dplyr::rename(pre = 5) %>%
  dplyr::rename(ret = 6) %>%
  dplyr::rename(trn = 7)

expt2_wide_prt_deg_p <- expt2_performance_prt_p %>%
  dplyr::select(-c(n, p_mean_e_ms)) %>%
  tidyr::pivot_wider(
    names_from = block_id,
    values_from = p_mean_e_deg
  ) %>%
  # rename phases based on column number
  dplyr::rename(pre = 3) %>%
  dplyr::rename(ret = 4) %>%
  dplyr::rename(trn = 5)

expt2_wide_prt_ms_p <- expt2_performance_prt_p %>%
  dplyr::select(-c(n, p_mean_e_deg)) %>%
  tidyr::pivot_wider(
    names_from = block_id,
    values_from = p_mean_e_ms
  ) %>%
  # rename phases based on column number
  dplyr::rename(pre = 3) %>%
  dplyr::rename(ret = 4) %>%
  dplyr::rename(trn = 5)

cor(expt1_wide_prt_deg_p$ret, expt1_wide_prt_ms_p$ret)
cor(expt2_wide_prt_deg_p$ret, expt2_wide_prt_ms_p$ret)

meta_agg <- metafor::aggregate.escalc(
  meta_dat,
  cluster = meta_dat$study,
  rho = .185
)

# Meta-analyze spatial and timing effect sizes
meta_spatial_res <- metafor::rma(
  g,
  var,
  data = meta_spatial,
  level = 90
)

meta_timing_res <- metafor::rma(
  g,
  var,
  data = meta_timing,
  level = 90
)

meta_spatial_res
meta_timing_res

# Meta-analysis for paper overall with spatial and timing goals collapsed
meta_combined_res <- metafor::rma(
  yi,
  vi,
  data = meta_agg,
  level = 90
)
meta_combined_res
