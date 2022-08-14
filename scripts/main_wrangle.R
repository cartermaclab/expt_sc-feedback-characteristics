# -------------------------------------------
# CHOICE AND FEEDBACK CHARACTERISTICS PROJECT
# -- St. Germain, McKay, Poskus, Williams, Leshchyshen, Feldman, Cashaback,
# and Carter
#
# Manuscript - Data wrangling
#
# Authors:
#   Laura St. Germain
#   Brad McKay
#   Mike Carter
#
# Last update: August 10 2022
#
# Website: https://www.cartermaclab.org
# -------------------------------------------


# SCRIPT SETUP ----
#
# Required libraries
source("scripts/libraries.R")

# Seed for reproducible bootstrapped confidence intervals
set.seed(8693)

# Load data files
expt1_performance_data <- readr::read_csv("data/expt1_performance-data.csv")
expt1_qaire_data <- readr::read_csv("data/expt1_qaire-data.csv")
expt1_cronbach_data <- readr::read_csv("data/expt1_cronbach.csv")

expt2_performance_data <- readr::read_csv("data/expt2_performance-data.csv")
expt2_qaire_data <- readr::read_csv("data/expt2_qaire-data.csv")
expt2_cronbach_data <- readr::read_csv("data/expt2_cronbach.csv")


# DATA SETUP ----
#
# Create tibbles to work with
#
# EXPERIMENT 1
#
# PERFORMANCE DATA
#
# Add columns for choice, feedback, blocks, and ce^2. For blocks, we will
# collapse across the two different pre-tests for now.
expt1_performance_tib <- expt1_performance_data %>%
  dplyr::mutate(choice_id = dplyr::if_else(group_id <= 2, 1, 2)) %>%
  dplyr::mutate(fb_id = dplyr::if_else(group_id %in% c(1, 3), 1, 2)) %>%
  dplyr::mutate(block_id = rep(rep(1:9, each = 12), 152)) %>%
  dplyr::mutate(ce2_deg = dplyr::if_else(phase_id != 4, (xi_deg - 40)^2,
                                         (xi_deg - 60)^2)) %>%
  dplyr::mutate(ce2_ms = (xi_ms - 225)^2)

# Make factors
expt1_performance_tib <- expt1_performance_tib %>%
  dplyr::mutate(
    group_id = forcats::as_factor(group_id),
    phase_id = forcats::as_factor(phase_id),
    choice_id = forcats::as_factor(choice_id),
    fb_id = forcats::as_factor(fb_id),
    block_id = forcats::as_factor(block_id)
  )

# Tibble for pre-test, retention, and transfer data at the participant (p)
# and group (g) levels
expt1_performance_prt_p <- expt1_performance_tib %>%
  dplyr::filter(block_id %in% c(1, 8, 9)) %>%
  dplyr::group_by(id, group_id, choice_id, fb_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    p_mean_e_deg = sqrt(mean(ce2_deg, na.rm = TRUE)),
    p_mean_e_ms = sqrt(mean(ce2_ms, na.rm = TRUE)),
    .groups = "drop"
  )

expt1_performance_prt_g <- expt1_performance_prt_p %>%
  dplyr::group_by(group_id, choice_id, fb_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    g_mean_e_deg = mean(p_mean_e_deg, na.rm = TRUE),
    ci_low_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymin,
    ci_upp_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymax,
    g_mean_e_ms = mean(p_mean_e_ms, na.rm = TRUE),
    ci_low_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymin,
    ci_upp_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymax,
    .groups = "drop"
  )

# Tibble for acquisition at the participant (p) and group (g) levels
expt1_performance_acq_p <- expt1_performance_tib %>%
  dplyr::filter(block_id %in% c(2:7)) %>%
  dplyr::group_by(id, group_id, choice_id, fb_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    p_mean_e_deg = sqrt(mean(ce2_deg, na.rm = TRUE)),
    p_mean_e_ms = sqrt(mean(ce2_ms, na.rm = TRUE)),
    .groups = "drop"
  )

expt1_performance_acq_g <- expt1_performance_acq_p %>%
  dplyr::group_by(group_id, choice_id, fb_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    g_mean_e_deg = mean(p_mean_e_deg, na.rm = TRUE),
    ci_low_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymin,
    ci_upp_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymax,
    g_mean_e_ms = mean(p_mean_e_ms, na.rm = TRUE),
    ci_low_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymin,
    ci_upp_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymax,
    .groups = "drop"
  )

# Number of hits during acquisition at the participant (p) and group (g) levels
expt1_deg_hit_p <- expt1_performance_tib %>%
  dplyr::filter(phase_id == 2) %>%
  dplyr::mutate(hit = dplyr::if_else(
    xi_deg > 39.99 & xi_deg < 40.01, 1, 0)) %>%
  dplyr::group_by(id, group_id) %>%
  dplyr::summarize(
    n = n(),
    sum_hit = sum(hit, na.rm = TRUE)
  )

expt1_deg_hit_g <- expt1_deg_hit_p %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarize(
    n = n(),
    total = sum(sum_hit, na.rm = TRUE),
    min = min(sum_hit, na.rm = TRUE),
    max = max(sum_hit, na.rm = TRUE)
  )

expt1_ms_hit_p <- expt1_performance_tib %>%
  dplyr::filter(phase_id == 2) %>%
  dplyr::mutate(hit = dplyr::if_else(xi_ms == 225, 1, 0)) %>%
  dplyr::group_by(id, group_id) %>%
  dplyr::summarize(
    n = n(),
    sum_hit = sum(hit, na.rm = TRUE)
  )

expt1_ms_hit_g <- expt1_ms_hit_p %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarize(
    n = n(),
    total = sum(sum_hit, na.rm = TRUE),
    min = min(sum_hit, na.rm = TRUE),
    max = max(sum_hit, na.rm = TRUE)
  )

# QUESTIONNAIRE DATA
#
# Add columns for choice and feedback
expt1_qaire_tib <- expt1_qaire_data %>%
  dplyr::mutate(choice_id = dplyr::if_else(group_id <= 2, 1, 2)) %>%
  dplyr::mutate(fb_id = dplyr::if_else(group_id %in% c(1, 3), 1, 2))

# Make factors
expt1_qaire_tib <- expt1_qaire_tib %>%
  dplyr::mutate(
    group_id = forcats::as_factor(group_id),
    time_id = forcats::as_factor(time_id),
    scale_id = forcats::as_factor(scale_id),
    choice_id = forcats::as_factor(choice_id),
    fb_id = forcats::as_factor(fb_id)
  )

# Tibbles for intrinsic motivation (im), perceived competence (pc), and
# perceived autonomy (pa) at participant (p) and group (g) levels
expt1_qaire_pa_p <- expt1_qaire_tib %>%
  dplyr::filter(scale_id == "pa")

expt1_qaire_pc_p <- expt1_qaire_tib %>%
  dplyr::filter(scale_id == "pc")

expt1_qaire_im_p <- expt1_qaire_tib %>%
  dplyr::filter(scale_id == "im")

expt1_qaire_pa_g <- expt1_qaire_pa_p %>%
  dplyr::group_by(group_id, choice_id, fb_id, time_id) %>%
  dplyr::summarize(
    n = n(),
    mean_rating = mean(score, na.rm = TRUE),
    ci_low = ggplot2::mean_cl_boot(score)$ymin,
    ci_upp = ggplot2::mean_cl_boot(score)$ymax,
    .groups = "drop"
  )

expt1_qaire_pc_g <- expt1_qaire_pc_p %>%
  dplyr::group_by(group_id, choice_id, fb_id, time_id) %>%
  dplyr::summarize(
    n = n(),
    mean_rating = mean(score, na.rm = TRUE),
    ci_low = ggplot2::mean_cl_boot(score)$ymin,
    ci_upp = ggplot2::mean_cl_boot(score)$ymax,
    .groups = "drop"
  )

expt1_qaire_im_g <- expt1_qaire_im_p %>%
  dplyr::group_by(group_id, choice_id, fb_id, time_id) %>%
  dplyr::summarize(
    n = n(),
    mean_rating = mean(score, na.rm = TRUE),
    ci_low = ggplot2::mean_cl_boot(score)$ymin,
    ci_upp = ggplot2::mean_cl_boot(score)$ymax,
    .groups = "drop"
  )


# EXPERIMENT 2
#
# PERFORMANCE DATA
#
# Add columns for blocks, and ce^2. For blocks, we will collapse across the
# two different pre-tests for now.
expt2_performance_tib <- expt2_performance_data %>%
  dplyr::mutate(block_id = rep(rep(1:9, each = 12), 76)) %>%
  dplyr::mutate(ce2_deg = dplyr::if_else(phase_id != 4, (xi_deg - 40)^2,
                                         (xi_deg - 60)^2)) %>%
  dplyr::mutate(ce2_ms = (xi_ms - 225)^2)

# Make factors
expt2_performance_tib <- expt2_performance_tib %>%
  dplyr::mutate(
    group_id = forcats::as_factor(group_id),
    phase_id = forcats::as_factor(phase_id),
    block_id = forcats::as_factor(block_id)
  )

# Tibble for pre-test, retention, and transfer data at the participant (p)
# and group (g) levels
expt2_performance_prt_p <- expt2_performance_tib %>%
  dplyr::filter(block_id %in% c(1, 8, 9)) %>%
  dplyr::group_by(id, group_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    p_mean_e_deg = sqrt(mean(ce2_deg, na.rm = TRUE)),
    p_mean_e_ms = sqrt(mean(ce2_ms, na.rm = TRUE)),
    .groups = "drop"
  )

expt2_performance_prt_g <- expt2_performance_prt_p %>%
  dplyr::group_by(group_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    g_mean_e_deg = mean(p_mean_e_deg, na.rm = TRUE),
    ci_low_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymin,
    ci_upp_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymax,
    g_mean_e_ms = mean(p_mean_e_ms, na.rm = TRUE),
    ci_low_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymin,
    ci_upp_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymax,
    .groups = "drop"
  )

# Tibble for acquisition at the participant (p) and group (g) levels
expt2_performance_acq_p <- expt2_performance_tib %>%
  dplyr::filter(block_id %in% c(2:7)) %>%
  dplyr::group_by(id, group_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    p_mean_e_deg = sqrt(mean(ce2_deg, na.rm = TRUE)),
    p_mean_e_ms = sqrt(mean(ce2_ms, na.rm = TRUE)),
    .groups = "drop"
  )

expt2_performance_acq_g <- expt2_performance_acq_p %>%
  dplyr::group_by(group_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    g_mean_e_deg = mean(p_mean_e_deg, na.rm = TRUE),
    ci_low_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymin,
    ci_upp_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymax,
    g_mean_e_ms = mean(p_mean_e_ms, na.rm = TRUE),
    ci_low_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymin,
    ci_upp_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymax,
    .groups = "drop"
  )

# Number of hits during acquisition at the participant (p) and group (g) levels
expt2_deg_hit_p <- expt2_performance_tib %>%
  dplyr::filter(phase_id == 2) %>%
  dplyr::mutate(hit = dplyr::if_else(
    xi_deg > 39.99 & xi_deg < 40.01, 1, 0)) %>%
  dplyr::group_by(id, group_id) %>%
  dplyr::summarize(
    n = n(),
    sum_hit = sum(hit, na.rm = TRUE)
  )

expt2_deg_hit_g <- expt2_deg_hit_p %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarize(
    n = n(),
    total = sum(sum_hit, na.rm = TRUE),
    min = min(sum_hit, na.rm = TRUE),
    max = max(sum_hit, na.rm = TRUE)
  )

expt2_ms_hit_p <- expt2_performance_tib %>%
  dplyr::filter(phase_id == 2) %>%
  dplyr::mutate(hit = dplyr::if_else(xi_ms == 225, 1, 0)) %>%
  dplyr::group_by(id, group_id) %>%
  dplyr::summarize(
    n = n(),
    sum_hit = sum(hit, na.rm = TRUE)
  )

expt2_ms_hit_g <- expt2_ms_hit_p %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarize(
    n = n(),
    total = sum(sum_hit, na.rm = TRUE),
    min = min(sum_hit, na.rm = TRUE),
    max = max(sum_hit, na.rm = TRUE)
  )


# QUESTIONNAIRE DATA
#
# Make factors
expt2_qaire_tib <- expt2_qaire_data %>%
  dplyr::mutate(
    group_id = forcats::as_factor(group_id),
    time_id = forcats::as_factor(time_id),
    scale_id = forcats::as_factor(scale_id)
  )

# Tibbles for intrinsic motivation (im), perceived competence (pc), and
# perceived autonomy (pa) at participant (p) and group (g) levels
expt2_qaire_pa_p <- expt2_qaire_tib %>%
  dplyr::filter(scale_id == "pa")

expt2_qaire_im_p <- expt2_qaire_tib %>%
  dplyr::filter(scale_id == "im")

expt2_qaire_pc_p <- expt2_qaire_tib %>%
  dplyr::filter(scale_id == "pc")

expt2_qaire_pa_g <- expt2_qaire_pa_p %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarize(
    n = n(),
    mean_rating = mean(score, na.rm = TRUE),
    ci_low = ggplot2::mean_cl_boot(score)$ymin,
    ci_upp = ggplot2::mean_cl_boot(score)$ymax,
    .groups = "drop"
  )

expt2_qaire_im_g <- expt2_qaire_im_p %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarize(
    n = n(),
    mean_rating = mean(score, na.rm = TRUE),
    ci_low = ggplot2::mean_cl_boot(score)$ymin,
    ci_upp = ggplot2::mean_cl_boot(score)$ymax,
    .groups = "drop"
  )

expt2_qaire_pc_g <- expt2_qaire_pc_p %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarize(
    n = n(),
    mean_rating = mean(score, na.rm = TRUE),
    ci_low = ggplot2::mean_cl_boot(score)$ymin,
    ci_upp = ggplot2::mean_cl_boot(score)$ymax,
    .groups = "drop"
  )


# CRONBACH'S ALPHA
#
# EXPERIMENT 1
#
# After pre-test
# Create tibble extracting after pre-test (t1)
expt1_cronbach_pre <- expt1_cronbach_data %>%
  dplyr::select(1:19)

# Getting each construct
expt1_pa_pre_tib <- dplyr::select(
  expt1_cronbach_pre, 5, 10, 14, 18
  )
expt1_pc_pre_tib <- dplyr::select(
  expt1_cronbach_pre, 4, 6, 8, 11, 13, 16
  )
expt1_im_pre_tib <- dplyr::select(
  expt1_cronbach_pre, 3, 7, 9, 12, 15, 17, 19
  )

# After trial 12 (block 1 - b1)
# Create tibble extracting after trial 12 (t2)
expt1_cronbach_b1 <- expt1_cronbach_data %>%
  dplyr::select(1, 2, 20:36)

# Getting each construct
expt1_pa_b1_tib <- dplyr::select(
  expt1_cronbach_b1, 5, 10, 14, 18
  )
expt1_pc_b1_tib <- dplyr::select(
  expt1_cronbach_b1, 4, 6, 8, 11, 13, 16
  )
expt1_im_b1_tib <- dplyr::select(
  expt1_cronbach_b1, 3, 7, 9, 12, 15, 17, 19
  )

# After trial 72 (block 6 - b6)
# Create tibble extracting after trial 72 (t3)
expt1_cronbach_b6 <- expt1_cronbach_data %>%
  dplyr::select(1, 2, 37:53)

# Getting each construct
expt1_pa_b6_tib <- dplyr::select(
  expt1_cronbach_b6, 5, 10, 14, 18
  )
expt1_pc_b6_tib <- dplyr::select(
  expt1_cronbach_b6, 4, 6, 8, 11, 13, 16
  )
expt1_im_b6_tib <- dplyr::select(
  expt1_cronbach_b6, 3, 7, 9, 12, 15, 17, 19
  )

# Before 24-h retention
# Create tibble extracting before retention (t4)
expt1_cronbach_ret <- expt1_cronbach_data %>%
  dplyr::select(1, 2, 54:70)

# Getting each construct
expt1_pa_ret_tib <- dplyr::select(
  expt1_cronbach_ret, 5, 10, 14, 18
  )
expt1_pc_ret_tib <- dplyr::select(
  expt1_cronbach_ret, 4, 6, 8, 11, 13, 16
  )
expt1_im_ret_tib <- dplyr::select(
  expt1_cronbach_ret, 3, 7, 9, 12, 15, 17, 19
  )

# EXPERIMENT 2
#
# After pre-test
# Create tibble extracting after pre-test (t1)
expt2_cronbach_pre <- expt2_cronbach_data %>%
  dplyr::select(1:19)

# Getting each construct
expt2_pa_pre_tib <- dplyr::select(
  expt2_cronbach_pre, 5, 10, 14, 18
  )
expt2_pc_pre_tib <- dplyr::select(
  expt2_cronbach_pre, 4, 6, 8, 11, 13, 16
  )
expt2_im_pre_tib <- dplyr::select(
  expt2_cronbach_pre, 3, 7, 9, 12, 15, 17, 19
  )

# After trial 12 (block 1 - b1)
# Create tibble extracting after trial 12 (t2)
expt2_cronbach_b1 <- expt2_cronbach_data %>%
  dplyr::select(1, 2, 20:36)

# Getting each construct
expt2_pa_b1_tib <- dplyr::select(
  expt2_cronbach_b1, 5, 10, 14, 18
  )
expt2_pc_b1_tib <- dplyr::select(
  expt2_cronbach_b1, 4, 6, 8, 11, 13, 16
  )
expt2_im_b1_tib <- dplyr::select(
  expt2_cronbach_b1, 3, 7, 9, 12, 15, 17, 19
  )

# After trial 72 (block 6 - b6)
# Create tibble extracting after trial 72 (t3)
expt2_cronbach_b6 <- expt2_cronbach_data %>%
  dplyr::select(1, 2, 37:53)

# Getting each construct
expt2_pa_b6_tib <- dplyr::select(
  expt2_cronbach_b6, 5, 10, 14, 18
  )
expt2_pc_b6_tib <- dplyr::select(
  expt2_cronbach_b6, 4, 6, 8, 11, 13, 16
  )
expt2_im_b6_tib <- dplyr::select(
  expt2_cronbach_b6, 3, 7, 9, 12, 15, 17, 19
  )

# Before 24-h retention
# Create tibble extracting before retention (t4)
expt2_cronbach_ret <- expt2_cronbach_data %>%
  dplyr::select(1, 2, 54:70)

# Getting each construct
expt2_pa_ret_tib <- dplyr::select(
  expt2_cronbach_ret, 5, 10, 14, 18
  )
expt2_pc_ret_tib <- dplyr::select(
  expt2_cronbach_ret, 4, 6, 8, 11, 13, 16
  )
expt2_im_ret_tib <- dplyr::select(
  expt2_cronbach_ret, 3, 7, 9, 12, 15, 17, 19
  )


# EQUIVALENCE TESTS
#
# Compute means and SDs at retention
#
# EXPERIMENT 1
#
# Collapse across fb_id before calculations
#
expt1_choice_g_ret <- expt1_performance_prt_p %>%
  dplyr::filter(block_id == 8) %>%
  dplyr::group_by(choice_id) %>%
  dplyr::summarize(
    mean_deg = mean(p_mean_e_deg, na.rm = TRUE),
    sd_deg = sd(p_mean_e_deg, na.rm = TRUE),
    mean_ms = mean(p_mean_e_ms, na.rm = TRUE),
    sd_ms = sd(p_mean_e_ms, na.rm = TRUE)
  )

# EXPERIMENT 2
#
expt2_choice_g_ret <- expt2_performance_prt_p %>%
  dplyr::filter(block_id == 8) %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarize(
    mean_deg = mean(p_mean_e_deg, na.rm = TRUE),
    sd_deg = sd(p_mean_e_deg, na.rm = TRUE),
    mean_ms = mean(p_mean_e_ms, na.rm = TRUE),
    sd_ms = sd(p_mean_e_ms, na.rm = TRUE)
  )
