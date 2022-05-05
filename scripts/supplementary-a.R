# -------------------------------------------
# CHOICE AND FEEDBACK CHARACTERISTICS PROJECT
# -- St. Germain, McKay, Poskus, Williams, Leshchyshen, Feldman, Cashaback,
# and Carter
#
# Supplementary A (Error estimation) - Wrangle, analyses, and figures
#
# Authors:
#   Laura St. Germain
#   Brad McKay
#   Mike Carter
#
# Last update: April 28 2022
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
expt1_estimation_data <- readr::read_csv("data/expt1_estimation-data.csv")
expt2_estimation_data <- readr::read_csv("data/expt2_estimation-data.csv")

# Function to calculate variable error (VE)
ve <- function(x){
  sd(x, na.rm = TRUE) * (sqrt((length(x) - 1) / length(x)))
}


# WRANGLE: DATA SETUP ----
#
# EXPERIMENT 1
#
# Add columns for choice, feedback, and constant error for each estimation and
# actual performance for timing and spatial components
expt1_estimation_tib <- expt1_estimation_data %>%
  dplyr::mutate(choice_id = dplyr::if_else(group_id <= 2, 1, 2)) %>%
  dplyr::mutate(fb_id = dplyr::if_else(group_id %in% c(1, 3), 1, 2)) %>%
  dplyr::mutate(ce_deg = ee_deg - xi_deg) %>%
  dplyr::mutate(ce_ms = ee_ms - xi_ms)

# Make factors
expt1_estimation_tib <- expt1_estimation_tib %>%
  dplyr::mutate(
    group_id = forcats::as_factor(group_id),
    phase_id = forcats::as_factor(phase_id),
    choice_id = forcats::as_factor(choice_id),
    fb_id = forcats::as_factor(fb_id)
  )

# Create tibbles to work with. One with all participants for retention and
# transfer and one with subset of participants that estimated in pre-test
expt1_estimation_all <- expt1_estimation_tib %>%
  dplyr::filter(phase_id != 0)

expt1_pre_ids <- expt1_estimation_tib %>%
  dplyr::filter(phase_id == 0) %>%
  dplyr::pull(sub_id) %>%
  dplyr::as_tibble() %>%
  dplyr::distinct()

expt1_estimation_pre <- expt1_estimation_tib %>%
  dplyr::filter(sub_id %in% expt1_pre_ids$value)

# Tibble calculating mean constant error and variable error for retention
# and transfer at the participant (p) and group (g) levels
expt1_estimation_all_p <- expt1_estimation_all %>%
  dplyr::group_by(sub_id, group_id, choice_id, fb_id, phase_id) %>%
  dplyr::summarize(
    n = n(),
    mean_ce_deg = mean(ce_deg, na.rm = TRUE),
    mean_ve_deg = ve(ce_deg),
    mean_ce_ms = mean(ce_ms, na.rm = TRUE),
    mean_ve_ms = ve(ce_ms),
    .groups = "drop"
  ) %>%
  dplyr::group_by(sub_id, group_id, choice_id, fb_id, phase_id) %>%
  dplyr::summarise(
    p_mean_ee_deg = sqrt(mean_ce_deg^2 + mean_ve_deg^2),
    p_mean_ee_ms = sqrt(mean_ce_ms^2 + mean_ve_ms^2),
    .groups = "drop"
  )

expt1_estimation_all_g <- expt1_estimation_all_p %>%
  dplyr::group_by(group_id, choice_id, fb_id, phase_id) %>%
  dplyr::summarize(
    g_mean_ee_deg = mean(p_mean_ee_deg, na.rm = TRUE),
    ci_low_deg = ggplot2::mean_cl_boot(p_mean_ee_deg)$ymin,
    ci_upp_deg = ggplot2::mean_cl_boot(p_mean_ee_deg)$ymax,
    g_mean_ee_ms = mean(p_mean_ee_ms, na.rm = TRUE),
    ci_low_ms = ggplot2::mean_cl_boot(p_mean_ee_ms)$ymin,
    ci_upp_ms = ggplot2::mean_cl_boot(p_mean_ee_ms)$ymax,
    .groups = "drop"
  )

# Tibble calculating mean constant error and variable error for subset that
# estimated in pre-test at the participant (p) and group (g) levels
expt1_estimation_pre_p <- expt1_estimation_pre %>%
  dplyr::group_by(sub_id, group_id, choice_id, fb_id, phase_id) %>%
  dplyr::summarize(
    n = n(),
    mean_ce_deg = mean(ce_deg, na.rm = TRUE),
    mean_ve_deg = ve(ce_deg),
    mean_ce_ms = mean(ce_ms, na.rm = TRUE),
    mean_ve_ms = ve(ce_ms),
    .groups = "drop"
  ) %>%
  dplyr::group_by(sub_id, group_id, choice_id, fb_id, phase_id) %>%
  dplyr::summarize(
    p_mean_ee_deg = sqrt(mean_ce_deg^2 + mean_ve_deg^2),
    p_mean_ee_ms = sqrt(mean_ce_ms^2 + mean_ve_ms^2),
    .groups = "drop"
  )

expt1_estimation_pre_g <- expt1_estimation_pre_p %>%
  dplyr::group_by(group_id, choice_id, fb_id, phase_id) %>%
  dplyr::summarise(
    g_mean_ee_deg = mean(p_mean_ee_deg, na.rm = TRUE),
    ci_low_deg = ggplot2::mean_cl_boot(p_mean_ee_deg)$ymin,
    ci_upp_deg = ggplot2::mean_cl_boot(p_mean_ee_deg)$ymax,
    g_mean_ee_ms = mean(p_mean_ee_ms, na.rm = TRUE),
    ci_low_ms = ggplot2::mean_cl_boot(p_mean_ee_ms)$ymin,
    ci_upp_ms = ggplot2::mean_cl_boot(p_mean_ee_ms)$ymax,
    .groups = "drop"
  )


# EXPERIMENT 2
#
# Add columns for constant error for each estimation and actual performance
# for timing and spatial components
expt2_estimation_tib <- expt2_estimation_data %>%
  dplyr::mutate(ce_deg = ee_deg - xi_deg) %>%
  dplyr::mutate(ce_ms = ee_ms - xi_ms)

# Make factors
expt2_estimation_tib <- expt2_estimation_tib %>%
  dplyr::mutate(
    group_id = forcats::as_factor(group_id),
    phase_id = forcats::as_factor(phase_id)
  )

# Create tibbles to work with. One with all participants for retention and
# transfer and one with subset of participants that estimated in pre-test
expt2_estimation_all <- expt2_estimation_tib %>%
  dplyr::filter(phase_id != 0)

expt2_pre_ids <- expt2_estimation_tib %>%
  dplyr::filter(phase_id == 0) %>%
  dplyr::pull(sub_id) %>%
  dplyr::as_tibble() %>%
  dplyr::distinct()

expt2_estimation_pre <- expt2_estimation_tib %>%
  dplyr::filter(sub_id %in% expt2_pre_ids$value)

# Tibble calculating mean constant error and variable error for retention
# and transfer at the participant (p) and group (g) levels
expt2_estimation_all_p <- expt2_estimation_all %>%
  dplyr::group_by(sub_id, group_id, phase_id) %>%
  dplyr::summarize(
    n = n(),
    mean_ce_deg = mean(ce_deg, na.rm = TRUE),
    mean_ve_deg = ve(ce_deg),
    mean_ce_ms = mean(ce_ms, na.rm = TRUE),
    mean_ve_ms = ve(ce_ms),
    .groups = "drop"
  ) %>%
  dplyr::group_by(sub_id, group_id, phase_id) %>%
  dplyr::summarise(
    p_mean_ee_deg = sqrt(mean_ce_deg^2 + mean_ve_deg^2),
    p_mean_ee_ms = sqrt(mean_ce_ms^2 + mean_ve_ms^2),
    .groups = "drop"
  )

expt2_estimation_all_g <- expt2_estimation_all_p %>%
  dplyr::group_by(group_id, phase_id) %>%
  dplyr::summarize(
    g_mean_ee_deg = mean(p_mean_ee_deg, na.rm = TRUE),
    ci_low_deg = ggplot2::mean_cl_boot(p_mean_ee_deg)$ymin,
    ci_upp_deg = ggplot2::mean_cl_boot(p_mean_ee_deg)$ymax,
    g_mean_ee_ms = mean(p_mean_ee_ms, na.rm = TRUE),
    ci_low_ms = ggplot2::mean_cl_boot(p_mean_ee_ms)$ymin,
    ci_upp_ms = ggplot2::mean_cl_boot(p_mean_ee_ms)$ymax,
    .groups = "drop"
  )

# Tibble calculating mean constant error and variable error for subset that
# estimated in pre-test at the participant (p) and group (g) levels
expt2_estimation_pre_p <- expt2_estimation_pre %>%
  dplyr::group_by(sub_id, group_id, phase_id) %>%
  dplyr::summarize(
    n = n(),
    mean_ce_deg = mean(ce_deg, na.rm = TRUE),
    mean_ve_deg = ve(ce_deg),
    mean_ce_ms = mean(ce_ms, na.rm = TRUE),
    mean_ve_ms = ve(ce_ms),
    .groups = "drop"
  ) %>%
  dplyr::group_by(sub_id, group_id, phase_id) %>%
  dplyr::summarize(
    p_mean_ee_deg = sqrt(mean_ce_deg^2 + mean_ve_deg^2),
    p_mean_ee_ms = sqrt(mean_ce_ms^2 + mean_ve_ms^2),
    .groups = "drop"
  )

expt2_estimation_pre_g <- expt2_estimation_pre_p %>%
  dplyr::group_by(group_id, phase_id) %>%
  dplyr::summarise(
    g_mean_ee_deg = mean(p_mean_ee_deg, na.rm = TRUE),
    ci_low_deg = ggplot2::mean_cl_boot(p_mean_ee_deg)$ymin,
    ci_upp_deg = ggplot2::mean_cl_boot(p_mean_ee_deg)$ymax,
    g_mean_ee_ms = mean(p_mean_ee_ms, na.rm = TRUE),
    ci_low_ms = ggplot2::mean_cl_boot(p_mean_ee_ms)$ymin,
    ci_upp_ms = ggplot2::mean_cl_boot(p_mean_ee_ms)$ymax,
    .groups = "drop"
  )



# ANALYSES: RETENTION AND TRANSFER ----
#
# EXPERIMENT 1
#
# Spatial goal
#
# Omnibus test
expt1_ee_all_deg_lm <- afex::aov_ez(
  "sub_id", "p_mean_ee_deg", expt1_estimation_all_p,
  between = c("choice_id", "fb_id"),
  within = "phase_id"
)
expt1_ee_all_deg_lm

# Timing goal
#
# Omnibus test
expt1_ee_all_ms_lm <- afex::aov_ez(
  "sub_id", "p_mean_ee_ms", expt1_estimation_all_p,
  between = c("choice_id", "fb_id"),
  within = "phase_id"
)
expt1_ee_all_ms_lm


# EXPERIMENT 2
#
# Spatial goal
#
# Omnibus test
expt2_ee_all_deg_lm <- afex::aov_ez(
  "sub_id", "p_mean_ee_deg", expt2_estimation_all_p,
  between = "group_id",
  within = "phase_id"
)
expt2_ee_all_deg_lm

# Timing goal
#
# Omnibus test
expt2_ee_all_ms_lm <- afex::aov_ez(
  "sub_id", "p_mean_ee_ms", expt2_estimation_all_p,
  between = "group_id",
  within = "phase_id"
)
expt2_ee_all_ms_lm


# ANALYSES: SUBSET OF PARTICIPANTS THAT ESTIMATED IN PRE-TEST
#
# EXPERIMENT 1
#
# Spatial goal
#
# Omnibus test
expt1_ee_pre_deg_lm <- afex::aov_ez(
  "sub_id", "p_mean_ee_deg", expt1_estimation_pre_p,
  between = c("choice_id", "fb_id"),
  within = "phase_id"
)
expt1_ee_pre_deg_lm

# Post hoc main effect of Test (phase_id)
expt1_ee_pre_deg_me_test <- emmeans::emmeans(
  expt1_ee_pre_deg_lm, ~ phase_id
)
pairs(expt1_ee_pre_deg_me_test, adjust = "holm")

# Timing goal
#
# Omnibus test
expt1_ee_pre_ms_lm <- afex::aov_ez(
  "sub_id", "p_mean_ee_ms", expt1_estimation_pre_p,
  between = c("choice_id", "fb_id"),
  within = "phase_id"
)
expt1_ee_pre_ms_lm

# Post hoc main effect of Test (phase_id)
expt1_ee_pre_ms_me_test <- emmeans::emmeans(
  expt1_ee_pre_ms_lm, ~ phase_id
)
pairs(expt1_ee_pre_ms_me_test, adjust = "holm")


# EXPERIMENT 2
#
# Spatial goal
#
# Omnibus test
expt2_ee_pre_deg_lm <- afex::aov_ez(
  "sub_id", "p_mean_ee_deg", expt2_estimation_pre_p,
  between = "group_id",
  within = "phase_id"
)
expt2_ee_pre_deg_lm

# Timing goal
#
# Omnibus test
expt2_ee_pre_ms_lm <- afex::aov_ez(
  "sub_id", "p_mean_ee_ms", expt2_estimation_pre_p,
  between = "group_id",
  within = "phase_id"
)
expt2_ee_pre_ms_lm



# FIGURES: RETENTION AND TRANSFER
#
# Color and theme setup
color_theme1 <- c("#5e81ac","#88c0d0", "#bf616a", "#d08770")
color_theme2 <- c("#a3be8c", "#b48ead")

theme_set(
  theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "bold")
    )
)

# EXPERIMENT 1
#
# Create separate tibbles for each group at the participant level that will
# be used for multipanel boxplot figure
expt1_sce_ee_p <- expt1_estimation_all_p %>%
  dplyr::filter(group_id == 1)

expt1_scg_ee_p <- expt1_estimation_all_p %>%
  dplyr::filter(group_id == 2)

expt1_yke_ee_p <- expt1_estimation_all_p %>%
  dplyr::filter(group_id == 3)

expt1_ykg_ee_p <- expt1_estimation_all_p %>%
  dplyr::filter(group_id == 4)


# Spatial goal
figS1_sce_deg <- ggplot2::ggplot(
  expt1_sce_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_deg,
      group = phase_id)
  ) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme1[1]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = "Spatial error estimation (deg)",
    limits = c(0, 80),
    breaks = seq(0, 80, 20)
  ) +
  labs(
    title = "A. Experiment 1",
    subtitle = "Choice + Error"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank()
  )

figS1_scg_deg <- ggplot2::ggplot(
  expt1_scg_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_deg,
      group = phase_id)
  ) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme1[2]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(0, 80),
    breaks = seq(0, 80, 20)
  ) +
  labs(
    title = "",
    subtitle = "Choice + Graded"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank()
  )

figS1_yke_deg <- ggplot2::ggplot(
  expt1_yke_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_deg,
      group = phase_id)
) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme1[3]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = "Spatial error estimation (deg)",
    limits = c(0, 80),
    breaks = seq(0, 80, 20)
  ) +
  labs(
    title = "",
    subtitle = "Yoked + Error"
  ) +
  theme(
    legend.position = "none"
  )

figS1_ykg_deg <- ggplot2::ggplot(
  expt1_ykg_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_deg,
      group = phase_id)
) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme1[4]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(0, 80),
    breaks = seq(0, 80, 20)
  ) +
  labs(
    title = "",
    subtitle = "Yoked + Graded"
  ) +
  theme(
    legend.position = "none"
  )


# Timing goal
figS2_sce_ms <- ggplot2::ggplot(
  expt1_sce_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_ms,
      group = phase_id)
) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme1[1]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = "Timing error estimation (ms)",
    limits = c(0, 500),
    breaks = seq(0, 500, 100)
  ) +
  labs(
    title = "A. Experiment 1",
    subtitle = "Choice + Error"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank()
  )

figS2_scg_ms <- ggplot2::ggplot(
  expt1_scg_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_ms,
      group = phase_id)
) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme1[2]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(0, 500),
    breaks = seq(0, 500, 100)
  ) +
  labs(
    title = "",
    subtitle = "Choice + Graded"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank()
  )

figS2_yke_ms <- ggplot2::ggplot(
  expt1_yke_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_ms,
      group = phase_id)
) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme1[3]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = "Timing error estimation (ms)",
    limits = c(0, 500),
    breaks = seq(0, 500, 100)
  ) +
  labs(
    title = "",
    subtitle = "Yoked + Error"
  ) +
  theme(
    legend.position = "none"
  )

figS2_ykg_ms <- ggplot2::ggplot(
  expt1_ykg_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_ms,
      group = phase_id)
) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme1[4]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(0, 500),
    breaks = seq(0, 500, 100)
  ) +
  labs(
    title = "",
    subtitle = "Yoked + Graded"
  ) +
  theme(
    legend.position = "none"
  )

# EXPERIMENT 2
#
# Create separate tibbles for each group at the participant level that will
# be used for multipanel boxplot figure
expt2_scb_ee_p <- expt2_estimation_all_p %>%
  dplyr::filter(group_id == 5)

expt2_ykb_ee_p <- expt2_estimation_all_p %>%
  dplyr::filter(group_id == 6)

# Spatial goal
figS1_scb_deg <- ggplot2::ggplot(
  expt2_scb_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_deg,
      group = phase_id)
) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme2[1]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(0, 80),
    breaks = seq(0, 80, 20)
  ) +
  labs(
    title = "B. Experiment 2",
    subtitle = "Choice + Binary"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank()
  )

figS1_ykb_deg <- ggplot2::ggplot(
  expt2_ykb_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_deg,
      group = phase_id)
) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme2[2]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(0, 80),
    breaks = seq(0, 80, 20)
  ) +
  labs(
    title = "",
    subtitle = "Yoked + Binary"
  ) +
  theme(
    legend.position = "none"
  )

# Timing goal
figS2_scb_ms <- ggplot2::ggplot(
  expt2_scb_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_ms,
      group = phase_id)
) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme2[1]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(0, 500),
    breaks = seq(0, 500, 100)
  ) +
  labs(
    title = "B. Experiment 2",
    subtitle = "Choice + Binary"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank()
  )

figS2_ykb_ms <- ggplot2::ggplot(
  expt2_ykb_ee_p,
  aes(x = phase_id,
      y = p_mean_ee_ms,
      group = phase_id)
) +
  geom_point(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_line(
    aes(group = sub_id),
    color = "#d8dee9",
    position = position_dodge(0.3)
  ) +
  geom_boxplot(
    lwd = 1,
    fatten = 1,
    alpha = 0,
    color = color_theme2[2]
  ) +
  stat_summary(
    fun = "mean",
    geom = "segment",
    aes(xend = ..x.. + 0.25,
        yend = ..y..),
    position = position_nudge(-0.125),
    size = 1
  ) +
  scale_x_discrete(
    name = NULL,
    labels = c("3" = "Retention",
               "4" = "Transfer")
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(0, 500),
    breaks = seq(0, 500, 100)
  ) +
  labs(
    title = "",
    subtitle = "Yoked + Binary"
  ) +
  theme(
    legend.position = "none"
  )


# MULTIPANEL FIGURES
figS1 <- (figS1_sce_deg / figS1_yke_deg) |
         (figS1_scg_deg / figS1_ykg_deg) |
         (figS1_scb_deg / figS1_ykb_deg)
figS1

figS2 <- (figS2_sce_ms / figS2_yke_ms) |
         (figS2_scg_ms / figS2_ykg_ms) |
         (figS2_scb_ms / figS2_ykb_ms)
figS2
