# -------------------------------------------
# CHOICE AND FEEDBACK CHARACTERISTICS PROJECT
# -- St. Germain, McKay, Poskus, Williams, Leshchyshen, Feldman, Cashaback,
# and Carter
#
# Supplementary C (Questionnaire) - Wrangle and analyses
#
# Authors:
#   Laura St. Germain
#   Brad McKay
#   Mike Carter
#
# Last update: August 13 2022
#
# Website: https://www.cartermaclab.org
# -------------------------------------------


# SCRIPT SETUP ----
#
# Required libraries
source("scripts/main_wrangle.R")

# Seed for reproducible bootstrapped confidence intervals
set.seed(8693)


# WRANGLE: DATA SETUP ----
#
# EXPERIMENT 1
#
# Create tibbles to work with. Collapse across feedback to get mean and SD
# for choice and yoked
#
# Perceived autonomy
expt1_qaire_choice_pa_g <- expt1_qaire_pa_p %>%
  dplyr::group_by(choice_id, time_id) %>%
  dplyr::summarise(
    mean_rating = mean(score, na.rm = TRUE),
    sd_rating = sd(score, na.rm = TRUE)
  )

# Perceived competence
expt1_qaire_choice_pc_g <- expt1_qaire_pc_p %>%
  dplyr::group_by(choice_id, time_id) %>%
  dplyr::summarise(
    mean_rating = mean(score, na.rm = TRUE),
    sd_rating = sd(score, na.rm = TRUE)
  )

# Intrinsic motivation
expt1_qaire_choice_im_g <- expt1_qaire_im_p %>%
  dplyr::group_by(choice_id, time_id) %>%
  dplyr::summarise(
    mean_rating = mean(score, na.rm = TRUE),
    sd_rating = sd(score, na.rm = TRUE)
  )


# EXPERIMENT 2
#
# Create tibbles to work with. Collapse across feedback to get mean and SD
# for choice and yoked
#
# Perceived autonomy
expt2_qaire_choice_pa_g <- expt2_qaire_pa_p %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarise(
    mean_rating = mean(score, na.rm = TRUE),
    sd_rating = sd(score, na.rm = TRUE)
  )

# Perceived competence
expt2_qaire_choice_pc_g <- expt2_qaire_pc_p %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarise(
    mean_rating = mean(score, na.rm = TRUE),
    sd_rating = sd(score, na.rm = TRUE)
  )

# Intrinsic motivation
expt2_qaire_choice_im_g <- expt2_qaire_im_p %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarise(
    mean_rating = mean(score, na.rm = TRUE),
    sd_rating = sd(score, na.rm = TRUE)
  )


# ANALYSES ----
#
# EXPERIMENT 1
#
# Calculate hedge's g and 90% confidence intervals for each construct at
# each time point
#
# Perceived autonomy
#
# After pre-test
expt1_pa_es_t1 <- compute.es::mes(
  m.1 = expt1_qaire_choice_pa_g$mean_rating[1],
  m.2 = expt1_qaire_choice_pa_g$mean_rating[5],
  sd.1 = expt1_qaire_choice_pa_g$sd_rating[1],
  sd.2 = expt1_qaire_choice_pa_g$sd_rating[5],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 1
expt1_pa_es_t2 <- compute.es::mes(
  m.1 = expt1_qaire_choice_pa_g$mean_rating[2],
  m.2 = expt1_qaire_choice_pa_g$mean_rating[6],
  sd.1 = expt1_qaire_choice_pa_g$sd_rating[2],
  sd.2 = expt1_qaire_choice_pa_g$sd_rating[6],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 6
expt1_pa_es_t3 <- compute.es::mes(
  m.1 = expt1_qaire_choice_pa_g$mean_rating[3],
  m.2 = expt1_qaire_choice_pa_g$mean_rating[7],
  sd.1 = expt1_qaire_choice_pa_g$sd_rating[3],
  sd.2 = expt1_qaire_choice_pa_g$sd_rating[7],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# Before retention
expt1_pa_es_t4 <- compute.es::mes(
  m.1 = expt1_qaire_choice_pa_g$mean_rating[4],
  m.2 = expt1_qaire_choice_pa_g$mean_rating[8],
  sd.1 = expt1_qaire_choice_pa_g$sd_rating[4],
  sd.2 = expt1_qaire_choice_pa_g$sd_rating[8],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# Perceived competence
#
# After pre-test
expt1_pc_es_t1 <- compute.es::mes(
  m.1 = expt1_qaire_choice_pc_g$mean_rating[1],
  m.2 = expt1_qaire_choice_pc_g$mean_rating[5],
  sd.1 = expt1_qaire_choice_pc_g$sd_rating[1],
  sd.2 = expt1_qaire_choice_pc_g$sd_rating[5],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 1
expt1_pc_es_t2 <- compute.es::mes(
  m.1 = expt1_qaire_choice_pc_g$mean_rating[2],
  m.2 = expt1_qaire_choice_pc_g$mean_rating[6],
  sd.1 = expt1_qaire_choice_pc_g$sd_rating[2],
  sd.2 = expt1_qaire_choice_pc_g$sd_rating[6],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 6
expt1_pc_es_t3 <- compute.es::mes(
  m.1 = expt1_qaire_choice_pc_g$mean_rating[3],
  m.2 = expt1_qaire_choice_pc_g$mean_rating[7],
  sd.1 = expt1_qaire_choice_pc_g$sd_rating[3],
  sd.2 = expt1_qaire_choice_pc_g$sd_rating[7],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# Before retention
expt1_pc_es_t4 <- compute.es::mes(
  m.1 = expt1_qaire_choice_pc_g$mean_rating[4],
  m.2 = expt1_qaire_choice_pc_g$mean_rating[8],
  sd.1 = expt1_qaire_choice_pc_g$sd_rating[4],
  sd.2 = expt1_qaire_choice_pc_g$sd_rating[8],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# Intrinsic motivation
#
# After pre-test
expt1_im_es_t1 <- compute.es::mes(
  m.1 = expt1_qaire_choice_im_g$mean_rating[1],
  m.2 = expt1_qaire_choice_im_g$mean_rating[5],
  sd.1 = expt1_qaire_choice_im_g$sd_rating[1],
  sd.2 = expt1_qaire_choice_im_g$sd_rating[5],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 1
expt1_im_es_t2 <- compute.es::mes(
  m.1 = expt1_qaire_choice_im_g$mean_rating[2],
  m.2 = expt1_qaire_choice_im_g$mean_rating[6],
  sd.1 = expt1_qaire_choice_im_g$sd_rating[2],
  sd.2 = expt1_qaire_choice_im_g$sd_rating[6],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 6
expt1_im_es_t3 <- compute.es::mes(
  m.1 = expt1_qaire_choice_im_g$mean_rating[3],
  m.2 = expt1_qaire_choice_im_g$mean_rating[7],
  sd.1 = expt1_qaire_choice_im_g$sd_rating[3],
  sd.2 = expt1_qaire_choice_im_g$sd_rating[7],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# Before retention
expt1_im_es_t4 <- compute.es::mes(
  m.1 = expt1_qaire_choice_im_g$mean_rating[4],
  m.2 = expt1_qaire_choice_im_g$mean_rating[8],
  sd.1 = expt1_qaire_choice_im_g$sd_rating[4],
  sd.2 = expt1_qaire_choice_im_g$sd_rating[8],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# EXPERIMENT 2
#
# Calculate hedge's g and 90% confidence intervals for each construct at
# each time point
#
# Perceived autonomy
#
# After pre-test
expt2_pa_es_t1 <- compute.es::mes(
  m.1 = expt2_qaire_choice_pa_g$mean_rating[1],
  m.2 = expt2_qaire_choice_pa_g$mean_rating[5],
  sd.1 = expt2_qaire_choice_pa_g$sd_rating[1],
  sd.2 = expt2_qaire_choice_pa_g$sd_rating[5],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 1
expt2_pa_es_t2 <- compute.es::mes(
  m.1 = expt2_qaire_choice_pa_g$mean_rating[2],
  m.2 = expt2_qaire_choice_pa_g$mean_rating[6],
  sd.1 = expt2_qaire_choice_pa_g$sd_rating[2],
  sd.2 = expt2_qaire_choice_pa_g$sd_rating[6],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 6
expt2_pa_es_t3 <- compute.es::mes(
  m.1 = expt2_qaire_choice_pa_g$mean_rating[3],
  m.2 = expt2_qaire_choice_pa_g$mean_rating[7],
  sd.1 = expt2_qaire_choice_pa_g$sd_rating[3],
  sd.2 = expt2_qaire_choice_pa_g$sd_rating[7],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# Before retention
expt2_pa_es_t4 <- compute.es::mes(
  m.1 = expt2_qaire_choice_pa_g$mean_rating[4],
  m.2 = expt2_qaire_choice_pa_g$mean_rating[8],
  sd.1 = expt2_qaire_choice_pa_g$sd_rating[4],
  sd.2 = expt2_qaire_choice_pa_g$sd_rating[8],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# Perceived competence
#
# After pre-test
expt2_pc_es_t1 <- compute.es::mes(
  m.1 = expt2_qaire_choice_pc_g$mean_rating[1],
  m.2 = expt2_qaire_choice_pc_g$mean_rating[5],
  sd.1 = expt2_qaire_choice_pc_g$sd_rating[1],
  sd.2 = expt2_qaire_choice_pc_g$sd_rating[5],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 1
expt2_pc_es_t2 <- compute.es::mes(
  m.1 = expt2_qaire_choice_pc_g$mean_rating[2],
  m.2 = expt2_qaire_choice_pc_g$mean_rating[6],
  sd.1 = expt2_qaire_choice_pc_g$sd_rating[2],
  sd.2 = expt2_qaire_choice_pc_g$sd_rating[6],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 6
expt2_pc_es_t3 <- compute.es::mes(
  m.1 = expt2_qaire_choice_pc_g$mean_rating[3],
  m.2 = expt2_qaire_choice_pc_g$mean_rating[7],
  sd.1 = expt2_qaire_choice_pc_g$sd_rating[3],
  sd.2 = expt2_qaire_choice_pc_g$sd_rating[7],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# Before retention
expt2_pc_es_t4 <- compute.es::mes(
  m.1 = expt2_qaire_choice_pc_g$mean_rating[4],
  m.2 = expt2_qaire_choice_pc_g$mean_rating[8],
  sd.1 = expt2_qaire_choice_pc_g$sd_rating[4],
  sd.2 = expt2_qaire_choice_pc_g$sd_rating[8],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# Intrinsic motivation
#
# After pre-test
expt2_im_es_t1 <- compute.es::mes(
  m.1 = expt2_qaire_choice_im_g$mean_rating[1],
  m.2 = expt2_qaire_choice_im_g$mean_rating[5],
  sd.1 = expt2_qaire_choice_im_g$sd_rating[1],
  sd.2 = expt2_qaire_choice_im_g$sd_rating[5],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 1
expt2_im_es_t2 <- compute.es::mes(
  m.1 = expt2_qaire_choice_im_g$mean_rating[2],
  m.2 = expt2_qaire_choice_im_g$mean_rating[6],
  sd.1 = expt2_qaire_choice_im_g$sd_rating[2],
  sd.2 = expt2_qaire_choice_im_g$sd_rating[6],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# After block 6
expt2_im_es_t3 <- compute.es::mes(
  m.1 = expt2_qaire_choice_im_g$mean_rating[3],
  m.2 = expt2_qaire_choice_im_g$mean_rating[7],
  sd.1 = expt2_qaire_choice_im_g$sd_rating[3],
  sd.2 = expt2_qaire_choice_im_g$sd_rating[7],
  n.1 = 76,
  n.2 = 76,
  level = 90
)

# Before retention
expt2_im_es_t4 <- compute.es::mes(
  m.1 = expt2_qaire_choice_im_g$mean_rating[4],
  m.2 = expt2_qaire_choice_im_g$mean_rating[8],
  sd.1 = expt2_qaire_choice_im_g$sd_rating[4],
  sd.2 = expt2_qaire_choice_im_g$sd_rating[8],
  n.1 = 76,
  n.2 = 76,
  level = 90
)
