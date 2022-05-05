# -------------------------------------------
# CHOICE AND FEEDBACK CHARACTERISTICS PROJECT
# -- St. Germain, McKay, Poskus, Williams, Leshchyshen, Feldman, Cashaback,
# and Carter
#
# Supplementary B (Shift functions) - Wrangle, analyses, and figures
#
# Authors:
#   Laura St. Germain
#   Brad McKay
#   Mike Carter
#
# Last update: May 03 2022
#
# Website: https://www.cartermaclab.org
# -------------------------------------------


# SCRIPT SETUP ----
#
# Required libraries
source("scripts/main_wrangle.R")


# DATA SETUP ----
#
# EXPERIMENT 1
#
# Trimmed means
expt1_prt_trim_p <- expt1_performance_tib %>%
  dplyr::filter(phase_id != 2) %>%
  dplyr::group_by(id, group_id, choice_id, fb_id, phase_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    p_tmean_e_deg = sqrt(mean(ce2_deg, trim = 0.2, na.rm = TRUE)),
    p_tmean_e_ms = sqrt(mean(ce2_ms, trim = 0.2, na.rm = TRUE)),
    .groups = "drop"
  )

# Spatial goal and wide format
expt1_prt_trim_deg_wide_p <- expt1_prt_trim_p %>%
  dplyr::select(id, choice_id, fb_id, block_id, p_tmean_e_deg) %>%
  tidyr::pivot_wider(
    names_from = block_id,
    values_from = p_tmean_e_deg) %>%
  # rename phases based on column number
  dplyr::rename(pre = 4) %>%
  dplyr::rename(ret = 5) %>%
  dplyr::rename(trans = 6) %>%
  # Create post-test score that collapses across ret and trans
  dplyr::mutate(post = (ret + trans) / 2)

# Spatial goal post-test choice versus yoked
expt1_deg_choice <- expt1_prt_trim_deg_wide_p %>%
  dplyr::select(id, choice_id, post) %>%
  pivot_wider(
    names_from = choice_id,
    values_from = post)

expt1_deg_choice_sc <- na.omit(expt1_deg_choice$`1`)
expt1_deg_choice_yk <- na.omit(expt1_deg_choice$`2`)

expt1_deg_choice_shift <- rogme::mkt2(
  expt1_deg_choice_sc,
  expt1_deg_choice_yk,
  group_labels = c("Choice", "Yoked")
)

# Timing goal and wide format
expt1_prt_trim_ms_wide_p <- expt1_prt_trim_p %>%
  dplyr::select(id, choice_id, fb_id, block_id, p_tmean_e_ms) %>%
  tidyr::pivot_wider(
    names_from = block_id,
    values_from = p_tmean_e_ms) %>%
  # rename phases based on column number
  dplyr::rename(pre = 4) %>%
  dplyr::rename(ret = 5) %>%
  dplyr::rename(trans = 6) %>%
  # Create post-test score that collapses across ret and trans
  dplyr::mutate(post = (ret + trans) / 2)

# Timing goal post-test choice versus yoked
expt1_ms_choice <- expt1_prt_trim_ms_wide_p %>%
  dplyr::select(id, choice_id, post) %>%
  pivot_wider(
    names_from = choice_id,
    values_from = post)

expt1_ms_choice_sc <- na.omit(expt1_ms_choice$`1`)
expt1_ms_choice_yk <- na.omit(expt1_ms_choice$`2`)

expt1_ms_choice_shift <- rogme::mkt2(
  expt1_ms_choice_sc,
  expt1_ms_choice_yk,
  group_labels = c("Choice", "Yoked")
)


# EXPERIMENT 2
#
# Trimmed means
expt2_prt_trim_p <- expt2_performance_tib %>%
  dplyr::filter(phase_id != 2) %>%
  dplyr::group_by(id, group_id, phase_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    p_tmean_e_deg = sqrt(mean(ce2_deg, trim = 0.2, na.rm = TRUE)),
    p_tmean_e_ms = sqrt(mean(ce2_ms, trim = 0.2, na.rm = TRUE)),
    .groups = "drop"
  )

# Spatial goal and wide format
expt2_prt_trim_deg_wide_p <- expt2_prt_trim_p %>%
  dplyr::select(id, group_id, block_id, p_tmean_e_deg) %>%
  tidyr::pivot_wider(
    names_from = block_id,
    values_from = p_tmean_e_deg) %>%
  # rename phases based on column number
  dplyr::rename(pre = 3) %>%
  dplyr::rename(ret = 4) %>%
  dplyr::rename(trans = 5) %>%
  # Create post-test score that collapses across ret and trans
  dplyr::mutate(post = (ret + trans) / 2)

# Spatial goal post-test choice versus yoked
expt2_deg_choice <- expt2_prt_trim_deg_wide_p %>%
  dplyr::select(id, group_id, post) %>%
  pivot_wider(
    names_from = group_id,
    values_from = post)

expt2_deg_choice_sc <- na.omit(expt2_deg_choice$`5`)
expt2_deg_choice_yk <- na.omit(expt2_deg_choice$`6`)

expt2_deg_choice_shift <- rogme::mkt2(
  expt2_deg_choice_sc,
  expt2_deg_choice_yk,
  group_labels = c("Choice", "Yoked")
)


# Timing goal and wide format
expt2_prt_trim_ms_wide_p <- expt2_prt_trim_p %>%
  dplyr::select(id, group_id, block_id, p_tmean_e_ms) %>%
  tidyr::pivot_wider(
    names_from = block_id,
    values_from = p_tmean_e_ms) %>%
  # rename phases based on column number
  dplyr::rename(pre = 3) %>%
  dplyr::rename(ret = 4) %>%
  dplyr::rename(trans = 5) %>%
  # Create post-test score that collapses across ret and trans
  dplyr::mutate(post = (ret + trans) / 2)

# Spatial goal post-test choice versus yoked
expt2_ms_choice <- expt2_prt_trim_ms_wide_p %>%
  dplyr::select(id, group_id, post) %>%
  pivot_wider(
    names_from = group_id,
    values_from = post)

expt2_ms_choice_sc <- na.omit(expt2_ms_choice$`5`)
expt2_ms_choice_yk <- na.omit(expt2_ms_choice$`6`)

expt2_ms_choice_shift <- rogme::mkt2(
  expt2_ms_choice_sc,
  expt2_ms_choice_yk,
  group_labels = c("Choice", "Yoked")
)


# SHIFT FUNCTIONS - FIGS
#
# EXPERIMENT 1
#
# Spatial goal choice shift function
as.factor(expt1_deg_choice_shift$gr)

# Scatterplot
expt1_deg_choice_shift_ps <- rogme::plot_scat2(
  data = expt1_deg_choice_shift,
  formula = obs ~ gr,
  xlabel = "",
  ylabel = "",
  alpha = 1,
  shape = 21,
  color = "grey10",
  fill = "grey90"
) +
  coord_flip() +
  ggtitle("A. Experiment 1") +
  theme(plot.title = element_text(size = 16, face = "bold"))
expt1_deg_choice_shift_ps

# Compute shift function
expt1_deg_choice_sf <- rogme::shifthd(
  data = expt1_deg_choice_shift,
  formula = obs ~ gr,
  nboot = 1000
)

# Plot shift function
expt1_deg_choice_psf <- rogme::plot_sf(
  expt1_deg_choice_sf,
  plot_theme = 2,
  symb_fill = c("#73a1be", "#c8746d"),
  diffint_col = c("#73a1be", "#c8746d"),
  theme2_alpha = c(1, 1)
)

# Add labels
# expt1_deg_choice_psf <- rogme::add_sf_lab(
#   expt1_deg_choice_psf,
#   expt1_deg_choice_sf,
#   link_col = c("#73a1be", "#c8746d"),
#   y_lab_nudge = .4,
#   text_size = 3
# )

# Change axis labels
expt1_deg_choice_psf[[1]] <- expt1_deg_choice_psf[[1]] +
  ggplot2::labs(
    x = "Choice quantiles of spatial E (deg)",
    y = "Choice - Yoked \nquantile differences (deg)"
  )
expt1_deg_choice_psf[[1]]

# Create scatterplots with color coded differences
expt1_deg_choice_p <- rogme::plot_scat2(
  expt1_deg_choice_shift,
  xlabel = "",
  ylabel = "Spatial total E (deg)",
  alpha = .3,
  shape = 21,
  color = "grey10",
  fill = "grey90"
)

expt1_deg_choice_p <- rogme::plot_hd_links(
  expt1_deg_choice_p,
  expt1_deg_choice_sf[[1]],
  q_size = 1,
  md_size = 1.5,
  link_col = c("#73a1be", "#c8746d"),
  add_rect = TRUE,
  rect_alpha = 0.1,
  rect_col = "grey50",
  add_lab = TRUE,
  text_size = 5
) +
  coord_flip()
expt1_deg_choice_p

# Choice versus yoked combine the 3 plots
figS3A <- expt1_deg_choice_shift_ps +
  expt1_deg_choice_p +
  expt1_deg_choice_psf[[1]] +
  patchwork::plot_layout(ncol = 1)
figS3A


# Timing goal choice shift function
as.factor(expt1_ms_choice_shift$gr)

# Scatterplot
expt1_ms_choice_shift_ps <- rogme::plot_scat2(
  data = expt1_ms_choice_shift,
  formula = obs ~ gr,
  xlabel = "",
  ylabel = "",
  alpha = 1,
  shape = 21,
  color = "grey10",
  fill = "grey90"
) +
  coord_flip() +
  ggtitle("A. Experiment 1") +
  theme(plot.title = element_text(size = 16, face = "bold"))
expt1_ms_choice_shift_ps

# Compute shift function
expt1_ms_choice_sf <- rogme::shifthd(
  data = expt1_ms_choice_shift,
  formula = obs ~ gr,
  nboot = 1000
)

# Plot shift function
expt1_ms_choice_psf <- rogme::plot_sf(
  expt1_ms_choice_sf,
  plot_theme = 2,
  symb_fill = c("#73a1be", "#c8746d"),
  diffint_col = c("#73a1be", "#c8746d"),
  theme2_alpha = c(1, 1)
)

# Add labels
# expt1_ms_choice_psf <- rogme::add_sf_lab(
#   expt1_ms_choice_psf,
#   expt1_ms_choice_sf,
#   link_col = c("#73a1be", "#c8746d"),
#   y_lab_nudge = .4,
#   text_size = 3
# )

# Change axis labels
expt1_ms_choice_psf[[1]] <- expt1_ms_choice_psf[[1]] +
  ggplot2::labs(
    x = "Choice quantiles of timing E (ms)",
    y = "Choice - Yoked \nquantile differences (ms)"
  )
expt1_ms_choice_psf[[1]]

# Create scatterplots with color coded differences
expt1_ms_choice_p <- rogme::plot_scat2(
  expt1_ms_choice_shift,
  xlabel = "",
  ylabel = "Timing total E (ms)",
  alpha = .3,
  shape = 21,
  color = "grey10",
  fill = "grey90"
)

expt1_ms_choice_p <- rogme::plot_hd_links(
  expt1_ms_choice_p,
  expt1_ms_choice_sf[[1]],
  q_size = 1,
  md_size = 1.5,
  link_col = c("#73a1be", "#c8746d"),
  add_rect = TRUE,
  rect_alpha = 0.1,
  rect_col = "grey50",
  add_lab = TRUE,
  text_size = 5
) +
  coord_flip()
expt1_ms_choice_p

# Choice versus yoked combine the 3 plots
figS4A <- expt1_ms_choice_shift_ps +
  expt1_ms_choice_p +
  expt1_ms_choice_psf[[1]] +
  patchwork::plot_layout(ncol = 1)
figS4A


# EXPERIMENT 2
#
# Spatial goal choice shift function
as.factor(expt2_deg_choice_shift$gr)

# Scatterplot
expt2_deg_choice_shift_ps <- rogme::plot_scat2(
  data = expt2_deg_choice_shift,
  formula = obs ~ gr,
  xlabel = "",
  ylabel = "",
  alpha = 1,
  shape = 21,
  color = "grey10",
  fill = "grey90"
) +
  coord_flip() +
  ggtitle("B. Experiment 2") +
  theme(plot.title = element_text(size = 16, face = "bold"))
expt2_deg_choice_shift_ps

# Compute shift function
expt2_deg_choice_sf <- rogme::shifthd(
  data = expt2_deg_choice_shift,
  formula = obs ~ gr,
  nboot = 1000
)

# Plot shift function
expt2_deg_choice_psf <- rogme::plot_sf(
  expt2_deg_choice_sf,
  plot_theme = 2,
  symb_fill = c("#a3be8c", "#b48ead"),
  diffint_col = c("#a3be8c", "#b48ead"),
  theme2_alpha = c(1, 1)
)

# Add labels
# expt2_deg_choice_psf <- rogme::add_sf_lab(
#   expt2_deg_choice_psf,
#   expt2_deg_choice_sf,
#   link_col = c("#a3be8c", "#b48ead"),
#   y_lab_nudge = .4,
#   text_size = 3
# )

# Change axis labels
expt2_deg_choice_psf[[1]] <- expt2_deg_choice_psf[[1]] +
  ggplot2::labs(
    x = "Choice quantiles of spatial E (deg)",
    y = "Choice - Yoked \nquantile differences (deg)"
  )
expt2_deg_choice_psf[[1]]

# Create scatterplots with color coded differences
expt2_deg_choice_p <- rogme::plot_scat2(
  expt2_deg_choice_shift,
  xlabel = "",
  ylabel = "Spatial total E (deg)",
  alpha = .3,
  shape = 21,
  color = "grey10",
  fill = "grey90"
)

expt2_deg_choice_p <- rogme::plot_hd_links(
  expt2_deg_choice_p,
  expt2_deg_choice_sf[[1]],
  q_size = 1,
  md_size = 1.5,
  link_col = c("#a3be8c", "#b48ead"),
  add_rect = TRUE,
  rect_alpha = 0.1,
  rect_col = "grey50",
  add_lab = TRUE,
  text_size = 5
) +
  coord_flip()
expt2_deg_choice_p

# Choice versus yoked combine the 3 plots
figS3B <- expt2_deg_choice_shift_ps +
  expt2_deg_choice_p +
  expt2_deg_choice_psf[[1]] +
  patchwork::plot_layout(ncol = 1)
figS3B


# Timing goal choice shift function
as.factor(expt2_ms_choice_shift$gr)

# Scatterplot
expt2_ms_choice_shift_ps <- rogme::plot_scat2(
  data = expt2_ms_choice_shift,
  formula = obs ~ gr,
  xlabel = "",
  ylabel = "",
  alpha = 1,
  shape = 21,
  color = "grey10",
  fill = "grey90"
) +
  coord_flip() +
  ggtitle("B. Experiment 2") +
  theme(plot.title = element_text(size = 16, face = "bold"))
expt2_ms_choice_shift_ps

# Compute shift function
expt2_ms_choice_sf <- rogme::shifthd(
  data = expt2_ms_choice_shift,
  formula = obs ~ gr,
  nboot = 1000
)

# Plot shift function
expt2_ms_choice_psf <- rogme::plot_sf(
  expt2_ms_choice_sf,
  plot_theme = 2,
  symb_fill = c("#a3be8c", "#b48ead"),
  diffint_col = c("#a3be8c", "#b48ead"),
  theme2_alpha = c(1, 1)
)

# Add labels
# expt2_ms_choice_psf <- rogme::add_sf_lab(
#   expt2_ms_choice_psf,
#   expt2_ms_choice_sf,
#   link_col = c("#a3be8c", "#b48ead"),
#   y_lab_nudge = .4,
#   text_size = 3
# )

# Change axis labels
expt2_ms_choice_psf[[1]] <- expt2_ms_choice_psf[[1]] +
  ggplot2::labs(
    x = "Choice quantiles of timing E (ms)",
    y = "Choice - Yoked \nquantile differences (ms)"
  )
expt2_ms_choice_psf[[1]]

# Create scatterplots with color coded differences
expt2_ms_choice_p <- rogme::plot_scat2(
  expt2_ms_choice_shift,
  xlabel = "",
  ylabel = "Timing total E (ms)",
  alpha = .3,
  shape = 21,
  color = "grey10",
  fill = "grey90"
)

expt2_ms_choice_p <- rogme::plot_hd_links(
  expt2_ms_choice_p,
  expt2_ms_choice_sf[[1]],
  q_size = 1,
  md_size = 1.5,
  link_col = c("#a3be8c", "#b48ead"),
  add_rect = TRUE,
  rect_alpha = 0.1,
  rect_col = "grey50",
  add_lab = TRUE,
  text_size = 5
) +
  coord_flip()
expt2_ms_choice_p

# Choice versus yoked combine the 3 plots
figS4B <- expt2_ms_choice_shift_ps +
  expt2_ms_choice_p +
  expt2_ms_choice_psf[[1]] +
  patchwork::plot_layout(ncol = 1)
figS4B


# MULTIPANEL PLOTS FOR SUPPLEMENTARY
#
figS3 <- figS3A | figS3B
figS3

figS4 <- figS4A | figS4B
figS4

