#### Load RDS files, create plots ####

# This avoids issues with linux rendering of plots

library(dplyr)
library(ggplot2)
library(cowplot)
library(sf)
library(ggsci)
library(geofacet)
library(patchwork)

# Figure 1: Survey table
p1 <- readRDS("paper_poster_plots_threemc/paper/plots/01_survey_table.RDS")
ggplot2::ggsave(
  "paper_poster_plots_threemc/paper/plots/01_survey_table.png", 
  p1, 
  width = 6.3, 
  height = 8,
  units = "in"
)
rm(p1); gc()

# Figure 2: Coverage Map 
p2final <- readRDS("paper_poster_plots_threemc/paper/plots/02_map_plot_facet.RDS")
ggsave(
  "paper_poster_plots_threemc/paper/plots/02_map_plot_facet.png", 
  p2final,
  width = 6.3, 
  height = 6.5, 
  units = "in"
)
rm(p2final); gc()

# Figure 3: Subnational coverage variation
p3 <- readRDS("paper_poster_plots_threemc/paper/plots/03_subnat_plot.RDS")
ggplot2::ggsave(
  "paper_poster_plots_threemc/paper/plots/03_subnat_plot.png", 
  p3, 
  width = 6.3, 
  height = 8,
  units = "in"
)
rm(p3); gc()

# Figure 4: Geofacet of coverage by age
p4_geo <- readRDS("paper_poster_plots_threemc/paper/plots/04_geo_age.RDS")
ggplot2::ggsave(
  "paper_poster_plots_threemc/paper/plots/04_geo_age.png", 
  p4_geo, 
  width = 6.3, 
  height = 8,
  units = "in"
)
rm(p4_geo); gc()

# Figure 5: Map & scatter of mean age at circumcision
p5 <- readRDS(
  "paper_poster_plots_threemc/paper/plots/051_map_plot_mean_circ_age_map.RDS"
)
p5_scatter <- readRDS(
  "paper_poster_plots_threemc/paper/plots/052_map_plot_mean_circ_age_scatter.RDS"
)
ggsave(
  "paper_poster_plots_threemc/paper/plots/05_map_plot_mean_circ_age.png", 
  # p5,
  # plot_grid(
  #   plotlist = list(p5, p5_scatter), ncol = 1, rel_heights = c(1.3, 1)
  # ),
  p5 / p5_scatter +  plot_layout(heights = c(1, 1.2)),
  width = 6.3, 
  height = 8, 
  units = "in"
)
rm(p5, p5_scatter); gc()

# Figure 6: 
p6 <- readRDS("paper_poster_plots_threemc/paper/plots/06_change_00_20.RDS")
tmp_long_upper <- p6$tmp_long_upper
ggplot2::ggsave(
  "paper_poster_plots_threemc/paper/plots/06_change_00_20.png",
  p6,
  width = 6.3,
  height = 6,
  units = "in"
)
rm(p6); gc()

