#### Figure 5: Map + scatter plot of age at circumcision ####

stopifnot(basename(getwd()) == "paper_poster_plots_threemc")

#### Libraries ####

library(ggplot2)
library(dplyr)
library(sf)
# devtools::install_github("thomasp85/patchwork")
library(patchwork)

#### Setup ####

plt_data5 <- read_sf("paper/data/plots_ind/05_age_at_circ.geojson")
areas <- read_sf("paper/data/plots_ind/05_areas.geojson") %>% 
  filter(area_level == 0)
# Add Lake Victoria for blue in map plot
lake_vic <- rnaturalearth::ne_download(
  scale = 110, type = "lakes", category = "physical") %>% 
  sf::st_as_sf(lakes110, crs = 4269) %>% 
  filter(name == "Lake Victoria") %>% 
  select(name)

# number of discrete age groups to group plot into
n_breaks <- 7

vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", "UGA", "MWI",
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF"
)

#### Plot & Save ####

map_plot_circ_age <- function(
    spec_results, spec_areas, lake_vic, colourPalette, n_breaks = 4
  ) {
  
  levs = c("Total", "Medical", "Traditional")
  levs <- levs[levs %in% spec_results$type]
  
  stopifnot(n_breaks %in% c(4, 7, 10))
  
  spec_results$type <- factor(spec_results$type, levels = levs)
  
  # functionalise this
  # create discrete age bands, depending on the number of "breaks" specified
  cut_breaks <- c(seq(0, 30, length.out = n_breaks), Inf)
  if (n_breaks == 4) {
    cut_breaks <- cut_breaks + 1
    break_labels <- c("0-10", "11-20", "21-30", "30+")
  } else if (n_breaks == 7) {
    break_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30+")
  } else if (n_breaks == 10) {
    cut_breaks <- cut_breaks + 1
    break_labels <- c(
      "0-3",   "4-6",   "7-10", "11-13", "14-16", 
      "17-20", "21-23", "24-26", "27-30", "30+"
    )
  }
  
  spec_results$mean_f <- cut(
    spec_results$mean, breaks = cut_breaks, labels = break_labels
  )
  
  if (is.function(colourPalette)) {
    cols <- colourPalette(n_breaks)
  } else cols <- colourPalette
  names(cols) = break_labels
  
  ggplot() +
    geom_sf(
      data = spec_results,
      aes(fill = mean_f), 
      size = 0.5,
      colour = NA
    ) +
    labs(fill = "") +
    # add colour bar for discrete age bands
    # viridis::scale_fill_viridis(
    #   discrete = TRUE, na.value = "grey", option = "D"
    # ) + 
    # ggsci::scale_fill_tron() + 
    scale_fill_manual(
      values = cols, 
      limits = levels(spec_results$mean_f), 
      labels = break_labels, 
      na.value = "grey"
    ) + 
    # fill in country borders in black
    geom_sf(
      data = spec_areas,
      colour = "black",
      size = 0.5,
      fill = NA
    ) +
    # fill in lake victoria in light blue
    geom_sf(
      data   = lake_vic, 
      colour = "lightblue", 
      fill   = "lightblue",
      size   = 0.5
    ) + 
    labs(fill = "", tag = "A") +
    facet_wrap(~ type) + 
    theme_minimal(base_size = 9) +
    theme(
      strip.text        = element_text(size = rel(1.5), face = "bold"), 
      legend.text       = element_text(size = rel(1.8)),
      axis.text         = element_blank(),
      axis.ticks        = element_blank(),
      legend.position   = "bottom",
      panel.grid        = element_blank(),
      # make plot as "dense" as possible
      panel.spacing     = unit(0.001, "lines"), 
      plot.background   = element_rect(fill = "white", colour = "white"),
      plot.tag          = element_text(size = rel(2), face = "bold"),
      # plot.tag.position = c(0.1, 1),
      # add margin to remove grey spaces in combined plots
      # plot.margin     = unit(c(1.1, 0, 1.1, 0), "cm"),
      # remove margin to fit plots together easily
      # plot.margin     = unit(c(1.1, -0.3, 1.1, -0.35), "cm"),
      plot.margin     = unit(c(0, -0.3, 0, -0.35), "cm"),
      # move legend items closer together
      legend.spacing.x = unit(0.1, "cm")
    )
}

p5 <- map_plot_circ_age(
  plt_data5, 
  areas, 
  lake_vic, 
  rev(viridis::viridis(n_breaks)), 
  n_breaks
)

# scatter plot of TMC vs MMC for districts
p5_scatter <- sf::st_drop_geometry(plt_data5) %>% 
  select(-c(upper, lower)) %>% 
  tidyr::pivot_wider(names_from = "type", values_from = "mean") %>% 
  mutate(vmmc = ifelse(iso3 %in% vmmc_iso3, "VMMC", "Non-VMMC")) %>% 
  ggplot(aes(x = Traditional, y = Medical, colour = vmmc)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  scale_colour_manual(
    values = wesanderson::wes_palette("Darjeeling1", 5)[4:5]
  ) + 
  labs(
    x      = "Mean traditional circumcision age, 2020",
    y      = "Mean medical circumcision age", 
    colour = "", 
    tag    = "B"
  ) + 
  # define x and y breaks
  scale_x_continuous(
    breaks = seq(0, 30, by = 5), limits = c(0, 26), expand = c(0, 0)
  ) + 
  scale_y_continuous(
    breaks = seq(0, 30, by = 5), limits = c(0, 26), expand = c(0, 0)
  ) + 
  # increase dot size in legend
  guides(colour = guide_legend(override.aes = list(size = 4))) + 
  theme_bw(base_size = 9) + 
  theme(
    axis.text.x       = element_text(size = rel(1.6), colour = "black"),
    axis.title.x      = element_text(size = rel(1.3), face = "bold"),
    axis.text.y       = element_text(size = rel(1.6), colour = "black"), 
    axis.title.y      = element_text(size = rel(1.3), face = "bold"),
    legend.text       = element_text(size = rel(1.5), colour = "black"),
    legend.position   = c(0.85, 0.15),
    # remove white box behind legend
    legend.background = element_rect(colour = NA, fill = NA),
    legend.key        = element_rect(colour = NA, fill = NA),
    # increase tag size
    plot.tag          = element_text(size = rel(2), face = "bold"),
    # plot.tag.position = c(0.12, 0.92),
    # add margin so last x break is included
    plot.margin         = unit(c(0, 0.5, 0, 0.1), "cm"),
    # change aspect ration so plot is square when saved
    aspect.ratio        = 0.7
  )

# combine plots
p5 / p5_scatter + 
  plot_layout(heights = c(1, 1.2))

# save plots
# ggsave(
#   "paper_poster_plots_threemc/paper/plots/05_map_plot_mean_circ_age.png",
#   p5 / p5_scatter +  plot_layout(heights = c(1, 1.2)),
#   width = 6.3,
#   # height = 10,
#   height = 8,
#   units = "in"
# )
