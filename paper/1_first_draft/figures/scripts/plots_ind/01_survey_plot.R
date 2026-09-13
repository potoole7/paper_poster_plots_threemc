#### Figure 1: Survey plot ####

stopifnot(basename(getwd()) == "paper_poster_plots_threemc")

#### Libraries ####

library(ggplot2)
library(dplyr)

#### Setup ####

# fig1data <- readr::read_csv("path/to/")
fig1data <- readr::read_csv("paper/data/plots_ind/01_survey_plot.csv")

vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", "UGA", "MWI",
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF", "BWA"
)
# where to add horizontal line for VMMC vs non-VMMC
country_positions <- length(vmmc_iso3) + 1

plot_order <- c(
  "BEN", "BFA", "CIV", "GHA", "GIN", "GNB", "LBR", "MLI", "NER", 
  "NGA", "SEN", "SLE", "GMB", "TGO", "AGO", "CMR", "CAF", "TCD", 
  "COG", "COD", "GAB", "BDI", "ETH", "KEN", "MWI", "MOZ", "RWA", 
  "TZA", "UGA", "ZMB", "ZWE", "BWA", "SWZ", "LSO", "NAM", "ZAF"
)

country_name_convention <- function(x) {
  x <- dplyr::case_when(
    x == "Congo - Kinshasa"    ~ "DR Congo",
    x == "Congo - Brazzaville" ~ "Congo",
    grepl("Ivoire", x)         ~ "Cote d'Ivoire",
    x == "Gambia"              ~ "The Gambia",
    TRUE                       ~ x
  )
}

country_pos_df <- data.frame(
  "iso3"        = plot_order, 
  "country_idx" = rev(seq_along(plot_order))
) %>% 
  mutate(
    country_idx = ifelse(!iso3 %in% vmmc_iso3, country_idx + 1, country_idx), 
    country = country_name_convention(
      countrycode::countrycode(iso3, "iso3c", "country.name")
    ),
    country = case_when(
      country == "Central African Republic" ~ "Cent. Af. Rep.", 
      country == "Gambia"                   ~ "The Gambia",
      TRUE                                  ~ country
    )
  )

#### Plot & Save ####

p1 <- fig1data %>%
  # specify aesthetic variables
  ggplot(
    aes(year, country_idx, colour = provider, shape = data, size = N)
  ) +
  # add horizontal lines for regional country (don't have line on top)
  # geom_hline(yintercept = country_positions[-1]) +
  # add horizontal line to split VMMC and non-VMMC countries
  geom_hline(yintercept = country_positions) + 
  # annotate plot with (vertical) regional labels
  # annotate(
  #   "text",
  #   2020.2,
  #   # have labels in between horizontal lines
  #   zoo::rollmean(c(country_positions, 0), 2),
  #   # label = c("Western", "Central", "Eastern", "Southern"),
  #   label = paste(c("VMMC", "Non-VMMC"), "priority countries"),
  #   angle = 270,
  #   fontface = "bold",
  #   size = 4
  # ) +
  # annotate with VMMC status above and below horizontal
  annotate(
    geom = "text",
    x = 1999.2,
    y = c(length(plot_order) - 0.2),
    # y = c(length(plot_order) + ),
    label = "Non-VMMC \nPriority \nCountries",
    fontface = "bold",
    size = 3.5,
    hjust = 0
  ) +
  annotate(
    geom = "text",
    x = 1999.2,
    # y =  c(length(plot_order) - (country_positions + 3.9)),
    y =  c(length(plot_order) - (country_positions + 7.9)),
    label = "VMMC \nPriority \nCountries",
    # angle = 270,
    fontface = "bold",
    size = 3.5,
    hjust = 0
  ) +
  geom_point(stroke = 2) +
  # specify x and y labels, subbing country name for country_idx
  scale_x_continuous(
    "Survey Year", 
    # may break if not 2000, 2020
    # breaks = seq(min(fig1data$year) - 2, max(fig1data$year) + 1, by = 2), 
    breaks = seq(2000, 2020, by = 2),
    # minor_breaks = NULL
    # minor_breaks = waiver(), 
    limits = c(1999, 2020),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    element_blank(),
    breaks = country_pos_df$country_idx,
    minor_breaks = NULL,
    labels = country_pos_df$country,
    # position = "left",
    expand = expansion(add = 0.6)
  ) +
  # New England Journal of Medicine colourscheme
  ggsci::scale_colour_nejm() +
  theme_light(10) +
  guides(
    colour = guide_legend(order = 1),
    shape = guide_legend(order = 2),
    size = guide_legend(order = 3)
  ) +
  labs(
    colour = "Survey type",
    # shape = "Type Distinction",
    shape = "Info on circumcision type",
    size = "Sample size"
  ) +
  theme_bw(base_size = 9) + 
  theme(
    axis.title.x = element_text(size = rel(1.5), colour = "black"),
    axis.text.x = element_text(
      size = rel(1.5), 
      angle = 45, 
      hjust = 1, 
      colour = "black"
    ),
    axis.text.y = element_text(size = rel(1.5), colour = "black"),
    legend.text = element_text(size = rel(1.5), colour = "black"),
    legend.title = element_text(size = rel(1.5), colour = "black"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing = unit(0, "cm"),
    legend.key.size = unit(0, "pt"),
    legend.justification = c(1, 0),
    legend.text.align = 0,
    # plot.margin = margin(t = 1, l = 18, unit = "pt")
    plot.margin = margin(0.25, 1, 0, 0, "cm") 
  )

# save plot
# saveRDS(p1, "paper_poster_plots_threemc/paper/plots/01_survey_table.RDS")
# ggplot2::ggsave(
#   "paper/plots/01_survey_table.png", 
#   p1, 
#   width = 6.3, 
#   height = 8,
#   units = "in"
# )
