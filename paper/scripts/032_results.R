#### Create plots for results ####

# Produce summary stats here as well

# stopifnot(dirname(getwd()) == "threemc-orderly")
while(basename(getwd()) != "threemc-orderly") setwd("../.")

#### Libs ####
library(dplyr, warn.conflicts = FALSE)
library(forcats)
library(ggplot2)
library(reactable)
library(sf)
library(threemc)
library(orderly)
library(ggsci)
library(geofacet)
library(glue)
library(ggtext)
source("Shiny/src/functions.R") # plotting functions
source("paper_poster_plots/paper/scripts/00_funs.R")

#### Metadata ####

orderly_root <- here::here()

spec_age_group <- "15-29" # changed from 10-29 upon Jeff's request
spec_years <- c(2006, 2020)

## for tabulating surveys
# remove circumcisions with missing type?
rm_missing_type <- FALSE

start_year <- 2002
cens_age <- 59

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

# countries we attempted to model
ssa_iso3 <- sort(c(
  "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD",
  "COG", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN",
  "LBR", "LSO", "MLI", "MOZ", "MWI", "NAM", "NER", "NGA", "RWA",
  "SEN", "SLE", "SWZ", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
))

ssa_countries <- countrycode::countrycode(ssa_iso3, "iso3c", "country.name")
ssa_countries <- case_when(
  ssa_countries == "Congo - Kinshasa"    ~ "DR Congo",
  ssa_countries == "Congo - Brazzaville" ~ "Congo",
  grepl("Ivoire", ssa_countries)         ~ "Cote d'Ivoire",
  ssa_countries == "Gambia"              ~ "The Gambia",
  TRUE                                   ~ ssa_countries
)

# countries/regions which have been targeted for 90% circumcision by 2021
target_iso3 <- c(
  # In Ethiopia Gambella is the only target province
  "BWA", "ETH_1_15", "KEN", "LSO", "MWI", "MOZ", "NAM", 
  "RWA", "ZAF", "SWZ", "UGA", "TZA", "ZMB", "ZWE"
)


# Colour palette for map plot
colourPalette <- rev(colorRampPalette(
  c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf",
    "#e6f598", "#abdda4", "#66c2a5", "#3288bd", 
    "#5e4fa2")
)(100))

# colourPalette for % changeR
colourPalette2 <- rev(heat.colors(100))
# colourPalette2 <- viridis::viridis(100)


#### Load Data ####

# orderly archives
archives <- orderly::orderly_list_archive()

# VMMC countries  
vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", "UGA", "MWI",
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF" # LSO, SWZ ran already
)
no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD")

iso3 <- ssa_iso3[!ssa_iso3 %in% c(vmmc_iso3, no_type_iso3)]

pars_df <- bind_rows(
  # parameters for VMMC countries 
  data.frame(
    cntry           = vmmc_iso3, 
    rw_order        = 0, 
    paed_age_cutoff = 10, 
    inc_time_tmc    = TRUE
  ), 
  # parameters for non-VMMC countries
  data.frame(
    cntry           = iso3, 
    rw_order        = 0, 
    paed_age_cutoff = Inf, 
    inc_time_tmc    = TRUE
  ), 
  # parameters for countries with no type information,
  data.frame(
    cntry           = no_type_iso3, 
    rw_order        = 0, 
    paed_age_cutoff = Inf, 
    inc_time_tmc    = FALSE
  )
)

# pull most recent results for age groups
results_dirs <- load_orderly_data(
  task = "02final_aggregations", 
  parameters = pars_df,
  query = "latest(
      parameter:cntry           == cntry && 
      parameter:rw_order        == rw_order &&
      parameter:paed_age_cutoff == paed_age_cutoff &&
      parameter:inc_time_tmc    == inc_time_tmc
    )"
)$dirs

results_agegroup <- load_orderly_data(
  task = "02final_aggregations",
  dirs = results_dirs[!is.na(results_dirs)],
  filenames = "Results_AgeGroup_Prevalence.csv.gz"
)$output %>%
  bind_rows()
gc()

# # most recent results for single ages
# results_age <- load_orderly_data(
#   task = "02final_aggregations", 
#   dirs = results_dirs[!is.na(results_dirs)],
#   filenames = "Results_Age_Prevalence.csv.gz"
# )$output %>% 
#   bind_rows() %>% 
#   filter(type %in% paste(c("MC", "MMC", "TMC"), "coverage"))
# gc()
# 
if (!"iso3" %in% names(results_agegroup)) {
  results_agegroup$iso3 <- substr(results_agegroup$area_id, 0, 3)
}
# 
# if (!"iso3" %in% names(results_age)) {
#   results_age$iso3 <- substr(results_age$area_id, 0, 3)
# }

# results_agegroup_n_circ <- readr::read_csv(file.path(
#   orderly_root, 
#   "archive/03_shiny_consolidation",
#   dir_name,
#   "/artefacts/results_agegroup_n_circ.csv.gz"
# ))

# results_agegroup_n_circ <- load_orderly_data(
#   task = "02final_aggregations",
#   dirs = results_dirs[!is.na(results_dirs)],
#   filenames = "Results_AgeGroup_Incidence.csv.gz"
# )$output %>%
#   bind_rows()
# # filter not working here for some reason??
# results_agegroup_n_circ <- results_agegroup_n_circ[
#   results_agegroup_n_circ$type %in% paste0(
#     c("MC", "MMC", "TMC"), "s performed"
#   ), 
# ]
#   # dplyr::filter(type %in% paste0(c("MC", "MMC", "TMC"), "s performed"))
# gc()

# results_agegroup_rate <- load_orderly_data(
#   task = "02final_aggregations",
#   dirs = results_dirs[!is.na(results_dirs)],
#   filenames = "Results_AgeGroup_Probability.csv.gz"
# )$output %>%
#   bind_rows()
# # filter not working here for some reason??
# results_agegroup_rate <- results_agegroup_rate[
#   results_agegroup_rate$type %in% paste(
#     c("MC", "MMC", "TMC"), "probability"
#   ),
# ]
# gc()

# if (!"iso3" %in% names(results_agegroup_rate)) {
#   results_agegroup_rate$iso3 <- substr(results_agegroup_rate$area_id, 0, 3)
# }


# VMMC iso3s not in results
missing_iso3 <- target_iso3[!target_iso3 %in% results_agegroup$area_id]
# missing_iso3 <- target_iso3[!target_iso3 %in% results_age$area_id]
# missing_iso3 <- target_iso3[!target_iso3 %in% results_agegroup_rate$area_id]

# pull shapefiles
areas <- load_orderly_data("00a2_areas_join", 
                           query = "latest", 
                           file = "areas.geojson", 
                           load_fun  = sf::read_sf)$output[[1]]

# pull agegroup populations
populations <- load_orderly_data(
  "00c4_pops_aggregate", 
  query = "latest", 
  file = "population_agegroup_aggr.csv.gz"
)$output[[1]] %>% 
  filter(iso3 %in% results_agegroup$iso3) %>% 
  # filter(iso3 %in% results_age$iso3) %>% 
  # filter(iso3 %in% results_agegroup_rate$iso3) %>% 
  identity()

# last surveys 
last_surveys <- readr::read_csv("global/most_recent_surveys.csv")

# Pull regional information for each country
loc <- RCurl::getURL("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
iso_df <- read.csv(text = loc)
iso_df <- iso_df %>%
  filter(region == "Africa") %>%
  mutate(across(intermediate.region, ~ifelse(. == "", sub.region, .))) %>%
  select(
    iso3 = alpha.3, region = intermediate.region
  ) %>%
  mutate(region = ifelse(region == "", "Other", region))


#### Figure 2: Map of MC Coverage across SSA 20-2020 15-29 year olds ####

## Map Plot ##
main_title <- paste0(
  # "MC Coverage, ",
  "Circumcision Coverage ", 
  paste0(spec_years[1], "-", spec_years[2]),
  ", age ",
  spec_age_group,
  " years"
)
# Plot all types & with facets 

# additions: 
# - Have single faceted R plot, with tag in corner (done, no tags though ..)
# - Text very large compared to the images, reduce size!
# - Remove North African countries from plot
# - For "% Change%", have a different colour bar, which goes into negative numbers (e.g. -30% to 70%)
# - Can put year labels, can put in upper left of Africa, leaves more space for image

# Qs: 
# - How do we want colour bars displayed? Like Tristan, on the side of his plot? 
# 

country_area_level = 0
results_area_level = NULL
spec_model <- "No program data"
spec_main_title    = main_title

main_title <- paste0(
  # "MC Coverage, ",
  "Male circumcision coverage, ",
  paste0(spec_years[1], "-", spec_years[2]),
  ", age ",
  spec_age_group,
  " years"
)

results_agegroup1 <- results_agegroup
areas1 <- areas

## Plot ##

if (!"iso3" %in% names(results_agegroup1)) {
  results_agegroup1$iso3 <- substr(results_agegroup1$area_id, 0, 3)
}

# take only required columns in areas for later joining with results
areas_join <- areas1 %>%
    dplyr::select(iso3, area_id, area_name, area_level)
                  
# Subsetting results
if (!is.null(results_area_level)) {
    results_agegroup1 <- results_agegroup1 %>%
        filter(area_level == results_area_level)
} else {
  results_agegroup1 <- results_agegroup1 %>%
    group_by(iso3) %>%
    filter(area_level == max(area_level)) %>%
    ungroup()
}
tmp <- results_agegroup1 %>%
    filter(
        area_id != "",
        year %in%       spec_years,
        age_group ==    spec_age_group,
        # area_level <=   results_area_level,
        model ==        spec_model,
        type %in%       c("MC coverage", "MMC coverage", "TMC coverage")
    )

tmp <- tmp %>% 
        select(-matches("area_name")) %>%
        # Merging to shapefiles
        left_join(areas_join)

tmp <- tmp %>% 
    # filter out areas with missing iso3, which cause errors with below
    filter(!is.na(iso3)) %>%
    # take maximum area level for known regions
    group_by(iso3) %>%
    filter(area_level == max(area_level, na.rm = TRUE)) %>%
    ungroup() %>%
    # Altering labels for the plot
    dplyr::mutate(
        type = ifelse(grepl("MMC", type), "Medical",
                      ifelse(grepl("TMC", type), "Traditional", "Total"))
    ) %>%
    # change data to sf object
    st_as_sf()

# filter overlaying area shapes for specified area level
areas_plot <- areas1
if (!is.null(country_area_level)) {
  areas_plot <- areas_plot %>%
    filter(area_level == country_area_level)
}

# repair polygons which may be invalid
tmp <- st_make_valid(tmp)
areas_plot <- st_make_valid(areas_plot)

# Add difference, if specified
tmp$year <- as.factor(tmp$year)
# if (inc_difference == TRUE) {

# split by country, take spec_years, calculate difference between the two
diff_df <- tmp %>%
  arrange(year) %>%
  group_split(area_id, model, type, age_group)
  
diff_df <- lapply(diff_df, function(x) {
     # take negative difference for TMC (expecting decline)
     # Now colouring % Change differently to total coverage
     # if (all(x$type == "Traditional")) x <- x[nrow(x):1, ]
     x <- x%>% 
       # don't allow change to be < 0
       # mutate(across(mean:upper, ~ max(0, diff(.)))) %>% 
       mutate(across(mean:upper, ~ case_when(
         type == "Traditional" ~ diff(.), 
         TRUE                  ~ max(0, diff(.))
       ))) %>% 
       # take final line, only this shows difference
       slice(n())
 }) %>%
  bind_rows() %>%
  mutate(
    year = "% Change"
    # year = ifelse(type == "Traditional", "-% Change"  ,"% Change")
  )

levels <- c(spec_years, unique(diff_df$year))
tmp <- bind_rows(tmp, diff_df) %>%
    mutate(year = factor(year, levels = levels))

# finally, change 0s for countries with no type info to NAs
tmp <- tmp %>% 
  mutate(mean = ifelse(
    iso3 %in% no_type_iso3 & type != "Total", NA, mean
  ))

# for testing plot
# spec_results <- tmp
# spec_areas <- areas_plot

map_plot <- function(spec_results, spec_areas, colourPalette, colourPalette2) {

  spec_results$type <- factor(
    spec_results$type, 
    levels = c("Total", "Medical", "Traditional")
  )
  
  spec_results_change <- filter(spec_results, year == "% Change")
  spec_results_year <- filter(spec_results, year != "% Change")
  
  p <- ggplot() +
    geom_sf(
      data = spec_results_year,
      aes(fill = mean), 
      size = 0.5,
      colour = NA
    ) +
    geom_sf(
      data = spec_areas,
      colour = "black",
      size = 0.5,
      fill = NA
    ) +
    labs(fill = "") +
    scale_fill_gradientn(
      colours = colourPalette,
      na.value = "grey",
      breaks = seq(0, 1, by = 0.1),
      limits = c(0, 1),
      label = scales::label_percent(accuracy = 1), 
      guide = guide_colourbar(
        # direction = "vertical",
        label = TRUE,
        draw.ulim = TRUE,
        draw.llim = TRUE,
        frame.colour = "black",
        ticks = TRUE,
        # barheight = 1,
        barheight = 15,
        # barwidth = 30
        # barwidth = 20
        barwidth = 1,
        title.position = "right"
      )
    ) +
    # guides(fill = guide_colourbar(title.position = "right")) +
    ggnewscale::new_scale_fill() +
    # colour percentage change differently
    geom_sf(
      data = spec_results_change,
      aes(fill = mean),
      size = 0.5,
      colour = NA
    ) + 
    labs(fill = "") +
    scale_fill_gradientn(
      colours = colourPalette2,
      na.value = "grey",
      breaks = seq(-0.5, 0.5, by = 0.1),
      limits = c(-0.5, 0.5),
      label = scales::label_percent(accuracy = 1),
      guide = guide_colourbar(
        # direction = "vertical", 
        label = TRUE, 
        draw.ulim = TRUE,
        draw.llim = TRUE,
        frame.colour = "black", 
        ticks = TRUE, 
        # barheight = 1,
        barheight = 15,
        # barwidth = 30,
        # barwidth = 20,
        barwidth = 1,
        title.position = "right"
      )
    ) +
    # guides(fill = guide_colourbar(title.position = "right")) +
    facet_grid(type ~ year) + 
    theme_minimal() +
    theme(
      axis.text       = element_blank(),
      axis.ticks      = element_blank(),
      strip.text      = element_text(size = 20), #  face = "bold"),
      legend.text     = element_text(size = 12),
      plot.title      = element_text(size = 26, hjust = 0.5),
      # legend.position = "bottom",
      panel.grid      = element_blank(),
      panel.spacing   = unit(0.01, "lines") # make plot as "dense" as possible
    )
}

p2final <- map_plot(tmp1, areas_plot, colourPalette, colourPalette2) + 
  ggtitle(main_title)
 
p2final

# save plots
# ggsave(plot = p2a, filename = "poster/plots/p2a.png", width = 1980, height = 1060, units = "px")
# ggplot2::ggsave(
#   "paper_poster_plots/paper/plots/02_map_plot.png",
#   p2,
#   width = 9,
#   height = 11,
#   units = "in"
# )
# types <- c("MC", "MMC", "TMC")
# lapply(seq_along(p2final), function(i) {
#   ggplot2::ggsave(
#     paste0("paper_poster_plots/paper/plots/02_map_plot_", types[[i]], ".png"),
#     p2final[[i]],
#     width = 9,
#     height = 11,
#     units = "in"
#   )
# })
ggplot2::ggsave(
  "paper_poster_plots/paper/plots/02_map_plot_facet.png",
  p2final,
  width = 12,
  height = 10,
  units = "in"
)

# rm(p2final); gc()


#### Figure 3: Sub-National Variation in MC Coverage Plot ####

# TODO: Fix country orderings

# order plot West to East, North to South
# plot_order <- c(
#   "SEN", "GMB", "GNB", "GIN", "SLE", "LBR", "MLI", "BFA", "CIV", "GHA", "TGO", 
#   "BEN", "NER", "NGA", "CMR", "TCD", "CAF", "SSD", "ETH", "GAB", "COG", "COD",
#   "UGA", "KEN", "RWA", "BDI", "TZA", "AGO", "ZMB", "MWI", "MOZ", "ZWE", "NAM", 
#   "SWZ", "LSO", "ZAF"
# )
# plot order from figure 1
plot_order <- c(
  "BEN", "BFA", "CIV", "GHA", "GIN", "GNB", "LBR", "MLI", "NER", 
  "NGA", "SEN", "SLE", "GMB", "TGO", "AGO", "CMR", "CAF", "TCD", 
  "COG", "COD", "GAB", "BDI", "ETH", "KEN", "MWI", "MOZ", "RWA", 
  "TZA", "UGA", "ZMB", "ZWE", "BWA", "SWZ", "LSO", "NAM", "ZAF"
)
plot_order <- plot_order[plot_order %in% results_agegroup$iso3]

# take MC coverage for specific age group and final year
plt_data <- results_agegroup %>%
  filter(
    age_group == spec_age_group, 
    year == last(spec_years),
    type == "MC coverage"
  ) %>% 
  # add country column 
  mutate(
    country = countrycode::countrycode(iso3, "iso3c", "country.name"),
    country = case_when(
      country == "Congo - Brazzaville" ~ "Congo",
      country == "Congo - Kinshasa"    ~ "DR Congo",
      TRUE                             ~ country)
  ) %>%
  # join in ESA-WCA distinctions
  left_join(threemc::esa_wca_regions) %>% 
  # join in age-group populations
  left_join(select(populations, area_id, age_group, population)) %>% 
  group_by(iso3, area_level) %>%
  # for each country, take district-level results (or provincial if not possible)
  # filter(area_level == min(2, max(area_level))) %>% 
  # weigh population by average district population in each country
  mutate(population = population / median(population)) %>%
  ungroup()

# where to add horizontal line for VMMC vs non-VMMC
country_positions1 <- length(vmmc_iso3) + 1

# add country position (need whitespace for horizontal line)
country_pos_df <- data.frame(
  "iso3"        = plot_order, 
  "country_idx" = rev(seq_along(plot_order))
) %>% 
  mutate(
    country_idx = ifelse(!iso3 %in% vmmc_iso3, country_idx + 1, country_idx), 
    country = country_name_convention(
      countrycode::countrycode(iso3, "iso3c", "country.name")
    )
  )

plt_data <- plt_data %>% 
  mutate(
    vmmc = ifelse(iso3 %in% vmmc_iso3, "VMMC", "None-VMMC"), 
    # country_idx = as.integer(fct_rev(iso3)) +
    # country_idx = as.integer(iso3) +
    #   c(0, 1, 2, 3)[
    #     match(region, rev(c("None-VMMC", "VMMC")))
    #   ]
  ) %>% 
  left_join(country_pos_df)

p3 <- plt_data %>% 
  # take max area level for each country
  group_by(iso3) %>% 
  filter(area_level == max(area_level)) %>% 
  ggplot(
    aes(
      # countries on the x-axis, in specified order
      # x = forcats::fct_rev(forcats::fct_relevel(iso3, levels = plot_order)), 
      # x = factor(iso3, levels = rev(plot_order)),
      x = country_idx, 
      # median MC Coverage on the y-axis
      y = median
    )
  ) +
  # add points coloured by region with weighted populations determining size
  geom_jitter(
    # aes(color = region, size = population), 
    aes(size = population), 
    colour = wesanderson::wes_palette("Zissou1")[1],
    shape = 20, 
    width = 0.1, 
    alpha = 0.5
  ) +
  # add median national level to plot as white dots
  geom_point(
    data = filter(plt_data, area_level == 0),
    size = 5, 
    # fill = "white", 
    # fill = "#DCDCDC",
    # fill = "#F5F5F5",
    fill = "#F1F1F1",
    col = "black", 
    alpha = 1, 
    pch = 21, 
    stroke = 1.1
  ) +
  # add vertical line at 90% circumcision
  geom_hline(
    yintercept = 0.9, 
    size = 0.8,
    linetype = "dashed",
    colour = "grey50"
  ) +
  # add horizontal line to split VMMC and non-VMMC countries
  geom_vline(xintercept = country_positions1) + 
  # annotate plot with regional labels
  annotate(
    geom = "text",
    # x = zoo::rollmean(c(country_positions1, 0), 2),
    x = c(length(plot_order) + 1), # , length(plot_order) - (country_positions1 + 4)),
    y = 0.04,
    # label = c("non-VMMC", "VMMC"),
    label = "non-VMMC",
    # angle = 270,
    fontface = "bold",
    size = 5
  ) +
  annotate(
    geom = "text",
    # x = zoo::rollmean(c(country_positions1, 0), 2),
    x = c(length(plot_order) - (country_positions1 + 4)),
    y = 0.025,
    label = "VMMC",
    fontface = "bold",
    size = 5
  ) +
  # add Oli's (unbroken) theme 
  theme_minimal() + 
  theme(
    legend.position = "bottom", 
    strip.text = element_text(size = 13), 
    plot.title = element_text(size = 16), 
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    # strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = NA, colour = "white"), 
    plot.tag = element_text(size = 16), 
    panel.background = element_rect(fill = NA, color = "black")
  ) +
  scale_x_continuous(
    element_blank(),
    breaks = country_pos_df$country_idx,
    minor_breaks = NULL,
    labels = country_pos_df$country,
    # position = "left",
    expand = expansion(add = 0.6)
  ) +
  scale_y_continuous(
    n.breaks = 6, 
    breaks = c(0, 0.25, 0.5, 0.75, 0.9, 1),
    limits = c(0, 1),
    labels = scales::percent
  ) +
  scale_size_continuous(
    # breaks = c(0.5, 1, 10, 20, 100), 
    breaks = c(1, 10, 20), 
    # labels = paste0(c(0.5, 1, 10, 20, 100), "x")
    labels = paste0(c(1, 10, 20), "x")
  ) +
  labs(
    y = "Median Male Circumcision Coverage", 
    x = element_blank(), 
    # size = "District pop. relative\nto median district size", 
    size = "District pop. relative to\n median district size", 
    color = "Region"
  ) +
  ggtitle(paste0(
    "District-Level MC Coverage, ", spec_years[2], " ages ", spec_age_group, " years old"
  )) + 
  scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1, 4)]) +
  theme(
    legend.title.align = 0.5,
    # legend.text.align = -3, # not working!
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 15),
    legend.position = "bottom",
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = c(rep(15, 3), 18, 15)),
    axis.text.y = ggtext::element_markdown(size = 17), # hjust = 0.5),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) + 
  # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  # coord_flip(ylim = c(0.04, 0.975), clip = "off")
  coord_flip(clip = "off")
p3

p3$plot_order <- plot_order

ggplot2::ggsave(
  "paper_poster_plots/paper/plots/03_subnat_plot.png", 
  p3, 
  width = 12, 
  height = 10,
  units = "in"
)

saveRDS(p3, "paper_poster_plots/paper/plots/03_subnat_plot.RDS")

rm(p3); gc()

# may have to make narrower (save with RStudio exporter)
# ggsave(plot = p2b, filename = "poster/plots/p2b.png", width = 650, height = 632, units = "px")

# export with width = 900, height = 644
# export with width = 800, height = 632
# can afford to make this taller, have it as width = 830, height = 900

#### Figure 4: Geofaceted plot for single ages ####

p4 <- plt_coverage_year_national_single_age(
  results_age  = results_age,
  areas        = areas,
  last_surveys = last_surveys,
  spec_ages    = 0:60,
  spec_year    = last(spec_years),
  spec_model   = "No program data",
  main         = "",
  # n_plots      = length(unique(results_agegroup$iso3))
  n_plots      = length(unique(results_age$iso3))
)[[1]]

# grid for geofaceting, standardise country names
ssa_grid <- geofacet::africa_countries_grid1 %>%
  mutate(
    name = case_when(
      grepl("Ivoire", name)                      ~ "Cote d'Ivoire",
      name == "Gambia"                           ~ "The Gambia",
      name == "Democratic Republic of the Congo" ~ "DR Congo",
      name == "Republic of the Congo"            ~ "Congo",
      name == "Equatorial Guinea"                ~ "Eq. Guinea",
      name == "Central African Republic"          ~ "Cent. Af. Rep.",
      TRUE                                       ~ name
    )
  ) %>%
  filter(name %in% c(ssa_countries, "Eq. Guinea", "Cent. Af. Rep."))

# remove missing rows and columns (only looking at SSA, not all of Africa)
min_row <- min(ssa_grid$row)
if (min_row > 1) {
  ssa_grid <- ssa_grid %>%
    mutate(row = row - (min_row - 1))
}
min_col <- min(ssa_grid$col)
if (min_col > 1) {
  ssa_grid <- ssa_grid %>%
    mutate(col = col - (min_col - 1))
}

p4_geo <- p4 +
  # geofacet based on SSA shape
  geofacet::facet_geo(~ area_name, grid = ssa_grid) +
  # make x-axis text smaller to fit all of map
  # ggtitle(
  #   paste0("MMC & TMC Coverage, 2010-2020, ", spec_age_group, " year olds")
  # ) +
  ggtitle(paste0("MMC & TMC Coverage vs Age, 2020")) +
  theme(
    strip.text = element_text(size = 20, face = "bold"),
    legend.position = "none", # remove legend, add with crop
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Arial"),
    axis.text.x = element_text(size = 14, face = "bold", angle = 0, hjust = 1, vjust = 1)
  ) +
  labs(y = "") # remove x-axis
  # scale_x_continuous(
  #   # breaks = seq(spec_years[1], last(spec_years), by = 2),
  #   breaks = seq(spec_years[1], last(spec_years), by = 5),
  #   limits = c(spec_years[1] - 0.25, last(spec_years) + 0.75)
  # )
p4_geo

# for legend:
p4_geo <- p4_geo +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20, face = "bold")
  )

# saveRDS(p4_geo, "paper_poster_plots/paper/data/0x_circ_type_vs_age_2020.RDS")
# p4_geo <- readRDS("paper_poster_plots/paper/data/0x_circ_type_vs_age_2020.RDS")

ggplot2::ggsave(
  "paper_poster_plots/paper/plots/04_geo_age.png", 
  p4_geo, 
  width = 16, 
  height = 13,
  units = "in"
)

saveRDS(p4_geo, "paper_poster_plots/paper/plots/04_geo_age.RDS")

#### Figure 5: Geofaceted plot for circ rates (i.e. n circs by type) ####

p5 <- plt_coverage_year_national(
  results_agegroup = results_agegroup_rate,
  areas            = areas,
  last_surveys     = last_surveys,
  spec_age_group   = "0+",
  # spec_years       = spec_years,
  spec_years       = spec_years,
  spec_model       = "No program data", 
  spec_types = paste(c("MC", "MMC", "TMC"), "probability"),
  main             = "",
  n_plots          = length(unique(results_agegroup_rate$iso3))
)[[1]]

# grid for geofaceting, standardise country names
ssa_grid <- geofacet::africa_countries_grid1 %>%
  mutate(
    name = case_when(
      grepl("Ivoire", name)                      ~ "Cote d'Ivoire",
      name == "Gambia"                           ~ "The Gambia",
      name == "Democratic Republic of the Congo" ~ "DR Congo",
      name == "Republic of the Congo"            ~ "Congo",
      name == "Equatorial Guinea"                ~ "Eq. Guinea",
      name == "Central African Republic"          ~ "Cent. Af. Rep.",
      TRUE                                       ~ name
    )
  ) %>%
  filter(name %in% c(ssa_countries, "Eq. Guinea", "Cent. Af. Rep."))

# remove missing rows and columns (only looking at SSA, not all of Africa)
min_row <- min(ssa_grid$row)
if (min_row > 1) {
  ssa_grid <- ssa_grid %>%
    mutate(row = row - (min_row - 1))
}
min_col <- min(ssa_grid$col)
if (min_col > 1) {
  ssa_grid <- ssa_grid %>%
    mutate(col = col - (min_col - 1))
}

p5_geo <- p5 +
  # geofacet based on SSA shape
  geofacet::facet_geo(~ area_name, grid = ssa_grid) +
  # make x-axis text smaller to fit all of map
  ggtitle(
    paste0("MMC & TMC Rates vs Year, 2010-2020, ", spec_age_group, " year olds")
  ) +
  theme(
    strip.text = element_text(size = 20, face = "bold"),
    legend.position = "none", # remove legend, add with crop
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Arial"),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 1, vjust = 1)
  ) + 
  labs(y = "") + # remove x-axis
  scale_x_continuous(
    # breaks = seq(spec_years[1], last(spec_years), by = 2),
    breaks = seq(spec_years[1], last(spec_years), by = 5),
    limits = c(spec_years[1] - 0.25, last(spec_years) + 0.75)
  ) + 
  scale_y_continuous(
    breaks = scales::pretty_breaks(5),
    limits = c(0, 100 * (round(max(p5$data$mean.y) + 5 * 10 ^ (-2), 1)))
  ) + 
  NULL
# remove hline, only applicable for coverage
p5_geo$layers[[1]] <- NULL
p5_geo

# for legend: 
p5_geo +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20, face = "bold")
  )

saveRDS(p5_geo, "paper_poster_plots/paper/data/05_circ_rates.RDS")
ggsave("paper_poster_plots/paper/plots/05_circ_rates.png",
       p5_geo,
       width = 12, 
       height = 10,
       units = "in")

#### Figure y: Change in MC/MMC/TMC from 2000 ####

no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD")
vmmc_cntries <- countrycode::countrycode(vmmc_iso3, "iso3c", "country.name")

# Want to show change in MC/TMC/MMC from 2000 to 2020
# On y axis, will have country names 
# On x axis, will have coverages in both 2000 and 2020
# An arrow will show the direction of change (i.e. increase/decrease)
# faceting by circumcision type, clearly shows decreasing TMC!!

## Preprocessing ##

# Try add relative change as well as absolute!
no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD")
vmmc_cntries <- countrycode::countrycode(vmmc_iso3, "iso3c", "country.name")

tmp <- results_agegroup %>% 
  # initial filter
  filter(
    area_level == 0, 
    type %in% paste0(c("MC", "MMC", "TMC"), " coverage"),
    # age_group == spec_age_group, 
    age_group == "15-29",
    year %in% c(2000, 2020)
  ) %>% 
  mutate(
    area_name = case_when(
      grepl("Tanzania", area_name) ~ "Tanzania", 
      TRUE                         ~ area_name
    ),
    area_name = case_when(
      grepl("Gambia", area_name) ~ "Gambia", 
      TRUE                         ~ area_name
    )
  ) %>% 
  # join in ESA-WCA regions for colour
  left_join(threemc::esa_wca_regions, by = c("area_id" = "iso3")) %>% 
  arrange(region, desc(mean)) %>% 
  # convert coverages to wide format by year
  select(-c(population, lower, upper, median, sd)) %>% 
  tidyr::pivot_wider(values_from = mean, names_from = year) %>% 
  rename(upper = `2020`, lower = `2000`) %>% 
  relocate(upper, .after = lower) %>% 
  mutate(
    # have NAs for countries with no type information
    across(lower:upper, ~ ifelse(
      area_id %in% no_type_iso3 & grepl("MMC|TMC", type),
      NA, 
      .
    ))
  ) %>% 
  # calculate difference from 2000 to 2020 (may be positive or negative)
  mutate(
    diff = upper - lower
  ) %>% 
  # simplify type name
  mutate(
    type = case_when(
      grepl("MMC", type)  ~ "MMC", 
      grepl("TMC", type)  ~ "TMC", 
      TRUE                ~ "MC"
    )
  ) 
  

## Plotting ##
  
# Setting factor for ggplot2
plot_order <- c(
  "SEN", "GMB", "GNB", "GIN", "SLE", "LBR", "MLI", "BFA", "CIV", "GHA", "TGO", 
  "BEN", "NER", "NGA", "CMR", "TCD", "CAF", "SSD", "ETH", "GAB", "COG", "COD",
  "UGA", "KEN", "RWA", "BDI", "TZA", "AGO", "ZMB", "MWI", "MOZ", "ZWE", "NAM", 
  "SWZ", "LSO", "ZAF"
)
plot_order <- plot_order[plot_order %in% results_agegroup$iso3]
tmp$area_id <- factor(tmp$area_id, levels = plot_order)
tmp <- tmp[order(tmp$area_id), ]

# join region
tmp <- tmp %>%
  # remove WCA/ESA categorisation
  select(-region) %>% 
  # Add Western/Middle/Eastern/Southern categorisation
  left_join(iso_df, by = c("area_id" = "iso3")) %>% 
  # arrange region as a factor, approximately counter-clockwise
  mutate(
    region = factor(region, levels = c("Western Africa", 
                                       "Middle Africa",
                                       "Eastern Africa", 
                                       "Southern Africa")
    )
  ) %>%
  # arrange by region (as a factor) and area_name (alphabetically)
  arrange(region, area_name) %>%
  # convert country to factor
  mutate(area_name = fct_rev(fct_inorder(area_name)))

# finally, add country position as in figure 3 (need to functionalise!)
tmp <- tmp %>% 
  mutate(
    vmmc = ifelse(iso3 %in% vmmc_iso3, "VMMC", "None-VMMC"), 
    # country_idx = as.integer(fct_rev(iso3)) +
    # country_idx = as.integer(iso3) +
    #   c(0, 1, 2, 3)[
    #     match(region, rev(c("None-VMMC", "VMMC")))
    #   ]
  ) %>% 
  left_join(country_pos_df)

annotate_df <- data.frame(
  country_idx = c(
    c(length(plot_order) + 1),
    c(length(plot_order) - (country_positions1 + 4))
  ),
  value = c(-0.12, -0.15), 
  type  = factor(c("MC", "MC"), levels = c("MC", "MMC", "TMC")),
  label = c("Non-VMMC", "VMMC")
)

# convert to long format for plot
tmp_long <- tmp %>% 
  # select(-c(mean, sd, median)) %>% 
  tidyr::pivot_longer(lower:upper, names_to = "year") %>% 
  mutate(year = ifelse(year == "lower", 2000, 2020))

tmp_long_lower <- tmp_long %>% filter(year == 2000)
tmp_long_upper <- tmp_long %>% filter(year == 2020)

py <- tmp_long %>%
  ggplot() +
  geom_point(aes(x = country_idx, y = value, colour = factor(year)), size = 3) +
  geom_segment(
    data = tmp_long_lower,
    aes(
      x      = country_idx,
      y      = value,
      xend   = tmp_long_upper$country_idx,
      yend   = tmp_long_upper$value # ,
      # colour = factor(year)
    ), 
    arrow = arrow(length = unit(0.3, "cm")), 
    size = 1
  ) +
  # horizontal line at 0% 
  # geom_hline(
  #   yintercept = 0,
  #   size = 0.8,
  #   colour = "grey50"
  # ) +
  # add vline for VMMC - non-VMMC split
  geom_vline(xintercept = country_positions1) +
  geom_text(
    data = annotate_df, 
    # aes(label = label),
    aes(x = country_idx,  y = value, label = label), 
    size = 5, 
    fontface = "bold"
  ) +
  # label countries, with space for vline splitting VMMC & non-VMMC
  scale_x_continuous(
    element_blank(),
    breaks = country_pos_df$country_idx,
    minor_breaks = NULL,
    labels = country_pos_df$country,
    expand = expansion(add = 0.6)
  ) +
  scale_y_continuous(
    label = scales::label_percent(),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2)
  ) +
  labs(
    x        = "Country",
    y        = "Change in Coverage (%)",
    colour   = "Year",
    title    = "Absolute change in male circumcision coverage between 2000 and 2020 (15-29 year olds)",
    subtitle = ""
  ) +
  facet_wrap(type ~ .) + # , scales = "free") +
  theme_minimal() +
  # Altering plot text size
  theme(
    axis.text.x      = element_text(size = 14),
    axis.text.y      = element_text(size = 16),
    strip.text       = element_text(size = 16),
    legend.text      = element_text(size = 18),
    legend.title     = element_text(size = 18),
    axis.title       = element_text(size = 18),
    plot.title       = element_text(size = 22, hjust = 0.5),
    legend.position  = "bottom",
    strip.background = element_rect(fill = NA, colour = "white"),
    plot.tag         = element_text(size = 16, face = "bold"),
    panel.background = element_rect(fill = NA, color = "black")
  ) +
  coord_flip(clip = "off")

py

ggplot2::ggsave(
  "paper_poster_plots/paper/plots/0y_change_00_20.png",
  py,
  width = 16,
  height = 10,
  units = "in"
)


#### Figure x: National Level Coverage 2010-2020 ####

# Create area plot of (type-split) coverage for each country

# TODO: Add boxes around countries in this plot, as sometimes country names 
# unclearly linked to 
# TODO: Could also do for other age groups?? Without dashed line/with lower for 15-49 year olds

px <- plt_coverage_year_national(
  results_agegroup = results_agegroup,
  areas            = areas,
  last_surveys     = last_surveys,
  spec_age_group   = spec_age_group,
  # spec_years       = spec_years,
  spec_years       = spec_years[1]:last(spec_years),
  spec_model       = "No program data",
  main             = "",
  # n_plots          = length(unique(results_agegroup_esa_wca[[i]]$iso3))
  n_plots          = length(unique(results_agegroup$iso3))
)[[1]]

# grid for geofaceting, standardise country names
ssa_grid <- geofacet::africa_countries_grid1 %>%
  mutate(
    name = case_when(
      grepl("Ivoire", name)                      ~ "Cote d'Ivoire",
      name == "Gambia"                           ~ "The Gambia",
      name == "Democratic Republic of the Congo" ~ "DR Congo",
      name == "Republic of the Congo"            ~ "Congo",
      name == "Equatorial Guinea"                ~ "Eq. Guinea",
      name == "Central African Republic"          ~ "Cent. Af. Rep.",
      TRUE                                       ~ name
    )
  ) %>%
  filter(name %in% c(ssa_countries, "Eq. Guinea", "Cent. Af. Rep."))

# remove missing rows and columns (only looking at SSA, not all of Africa)
min_row <- min(ssa_grid$row)
if (min_row > 1) {
  ssa_grid <- ssa_grid %>%
    mutate(row = row - (min_row - 1))
}
min_col <- min(ssa_grid$col)
if (min_col > 1) {
  ssa_grid <- ssa_grid %>%
    mutate(col = col - (min_col - 1))
}

px_geo <- px +
  # geofacet based on SSA shape
  geofacet::facet_geo(~ area_name, grid = ssa_grid) +
  # make x-axis text smaller to fit all of map
  ggtitle(
    paste0("MMC & TMC Coverage vs Year, 2010-2020, ", spec_age_group, " year olds")
  ) +
  theme(
    strip.text = element_text(size = 20, face = "bold"),
    legend.position = "none", # remove legend, add with crop
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Arial"),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 1, vjust = 1)
  ) + 
  labs(y = "") + # remove x-axis
  scale_x_continuous(
    # breaks = seq(spec_years[1], last(spec_years), by = 2),
    breaks = seq(spec_years[1], last(spec_years), by = 5),
    limits = c(spec_years[1] - 0.25, last(spec_years) + 0.75)
  )
px_geo

# for legend: 
px_geo +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20, face = "bold")
  )

# ggsave(plot = px_geo, filename = "poster/plots/p3.png", width = 1000, height = 1060, units = "px")


#### Statistics for Abstract ####

#### Male Circumcision Coverage (MC coverage) ####

## number of circumcisions performed from 2010 to 2020 (with error bounds)
results_agegroup_n_circ %>%
  filter(
    area_level == 0,
    age_group == spec_age_group,
    type %in% c(
      "Number circumcised (MC)",
      "Number circumcised (MMC)",
      "Number circumcised (TMC)"
    ),
    year %in% spec_years
  ) %>%
  arrange(area_id, type, year) %>%
  group_by(area_name, type) %>%
  mutate(
    circs = c(0, diff(mean)),
    upper_circs = c(0, diff(upper)),
    lower_circs = c(0, diff(lower))
  ) %>%
  ungroup() %>% 
  # group_by(type) %>%
  group_by(type) %>% 
  summarise(across(contains("circs"), sum))

results_agegroup_n_circ %>%
  filter(
    area_id %in% target_iso3,
    age_group == spec_age_group,
    type %in% c(
      "Number circumcised (MC)",
      "Number circumcised (MMC)",
      "Number circumcised (TMC)"
    ),
    year %in% spec_years
  ) %>%
  arrange(area_id, type, year) %>%
  group_by(area_name, type) %>%
  mutate(
    circs = c(0, diff(mean)),
    upper_circs = c(0, diff(upper)),
    lower_circs = c(0, diff(lower))
  ) %>%
  ungroup() %>% 
  # group_by(type) %>%
  group_by(type) %>% 
  summarise(across(contains("circs"), sum))

# largest (and lowest) Coverage in 2020
results_agegroup %>% 
  filter(
    area_level == 0,
    age_group == spec_age_group,
    type %in% c("MC coverage", "MMC coverage", "TMC coverage"),
    year == last(spec_years),
    mean != 0 # remove unknown types
  ) %>% 
  arrange(type, desc(mean)) %>% 
  group_by(type) %>% 
  slice(c(1, n())) %>% 
  select(iso3, type, mean, lower, upper)


# largest (and lowest) increase in Coverage (Percentage)
(temp <- results_agegroup %>% 
    filter(
      area_level == 0,
      age_group == spec_age_group,
      type %in% c("MC coverage", "MMC coverage", "TMC coverage"),
      year %in% spec_years
    ) %>% 
    group_by(area_name, type) %>%
    mutate(
      mean = c(0, diff(mean)),
      upper = c(0, diff(upper)),
      lower = c(0, diff(lower))
    ) %>%
    ungroup() %>% 
    group_by(iso3, type) %>%
    summarise(across(mean:upper, sum), .groups = "drop") %>% 
    arrange(type, desc(mean)) %>% 
    group_by(type) %>% 
    slice(c(1, n())))

# pull circumcision coverage in both years for country with the highest 
# increase in coverage
results_agegroup %>% 
  filter(
    area_level == 0,
    age_group == spec_age_group,
    type %in% c("MC coverage", "MMC coverage", "TMC coverage"),
    year %in% spec_years
  ) %>% 
  semi_join(temp, by = c("iso3", "type")) %>% 
  select(iso3, type, mean, lower, upper) %>% 
  arrange(iso3, type, desc(mean)) 

# largest increase in Coverage (Absolute)
results_agegroup_n_circ %>%
  filter(
    area_level == 0,
    age_group == spec_age_group,
    type %in% c(
      "Number circumcised (MC)",
      "Number circumcised (MMC)",
      "Number circumcised (TMC)"
    ),
    year %in% spec_years,
    mean != 0
  ) %>%
  arrange(area_id, type, year) %>%
  group_by(area_name, type) %>%
  mutate(
    circs = c(0, diff(mean)),
    upper_circs = c(0, diff(upper)),
    lower_circs = c(0, diff(lower))
  ) %>%
  ungroup() %>% 
  group_by(iso3, type) %>%
  summarise(across(contains("circs"), sum), .groups = "drop") %>% 
  arrange(type, desc(circs)) %>% 
  group_by(type) %>% 
  slice(c(1, n()))


# Within country median difference in coverage between districts
# lowest and highest coverage, smallest and largest 

temp <- results_agegroup %>%
  filter(
    age_group == spec_age_group,
    type == "MC coverage",
    year == last(spec_years)
  ) %>%
  group_by(iso3) %>%
  # filter(area_level == min(max(area_level), 2))
  filter(area_level == max(area_level))

temp1 <- temp %>%
  filter(mean == min(mean))
temp2 <- temp %>%
  filter(mean == max(mean))

temp <- rbind(temp1, temp2) %>%
  ungroup() %>%
  arrange(iso3, mean)

# find difference between highest and lowest coverage for each country
final <- temp %>%
  group_by(iso3) %>%
  summarise(mean = diff(mean), .groups = "drop") %>%
  arrange(desc(mean))

# find median
median(final$mean)

# find range for country with lowest and highest variation
temp %>%
  filter(iso3 == !!last(final$iso3))
temp %>%
  filter(iso3 == !!first(final$iso3))

## remaining circumcisions required to reach 90% 10-29 in all countries by 2020
# also do for just 15 priority countries

n_remaining <- function(results_agegroup_n_circ) {
  results_agegroup_n_circ %>%
    group_by(iso3) %>% 
    filter(area_level == max(area_level)) %>% 
    ungroup() %>% 
    filter(
      # area_level == 0,
      age_group == spec_age_group,
      # type == "MC coverage",
      type == "Number circumcised (MC)",
      # year == 2020
      year == last(spec_years)
    ) %>%
    # n people required to reach 90% vaccination
    mutate(diff = 0.9 * (population - mean)) %>%
    filter(diff >= 0) %>%
    summarise(sum(diff), .groups = "drop") %>%
    pull()
}

# n circs required to get every country to 90% circ
n_remaining(filter(results_agegroup_n_circ, area_level == 0))
# n circs required to get all priority countries to 90% circ
results_agegroup_n_circ %>% 
  filter(
    area_id %in% target_iso3,
    area_level == 0
  ) %>% 
  n_remaining()

# number of circumcisions performed from 2019 to 2020
n_performed_last_year <- function(results_agegroup_n_circ) {
  results_agegroup_n_circ %>% 
    filter(
      # area_level == 0,
      age_group == spec_age_group,
      grepl("Number circumcised", type),
      year %in% c(last(spec_years) - 1, last(spec_years))
    ) %>% 
    # group_by(type, year) %>% 
    # summarise(across(mean:upper, sum), .groups = "drop") %>%
    # arrange(type, year) %>% 
    # group_by(type) %>% 
    # summarise(across(mean:upper, ~ c(0, diff(.))), .groups = "drop")
    arrange(area_id, type, year) %>%
    group_by(area_name, type) %>%
    mutate(
      circs = c(0, diff(mean)),
      upper_circs = c(0, diff(upper)),
      lower_circs = c(0, diff(lower))
    ) %>%
    ungroup() %>% 
    # group_by(type) %>%
    group_by(type) %>% 
    summarise(across(contains("circs"), sum))
}
n_performed_last_year(filter(results_agegroup_n_circ, area_level == 0))
n_performed_last_year(filter(results_agegroup_n_circ, area_id %in% target_iso3))


results_agegroup_n_circ %>%
  filter(
    area_level == 0,
    age_group == spec_age_group,
    type %in% c(
      "Number circumcised (MC)",
      "Number circumcised (MMC)",
      "Number circumcised (TMC)"
    ),
    year %in% spec_years
  ) %>%
  arrange(area_id, type, year) %>%
  group_by(area_name, type) %>%
  mutate(
    circs = c(0, diff(mean)),
    upper_circs = c(0, diff(upper)),
    lower_circs = c(0, diff(lower))
  ) %>%
  ungroup() %>% 
  # group_by(type) %>%
  group_by(type) %>% 
  summarise(across(contains("circs"), sum))

# Number of districts in priority countries that have achieved 90% circumcision
temp <- results_agegroup %>%
  filter(
    # area_id %in% target_iso3,
    age_group == spec_age_group,
    type == "MC coverage",
    year == last(spec_years)
  ) %>%
  mutate(iso3 = substr(area_id, 1, 3)) %>%
  filter(iso3 %in% target_iso3) %>% 
  group_by(iso3) %>%
  # filter(area_level == min(2, max(.data$area_level))) %>%
  filter(area_level == max(area_level)) %>% 
  ungroup()

nrow(temp)

temp %>% filter(lower >= 0.9) %>% nrow()
temp %>% filter(mean >= 0.9) %>% nrow()
temp %>% filter(upper >= 0.9) %>% nrow()
