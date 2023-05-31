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

#### Metadata ####

orderly_root <- here::here()

spec_age_group <- "10-29"
spec_years <- c(2010, 2020)

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

colourPalette <- rev(colorRampPalette(
  c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf",
    "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")
)(100))

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

# takes a data frame of parameters and performs an orderly search on each row.
# By default, can also load these files (also ran parallel)
load_orderly_data <- function(
    # parameters fed to orderly::orderly_search
  task,
  parameters = NULL, # needs to be a df, not a list!
  query = NULL, 
  filenames = NULL, # name of specific artefact file to load, if desired
  dirs = NULL, # optionally just provide dirs to skip orderly_search
  load_fun = readr::read_csv, # function to load data with, if so desired
  ncores = max(1, parallel::detectCores() - 2), 
  ...
) {
  
  # check that either parameters & query or just dirs have been provided
  # (could also add a query parser here for the parameters!)
  # stopifnot((!is.null(parameters) & !is.null(query)) || !is.null(dirs))
  
  if (is.null(dirs)) {
    # search parameter space specified for previously run orderly tasks
    if (!is.null(parameters)) {
      dirs <- unlist(parallel::mclapply(seq_len(nrow(parameters)), function(i) {
        # give progress (no longer works properly w/ mclapply rather than lapply)
        # message(100 * (i / nrow(parameters)), "% completed") 
        system(sprintf(
          'echo "\n%s\n"', 
          paste0(100 * (i / nrow(parameters)), "% completed", collapse = "")
        ))
        orderly::orderly_search(
          query = query, 
          name = task, 
          parameters = c(parameters[i, ]) # coerces pars df to a list 
        )
      }, mc.cores = ncores))  
    } else {
      dirs <- orderly::orderly_search(
        query = query, 
        name = task
      )
    }
  }
  
  # return NAs in parameters search, but only load files from found directories
  dirs_return <- dirs
  dirs <- dirs[!is.na(dirs)]
  # return dirs if filenames unspecified
  if (is.null(filenames)) return(list("dirs" = dirs_return))
  files <- file.path(
    "archive", 
    task,
    dirs, 
    "artefacts/", # prob don't need this? I just structure my tasks this way
    filenames
  )
  # return filenames if load_fun isn't specified
  if (!is.null(load_fun) == FALSE) return(files)
  return(list(
    "dirs" = dirs_return, 
    "output" = lapply(files, load_fun, ...)
  ))
}

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

# results_agegroup <- load_orderly_data(
#   task = "02final_aggregations", 
#   dirs = results_dirs[!is.na(results_dirs)],
#   filenames = "Results_AgeGroup_Prevalence.csv.gz"
# )$output %>% 
#   bind_rows()
# gc()
# 
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
# if (!"iso3" %in% names(results_agegroup)) {
#   results_agegroup$iso3 <- substr(results_agegroup$area_id, 0, 3)
# }
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

results_agegroup_rate <- load_orderly_data(
  task = "02final_aggregations",
  dirs = results_dirs[!is.na(results_dirs)],
  filenames = "Results_AgeGroup_Probability.csv.gz"
)$output %>%
  bind_rows()
# filter not working here for some reason??
results_agegroup_rate <- results_agegroup_rate[
  results_agegroup_rate$type %in% paste(
    c("MC", "MMC", "TMC"), "probability"
  ),
]
gc()

if (!"iso3" %in% names(results_agegroup_rate)) {
  results_agegroup_rate$iso3 <- substr(results_agegroup_rate$area_id, 0, 3)
}


# VMMC iso3s not in results
# missing_iso3 <- target_iso3[!target_iso3 %in% results_agegroup$area_id]
# missing_iso3 <- target_iso3[!target_iso3 %in% results_age$area_id]
missing_iso3 <- target_iso3[!target_iso3 %in% results_agegroup_rate$area_id]

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
  # filter(iso3 %in% results_agegroup$iso3) %>% 
  # filter(iso3 %in% results_age$iso3) %>% 
  filter(iso3 %in% results_agegroup_rate$iso3) %>% 
  identity()

# last surveys 
last_surveys <- readr::read_csv("global/most_recent_surveys.csv")

# Additional areas from Oli, so map plot can be for all of SSA
dat_loc <- "global/"
non_ssa_afr_areas <- sf::read_sf(
  file.path(
    dat_loc, 
    "Longitude_Graticules_and_World_Countries_Boundaries-shp/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp"
  )
) %>%
  filter(
    CNTRY_NAME %in% c(
      "Western Sahara", "Mauritania", "Morocco", "Algeria", 
      "Libya", "Tunisia", "Egypt", "Equatorial Guinea", 
      "Somalia", "Djibouti", "Eritrea", "Sudan"
    )
  )

# areas for SDN and SSD
sudanese_areas <-   
  bind_rows(
    sf::read_sf(file.path(dat_loc, "sdn_adm_cbs_nic_ssa_20200831_shp/sdn_admbnda_adm1_cbs_nic_ssa_20200831.shp")),
    sf::read_sf(file.path(dat_loc, "ssd_admbnda_imwg_nbs_shp/ssd_admbnda_adm0_imwg_nbs_20180817.shp"))
  ) %>% 
  select(CNTRY_NAME = ADM0_EN) %>% 
  group_split(CNTRY_NAME) %>% 
  purrr::map(function(x) {
    cntry <- unique(x$CNTRY_NAME)
    x <- sf::st_union(x) # may need sf::st_combine?
    df <- data.frame("CNTRY_NAME" = cntry)
    sf::st_geometry(df) <- x
    return(df)
  }) %>%  
  bind_rows()

non_ssa_afr_areas <- non_ssa_afr_areas %>% 
  bind_rows(sudanese_areas) %>% 
  mutate(
    iso3 = countrycode::countrycode(CNTRY_NAME, "country.name", "iso3c"),
    area_level = 0
  ) %>% 
  filter(!is.na(iso3)) %>% 
  select(iso3, area_level)

gc()

#### Figure 2: Map of MC Coverage across SSA 2010-2020 10-29 year olds ####

## Map Plot ##
main_title <- paste0(
  "Male Circumcision Coverage, ",
  paste0(spec_years[1], "-", spec_years[2]),
  ", age ",
  spec_age_group,
  " years"
)

# add non-SSA countries to areas
areas_all_afr <- bind_rows(areas, non_ssa_afr_areas) %>% 
  filter(!is.na(iso3))

p2 <- plt_coverage_map(
  # results_agegroup   = filter(results_agegroup, type == "MC coverage"),
  results_agegroup   = results_agegroup %>% 
    filter(type %in% paste(c("MC", "MMC", "TMC"), "coverage")),  
  areas              = areas_all_afr,
  colourPalette      = colourPalette,
  spec_age_group     = spec_age_group,
  spec_years         = spec_years,
  spec_model         = "No program data",
  plot_type          = "map",
  country_area_level = 0, 
  inc_difference     = TRUE,
  spec_main_title    = main_title
)

# make facets vertical rather than horizontal!
# p2 <- p2 + 
#   ggtitle("") + # remove title
#   # ggtitle("Male Circumcision Coverage, 2010-2020") +
#   facet_wrap(~ year, dir = "v") + # make maps vertical
#   # make colour bar vertical and smaller
#   scale_fill_gradientn(
#     colours = colourPalette, 
#     # breaks = seq(0, 1, by = 0.2),
#     breaks = seq(0, 1, by = 0.2),
#     limits = c(0, 1),  
#     label = scales::label_percent(accuracy = 1),
#     guide = guide_colourbar(
#       direction = "vertical", 
#       label = TRUE, 
#       draw.ulim = TRUE,
#       draw.llim = TRUE,
#       frame.colour = "black", 
#       ticks = TRUE, 
#       # barheight = 1,
#       barheight = 30,
#       # barwidth = 30
#       barwidth = 1
#     )
#   ) + 
#   theme(
#     plot.title = element_text(size = 30, face = "bold"),
#     # increase colourbar text size
#     legend.text = element_text(size = 20), 
#     # bold facet names
#     strip.text.x = element_text(size = 26, face = "bold"),
#     legend.position = "right"
#   )
p2


# ggsave(plot = p2a, filename = "poster/plots/p2a.png", width = 1980, height = 1060, units = "px")
# ggplot2::ggsave(
#   "paper_poster_plots/paper/plots/02_map_plot.png", 
#   p2, 
#   width = 9, 
#   height = 11,
#   units = "in"
# )
# saveRDS(p2, "paper_poster_plots/paper/plots/02_map_plot.RDS")
rm(p2); gc()
# export with width = 1350, height = 1400!

#### Figure 3: Sub-National Variation in MC Coverage Plot ####

# TODO: Fix country orderings

# order plot West to East, North to South
plot_order <- c(
  "SEN", "GMB", "GNB", "GIN", "SLE", "LBR", "MLI", "BFA", "CIV", "GHA", "TGO", 
  "BEN", "NER", "NGA", "CMR", "TCD", "CAF", "SSD", "ETH", "GAB", "COG", "COD",
  "UGA", "KEN", "RWA", "BDI", "TZA", "AGO", "ZMB", "MWI", "MOZ", "ZWE", "NAM", 
  "SWZ", "LSO", "ZAF"
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

p3 <- plt_data %>% 
  # take max area level for each country
  group_by(iso3) %>% 
  filter(area_level == max(area_level)) %>% 
  ggplot(
    aes(
      # countries on the x-axis, in specified order
      # x = forcats::fct_rev(forcats::fct_relevel(iso3, levels = plot_order)), 
      x = factor(iso3, levels = rev(plot_order)),
      # median MC Coverage on the y-axis
      y = median
    )
  ) +
  # add points coloured by region with weighted populations determining size
  geom_jitter(
    aes(color = region, size = population), 
    shape = 20, 
    width = 0.1, 
    alpha = 0.5
  ) +
  # add median national level to plot as white dots
  geom_point(
    data = filter(plt_data, area_level == 0),
    shape = 21, 
    size = 4, 
    fill = "white", 
    col = "black", 
    alpha = 0.9
  ) +
  # add horizontal line at 90% circumcision
  geom_hline(
    yintercept = 0.9, 
    size = 0.8,
    linetype = "dashed",
    colour = "grey50"
  ) +
  # moz.utils::standard_theme() + # Oli's pretty theme (not working atm)
  # add Oli's (unbroken) theme 
  theme_minimal() + 
  theme(
    legend.position = "bottom", 
    strip.text = element_text(size = 13, face = "bold"), 
    plot.title = element_text(size = 16), 
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14, face = "bold"), 
    legend.text = element_text(size = 12), 
    # strip.text = element_text(face = "bold"), 
    strip.background = element_rect(fill = NA, colour = "white"), 
    plot.tag = element_text(size = 16, face = "bold"), 
    panel.background = element_rect(fill = NA, color = "black")
  ) +
  scale_x_discrete(
    labels = function(x, family = "bold", colour = "black") {
      labs <- countrycode::countrycode(x, "iso3c", "country.name")
      labs <- ifelse(
        labs == "Congo - Brazzaville", 
        "DR Congo",
        ifelse(
          grepl("Congo", labs),
          "Congo",  
          countrycode::countrycode(x, "iso3c", "country.name")
        )
      )
      labs <- glue::glue(
        "<b style='font-family:{family}; color:{colour}'>{labs}</b>"
      )
    }
  ) +
  scale_y_continuous(
    # n.breaks = 5, 
    breaks = c(0, 0.25, 0.5, 0.75, 0.9, 1),
    labels = scales::percent # , 
    # limits = c(0, 1)
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
  ggtitle("District-Level MC Coverage, 2020, ages 10-29 years old") + 
  scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1, 4)]) +
  theme(
    legend.title.align = 0.5,
    # legend.text.align = -3, # not working!
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 15),
    legend.position = "bottom",
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = c(rep(15, 3), 18, 15)),
    axis.text.y = ggtext::element_markdown(size = 17), # hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
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
  spec_age_group   = spec_age_group,
  # spec_years       = spec_years,
  spec_years       = spec_years[1]:last(spec_years),
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

saveRDS(p5_geo, "paper_poster_plots/paper/data/0x_circ_type_vs_age_2020.RDS")
p5_geo <- readRDS("paper_poster_plots/paper/data/0x_circ_type_vs_age_2020.RDS")


#### Figure x: National Level Coverage 2010-2020 10-29 year olds ####

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
