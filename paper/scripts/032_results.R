#### Create plots for results ####

# Produce summary stats here as well
# must be run in threemc-orderly https://github.com/mrc-ide/threemc-orderly 
# Orderly `archive/` found on Sharepoint https://tinyurl.com/ufpxymmb

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

# VMMC countries  
vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", "UGA", "MWI",
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF" # LSO, SWZ ran already
)
vmmc_cntries <- countrycode::countrycode(vmmc_iso3, "iso3c", "country.name")

# countries with no type information
no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD")

# remaining countries
iso3 <- ssa_iso3[!ssa_iso3 %in% c(vmmc_iso3, no_type_iso3)]



# Colour palette for "% change" facets in map plot
# colourPalette <- rev(colorRampPalette(
#   c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf",
#     "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#13699e",
#     "#5e4fa2")
# )(100))

# Breaks are between -0.2 and -0.5 => 7 breaks overall
# Want everything negative to be blue (so 2 / 7 of colours)
# Change to 14 colours in scale, with first 4 as blue, then each "break" 
# constitutes one colour, and negative values will be in blue
# colourPalette <- rev(colorRampPalette(
colourPalette2 <- rev(colorRampPalette(
  c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf",
    "#e6f598", "#abdda4", "#8CD1A4", "#66C2A4", "#3D94B7", "#2C82B7", "#13699e",
    "#5e4fa2")
)(100))

# change region changing from blue to grean to only blue
# colourPalette[30:40] <- colourPalette[30]
# change greenish-blue to be more green
# colourPalette[41:45] <- colourPalette[45]

# Colour Palette for years in map plot
# colourPalette <- viridis::plasma(100)
colourPalette <- c(
  grDevices::colorRampPalette(c("blue","white"))(40), 
  grDevices::colorRampPalette(c("white", "red"))(100)
)


# plot order for countries, first non-VMMC & then VMMC countries
plot_order <- c(
  "BEN", "BFA", "CIV", "GHA", "GIN", "GNB", "LBR", "MLI", "NER", 
  "NGA", "SEN", "SLE", "GMB", "TGO", "AGO", "CMR", "CAF", "TCD", 
  "COG", "COD", "GAB", "BDI", "ETH", "KEN", "MWI", "MOZ", "RWA", 
  "TZA", "UGA", "ZMB", "ZWE", "BWA", "SWZ", "LSO", "NAM", "ZAF"
)

# where to add horizontal line for VMMC vs non-VMMC
country_positions1 <- length(vmmc_iso3) + 1


#### Load Data ####

# orderly archives
archives <- orderly::orderly_list_archive()

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

# add country position in figs. 3 & 6, with whitespace for horizontal line
plot_order <- plot_order[plot_order %in% results_agegroup$iso3]
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

# areas for SSD (want to add to map plot)
dat_loc <- "paper_poster_plots/aids_2022_poster/data/"
 
ssn_areas <- sf::read_sf(file.path(
  dat_loc, 
  "ssd_admbnda_imwg_nbs_shp/ssd_admbnda_adm0_imwg_nbs_20180817.shp"
)) %>% 
  select(CNTRY_NAME = ADM0_EN) %>% 
  group_split(CNTRY_NAME) %>% 
  purrr::map(function(x) {
    cntry <- unique(x$CNTRY_NAME)
    x <- sf::st_union(x) # may need sf::st_combine?
    df <- data.frame("CNTRY_NAME" = cntry)
    sf::st_geometry(df) <- x
    return(df)
  }) %>%  
  bind_rows() %>% 
  mutate(
    iso3 = countrycode::countrycode(CNTRY_NAME, "country.name", "iso3c"),
    area_id = iso3,
    area_name = CNTRY_NAME,
    area_level = 0
  ) %>% 
  filter(!is.na(iso3)) %>% 
  select(iso3, area_id, area_name, area_level)

# add to areas 
areas_map <- bind_rows(areas, ssn_areas) %>% 
  filter(!is.na(iso3))

# Also add Lake Victoria for blue in map plot
lake_vic <- rnaturalearth::ne_download(
  scale = 110, type = "lakes", category = "physical") %>% 
  sf::st_as_sf(lakes110, crs = 4269) %>% 
  filter(name == "Lake Victoria") %>% 
  select(name)


#### Figure 2: Map of MC Coverage across SSA 20-2020 15-29 year olds ####

## Map Plot ##

# additions: 
# - Have single faceted R plot, with tag in corner (done, no tags though ..)
# - Text very large compared to the images, reduce size!
# - Remove North African countries from plot
# - For "% Change%", have a different colour bar, which goes into negative numbers (e.g. -30% to 70%)
# - Can put year labels, can put in upper left of Africa, leaves more space for image

# Qs: 
# - How do we want colour bars displayed? Like Tristan, on the side of his plot? 

main_title <- paste0(
  # "MC Coverage, ",
  "Circumcision coverage ", 
  paste0(spec_years[1], "-", spec_years[2]),
  ", ",
  spec_age_group,
  " year olds"
)

country_area_level = 0
results_area_level = NULL
spec_model <- "No program data"
spec_main_title    = main_title

results_agegroup1 <- results_agegroup
areas1 <- areas_map

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

# Create dummy rows of NAs for missing countries - want them grey in map
missing_iso3 <- unique(
  areas_join$iso3[!areas_join$iso3 %in% results_agegroup$iso3]
)

tmp_missing <- tidyr::crossing(
  "iso3" = missing_iso3, 
  "age_group" = tmp$age_group, 
  "year" = tmp$year, 
  "type" = tmp$type
) %>% 
  left_join(areas_join, by = "iso3", relationship = "many-to-many")

# Merge to shapefiles
tmp <- tmp %>% 
  select(-matches("area_name")) %>%
  left_join(areas_join) %>% 
  bind_rows(tmp_missing)

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
spec_results <- tmp
spec_areas <- areas_plot

rm(results_agegroup1, areas1); gc()
   
map_plot <- function(
    spec_results, spec_areas, lake_vic, colourPalette, colourPalette2
  ) {
  
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
    geom_sf(
      data   = lake_vic, 
      colour = "lightblue", 
      fill   = "lightblue",
      size   = 0.5
    ) + 
    labs(fill = "") +
    scale_fill_gradientn(
      colours = colourPalette,
      na.value = "grey",
      breaks = seq(0, 1, by = 0.1),
      limits = c(0, 1),
      label = scales::label_percent(accuracy = 1, trim = FALSE), 
      guide = guide_colourbar(
        direction = "horizontal",
        label = TRUE,
        draw.ulim = TRUE,
        draw.llim = TRUE,
        frame.colour = "black",
        ticks = TRUE,
        barheight = 1,
        # barwidth = 17, # may want this?
        barwidth = 18,
        title.position = "bottom", 
        plot.background = element_rect(fill = "white", colour = "white")
      )
    ) +
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
      breaks = seq(-0.2, 0.5, by = 0.1), 
      limits = c(-0.2, 0.5),
      label = scales::label_percent(accuracy = 1, trim = TRUE), #  prefix = " "),
      guide = guide_colourbar(
        direction = "horizontal", 
        label = TRUE, 
        draw.ulim = TRUE,
        draw.llim = TRUE,
        frame.colour = "black", 
        ticks = TRUE, 
        barheight = 1,
        # barwidth = 10,
        barwidth = 10.5,
        title.position = "bottom"
      )
    ) +
    facet_grid(type ~ year) + 
    theme_minimal(base_size = 9) +
    theme(
      plot.title    = element_text(size = rel(1.6), hjust = 0.5), 
      strip.text    = element_text(size = rel(1.5)), 
      # probably too small, but so annoying to fix!!
      # legend.text   = element_text(size = rel(0.75)) # 0.70 too small
      # legend.text   = element_text(size = rel(0.72), hjust = 0.1),
      legend.text   = element_text(size = rel(0.71)),
      axis.text       = element_blank(),
      axis.ticks      = element_blank(),
      legend.position = "bottom",
      panel.grid      = element_blank(),
      panel.spacing   = unit(0.01, "lines"), # make plot as "dense" as possible
      plot.background = element_rect(fill = "white", colour = "white")
    )
}

p2final <- map_plot(tmp, areas_plot, lake_vic, colourPalette2, colourPalette) + 
  ggtitle(main_title)


# save object for org-mode paper draft
# saveRDS(
#   p2final,
#   "paper_poster_plots/paper/plots/02_map_plot_facet.RDS"
# )

# dev.new(width = 6.3, height = 6.5,  noRStudioGD = TRUE)
# p2final
# dev.off()

# save plots
ggsave(
# png(
  # "paper_poster_plots/paper/plots/02_map_plot_facet.pdf", 
  "paper_poster_plots/paper/plots/02_map_plot_facet.png", 
  p2final,
  width = 6.3, 
  height = 6.5, 
  units = "in"
)

# rm(p2final); gc()


#### Figure 3: Sub-National Variation in MC Coverage Plot ####

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

# add plot position for each country
plt_data <- plt_data %>% 
  mutate(
    vmmc = ifelse(iso3 %in% vmmc_iso3, "VMMC", "None-VMMC"), 
  ) %>% 
  left_join(country_pos_df)

set.seed(123)
p3 <- plt_data %>% 
  # take max area level for each country
  group_by(iso3) %>% 
  filter(area_level == max(area_level)) %>% 
  ggplot(
    aes(
     x = country_idx,  # countries on the x-axis, in specified order
     y = median # median MC Coverage on the y-axis
    )
  ) +
  # add points coloured by region with weighted populations determining size
  geom_jitter(
    aes(size = population), 
    colour = "#BC3C29", # red, to match coverage change plot below
    shape = 20, 
    width = 0.1, # width of jitter, not points!
    alpha = 0.5
  ) +
  scale_size_continuous(
    breaks = c(1, 10, 20), 
    labels = paste0(c(1, 10, 20), "x")
  ) +
  # add median national level to plot as white dots
  geom_point(
    data = filter(plt_data, area_level == 0),
    size = 4, 
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
    x = c(length(plot_order) + 0.85),
    y = 0.01,
    label = "Non-VMMC \nPriority Countries",
    fontface = "bold",
    size = 3.5, 
    hjust = 0
  ) +
  annotate(
    geom = "text",
    x = c(length(plot_order) - (country_positions1 + 3.9)),
    y = 0.01,
    label = "VMMC \nPriority Countries",
    fontface = "bold",
    size = 3.5, 
    hjust = 0
  ) +
  theme_bw(base_size = 8) + 
  scale_x_continuous(
    element_blank(),
    breaks = country_pos_df$country_idx,
    minor_breaks = NULL,
    labels = country_pos_df$country,
    # position = "left",
    expand = expansion(add = 0.6)
  ) +
  scale_y_continuous(
    expand = c(0, 0), 
    limits = c(0, 1), 
    label = scales::label_percent(),
    breaks = seq(0, 1, by = 0.25), 
    n.breaks = 5 
  ) + 
  scale_size_continuous(
    breaks = c(1, 10, 20), 
    range = c(1, 12),
    # breaks = c(100, 200, 300), 
    labels = paste0(c(1, 10, 20), "x")
    # labels = paste0(c(1, 50), "x")
  ) +
  labs(
    # y = "Median Male Circumcision Coverage", 
    y = paste0("Circumcision Coverage (%), ", spec_years[2], ", ", spec_age_group, " year olds"),
    x = element_blank(), 
    size = "District pop. relative to\n median district size", 
    color = "Region"
  ) +
  # ggtitle(paste0(
  #   # "District-Level MC Coverage, ", spec_years[2], " ages ", spec_age_group, " years old"
  #   "District-Level male circumcision coverage, ", spec_years[2], ", ", spec_age_group, " year olds"
  # )) + 
  scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1, 4)]) +
  theme(
    # axis.text.x = element_text(size = c(rep(12, 4), 15, 12), colour = "black"),
    axis.text.x = element_text(size = c(12), colour = "black"),
    axis.title.x = element_text(size = rel(1.5), colour = "black"),
    axis.text.y = element_text(size = rel(1.8), colour = "black"),
    strip.background = element_rect(fill = NA, colour = "white"), 
    panel.background = element_rect(fill = NA, colour = "black"),
    legend.text = element_text(size = rel(1.5), colour = "black"),
    legend.title = element_text(size = rel(1.5), colour = "black"),
    legend.title.align = 0.5,
    legend.position = "bottom",
    plot.title = element_text(hjust = 1, size = rel(1.6)),
    panel.grid.major.y = element_blank(), 
    plot.margin = unit(c(0.2, 0.8, 0, 0), "cm")
  ) + 
  coord_flip(clip = "off")

# p3$plot_order <- plot_order

# dev.new(width = 6.3, height = 8, noRStudioGD = TRUE)
# p3
# dev.off()

# saveRDS(p3, "paper_poster_plots/paper/plots/03_subnat_plot.RDS")
ggplot2::ggsave(
  "paper_poster_plots/paper/plots/03_subnat_plot.png", 
  p3, 
  width = 6.3, 
  height = 8,
  units = "in"
)

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
      # name == "Guinea-Bissau"                    ~ "Gin. Bissau",
      name == "Central African Republic"         ~ "Cent. Af. Rep.",
      TRUE                                       ~ name
    )
  ) %>%
  filter(
    name %in% c(ssa_countries, "Gin. Bissau", "Eq. Guinea", "Cent. Af. Rep.")
  )

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
  # ggtitle(paste0("Medical & traditional male circumcision by age, 2020")) +
  # set theme and base size for text
  theme_bw(base_size = 9) + 
  # reduce x-axis ticks, too crowded
  scale_x_continuous(breaks = seq(0, 60, by = 20)) + 
  # remove x-axis
  # labs(y = "") +  
  labs(
    x = "Age, 2020",
    y = "Circumcision Coverage (%)"
  ) + 
  # labs(title = "",
  #   y = "Circumcision coverage by age, 2020"
  # ) + 
  theme(
    axis.text.x     = element_text(size = rel(1.1)),
    axis.title.x    = element_text(size = rel(1.5)),
    axis.text.y     = element_text(size = rel(1.1)),
    axis.title.y    = element_text(size = rel(1.5)),
    legend.text     = element_text(size = rel(1.2)),
    legend.direction = "vertical",
    # legend.position = "bottom",
    legend.position = c(0.15, 0.2),
    # legend.justification = "left",
    strip.text      = element_text(size = 7.5), # , hjust = -0.1),
    plot.title      = element_text(size = rel(1.5)),
    strip.background = element_blank(),
    plot.background = element_rect(fill = "white", colour = "white")
  )

# dev.new(width = 6.3, height = 6.5,  noRStudioGD = TRUE)
# dev.new(width = 6.3, height = 8,  noRStudioGD = TRUE)
# p4_geo
# dev.off()

# saveRDS(p4_geo, "paper_poster_plots/paper/plots/04_geo_age.RDS")
ggplot2::ggsave(
  "paper_poster_plots/paper/plots/04_geo_age.png", 
  p4_geo, 
  width = 6.3, 
  height = 8,
  units = "in"
)


#### Figure 5: Geofaceted plot of age at circumcision ####


#### Figure 6: Change in MC/MMC/TMC from 2000 ####

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
      grepl("MMC", type) ~ "Medical Male Circumcision", # "MMC", 
      grepl("TMC", type) ~ "Traditional Male Circumcision", # "TMC", 
      TRUE               ~ "Male Circumcision" # "MC"
    )
  ) 
  

## Plotting ##
  
# Setting factor for ggplot2
# plot_order <- c(
#   "SEN", "GMB", "GNB", "GIN", "SLE", "LBR", "MLI", "BFA", "CIV", "GHA", "TGO", 
#   "BEN", "NER", "NGA", "CMR", "TCD", "CAF", "SSD", "ETH", "GAB", "COG", "COD",
#   "UGA", "KEN", "RWA", "BDI", "TZA", "AGO", "ZMB", "MWI", "MOZ", "ZWE", "NAM", 
#   "SWZ", "LSO", "ZAF"
# )
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
    c(length(plot_order) + 0.5),
    c(length(plot_order) - (country_positions1 + 4))
  ),
  # value = c(0.24, 0.14), 
  value = c(0.05, 0.04), 
  # type  = factor(c("MC", "MC"), levels = c("MC", "MMC", "TMC")),
  type  = factor(
    c("Male Circumcision", "Male Circumcision"), 
    levels = c(
      "Male Circumcision", 
      "Medical Male Circumcision", 
      "Traditional Male Circumcision"
    )
  ),
  # label = c("Non-VMMC", "VMMC")
  label = paste0(c("Non-VMMC", "VMMC"), " \nPriority Countries")
)

# convert to long format for plot
tmp_long <- tmp %>% 
  # select(-c(mean, sd, median)) %>% 
  tidyr::pivot_longer(lower:upper, names_to = "year") %>% 
  mutate(year = ifelse(year == "lower", 2000, 2020))

tmp_long_lower <- tmp_long %>% filter(year == 2000)
tmp_long_upper <- tmp_long %>% filter(year == 2020)

# tmp_long_lower1 <- tmp_long_lower %>% 
#   mutate(
#     value = ifelse(
#       !is.na(tmp_long_upper$value) & value < tmp_long_upper$value, 
#       value - 0.05, 
#       value + 0.05)
#   )

# "Nudge" arrows so they point to point edges, rather than centroids
# nudge <- 0.0375
# tmp_long_upper1 <- tmp_long_upper %>% 
#   mutate(
#     value = case_when(
#       is.na(tmp_long_lower$value)            ~ value, 
#       # if 2020 cov > 2000 cov, move back by `nudge` from dot centroid
#       value > (tmp_long_lower$value + nudge) ~ value - nudge,
#       # if 2020 cov < 2000 cov, move forward
#       value + nudge < tmp_long_lower$value   ~ value + nudge,
#       TRUE                                   ~ value
#     )
#   )

p6 <- tmp_long %>%
  ggplot() +
  geom_point(aes(x = country_idx, y = value, colour = factor(year)), size = 3) +
  geom_segment(
    data = tmp_long_lower,
    aes(
      x      = country_idx,
      y      = value,
      xend   = tmp_long_upper$country_idx,
      yend   = tmp_long_upper$value
    ), 
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"), 
    size = 0.6
  ) +
  # add vline for VMMC - non-VMMC split
  geom_vline(xintercept = country_positions1) +
  geom_text(
    data = annotate_df, 
    aes(x = country_idx,  y = value, label = label), 
    size = 2.5, 
    fontface = "bold",
    hjust = 0
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
    expand = c(0, 0), 
    limits = c(0, 1), 
    label = scales::label_percent(),
    # breaks = seq(0, 1, by = 0.2), 
    breaks = seq(0, 1, by = 0.25), 
    n.breaks = 5 # , 
    # minor_breaks = NULL
  ) + 
  labs(
    x        = "Country",
    y        = "Circumcision Coverage (%), 15-29 year olds",
    colour   = ""
    # title    = "Absolute change in male circumcision coverage between 2000 and 2020 (15-29 year olds)",
    # title    = "Change in male circumcision coverage, 2000 - 2020, 15-29 year olds",
    # subtitle = ""
  ) +
  facet_wrap(type ~ .) + # , scales = "free") +
  theme_bw(base_size = 8) +
  # use NEJM colours (want 2020 in red though)
  scale_colour_manual(values = c("#0072B5", "#BC3C29")) + 
  # pal_nejm("default")(2)[2:1] + 
  # Altering plot text size
  theme(
    axis.text.x         = element_text(size = rel(1.2), colour = "black"),
    axis.title.x        = element_text(size = rel(1.2), colour = "black"),
    axis.text.y         = element_text(size = rel(1.2), colour = "black"),
    axis.ticks.length.y = unit(5, "pt"), 
    strip.text          = element_text(
      size = rel(0.9), face = "bold", colour = "black", hjust = 0
    ),
    legend.text         = element_text(size = rel(1.2), colour = "black"),
    legend.position     = c(0.062, 0.9),
    # remove white box behind legend
    legend.background   = element_rect(colour = NA, fill = NA),
    legend.key          = element_rect(colour = NA, fill = NA),
    plot.title          = element_text(
      size = rel(1.4), hjust = 0.5, vjust = -2
    ), 
    # panel spacing and right-hand margin so x-axis labels fit & don't touch
    panel.spacing       = unit(0.65, units = "cm"), 
    panel.border        = element_blank(),
    panel.grid.major.y  = element_blank(),
    plot.margin         = unit(c(0, 0.5, 0.2, 0), "cm"),
    strip.background    = element_rect(fill = NA, colour = "white"),
    panel.background    = element_rect(fill = NA, color = "black")
  ) +
  coord_flip(clip = "off", expand = TRUE)

# dev.new(width = 6.3, height = 6, noRStudioGD = TRUE)
# p6
# dev.off()

# saveRDS(p6, "paper_poster_plots/paper/plots/06_change_00_20.RDS")
ggplot2::ggsave(
  "paper_poster_plots/paper/plots/06_change_00_20.png",
  p6,
  width = 6.3,
  height = 6,
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
