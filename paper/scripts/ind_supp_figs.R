#### Individual Country Supplementary figures of final results ####

#### Libs ####

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(sf)
library(here)
library(gridExtra)
library(data.table)
library(geofacet)
library(scales)
library(ggridges)
library(threemc)
source("Shiny/src/functions.R")
source("paper_poster_plots/paper/scripts/00_funs.R")


#### Metadata ####

orderly_root <- here::here()

save_loc <- "paper_poster_plots/paper/plots/supp_figs/"
if (!dir.exists(save_loc)) create_dirs_r(save_loc)

cntry <- "UGA" # temp, run for one country, might loop thereafter

# spec_age_group <- "10-29"
spec_age_group <- "15-29"
spec_age_group_double <- c(spec_age_group, "30-49")
spec_years <- c(2006, 2020) # 2006 = first year of VMMC programmes
# spec_years_triple <- c(2006, 2015, 2020)
spec_years_triple <- c(
  spec_years[1], 
  # find year midway between first and last year
  spec_years[1] + ceiling(0.5 * (spec_years[2] - spec_years[1])), 
  spec_years[2]
)

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

# countries/regions which have been targeted for 90% circumcision by 2020
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

# run for paper
pars_df$is_paper <- TRUE

# Colour Palette for years in map plot
colourPalette <- rev(colorRampPalette(
  c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf",
    "#e6f598", "#abdda4", "#8CD1A4", "#66C2A4", "#3D94B7", "#2C82B7", "#13699e",
    "#5e4fa2")
)(50))

# Divergent colour palette for change in coverage facets of map plot
colourPalette2 <- c(
  grDevices::colorRampPalette(c("blue","white"))(20), 
  grDevices::colorRampPalette(c("white", "red"))(50)
)

# Add Lake Victoria for blue in map plot for countries it's in
if (cntry %in% c("UGA", "TZA", "KEN")) {
  lake_vic <- rnaturalearth::ne_download(
    scale = 110, type = "lakes", category = "physical") %>% 
    sf::st_as_sf(lakes110, crs = 4269) %>% 
    filter(name == "Lake Victoria") %>% 
    select(name)
} else lake_vic <- NULL


#### Load Data ####

# pull most recent results for age groups
results_dir <- load_orderly_data(
  task = "02final_aggregations", 
  # parameters = pars_df,
  parameters = filter(pars_df, cntry == !!cntry),
  query = "latest(
      parameter:cntry           == cntry && 
      parameter:rw_order        == rw_order &&
      parameter:paed_age_cutoff == paed_age_cutoff &&
      parameter:inc_time_tmc    == inc_time_tmc
    )"
)$dirs
results_dir <- file.path(
  "archive/02final_aggregations/", 
  results_dir, 
  "artefacts/"
)

# results_agegroup <- load_orderly_data(
#   task = "02final_aggregations",
#   dirs = results_dirs[!is.na(results_dirs)],
#   filenames = "Results_AgeGroup_Prevalence.csv.gz"
# )$output %>%
#   bind_rows() %>% 
#   # filter(type %in% paste(c("MC", "MMC", "TMC"), "coverage")) %>% 
#   identity()
# gc()
results_agegroup <- results_reader(type = "age groups", dir_path = results_dir)

# results_age <- load_orderly_data(
#   task = "02final_aggregations", 
#   dirs = results_dirs[!is.na(results_dirs)],
#   filenames = "Results_Age_Prevalence.csv.gz"
# )$output %>% 
#   bind_rows() %>% 
#   # filter(type %in% paste(c("MC", "MMC", "TMC"), "coverage")) %>% 
#   identity()
results_age <- results_reader(type = "age", dir_path = results_dir)
gc()

areas <- load_orderly_data("00a2_areas_join", 
                           query = "latest", 
                           file = "areas.geojson", 
                           load_fun  = sf::read_sf)$output[[1]] %>% 
  filter(iso3 == cntry)

# pull agegroup populations
populations <- load_orderly_data(
  "00c4_pops_aggregate", 
  query = "latest", 
  file = "population_agegroup_aggr.csv.gz"
)$output[[1]] %>% 
  filter(iso3 == cntry)

# pull survey results
# results_survey <- read_circ_data(
#   "global/survey-circumcision-coverage.csv.gz",
#   filters = c("iso3" = cntry, "sex" = "male")
# )
results_survey <- load_orderly_data(
  "00b4_survey_indicators", 
  query = "latest(parameter:is_paper == TRUE)", 
  file = "survey-circumcision-coverage.csv.gz"
)$output[[1]] %>% 
  filter(iso3 == cntry)

# DMPPT2 data
# results_dmppt2 <- read_circ_data(
#   "global/dmppt2-2021_circumcision_coverage.csv.gz",
#   filters = c("iso3" = cntry)
# )

# need to convert dmpppt2 (and survey) age group to our convention
results_survey <- survey_points_dmppt2_convert_convention(results_survey)
# results_dmppt2 <- survey_points_dmppt2_convert_convention(results_dmppt2)

# order by area hierarchy
results_agegroup <- order_area_name(results_agegroup, areas = areas)
results_age <- order_area_name(results_age, areas = areas)

# fixes case where there are areas with no survey data: model uses spatial
# smoothing to still produce a fit for these regions
results_survey <- results_survey %>%
  filter(area_name %in% unique(results_agegroup$area_name))

# add parent_area_name using area_name from areas, if missing
# TODO: Make into function in threemc
# areas <- setDT(areas)
# areas_join <- copy(areas)
# parent_names <- c("parent_area_id", "parent_area_name")
# keep_cols <- c("area_id", parent_names)
# keep_cols <- keep_cols[keep_cols %in% names(areas)]
# areas_join <- areas_join[, ..keep_cols]
# 
# # add parent_area_name using area_name from areas, if missing
# if (!"parent_area_name" %in% names(areas_join)) {
#   areas_join <- data.table::merge.data.table(
#     x = areas_join, 
#     y = data.table::setnames(
#       areas[, .(iso3, area_id, area_name, 
#                 area_level, area_level_label, area_sort_order)],
#       old = c("area_id", "area_name"),
#       new = parent_names
#     ),
#     all.x = TRUE
#   )
# }
# areas_join <- as_tibble(areas_join) %>% 
#   distinct()
areas <- setDT(areas)
areas_join <- copy(areas)
parent_names <- c("parent_area_id", "parent_area_name")
keep_cols <- c("area_id", parent_names)
keep_cols <- keep_cols[keep_cols %in% names(areas)]
areas_join <- areas[, ..keep_cols]
# add parent_area_name using area_name from areas, if missing
if (!"parent_area_name" %in% names(areas_join)) {
  areas_join <- data.table::merge.data.table(
    x = areas_join, 
    y = data.table::setnames(
      areas[, .(area_id, area_name)],
      old = c("area_id", "area_name"),
      new = parent_names
    ),
    all.x = TRUE
  )
}
# add back in other columns to areas_join
areas_join <- unique(areas_join)
areas_join <- data.table::merge.data.table(
  areas_join, 
  areas, 
  all.x = TRUE, 
  by = c("area_id", "parent_area_id")
)

# reset areas to sf 
areas <- st_as_sf(as_tibble(areas))

# add in required columns from areas and order area names
results_agegroup <- left_join(results_agegroup, areas_join)
results_age <- left_join(results_age, areas_join)
results_survey <- left_join(results_survey, areas_join)
# results_dmppt2 <- left_join(results_dmppt2, areas_join)

if (!"parent_area_id" %in% names(results_survey)) {
  results_survey <- results_survey %>%
    left_join(
      areas %>%
        distinct(area_id, parent_area_id)
    )
}

results_agegroup <- order_area_name(results_agegroup)
results_survey <- order_area_name(results_survey)


#### Single Plots ####

# save locs
save_loc_single <- file.path(save_loc, "01_single_plots", cntry, "/")
if (!dir.exists(save_loc_single)) create_dirs_r(save_loc_single)
save_loc_1 <- file.path(save_loc_single, "01_coverage_prevalence.pdf")
save_loc_2 <- file.path(save_loc_single, "02_coverage_probability_ages.pdf")
save_loc_3 <- file.path(save_loc_single, "03_map_coverage.pdf")
save_loc_4 <- file.path(save_loc_single, "04_area_facet_coverage.pdf")
save_loc_5 <- file.path(save_loc_single, "05_area_facet_coverage_ages.pdf")
save_loc_6 <- file.path(save_loc_single, "06_ridge_posterior_mean_age.pdf")

## First plot, of Circumcision Coverage vs Year
plt_mc_coverage_prevalence(
    results_agegroup,
    areas,
    # spec_age_group = "10+", # why this age group??? change to 0+!
    spec_age_group = "0+",
    spec_years = spec_years,
    area_levels = unique(results_agegroup$area_level), # use all area levels
    spec_model = "No program data",
    main = "Circumcision Coverage vs Year, ",
    str_save = save_loc_1,
    save_width = 16,
    save_height = 7.5,
    n_plots = 1
)

## second plot, of circumcision coverage vs age
plt_age_coverage_by_type(
    results_age,
    areas,
    spec_years = spec_years_triple,
    area_levels = unique(results_age$area_level), # use all area levels
    spec_model = "No program data",
    # spec_ages = c(0, 60),
    main = "Circumcision Coverage vs Age, ",
    str_save = save_loc_2,
    save_width = 15,
    save_height = 11,
    n_plots = 1
)

## map of country, showing difference in circumcision coverage since 2010 for
# different types
# plt_coverage_map(
#     results_agegroup, 
#     areas, 
#     colourPalette = colourPalette,
#     # spec_age_group = "15-49", 
#     spec_age_group = spec_age_group,
#     spec_years = spec_years,
#     spec_model = "No program data", 
#     plot_type = "single",
#     country_area_level = 1, 
#     spec_countries = cntry, 
#     str_save = save_loc_3,
#     inc_difference = TRUE
# )

plt_coverage_map_change(
    results_agegroup, 
    areas, 
    lake_vic = lake_vic, 
    colourPalette, # for coverages
    colourPalette2, # for change in coverages
    spec_age_group, 
    spec_years, 
    spec_model = "No program data",
    # spec_main_title    = main_title,
    spec_main_title = paste0(
      cntry, 
      " circumcision coverage, ", 
      paste0(spec_years[1], "-", spec_years[2]), 
      ", ",
      spec_age_group, 
      " year olds"
    ),
    country_area_level = 0,
    inc_difference     = TRUE # ,
    # str_save           = save_loc_3
)
 

## circumcision coverage vs year, split by type as ribbon plot
plt_area_facet_coverage(
    results_agegroup,
    areas,
    spec_years = spec_years,
    spec_age_group = spec_age_group,
    area_levels = unique(results_agegroup$area_level),
    spec_model = "No program data",
    str_save = save_loc_4,
    save_width = 24,
    save_height = 21,
    n_plots = 12
)

## circumcision coverage vs age for multiple years
plt_age_coverage_multi_years(
    results_age,
    areas,
    spec_years = spec_years_triple,
    str_save = save_loc_5,
    spec_ages = c(0, 60)
)

## distributions/ridges for mean TMIC and MMC-nT age for different areas
plt_circ_age_ridge(
    results_age,
    areas,
    spec_years = spec_years[2],
    # area_levels =  min(2, max(results_age$area_level)),
    area_levels = 1,
    spec_ages = 0:30, # no circumcisions over 30, is that right?
    # n_plots = 5,
    n_plots = 9,
    str_save = save_loc_6
)

plt_circ_age_ridge_multiple_years <- function(
        results_age,
        areas,
        spec_years = c(2021, 2015, 2010),
        area_levels = unique(results_age$area_level),
        spec_model = "No program data",
        spec_ages = 0:30,
        str_save = NULL,
        save_width = 9,
        save_height = 7,
        n_plots = 8
) {

    # temp fix
    if (length(spec_ages) != 31 || !all(spec_ages == 0:30)) {
        spec_ages <- 0:30
    }

    # Keeping relevant information
    tmp <- results_age %>%
        # Only keeping relevant data
        filter(
            type %in% c("MMC-nTs performed", "TMICs performed") ,
            area_level %in% area_levels,
            model == spec_model,
            year %in% spec_years,
            age %in% spec_ages
            # area_level %in% c(0:1)# ,
            # model == "With program data"
        ) %>%
        # Getting density for the ridge plot
        # Grouping for normalising
        group_by(area_id, year, type) %>%
        # Estimating density
        mutate(density = mean / (2 * sum(mean))) %>%
        ungroup() %>%
        # Altering labels for the plot
        mutate(type = ifelse(grepl("MMC-nT", type), "Medical", "Traditional")) #  %>%
    # order_area_name()

    tmp2 <- tmp %>%
        group_by(area_id, area_name, type, year) %>%
        summarise(
            average_age = weighted.mean(age, w = density),
            average_age_lower = weighted.mean(age, w = lower),
            average_age_upper = weighted.mean(age, w = upper),
            .groups = "drop"
        )

    # add in required columns from areas and order area names
    tmp <- add_area_info(tmp, areas)
    tmp2 <- add_area_info(tmp2, areas)

    # split by area level and number of desired plots
    # tmp <- split_area_level(tmp, n_plots = n_plots, years = TRUE)
    # tmp2 <- split_area_level(tmp2, n_plots = n_plots, years = TRUE)
    tmp <- list(tmp)
    tmp2 <- list(tmp2)

    plots <- lapply(seq_along(tmp), function(i) {
        # lapply(seq_along(tmp[[i]]), function(j) {
        # lapply(seq_along(tmp[[i]][[j]]), function(k) {
        # browser()

        # plt_data <- tmp[[i]][[j]][[k]]
        # plt_data2 <- tmp2[[i]][[j]][[k]]
        plt_data <- tmp[[i]]
        plt_data2 <- tmp2[[i]]

        spec_title <- paste(
            plt_data$year[1],
            plt_data$iso3[1],
            plt_data$area_level[1],
            plt_data$area_level_label[1],
            sep = ", "
        )

        ggplot(plt_data,
               aes(x = age,
                   y = area_name,
                   height = density,
                   # fill = type,
                   # color = type)) +
                   fill = as.factor(year),
                   colour = as.factor(year))) +
            ggridges::geom_density_ridges(
                stat = "identity",
                scale = 1,
                alpha = 0.5,
                color = NA
            )  +
            # Adding average age of circumcision
            geom_point(data = plt_data2,
                       aes(x = average_age,
                           y = as.integer(area_name) - 0.05,
                           # color = type),
                           color = year),
                       inherit.aes = FALSE,
                       show.legend = FALSE) +
            # Adding uncertainty interval of average age of circumcision
            geom_segment(data = plt_data2,
                         aes(x = average_age_lower,
                             xend = average_age_upper,
                             y = as.integer(area_name) - 0.05,
                             yend = as.integer(area_name) - 0.05,
                             # color = type),
                             color = year),
                         inherit.aes = FALSE,
                         show.legend = FALSE) +
            # Colour palette
            # scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)[c(1, 3)]) +
            # scale_fill_manual(values = wesanderson::wes_palette("Zissou1")) +
            # scale_colour_manual(values = wesanderson::wes_palette("Zissou1")) +
            # Setting theme
            theme_minimal() +
            # Splitting by circumcision type
            facet_grid(. ~ type) +
            # Setting labels
            ggtitle(spec_title) +
            labs(y = NULL,
                 x = "Age at circumcision",
                 color = NULL,
                 fill = NULL) +
            # Changing plot themes
            theme(
                axis.title = element_text(size = 24),
                axis.text = element_text(size = 16),
                plot.title = element_text(size = 40, hjust = 0.5),
                strip.text = element_text(size = 16),
                strip.background = element_blank(),
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 24),
                legend.position = "bottom",
                panel.spacing = unit(0.2, "lines")
            )
        # })
        # })
    })

    plots <- rlang::squash(plots)
    if (!is.null(str_save)) {
        ggsave(
            filename = str_save,
            plot = gridExtra:: marrangeGrob(plots, nrow = 1, ncol = 1),
            dpi = "retina",
            width = save_width,
            height = save_height
        )
    } else {
        return(plots)
    }
}

# plt_circ_age_ridge_multiple_years(
#   results_age,
#   areas,
#   spec_years = rev(spec_years_triple),
#   area_levels = unique(results_age$area_level),
#   spec_model = "No program data",
#   spec_ages = 0:30,
#   str_save = save_loc_6
# )

# TODO: Add population pyramid as well!

#### Survey comparison plots ####

# fixes case where there are areas with no survey data: model uses spatial
# smoothing to still produce a fit for these regions
results_survey <- results_survey %>% 
  filter(area_name %in% results_agegroup$area_name)
results_agegroup_comp <- results_agegroup %>% 
  filter(area_name %in% results_survey$area_name)

results_survey <- arrange(results_survey, area_sort_order)
results_agegroup_comp <- arrange(results_agegroup_comp, area_sort_order)

save_loc_survey <- file.path(save_loc, "02_survey_comps", cntry, "/")
if (!dir.exists(save_loc_survey)) create_dirs_r(save_loc_survey)

# Main parameters:
model_select <- unique(results_agegroup_comp$model)
area_lev <- max(results_agegroup_comp$area_level)
years <- unique(results_agegroup_comp$year)
plt_start_year <- max(min(results_agegroup_comp$year), 2000)

# different plot "types", corresponding to different (competing) circ hazards
types <- list("MMC coverage" = c("Medical", "_mmc.pdf"),
              "TMC coverage" = c("Traditional", "_tmc.pdf"),
              "MC coverage" = c("Total", "_all.pdf"))

# Coverage vs year
main_title = "Circumcision Coverage (2011-2021) (Black dots denote survey coverage) - "

lapply(seq_along(types), function(i) {
  plt_MC_modelfit_spec_age(df_results = results_agegroup_comp,
                        df_results_survey = results_survey,
                        # mc_type_model = "Total prevalence",
                        mc_type_model = names(types)[i],
                        # mc_type_survey = types[[i]][[1]],
                        mc_type_survey = names(types)[i],
                        # age_per = c("10-29", "30-49"),
                        age_per = spec_age_group_double,
                        years = plt_start_year:2021,
                        model_type = model_select,
                        # area_level_select = i,
                        # province_split = TRUE,
                        xlab = "Year",
                        ylab = "Circumcision Coverage",
                        title = paste(types[[i]][[1]], main_title),
                        # str_save = paste0("Runs/plots/survey_0", x + 3, "_prevalence_15to49", types[[x]][[2]]),
                        str_save = paste0(save_loc_survey, "survey_0", i,
                                          "_prevalence_15to49",
                                          types[[i]][[2]]),
                        save_width = 16, save_height = 12,
                        n_plots = 14)
})

# Coverage vs age(group)
main_title = "Circumcision Coverage by Age Group (Black dots denote survey coverage) - "

lapply(seq_along(types), function(i) {
    plt_MC_modelfit(df_results = results_agegroup,
                    df_results_survey = results_survey,
                    # mc_type_model = "MMC prevalence",
                    mc_type_model = names(types)[i],
                    # mc_type_survey = types[[i]][[1]],
                    mc_type_survey = names(types)[i],
                    age_per = c("0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29", "30-34",
                                "35-39", "40-44", "45-49", "50-54", "54-59", "60-64"),
                    survey_years =  years[years %in% results_agegroup$year],
                    model_type = model_select,
                    # province_split = TRUE,
                    # area_level_select = i,
                    facet_year = "colour",
                    xlab = "Age Group",
                    ylab = "Circumcision Coverage",
                    title = paste(types[[i]][[1]], main_title),
                    # str_save = paste0("~/survey_0", i, "_age_prevalence", types[[i]][[2]]),
                    str_save = paste0(save_loc_survey, "survey_0", i + 3,
                                      "_age_prevalence",
                                      types[[i]][[2]]),
                    save_width = 16, save_height = 12,
                    n_plots = 14)
})


#### DMPPT2 comparison plots (commented out, not needed anymore!) ####

# save_loc_dmppt2 <- file.path(save_loc, "03_dmppt2_comps", cntry, "/")
# if (!dir.exists(save_loc_dmppt2)) create_dirs_r(save_loc_dmppt2)
# 
# ## Plot 1: Coverage vs Year with DMPPT2 & Survey Points Overload ##
# years <- paste(c(min(results_agegroup$year), max(results_agegroup$year)), collapse = "-")
# plt_start_year <- min(results_agegroup$year)
# 
# main_title <- paste0(
#   "15-29 Prevalence ",
#   "(", years, ")",
#   " - Black line denotes DMPPT2 coverage,",
#   " Blue dots denote Surveyed coverage - "
# )
# 
# save_loc <- file.path(
#   save_loc_dmppt2, "01_prevalence_10-29_vs_dmppt2.pdf"
# )
# 
# # only produce if we have some values from DMPPT2 for these ages
# non_nas <- results_dmppt2 %>%
#   filter(age_group == "15-29", !is.na(mean)) %>%
#   nrow()
# if (non_nas > 0) {
#   plt_dmppt2_compare_year(
#     filter(results_agegroup, area_id %in% results_dmppt2$area_id),
#     results_dmppt2,
#     filter(results_survey, area_id %in% results_dmppt2$area_id),
#     age_per = "15-29",
#     years = plt_start_year:2020,
#     xlab = "Year",
#     ylab = "Circumcision Prevalence",
#     title = main_title,
#     str_save = save_loc,
#     save_width = 16,
#     save_height = 12,
#     n_plots = 14
#   )
# }
# 
# ## Plot 2: Various Ages, for final year(s) of DMPPT2 Data and Survey Data ##
# 
# # main_title <- "Circumcision Prevalence by Age Group - "
# # year_2013 <- max(2013, min(results_dmppt2$year))
# # # plt_years <- unique(c(DMPPT2_last_year, survey_last_year, year_2013))
# # plt_years <- sort(unique(c(max(results_dmppt2$year), year_2013)))
# # 
# # save_loc <- paste0(save_loc_dmppt2, "02_prevalence_age_groups_vs_dmppt2.pdf")
# # 
# # plt_dmppt2_compare_age_group(
# #   filter(results_agegroup, area_id %in% results_dmppt2$area_id),
# #   rename(results_dmppt2, dmppt2_circumcision_coverage = mean),
# #   survey_data = NULL,
# #   age_per = c("0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29", "30-34",
# #               "35-39", "40-44", "45-49", "50-54", "54-59", "60-64"),
# #   years = plt_years,
# #   xlab = "Age Group",
# #   ylab = "Circumcision Prevalence",
# #   title = main_title,
# #   str_save = save_loc,
# #   save_width = 16,
# #   save_height = 12,
# #   n_plots = 6
# # )
# 
# ## Plot 3/2: Compare DMPPT2 Prevalence to threemc Prevalence, 10-29 ##
# 
# main_title <- paste0("10-29 DMPPT2 Prevalence vs threemc Prevalence, ")
# 
# save_loc <- paste0(save_loc_dmppt2,
#                    "02_10-29_dmppt2_survey_vs_threemc_prevalence.pdf")
# 
# # take for maximum dmppt2 year (up to 2020), and (at least) 2013
# spec_years_dmppt2 <- unique(c(
#   # max(results_dmppt2$year), 
#   min(max(results_dmppt2$year), max(spec_years)),
#   max(2013, min(results_dmppt2$year))
# ))
# 
# # save_loc <- "test4.pdf"
# plt_dmppt2_compare_fits(
#     filter(results_agegroup, area_id %in% results_dmppt2$area_id),
#     rename(results_dmppt2, dmppt2_circumcision_coverage = mean),
#     age_per = "10-29",
#     years =  spec_years_dmppt2,
#     xlab = "DMPPT2 Prevalence",
#     ylab = "threemc Prevalence",
#     title = main_title,
#     str_save = save_loc,
#     save_width = 16,
#     save_height = 12
# )
