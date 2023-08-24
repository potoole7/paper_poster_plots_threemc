#### Pull through data and plots required for Data subsection of Results ####

# May end up joining this with other scripts or even copying to org file

# This script also contains info on missing values etc which may be of 
# interest in the appendix

# TODO: Check number of surveys, seems very low
# TODO: Remove BWA from plot

# stopifnot(dirname(getwd()) == "threemc-orderly")
while(basename(getwd()) != "threemc-orderly") setwd("../.")

#### Libs ####
library(dplyr, warn.conflicts = FALSE)
library(forcats)
library(ggplot2)
library(reactable)
library(sf)
library(purrr)
library(threemc)
library(orderly)
library(ggsci)
library(geofacet)
library(glue)
library(ggtext)
source("Shiny/src/functions.R") # plotting functions
source("paper_poster_plots/paper/scripts/00_funs.R") # shared functions

#### Metadata ####

# TODO: remove unneeded here 
orderly_root <- getwd()

spec_age_group <- "10-29" # change? 
spec_years <- c(2010, 2020)

# for tabulating surveys: 
rm_missing_type <- FALSE # remove circumcisions with missing type?
start_year <- 2000 # minimum survey year
cens_age <- 59 # maximum circumcision age

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

# countries we attempted to model
ssa_iso3 <- sort(c(
  "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD",
  "COG", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN",
  "LBR", "LSO", "MLI", "MOZ", "MWI", "NAM", "NER", "NGA", "RWA",
  "SEN", "SLE", "SWZ", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
))

# countries for which we have no results
no_mod_iso3 <- c("BWA", "CAF", "GNB")

ssa_countries <- countrycode::countrycode(ssa_iso3, "iso3c", "country.name")
country_name_convention <- function(x) {
  x <- dplyr::case_when(
    x == "Congo - Kinshasa"    ~ "DR Congo",
    x == "Congo - Brazzaville" ~ "Congo",
    grepl("Ivoire", x)         ~ "Cote d'Ivoire",
    x == "Gambia"              ~ "The Gambia",
    TRUE                       ~ x
  )
}
ssa_countries <- country_name_convention(ssa_countries)


#### Load Data ####

# pull most recent results for age groups
archives <- orderly::orderly_list_archive()

# pull shapefiles
areas <- load_archive(
  "00a2_areas_join", 
  orderly_root, 
  "areas.geojson", 
  archives = archives, 
  load_fun = read_sf
)

# pull surveys
survey_circumcision_orig <- survey_circumcision <- load_archive(
  "00b3_survey_join", 
  orderly_root, 
  "survey_circumcision.csv.gz", 
  query = "latest(parameter:is_paper == TRUE)"
)

survey_circumcision <- survey_circumcision %>% 
  filter(!iso3 %in% no_mod_iso3)

# pull agegroup populations
populations <- load_archive(
  "00c4_pops_aggregate", 
  orderly_root, 
  "population_agegroup_aggr.csv.gz", 
  archives = archives
)

# last surveys
# TODO: Update these
last_surveys <- readr::read_csv("global/most_recent_surveys.csv")

# Prepare circ data, normalise survey weights and apply Kish coefficients.
survey_circumcision <- prepare_survey_data(
  areas               = areas,
  survey_circumcision = select(survey_circumcision, -matches("area_level")),
  area_lev            = threemc::datapack_psnu_area_level,
  start_year          = start_year,
  cens_year           = TRUE,
  cens_age            = cens_age,
  rm_missing_type     = rm_missing_type,
  norm_kisk_weights   = TRUE
)

survey_circumcision <- data.table::rbindlist(survey_circumcision)

#### Figure: Tabulate Surveys ####

# create initial table and find country, provider, year from survey_id
provider_key <- tibble::tribble(
  ~provider, ~provider_name,
  "AIS",     "AIDS Indicator Survey",
  "DHS",     "Demographic and Health Survey",
  "PHIA",    "Population-Based HIV Impact Assessment",
  "HSRC",    "Human Sciences Research Council",
  # "BAIS",    "Botswana Aids Impact Survey",
  "MICS",    "Multiple Indicator Cluster Survey",
  "SBS",     "Sexual Behavior Survey"
)
survey_tbl <- survey_circumcision_orig %>%
  distinct(iso3, survey_id) %>%
  # split survey_id between year and provider after the 4th numchar
  mutate(survey_sep = stringr::str_remove(survey_id, iso3)) %>%
  tidyr::separate(
    survey_sep, c("year", "provider"), sep = "(?<=[0-9]{4})"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  # join in provider name
  left_join(provider_key, by = "provider")

# N and Neff for known surveys
n_survey <- survey_circumcision %>%
  distinct(survey_id, N, Neff)
survey_tbl <- survey_tbl %>%
  left_join(n_survey, by = "survey_id") %>%
  mutate(
    across(N:Neff, ~ ifelse(is.na(.), 0, .)),
    status = "Present"
  )

# Find what types of circumcision are in each survey
types_tbl <- survey_circumcision %>%
  distinct(survey_id, type) %>%
  mutate(is_present = 1) %>%  # initialise
  tidyr::pivot_wider(
    names_from = type, values_from = is_present, values_fill = 0
  ) %>%
  mutate(type = case_when(
    MMC == 1 & TMC == 1 & Missing == 1 ~ "MMC, TMC, MC",
    MMC == 1 & Missing == 1             ~ "MMC, MC",
    TMC == 1 & Missing == 1             ~ "TMC, MC",
    Missing == 1                         ~ "MC",
    TRUE                                 ~ NA_character_
  )) %>%
  select(survey_id, type)


# add missing surveys that are present in Cork paper
# NOTE: MICS Surveys may be obtainable through orderly + Oli's script
# could make this a lot simpler by just manually doing survey id!
missing_tbl <- tribble(
  ~iso3, ~survey_id,    ~year,  ~provider,
  # "BEN", "BEN2014MICS", 2014,   "MICS",
  # "BWA", "BWA2001AIS",  2001,   "AIS",
  # "BWA", "BWA2004AIS",  2004,   "AIS",
  # "BWA", "BWA2008AIS",  2008,   "AIS",
  # "BWA", "BWA2013AIS",  2013,   "AIS",
  # "CAF", "CAF2006MICS", 2006,   "MICS",
  "CIV", "CIV2005AIS",  2005,   "AIS",
  # "COG", "COG2014MICS", 2014,   "MICS",
  "ERI", "ERI2010",     2010,   "Population and Health Survey",
  "KEN", "KEN2007AIS",  2007,   "AIS",
  "KEN", "KEN2013AIS",  2013,   "AIS",
  "LSO", "LSO2014DHS",  2014,   "DHS",
  "NGA", "NGA2006",     2006,   "Core Welfare Indicators Questionnaire Survey",
  # "NGA", "NGA2017MICS", 2017,   "MICS",
  # "SWZ", "SWZ2010MICS", 2010,   "MICS",
  # "SWZ", "SWZ2014MICS", 2014,   "MICS",
  "TZA", "TZA2017AIS",  2017,   "AIS",
  "UGA", "UGA2005AIS",  2005,   "AIS",
  "ZMB", "ZMB2000SBS",  2000,   "SBS",
  "ZMB", "ZMB2003SBS",  2003,   "SBS",
  "ZMB", "ZMB2005SBS",  2005,   "SBS",
  "ZMB", "ZMB2009SBS",  2009,   "SBS",
  "ZWE", "ZWE2005",     2005,   "Chimanimani Behavioral Risks and HIV Serostatus Survey" #,
  # "ZWE", "ZWE2014MICS", 2014,   "MICS"
)

# are there any missing surveys which we have?
length(missing_tbl$survey_id[missing_tbl$survey_id %in% survey_tbl$survey_id]) == 0

missing_tbl <- missing_tbl %>%
  left_join(provider_key, by = "provider") %>%
  mutate(
    across(provider_name, ~ifelse(is.na(.), provider, .)),
    status = "Missing"
  )

# join missing surveys in with surveys present
survey_tbl <- bind_rows(survey_tbl, missing_tbl) %>%
  # join in types information
  left_join(types_tbl, by = "survey_id") %>%
  # translate iso3 to words
  mutate(
    country = country_name_convention(countrycode::countrycode(
      iso3, origin = "iso3c", destination = "country.name"
    ))
  ) %>%
  # re-order surveys and arrange appropriately
  select(
    iso3, country, survey_id, provider, provider_name, year, N, Neff, type, status
  ) %>%
  arrange(iso3, year, provider, N)

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

survey_tbl <- survey_tbl %>%
  left_join(iso_df, by = "iso3")

#### Plot ####

plot_surveys <- survey_tbl %>%
  # don't want missing surveys
  filter(!is.na(N)) %>%
  # pivot type column wider, with N as values
  tidyr::pivot_wider(names_from = type, values_from = N) %>%
  # drop any pre-existing factor levels
  droplevels() %>%
  # arrange region as a factor, approximately counter-clockwise
  mutate(
    region = factor(region, levels = c("Western Africa", 
                                       "Middle Africa",
                                       "Eastern Africa", 
                                       "Southern Africa")
    )
  ) %>%
  # arrange by region (as a factor) and country (alphabetically)
  arrange(region, country) %>%
  # change long country name, severely affecting plot width
  mutate(
    country = ifelse(
      country == "Central African Republic", "Cent. Af. Rep.", country
    ),
    # convert country to factor
    country = fct_inorder(country)
  )


fig1data <- plot_surveys %>%
  mutate(
    # plot position for countries, combining alphabetical order + region
    # add for region to highlight change in region on plot
    country_idx = as.integer(fct_rev(country)) +
      c(0, 1, 2, 3)[
        match(region, rev(c("Western Africa", "Middle Africa",
                            "Eastern Africa", "Southern Africa")))
      ],
    # Used for point shapes in later dotplot
    data = ifelse(
      !is.na(`MMC, TMC, MC`), "Present", "Unavailable"
    )
  ) %>%
  # get N again, as it is used for point sizes
  mutate(N = ifelse(is.na(`MMC, TMC, MC`), MC, `MMC, TMC, MC`))

# labels to display for each country
country_labels <- fig1data %>%
  distinct(region, country, country_idx)

# position on plot grid for each country
country_positions <- fig1data %>%
  group_by(region) %>%
  summarise(max(country_idx)) %>%
  pull()
country_positions <- country_positions + 1

p1 <- fig1data %>%
  # specify aesthetic variables
  ggplot(
    aes(year, country_idx, colour = provider, shape = data, size = N)
  ) +
  # add horizontal lines for regional country (don't have line on top)
  geom_hline(yintercept = country_positions[-1]) +
  # annotate plot with (vertical) regional labels
  annotate(
    "text",
    2020.2,
    # have labels in between horizontal lines
    zoo::rollmean(c(country_positions, 0), 2),
    label = c("Western", "Central", "Eastern", "Southern"),
    angle = 270,
    fontface = "bold",
    size = 4
  ) +
  geom_point(stroke = 2) +
  # specify x and y labels, subbing country name for country_idx
  scale_x_continuous(
    "Survey Year", 
    breaks = seq(2001, 2019, by = 2), 
    # minor_breaks = NULL
    minor_breaks = waiver()
  ) +
  scale_y_continuous(
    element_blank(),
    breaks = country_labels$country_idx,
    minor_breaks = NULL,
    labels = country_labels$country,
    position = "left",
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
      size = rel(1.5), angle = 45, hjust = 1, colour = "black"
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
  ) +
  coord_cartesian(xlim = c(2002, 2018.75), clip = "off")
p1

# dev.new(width = 6.3, height = 10.5, noRStudioGD = TRUE)
# p1
# dev.off()


#### Data inlines ####

data_inlines <- list(
  # n surveys, n countries, years
  "n_surveys_orig" = length(unique(survey_circumcision_orig$survey_id)) , 
  "n_surveys"      = length(unique(survey_circumcision$survey_id)) , 
  "n_iso3"         = length(unique(survey_circumcision$iso3)), 
  "min_year"       = min(survey_circumcision$year), 
  "max_year"       = max(survey_circumcision$year),
  "n_respondents"  = nrow(survey_circumcision), 
  # "min_cohort"     = min(survey_circumcision$year) - 60,
  "n_cohorts"      = length(unique(survey_circumcision$yob)),
  "min_cohort"     = min(survey_circumcision$yob),
  "max_cohort"     = max(survey_circumcision$yob) 
)

# function to convert survey id to proper print survey name
survey_id_to_name <- function(survey) {
  cntry <- countrycode::countrycode(
    substring(survey, 0, 3), 
    origin = "iso3c", 
    destination = "country.name"
  )
  cntry <- dplyr::case_when(
    cntry == "Congo - Kinshasa"    ~ "DR Congo",
    cntry == "Congo - Brazzaville" ~ "Congo",
    grepl("Ivoire", cntry)         ~ "Cote d'Ivoire",
    cntry == "Gambia"              ~ "The Gambia",
    TRUE                           ~ cntry
  )
  survey <- paste(cntry, substring(survey, 4, 7), substring(survey, 8, 11))
}

# convert decimal to percentage rounded to 3 SF
dec_to_percs <- function(x) {
  if (is.numeric(x) && (x >= 0 && x < 1)) {
    paste0(round(x * 100, 3), "%")
  } else x
}

# also need to add surveys with largest and smallest cohorts
n_repondents <- survey_circumcision %>% 
  group_by(survey_id) %>% 
  summarise(n = n(), .groups = "drop")
max_respondents <- filter(n_repondents, n == max(n))
min_respondents <- filter(n_repondents, n == min(n))
  
survey_sizes <- list(
  "max_survey_size" = list(
    "survey" = survey_id_to_name(max_respondents$survey_id),
    "n"      = max_respondents$n),
  "min_survey_size" = list(
    "survey" = survey_id_to_name(min_respondents$survey_id),
    "n"      = min_respondents$n)
)

data_inlines <- c(data_inlines, survey_sizes)


# function to check various censoring values
check_cens <- function(survey_circumcision, 
                       test_col, 
                       test_val, 
                       lst_name, 
                       data_inlines, 
                       test_fun = `==`) {
  
  
  # average, highest, lowest
  abs_vals <- survey_circumcision %>% 
   group_by(survey_id)
  if (!is.na(test_val)) {
   abs_vals <- abs_vals %>% 
    summarise(sum(test_fun(.data[[test_col]], !!test_val)), .groups = "drop")
  } else {
    abs_vals <- abs_vals %>% 
    summarise(sum(is.na(.data[[test_col]])), .groups = "drop")
  }
   
  max_val <- list(
    "max"       = max(abs_vals[, 2, drop = TRUE]),
    "survey" = survey_id_to_name(
      abs_vals[, 1][which.max(abs_vals[, 2, drop = TRUE]), ]
    )
  )
  min_val <- list(
    "min"       = min(abs_vals[, 2, drop = TRUE]),
    "survey" = survey_id_to_name(
      abs_vals[, 1][which.min(abs_vals[, 2, drop = TRUE]), ]
    )
  )
  
  # as percentage
  perc <- survey_circumcision %>% 
    group_by(survey_id)
  if (!is.na(test_val)) {
   perc <- perc %>% 
    summarise(
      sum(test_fun(.data[[test_col]], !!test_val)) / n(), .groups = "drop"
    )
  } else {
   perc <- perc %>% 
    summarise(sum(is.na(.data[[test_col]])) / n(), .groups = "drop")
  }
  
  max_perc <- list(
    "max"       = max(perc[, 2, drop = TRUE]),
    "survey" = survey_id_to_name(
      abs_vals[, 1][which.max(perc[, 2, drop = TRUE]), ]
    )
  )
  min_perc <- list(
    "min"       = min(perc[, 2, drop = TRUE]),
    "survey" = survey_id_to_name(
      abs_vals[, 1][which.min(perc[, 2, drop = TRUE]), ]
    )
  )
  
  n <- length(data_inlines)
  data_inlines <- c(
    data_inlines, 
    round(mean(abs_vals[, 2, drop = TRUE])),
    list(max_val),
    list(min_val),
    mean(perc[, 2, drop = TRUE]),
    list(max_perc),
    list(min_perc) 
  )
  names(data_inlines)[(n + 1):length(data_inlines)] <- c(
    paste0("mean_", lst_name),
    paste0("max_", lst_name),
    paste0("min_", lst_name),
    paste0("mean_", lst_name, "_perc"),
    paste0("max_", lst_name, "_perc"),
    paste0("min_", lst_name, "_perc")
  )
  
  # convert percentages to three SF with % sign
  convert_percs <- function(data_inlines) {
    modify_tree(data_inlines, leaf = dec_to_percs)
  }
  data_inlines <- convert_percs(data_inlines)
  
  # also of interest, surveys which have > 0.9 & < 0.1 left censoring
  surveys_lst <- list(
    "surveys_0.9" = as.vector(
      abs_vals[, 1][which(perc[, 2, drop = TRUE] >= 0.9), ]
    )[[1]],
    "surveys_0.1" = as.vector(
      abs_vals[, 1][which(perc[, 2, drop = TRUE] <= 0.1), ]
    )[[1]]
  )
  # names(surveys_lst) <- c("surveys_0.9", "surveys_0.1")
  
  # convert survey_ids to survey names
  surveys_lst <- lapply(surveys_lst, function(x) sapply(x, survey_id_to_name))
  
  data_inlines <- c(data_inlines, list(surveys_lst))
  names(data_inlines)[length(data_inlines)] <- paste0(lst_name, "_surveys")
  
  return(data_inlines)
}

# left censoring
data_inlines <- check_cens(
  survey_circumcision, "event", 2, "l_cens", data_inlines
)
# right censoring
data_inlines <- check_cens(
  survey_circumcision, "event", 0, "r_cens", data_inlines
)
# no censoring
data_inlines <- check_cens(
  survey_circumcision, "event", 0, "r_cens", data_inlines
)
# circumcision status unknown
data_inlines <- check_cens(
  filter(survey_circumcision_orig, 
         survey_id %in% survey_circumcision$survey_id), 
  "circ_status", 
  NA,
  "unknown_status", 
  data_inlines
)
# circ age unknown
data_inlines <- check_cens(
  filter(survey_circumcision_orig, 
         survey_id %in% survey_circumcision$survey_id), 
  "circ_age", 
  NA,
  "unknown_circ_age", 
  data_inlines
)

# display all figures with commas
comma_figs <- function(x) {
  if (is.numeric(x) && !(x > 1900 && x < 2100)) {
    format(x, big.mark = ",")
  } else x
}
data_inlines <- modify_tree(data_inlines, leaf = comma_figs)

# find countries with no type info
no_type_iso3 <- survey_circumcision %>% 
  group_by(iso3) %>% 
  filter(all(type == "Missing")) %>% 
  distinct(iso3) %>% 
  pull()

# average l cens amongst these countries
l_cens_no_type <- survey_circumcision %>% 
  filter(iso3 %in% no_type_iso3) %>% 
  group_by(iso3) %>% 
  summarise(l_cens_perc = sum(event == 2) / n(), .groups = "drop")

# average circumcision amongst these countries
circ_no_type <- survey_circumcision %>% 
  filter(iso3 %in% no_type_iso3) %>% 
  group_by(iso3) %>% 
  summarise(circ_perc = sum(circ_status == 1) / n(), .groups = "drop")

# pull country name from iso3 code
no_type_cntry <- country_name_convention(countrycode::countrycode(
  no_type_iso3, "iso3c", "country.name"
))
# paste names together
no_type_cntry <- paste(
  paste(no_type_cntry[1:(length(no_type_iso3) - 1)], collapse = ", "), 
  last(no_type_cntry), 
  sep = " and "
)

data_inlines <- c(
  data_inlines, 
  "no_type_iso3" = list(no_type_iso3),
  "no_type_cntry" = no_type_cntry, 
  "mean_no_type_cntry_l_cens" = dec_to_percs(mean(l_cens_no_type$l_cens_perc)),
  "mean_no_type_cntry_circ" = dec_to_percs(mean(circ_no_type$circ_perc))
)

#### Supplementary Figures ####

# TODO: All!

#### Saving ####

# save plot
# saveRDS(p1, "paper_poster_plots/paper/plots/01_survey_table.RDS")
ggplot2::ggsave(
  "paper_poster_plots/paper/plots/01_survey_table.png", 
  p1, 
  width = 6.3, 
  height = 8,
  units = "in"
)

# save data for inlines
saveRDS(data_inlines, "paper_poster_plots/paper/data/01_data_inlines.RDS")
