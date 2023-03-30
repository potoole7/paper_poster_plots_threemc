
* Initial Code                                                     :noexport:

#+begin_src R :exports none :session
# set directory to top level of git repo
setwd(system("git rev-parse --show-toplevel", intern = TRUE))
#+end_src

#+RESULTS:
: /home/paddy7wb/imperial_repos/threemc-orderly

** Load Libraries
#+begin_src R :exports none :session

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
source(here::here("Shiny/src/functions.R")) # plotting functions
#+end_src
#+RESULTS:

**  Define Metadata

#+begin_src R :exports none :session

orderly_root <- here::here()

spec_age_group <- "10-29"
# spec_age_groups <- c("15-29", "15-49")
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

ssa_countries <- countrycode::countrycode(ssa_iso3, "iso3c", "country.name")
ssa_countries <- dplyr::case_when(
  ssa_countries == "Congo - Kinshasa"    ~ "DR Congo",
  ssa_countries == "Congo - Brazzaville" ~ "Congo",
  grepl("Ivoire", ssa_countries)         ~ "Cote d'Ivoire",
  ssa_countries == "Gambia"              ~ "The Gambia",
  TRUE                                   ~ ssa_countries
)

# countries/regions with VMMC programs, targeting 90% 2021 MC coverage
target_iso3 <- c(
  # In Ethiopia Gambella is the only target province
  "BWA", "ETH_1_15", "KEN", "LSO", "MWI", "MOZ", "NAM", 
  "RWA", "ZAF", "SWZ", "UGA", "TZA", "ZMB", "ZWE"
)

# colour palette for map plot
colourPalette <- rev(colorRampPalette(
  c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf",
    "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")
  )(100))
  
NULL # don't print colourPalette
#+end_src

#+RESULTS:

** Load Data 

#+begin_src R :exports none :session

# pull most recent results for age groups
dir_name <- orderly::orderly_search(
  query      = "latest(parameter:is_paper == is_paper)",
  name       = "03_shiny_consolidation", 
  parameters = list(is_paper = TRUE), 
)

# results_agegroup <- readr::read_csv(file.path(
#   orderly_root, 
#   "archive/03_shiny_consolidation",
#   dir_name,
#   "/artefacts/results_agegroup.csv.gz"
# ))
# 
# results_agegroup_n_circ <- readr::read_csv(file.path(
#   orderly_root, 
#   "archive/03_shiny_consolidation",
#   dir_name,
#   "/artefacts/results_agegroup_n_circ.csv.gz"
# ))
# 
# missing_iso3 <- target_iso3[!target_iso3 %in% results_agegroup$area_id]

# pull shapefiles
areas <- sf::read_sf(file.path(
  orderly_root, 
  "archive/03_shiny_consolidation",
  dir_name,
  "depends/areas.geojson"
)) %>% 
  dplyr::mutate(space = seq_len(dplyr::n()))

# load surveys
survey_dir_name <- orderly::orderly_search(
  query = "latest(parameter:is_paper == is_paper)",
  name  = "00b3_survey_join",
  parameters = list(is_paper = TRUE)
)

survey_circumcision <- survey_circumcision_orig <- read_circ_data(file.path(
    orderly_root, 
    "archive/00b3_survey_join",
    survey_dir_name,
    "artefacts/survey_circumcision.csv.gz"
  ),
  filters = c("sex" = "male")
) %>% 
  # only keep surveys for countries we have results for
  # filter(iso3 %in% results_agegroup$iso3)
  identity()

# pull agegroup populations
pops_dir_name <- orderly::orderly_list_archive() %>% 
  filter(name == "00c4_pops_aggregate") %>%
  slice(n()) %>% 
  pull(id)
populations <- read_circ_data(file.path(
    orderly_root, 
    "archive/00c4_pops_aggregate",
    pops_dir_name,
    "artefacts/population_agegroup_aggr.csv.gz"
  ),
  filters = c("sex" = "male")
) %>% 
  # only keep surveys for countries we have results for
  # filter(iso3 %in% results_agegroup$iso3)
  identity()

# last surveys (TODO: Update these!)
last_surveys <- readr::read_csv("global/most_recent_surveys.csv")

# Additional areas from Oli, so map plot can be for all of SSA
dat_loc <- here::here("paper_poster_plots/paper/data/")
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
  
NULL
#+end_src

#+RESULTS:





** Prepare and Tabulate Survey Data 
  #+begin_src R :exports none :session

# Prepare circ data, normalise survey weights and apply Kish coefficients.
survey_circumcision <- data.table::rbindlist(prepare_survey_data(
  areas               = areas,
  survey_circumcision = select(survey_circumcision, -matches("area_level")),
  area_lev            = threemc::datapack_psnu_area_level,
  start_year          = start_year,
  cens_year           = TRUE,
  cens_age            = cens_age,
  rm_missing_type     = rm_missing_type,
  norm_kisk_weights   = TRUE
))

# what information do I want for each survey?
# - country, provider, do they have circ info, do they distinguish between types,
# number of observations, Neff
# Region in Africa (mostly for surveys plot)
# NID and citation (may have to do by hand?)
# Also add missing surveys as well!

# create initial table and find country, provider, year from survey_id
provider_key <- tibble::tribble(
  ~provider, ~provider_name,
  "AIS",     "AIDS Indicator Survey",
  "DHS",     "Demographic and Health Survey",
  "PHIA",    "Population-Based HIV Impact Assessment",
  "HSRC",    "Human Sciences Research Council",
  "BAIS",    "Botswana Aids Impact Survey",
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
  "BWA", "BWA2001AIS",  2001,   "AIS",
  "BWA", "BWA2004AIS",  2004,   "AIS",
  "BWA", "BWA2008AIS",  2008,   "AIS",
  "BWA", "BWA2013AIS",  2013,   "AIS",
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
    country = countrycode::countrycode(
      iso3, origin = "iso3c", destination = "country.name"
    ),
    country = case_when(
      # country == "Congo - Brazzaville" ~ "Republic of the Congo",
      country == "Congo - Brazzaville" ~ "Congo",
      # country == "Congo - Kinshasa"    ~ "Democratic Republic of the Congo",
      country == "Congo - Kinshasa"    ~ "DR Congo",
      TRUE                             ~ country)
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
    iso3 = alpha.3, 
    region = intermediate.region
  ) %>%
  mutate(region = ifelse(region == "", "Other", region))

survey_tbl <- survey_tbl %>%
  left_join(iso_df, by = "iso3")

  #+end_src

  #+RESULTS:

