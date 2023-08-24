#### Tables for Data section of Methods (and Sup. Figures) ####

#### Initial ####

### Libs ###
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
# source("Shiny/src/functions.R") # plotting functions
source("paper_poster_plots/paper/scripts/00_funs.R") # shared functions

### Metadata ###

orderly_root <- getwd()

# for tabulating surveys: 
rm_missing_type <- FALSE # remove circumcisions with missing type?
start_year <- 2000 # minimum survey year
cens_age <- 59 # maximum circumcision age

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

### Load Data ###

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

# survey_circumcision <- survey_circumcision %>% 
  # filter(!iso3 %in% no_mod_iso3)

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
) %>% 
  bind_rows()

#### Table 1: Circumcision Question Availability ####

# Table has different questions as cols, different surveys as rows 
# Questions: 
# Self-reported circumcision status, 
# Age at circumcision
# Who performed the circumcision?
# Where did the circumcision take place?

table1 <- survey_circumcision_orig %>% 
  group_by(survey_id) %>% 
  # find number of non nas for each Q: If they're 0; question wasn't asked
  summarise(
    circ_status = sum(!is.na(circ_status)),
    circ_age    = sum(!is.na(circ_age)),
    circ_who    = sum(!is.na(circ_who)), 
    circ_where  = sum(!is.na(circ_where)), 
    .groups     = "drop"
  ) %>% 
  # convert to boolean
  mutate(
    circ_status = ifelse(circ_status > 0, 1, 0),
    circ_age    = ifelse(circ_age > 0, 1, 0),
    circ_who    = ifelse(circ_who > 0, 1, 0),
    circ_where  = ifelse(circ_where > 0, 1, 0),
    # convert to latex ticks and crosses
    across(contains("circ"), ~ ifelse(. == 1, "[/]checkmark", "[/]xmark"))
  ) %>% 
  # convert survey_ids to longer names
  tidyr::separate(
    survey_id, 
    sep  = c(3, 7), 
    into = c("country", "year", "series")
  ) %>% 
  # paste back together with spaces
  mutate(survey_id = paste(country, year, series)) %>% 
  relocate(survey_id) %>% 
  # remove unneeded columns
  select(-c(country, year, series)) %>% 
  # rename(
  #   "Survey ID" = survey_id, 
  #   "Self-reported circumcision status" = circ_status, 
  #   "Age at circumcision" = circ_age,
  #   "Who performed the circumcision?" = circ_who,
  #   "Where did the circumcision take place?" = circ_where
  # )
  identity()

# from survey names, pull country name and survey name
# TODO: This is done somewhere else as well, don't repeat code
countrynames <- countrycode::countrycode(
  substr(table1$survey_id, 1, 3), "iso3c", "country.name"
)
countrynames <- case_when(
  grepl("Brazzaville", countrynames)  ~ "Repulic of the Congo", 
  grepl("Kingshana", countrynames)    ~ "DR Congo", 
  TRUE                                ~ countrynames
)

# order by countrynames
order <- order(countrynames)
countrynames <- countrynames[order]
table1 <- table1[order, ]

surveynames <- substr(table1$survey_id, 5, 13)
surveynames <- stringr::str_split(surveynames, " ")
surveynames <- lapply(surveynames, function(x) {
  x[[2]] <- paste0("(", x[[2]], ")")
  return(x)
})
surveynames <- vapply(surveynames, paste, collapse = " ", character(1))

# test <- table1[1:3, ]

# create table contents (table outline already in paper)
# TODO: Create text with desired size from the start!
text <- c()
# iterate through rows in table1
for (i in seq_len(nrow(table1))) {
  # add table line for country
  if (i == 1 || countrynames[i] != countrynames[i - 1]) {
    # paste0("\multicolumn{2}{l}{\textbf{", countrnames[i], "}} \\")
    text <- c(text, paste0("[/]multicolumn{2}{l}{[/]textbf{", countrynames[i], "}} [/][/]"))
  }
  # add table line for survey line [i]
  add_text <- paste0(
    "& ", 
    surveynames[i], 
    " & ", 
    paste0(table1[i, 2:5], collapse = " & "),
    " [/][/]"
    # "[/][/][5pt] [/]multicolumn{2}"
  )
  
  # for last row for specific country, add additional text
  if ((i + 1) > nrow(table1) || countrynames[i + 1] != countrynames[i]) {
    add_text <- paste0(add_text, "[5pt]")
  }
  text <- c(text, add_text)
}

# sink to text file
sink(
  "paper_poster_plots/paper/figures/supp_figs/table_1_survey_questions.txt", 
  append = FALSE
)
for (i in seq_along(text)) cat(text[i], "\n")
sink()
# TODO: When pasting into TeX table, must search and replace [/] with \


#### Table 2: Circumcision Type Definitions ####

# tab spanners: 
# Columns:  Who performed the circumcision?
# Rows: Where did the circumcision take place?

# Values: 
# Rows: Healthcare Professional, Traditional Practioner, Missing
# Columns: Health Facility, At Home/Ritual Site, Missing

# Made this completely in Latex as it was quite simple when looking at Matt's 
# example!

#### Sup Table: Survey Summaries ####

# Want: 
# For each survey: 
# Country, Provider, Year 
# sample size, Survey effective sample size, participation rate 
# Left Censoring, Right Censoring
# Unknown circumcision type

# separate country, provider and Year in survey name 
survey_names <- survey_circumcision %>% 
  distinct(survey_id) %>% 
  tidyr::separate(
    survey_id, 
    sep    = c(3, 7), 
    into   = c("country", "year", "series"), 
    remove = FALSE
  ) %>% 
  mutate(
    country = country_name_convention(country),
    series  = survey_series_expand(series)
  )

# For each survey, need sample size and effective sample size
sample_sizes <- survey_circumcision %>% 
  group_by(survey_id) %>% 
  distinct(N, Neff = floor(Neff)) %>% 
  ungroup()

# find original sample; this will give participation
n_orig <- survey_circumcision_orig %>% 
  count(survey_id) %>% 
  # select(-survey_id)
  identity()

# sample_sizes <- cbind(sample_sizes, n_orig) %>% 
sample_sizes <- left_join(sample_sizes, n_orig) %>% 
  relocate(n_orig = n, .before = everything()) %>% 
  # calculate participation rates
  # mutate(part_rates = N / n_orig) %>% 
  select(-survey_id)

# find level of left and right censoring occuring
cens <- survey_circumcision %>% 
  select(survey_id, event) %>% 
  mutate(event = case_when(
    event == 0 ~ "right_censored", 
    event == 1 ~ "uncensored",
    event == 2 ~ "left_censored"
  )) %>% 
  group_by(survey_id) %>% 
  count(event) %>% 
  tidyr::pivot_wider(names_from = event, values_from = n) %>% 
  mutate(across(contains("cens"), ~ ifelse(is.na(.), 0, .))) %>% 
  select(-survey_id)
  
# finally, find number of known circumcision types
types <- survey_circumcision %>% 
  mutate(type = ifelse(type == "Missing", NA, type)) %>% 
  group_by(survey_id) %>% 
  summarise(
    across(all_of(c("circ_who", "circ_where", "type")),
           ~ sum(!is.na(.)))
  ) %>% 
  rename(known_who = circ_who, known_where = circ_where, known_type = type) %>% 
  select(-survey_id)

# now join together!
table3 <- cbind(survey_names, sample_sizes, cens, types) %>% 
  # select(-c(survey_id, n_orig)) %>% 
  select(-survey_id) %>% 
  # remove questions about circ type; unneeded
  select(-c(known_who, known_where)) %>% 
  # mutate(part_rates = paste(round(100 * part_rates, 3), "%", sep = "")) %>% 
  select(
    "Country"                      = country, 
    "Year"                         = year, 
    "Series"                       = series, 
    # "Sample Size"                  = N, 
    "Sample Size"                  = n_orig,
    "Effective Sample Size"        = Neff, 
    # "Participation Rates"          = part_rates, 
    "Number Participated"          = N, 
    "Left Censored"                = left_censored, 
    "Right Censored/Uncircumcised" = right_censored, 
    "Uncensored/Circumcised"       = uncensored, 
    # "Known Circumcision Provider"  = known_who, 
    # "Known Circumcision Location"  = known_where, 
    "Known Circumcision Type"      = known_type
  )

# tabulate and export to Latex (to do!)
readr::write_csv(table3)