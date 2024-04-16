#### Table and inlines for methods - calibration section of paper ####

#### libs and sources ####

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
source("paper_poster_plots_threemc/paper/scripts/00_funs.R")

#### Parameter Values ####

# VMMC countries  
vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", "UGA", "MWI",
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF" 
)
# countries with no type info
no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD")
# all other SSA countries (that are modelled)
iso3 <- c("LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE",
          "ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV",
          "GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH",
          "ZAF", "LBR", "GHA", "GMB", "NGA", "COD")
# iso3 <- iso3[!iso3 %in% c(vmmc_iso3, no_type_iso3)]

pars_df <- tidyr::crossing(
  "cntry" = iso3,
  "rw_order" = c(0, 1, 2),
  "paed_age_cutoff" = c(10, Inf),
  "inc_time_tmc" = c(FALSE, TRUE)
)

pars_df <- pars_df %>%
  filter(
    !cntry %in% no_type_iso3 | 
      (cntry %in% no_type_iso3 & 
         paed_age_cutoff == Inf & 
         inc_time_tmc == FALSE)
  )


#### load orderly data ####
  
fit_stats <- load_orderly_data(
  task       = "01efinal_ppc_models", 
  parameters = pars_df,
  query      = "latest(
      parameter:cntry           == cntry && 
      parameter:rw_order        == rw_order &&
      parameter:paed_age_cutoff == paed_age_cutoff &&
      parameter:inc_time_tmc    == inc_time_tmc
    )",
  filenames  = "ppc_summary.rds",
  load_fun   = readRDS
)

# for running remaining tasks
pars_df_remaining <- pars_df[which(is.na(fit_stats$dirs)), ]

# remove rows with no matches from pars_df (57/156 for VMMC countries NA!!)
if (any(is.na(fit_stats$dirs))) {
  pars_df <- pars_df[-which(is.na(fit_stats$dirs)), ]
}

fit_stats1 <- fit_stats$output

fit_stats_join <- bind_rows(lapply(fit_stats1, function(x) {
  as.data.frame(t(unlist(x)))
}))
names(fit_stats_join)[
  grepl("oos_obs", names(fit_stats_join))
] <- str_remove(
  names(fit_stats_join)[grepl("oos_obs", names(fit_stats_join))], 
  "oos_observations_within_PPD_"
)
names(fit_stats_join)[
  grepl("elpd.estimates1", names(fit_stats_join))
] <- str_remove(
  names(fit_stats_join)[
    grepl("elpd.estimates1", names(fit_stats_join))
  ], 
 "1" 
)

# names(fit_stats_join)[
#   grepl("sment_pars", names(fit_stats_join))
# ] <- stringr::str_remove(
#   names(fit_stats_join)[
#     grepl("sment_pars", names(fit_stats_join))
#   ], 
#   "sment_pars."
# )

# fit stats for each model choice for each country
fit_stats_join <- fit_stats_join %>% 
  select(-c(matches("elpd.pointwise"), matches("elpd.estimates"))) %>% 
  bind_cols(pars_df) %>% 
  pivot_longer(MMC.CI.0.5:MC.rmse) %>% 
  separate(col = name, into = c("type"), remove = FALSE, extra = "drop") %>% 
  mutate(name = str_remove_all(name, "MMC.|TMC.|MC.")) %>% 
  pivot_wider(names_from = name, values_from = value)

#### Inlines ####

# best model for each country based on RMSE
fit_stats_join_best <- fit_stats_join %>% 
  # filter(rmse == min(rmse), .by = c(cntry, rw_order)) %>% 
  filter(crps == max(crps), .by = c(cntry, rw_order)) %>% 
  mutate(spec = paste0(
    "rw_order = ", 
    rw_order, 
    ", paed_age_cutoff = ", 
    paed_age_cutoff, 
    ", inc_time_tmc = ", 
    inc_time_tmc
  ))

# best by spec
fit_stats_join_best %>% 
  summarise(n = n(), .by = spec) %>% 
  arrange(desc(n))

  # filter(grepl("rw_order = 0", spec))

# best by region
fit_stats_join_best %>% 
  left_join(threemc::esa_wca_regions, by = c("cntry" = "iso3")) %>% 
  group_by(region, spec) %>% 
  summarise(n = n()) %>% 
  arrange(region, desc(n))

# Models pretty similar for VMMC countries, so essentially a modelling choice 
# Because TMC neads to vary for KEN, makes sense to give it a time TMC effect
# paediatric MMC cutoff agrees with VMMC policy, and does not significantly 
# effect fit (no survey data on coverage by age (current age, not circ age!)) 
# for under 15s anyway to assess fit for paediatric individuals, since surveys 
# do not ask under 15s
# -> Model used: time TMC, paediatric MMC age cutoff