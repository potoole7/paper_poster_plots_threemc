#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Loading required packages
require(tidyverse)
require(dplyr)
require(reshape2)
require(xtable)
require(plyr)
require(sf)

# Reading in tables 
results <- read_csv("~/Desktop/01e_new_final_ppc_models_fit_stats.csv")

# Loading shapefiles 
areas <- read_sf("~/Desktop/areas.geojson")

####################################
### Outputting PPC Part 1 Tables ###
####################################
# List of countries
# cntry <- unique(results$cntry)
cntry <- c('AGO', 'RWA', 'ZAF')

# Opening text file
sink('~/Desktop/PPC_P1_Tables.txt')

cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n")

# Loop for each country 
for (i in cntry){
  # filtering for country
  tmp <- results %>%
    filter(cntry == i) %>%
    as.data.frame()
  
  # Multiplying the coverage by 100 and rounding 
  tmp[,c("ppd_0.500", "ppd_0.800", "ppd_0.950")] <- 100 * tmp[,c("ppd_0.500", "ppd_0.800", "ppd_0.950")]
  
  # Formatting dataset for output
  tmp <- tmp %>%
    # Altering labels 
    mutate(rw = case_when(rw_order == 0 ~ "AR1",
                          rw_order == 1 ~ "RW1",
                          rw_order == 2 ~ "RW2"),
           paed = case_when(paed_age_cutoff == "10" ~ "Paediatric cut-off",
                            paed_age_cutoff == "Inf" ~ "No cut-off"),
           type = case_when(type == "MC coverage" ~ "MC",
                            type == "TMC coverage" ~ "MMC",
                            type == "MMC coverage" ~ 'TMC'),
           tmc = case_when(inc_time_tmc == TRUE ~ "Time invariant",
                           inc_time_tmc == FALSE ~ 'Time variant')) %>%
    # selecting relevant columns
    dplyr::select(rw, paed, type, tmc, crps, rmse, mae, ppd_0.500, ppd_0.800, ppd_0.950) %>%
    # wide to long dataset
    reshape2::melt(id.vars = c("rw", "paed", "type", "tmc")) %>%
    # Grouping to find best results
    dplyr::group_by(rw, variable, type) %>%
    # Finding best performing results
    dplyr::mutate(tmp = if_else(value == min(value) & variable %in% c('crps', 'mae', 'rmse'), 1, 
                                if_else(value == max(value) & variable %in% c("ppd_0.500", "ppd_0.800", "ppd_0.950"), 1, 0))) %>% 
    # Removing grouping 
    dplyr::ungroup() %>%
    # Formatting for table output
    dplyr::mutate(value = format(round(value, 2), nsmall = 2)) %>%
    dplyr::mutate(value = if_else(tmp == 1, paste0('\\bf', value), value)) %>%
    # removing dummy variable
    dplyr::select(-c(tmp)) %>%
    # Relabelling datset 
    mutate(variable = paste(tmc, variable, sep = ' '))%>%
    # selecting relevant columns
    dplyr::select(-c(tmc))%>%
    # Long to wide dataset
    reshape2::dcast(rw + paed + type ~ variable, 
                    value.var = c("value")) %>%
    # # Dummy data to add lines into tables
    # plyr::rbind.fill(expand.grid(rw = c("AR1", "RW1", "RW2"),
    #                        paed = c("Paediatric cut-off"), #, "No cut-off"),
    #                        type = 'XXX')) %>%
    # Sorting dataset
    arrange(rw, paed, type) 
  
  # Removing dummy data
  tmp$paed[duplicated(tmp[,c('rw', 'paed')])] <- ""
  tmp$rw[duplicated(tmp$rw)] <- ""
  tmp[which(tmp$type == "XXX"),] <- ""
  
  # Removing unnecessary columns
  tmp$`Time invariant rmse` <- NULL
  tmp$`Time variant rmse` <- NULL
  
  
  # Outputting table
  cat("\n")
  cat("{\\linespread{1}", "\n")
  cat("  \\begin{table}[H]", "\n")
  cat("  \\centering", "\n")
  cat("  \\footnotesize", "\n")
  cat("  \\begin{tabular}{>{\\bfseries}p{0.75cm} >{\\bfseries}p{3cm} p{1cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm}}", "\n")
  cat("  \\hline ", "\n")
  cat("  & & & \\multicolumn{10}{c}{\\bf TMC Model} \\\\", "\n")
  cat("  \\cmidrule(lr){4-13}", "\n")
  cat("  & & & \\multicolumn{5}{c}{\\bf Time invariant} & \\multicolumn{5}{c}{\\bf Time variant} \\\\", "\n")
  cat("  \\cmidrule(lr){4-8}", "\n")
  cat("  \\cmidrule(lr){9-13}", "\n")
  cat("  & {\\bf MMC Model} & {\\bf Type} & {\\bf CRPS} & {\\bf RMSE} & {\\bf 50\\% CI} & {\\bf 80\\% CI} & {\\bf 95\\% CI} & {\\bf CRPS} & {\\bf RMSE} & {\\bf 50\\% CI} & {\\bf 80\\% CI} & {\\bf 95\\% CI} \\\\", "\n")
  cat("  \\hline", "\n")
  cat(paste(paste(tmp[,1], tmp[,2], tmp[,3], tmp[,4], tmp[,5], tmp[,6], tmp[,7], tmp[,8], tmp[,9], tmp[,10], tmp[,11], tmp[,12], tmp[,13], sep = ' & '), "\\\\ \n"))
  cat("  \\hline", "\n")
  cat("  \\end{tabular}", "\n")
  cat(paste0("  \\caption{Results of the posterior predictive checking in total male circumcision (MC), medical male circumcision (MMC) and traditional male circumcision (TMC) from fitting the 12 candidate models in ",
             areas$area_name[which(areas$area_id == i)], 
             ". Combinations include (i) Time invariant or Time variant TMC, (ii) No cut off vs. Paediatric cut-off in MMC, and (iii) Autoregressive order 1 (AR1), Random Walk 1 (RW1) or Random Walk 2 (RW2) temporal prior. For all combinations, the within-sample continuous ranked probability scores (CRPS), mean absolute error (MAE), mean absolute error (MAE), and the proportion of empirical observations that fell within the 50\\%, 80\\%, and 95\\% quantiles are shown.}"), "\n")
  cat(paste0("  \\label{tab::PPC1", i, "}"), "\n")
  cat("\\end{table}}", "\n")
  cat("\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n")
  
}

# Closing text file
sink()
  
  




