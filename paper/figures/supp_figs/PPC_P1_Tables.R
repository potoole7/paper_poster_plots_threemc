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
sink('~/Desktop/PPC_P1_Tables.tex')

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
           paed = case_when(paed_age_cutoff == "10" ~ "\\bf PC",
                            paed_age_cutoff == "Inf" ~ "\\bf NC"),
           type = case_when(type == "MC coverage" ~ "\\bf MC",
                            type == "TMC coverage" ~ "\\bf MMC",
                            type == "MMC coverage" ~ '\\bf TMC'),
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
    arrange(rw, type, type) %>%
    dplyr::select(rw, type, paed, contains('crps'), contains('mae'), contains('rmse'), contains('0.5'), contains('0.8'), contains('0.9'))
  
  # Removing dummy data
  tmp$type[duplicated(tmp[,c('rw', 'type')])] <- ""
  
  # Adding spacing 
  tmp$num <- 1:nrow(tmp)
  tmp$nextrow <- ''
  tmp$nextrow[!((tmp$num %% 2) == 0)] <- '\\\\ \n'
  tmp$nextrow[(tmp$num %% 2) == 0] <- '\\\\[3pt] \n'
  tmp$num <- NULL
  
  # Outputting table
  cat("\n")
  cat("{\\linespread{1}", "\n")
  cat("  \\begin{table}[H]", "\n")
  cat("  \\centering", "\n")
  cat("  \\footnotesize", "\n")
  cat("  \\begin{tabular}{>{\\bfseries}p{0.05cm} p{0.85cm} p{0.75cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm}}", "\n")
  cat("  \\hline ", "\n")
  # cat("  & & & \\multicolumn{12}{c}{\\bf TMC} \\\\", "\n")
  # cat("  \\cmidrule(lr){4-15}", "\n")
  cat("  & & & \\multicolumn{2}{c}{\\bf CRPS} & \\multicolumn{2}{c}{\\bf MAE} & \\multicolumn{2}{c}{\\bf RMSE} & \\multicolumn{2}{c}{\\bf 50\\% CI} & \\multicolumn{2}{c}{\\bf 80\\% CI} & \\multicolumn{2}{c}{\\bf 95\\% CI}  \\\\", "\n")
  cat("  \\cmidrule(lr){4-5}", "\n")
  cat("  \\cmidrule(lr){6-7}", "\n")
  cat("  \\cmidrule(lr){8-9}", "\n")
  cat("  \\cmidrule(lr){10-11}", "\n")
  cat("  \\cmidrule(lr){12-13}", "\n")
  cat("  \\cmidrule(lr){14-15}", "\n")
  cat("  & & & {\\bf TI} & {\\bf TV} & {\\bf TI} & {\\bf TV} & {\\bf TI} & {\\bf TV} & {\\bf TI} & {\\bf TV} & {\\bf TI} & {\\bf TV} & {\\bf TI} & {\\bf TV}\\\\", "\n")
  # cat("  & {\\bf MMC} & {\\bf Type} & {\\bf TI} & {\\bf TV} & {\\bf TI} & {\\bf TV} & {\\bf TI} & {\\bf TV} & {\\bf TI} & {\\bf TV} & {\\bf TI} & {\\bf TV} & {\\bf TI} & {\\bf TV}\\\\", "\n")
  cat("  \\hline", "\n")
  
  # Loop for each country 
  for (j in unique(tmp$rw)){
    # filtering for country
    tmp2 <- tmp %>%
      filter(rw == j) %>%
      as.data.frame()
    
    # Converting to data frame
    tmp2 <- as.data.frame(tmp2)
    
    # Country Name 
    cat(paste("    \\multicolumn{8}{l}{\\textbf{", j, '}} \\\\', '\n'))
    # Data Table
    cat(paste(paste0("", ' & ', 
                     tmp2[,2],  ' & ', tmp2[,3],  ' & ',  tmp2[,4],  ' & ', tmp2[,5],' & ', 
                     tmp2[,6],  ' & ', tmp2[,7],  ' & ',  tmp2[,8],  ' & ', tmp2[,9],' & ', 
                     tmp2[,10], ' & ', tmp2[,11], ' & ',  tmp2[,12], ' & ', tmp2[,13],' & ',
                     tmp2[,14], ' & ', tmp2[,15],  tmp2[,16])))
  }
  
  cat("  \\hline", "\n")
  cat("  \\end{tabular}", "\n")
  cat(paste0("  \\caption{Results of the posterior predictive checking in total male circumcision (MC), medical male circumcision (MMC) and traditional male circumcision (TMC) from fitting the 12 candidate models in ",
             areas$area_name[which(areas$area_id == i)], 
             ". Combinations include (i) Time invariant (TI) or Time variant (TV) TMC, (ii) No cut off (NC) vs. Paediatric cut-off (PC) in MMC, and (iii) Autoregressive order 1 (AR1), Random Walk 1 (RW1) or Random Walk 2 (RW2) temporal prior. For all combinations, the within-sample continuous ranked probability scores (CRPS), mean absolute error (MAE), root mean square error (RMSE), and the proportion of empirical observations that fell within the 50\\%, 80\\%, and 95\\% quantiles are shown.}"), "\n")
  cat(paste0("  \\label{tab::PPC1", i, "}"), "\n")
  cat("\\end{table}}", "\n")
  cat("\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n")
  
}

# Closing text file
sink()
  
  




