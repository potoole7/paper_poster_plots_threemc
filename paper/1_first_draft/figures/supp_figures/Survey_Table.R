#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Loading required packages
require(tidyverse)
require(xtable)
require(sf)

# Read in the table 
survey_table <- read.csv("~/Downloads/circ_table.csv")

# Loading shapefiles 
areas <- read_sf("~/Downloads/areas.geojson")



tmp <- survey_table %>%
  # Merging on area label
  left_join(areas %>%
              filter(area_level == 0) %>%
              dplyr::select(Country = iso3, area_name) %>%
              st_drop_geometry(),
            by = "Country") %>%
  # Preparing columns
  mutate(
    # Survey series
    Series = case_when(Series == 'AIDS Indicator Survey' ~ paste(Year, '(AIS)'),
                       Series == 'Botswana Aids Impact Survey' ~ paste(Year, '(BAIS)'),
                       Series == 'Demographic and Health Survey' ~ paste(Year, '(DHS)'),
                       Series == 'Human Sciences Research Council' ~ paste(Year, '(HSRC)'),
                       Series == 'Multiple Indicator Cluster Survey' ~ paste(Year, '(MICS)'),
                       Series == 'Population-Based HIV Impact Assessment' ~ paste(Year, '(PHIA)')),
    # Survey questions
    Q1 = case_when(Circumcision.Status == TRUE  ~ '\\checkmark',
                   Circumcision.Status == FALSE ~ '\\xmark'),
    Q2 = case_when(Age.at.Circumcision == TRUE  ~ '\\checkmark',
                   Age.at.Circumcision == FALSE ~ '\\xmark'),
    Q3 = case_when(Who.performed. == TRUE  ~ '\\checkmark',
                   Who.performed. == FALSE ~ '\\xmark'),
    Q4 = case_when(Where.performed. == TRUE  ~ '\\checkmark',
                   Where.performed. == FALSE ~ '\\xmark'),
    # Sample size
    N1 = Sample.Size,
    N2 = paste(Number.Participated, 
               ' (', 
               gsub(' ', '',format(round(100 * Number.Participated/Sample.Size, 2), nsmall = 2)), 
               '\\%)',
               sep = ''),
    # Circumcision status
    N3 = paste(Left.Censored + Uncensored.Circumcised, 
               ' (', 
               gsub(' ', '',format(round(100 * (Left.Censored + Uncensored.Circumcised) / Number.Participated, 2), nsmall = 2)), 
               '\\%)',
               sep = ''),
    N4 = paste(Right.Censored.Uncircumcised, 
               ' (', 
               gsub(' ', '',format(round(100 * (Right.Censored.Uncircumcised) / Number.Participated, 2),, nsmall = 2)), 
               '\\%)',
               sep = ''),
    # Circumcision details 
    N5 = paste(Uncensored.Circumcised,
               ' (', 
               gsub(' ', '',format(round(100 * Uncensored.Circumcised / (Left.Censored + Uncensored.Circumcised), 2),, nsmall = 2)), 
               '\\%)',
               sep = ''),
    N6 = paste(Known.Circumcision.Type, 
               ' (', 
               gsub(' ', '',format(round(100 * Known.Circumcision.Type / (Left.Censored + Uncensored.Circumcised), 2),, nsmall = 2)), 
               '\\%)',
               sep = '')) %>%
  dplyr::arrange(area_name, Year) %>%
  # Ordering columns 
  dplyr::select(area_name, Series, Q1, Q2, Q3, Q4, N1, N2, N4, N3, N5, N6) %>%
  mutate(row = 1:dplyr::n() %% 16) %>%
  group_by(area_name) %>%
  mutate(n1 = 1:dplyr::n(),
         n2 = max(n1), 
         test = case_when((n1 == n2) & (row == 0) ~ "\\\\[3pt] \\pagebreak \n ",
                          (n1 == n2) & !(row == 0) ~ "\\\\[3pt] \n ",
                          !(n1 == n2) & (row == 0) ~ "\\\\ \\pagebreak \n",
                          !(n1 == n2) & !(row == 0) ~ "\\\\ \n "))%>%
  # Ordering columns 
  dplyr::select(-c(n2, n1, row)) %>%
  ungroup()


tmp$area_name[tmp$area_name == 'GHANA'] <- 'Ghana'
# List of countries
cntry <- unique(tmp$area_name)

# Opening text file
sink('~/Desktop/Survey_Table.tex')

# Table start
cat("{\\linespread{1} \n")
cat("\\footnotesize \n")
cat("\\begin{longtable}[c]{ll cccc ccc ccc} \n")
cat("      %%%%%%%%%%%%%%%%%%%% \n")
cat("      %%% Table header %%% \n")
cat("      %%%%%%%%%%%%%%%%%%%% \n")
cat("      % Upper layer of table header \n")
cat("      \\hline \n")
cat("      % Lines inside the header \n")
cat("      \\multicolumn{1}{l}{} & & \\multicolumn{4}{c}{\\bf Survey questions} & \\multicolumn{2}{c}{\\bf Sample size} & \\multicolumn{2}{c}{\\bf Circumcision status} & \\multicolumn{2}{c}{\\bf Circumcision details} \\\\ \n")
cat("      \\cmidrule(lr){3-6} \\cmidrule(lr){7-8}\\cmidrule(lr){9-10} \\cmidrule(lr){11-12} \n")
cat("       & & {\\bf Circ.} & {\\bf Age at} & {\\bf Who} & {\\bf Where } & \\multirow{2}{*}{\\bf Total} & \\multirow{2}{*}{\\bf Included (\\%)} & \\multirow{2}{*}{\\bf Uncircumcised (\\%)} & \\multirow{2}{*}{\\bf Circumcised (\\%)} & {\\bf Known age} & {\\bf Known type} \n")
cat("       \\\\ \n")
cat("       & & {\\bf Status} & {\\bf Circ.} & {\\bf performed?} & {\\bf performed?}  & & & & & {\\bf at circ. (\\%)} & {\\bf of circ. (\\%)} \\\\[3pt] \n")
cat("      % Lower lines \n")
cat("      \\hline \n")
cat("      \\vspace{-8pt} \n")
cat("      \\endhead \n")
cat("      %%%%%%%%%%%%%%%%%%%% \n")
cat("      %%% Table footer %%% \n")
cat("      %%%%%%%%%%%%%%%%%%%% \n")
cat("      \\\\[-8pt] \\hline \n")
cat("      \\caption{Summary of male circumcision surveys in each sub-Saharan country included in the study. These include the Demographic and Health Surveys (DHS), AIDS Indicator Surveys (AIS), Population-based HIV Impact Assessment (PHIA) surveys, Multiple Indicator Cluster Surveys (MICS) and Human Sciences Research Council (HSRC) surveys. Surveys which did not include a self-reported circumcision status question were not included. The availability of circumcision-related questions included in each survey are indicated. Sample sizes indicate the total number of males available and those included from each survey. For those included, the number reporting they were uncircumcised and circumcised are indicated. For those circumcised, the number of men who where an age and type of circumcision could be used are also indicated.} \n")
cat("      \\endfoot \n")
cat("    %%%%%%%%%%%%%%%%%%%%%% \n")
cat("    %%% Table contents %%% \n")
cat("    %%%%%%%%%%%%%%%%%%%%%% \n")

# Loop for each country 
for (i in cntry){
  # filtering for country
  tmp2 <- tmp %>%
    filter(area_name == i) %>%
    as.data.frame()
  
  # Converting to data frame
  tmp2 <- as.data.frame(tmp2)

  # Country Name 
  cat(paste("    \\multicolumn{8}{l}{\\textbf{", unique(tmp2$area_name), '}} \\\\', '\n'))
  # Data Table
  cat(paste(paste0("    ", ' & ', tmp2[,2],' & ', tmp2[,3],' & ',  tmp2[,4],' & ', 
                  tmp2[,5],' & ',  tmp2[,6],' & ',  tmp2[,7],' & ',  tmp2[,8],' & ',  
                  tmp2[,9],' & ',  tmp2[,10],' & ',  tmp2[,11],' & ',  tmp2[,12], tmp2[,13])))
}

# Closing table commands
cat("\\end{longtable}} \n")

# Closed text file
sink()
    


