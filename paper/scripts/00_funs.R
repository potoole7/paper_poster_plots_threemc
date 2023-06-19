#### Functions used across different scripts ####

# function to load data from orderly archive
load_archive <- function(task, 
                         orderly_root = here::here(),
                         file, 
                         load_fun = readr::read_csv, 
                         archives = NULL, 
                         query = NULL, ...) {
  
  if (is.null(archives) && !is.null(query)) {
    dir <- orderly::orderly_search(query = query, name = task, ...)
  } else if (!is.null(archives)) {
    dir <- archives %>% 
      filter(name == task) %>% 
      slice(n()) %>% 
      pull(id)
  } else stop("Please provide one of `archives` or `query` arguments")
  
  load_fun(file.path(
    orderly_root, 
    "archive",
    task, 
    dir,
    "artefacts", 
    file 
  ))
}

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

#### Functions to Fix Country Names & Print Survey Types ####

country_name_convention <- function(x) {
  x <- dplyr::case_when(
    x == "Congo - Kinshasa"    ~ "DR Congo",
    x == "Congo - Brazzaville" ~ "Congo",
    grepl("Ivoire", x)         ~ "Cote d'Ivoire",
    x == "Gambia"              ~ "The Gambia",
    TRUE                       ~ x
  )
}

survey_series_expand <- function(x) {
  x <- dplyr::case_when(
    x == "AIS"  ~ "AIDS Indicator Survey",
    x == "DHS"  ~ "Demographic and Health Survey",
    x == "PHIA" ~ "Population-Based HIV Impact Assessment",
    x == "HSRC" ~ "Human Sciences Research Council",
    x == "BAIS" ~ "Botswana Aids Impact Survey",
    x == "MICS" ~ "Multiple Indicator Cluster Survey",
    TRUE        ~ "Sexual Behavior Survey"
  )
}