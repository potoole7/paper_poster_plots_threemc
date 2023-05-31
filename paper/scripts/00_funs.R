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
