laziestLoad <- function(path=".") {
  files <- list.files(path,recursive=TRUE)
  cache_files <- sub(".rdb$", "", files[grepl(".rdb$", files)])
  for (i in cache_files) try(lazyLoad(i, envir = .GlobalEnv))
}
laziestLoad()