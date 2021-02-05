"+" <- function(e1, e2) {
  if (is.character(e1) & is.character(e2)) {
    paste(e1, e2, sep = "")
  } else {
    base::"+"(e1, e2)
  }
}

round2 = function(x, d=0) {
  p = 10^d
  return((x * p * 2 + 1) %/% 2 / p)
}

read_csv_all <- function(path = "./", extension = "csv", recursive = FALSE, basename = TRUE, sep = ","){
  names <- list.files(path = path,
                      full.names = FALSE, 
                      recursive = recursive, 
                      pattern = paste0("\\.", extension, "$"))
  names <- gsub(paste0(".", extension), "", names)
  
  if(basename == TRUE){
    names <- basename(names)
  }
  
  paths <- list.files(path = path,
                      full.names = TRUE, 
                      recursive = recursive, 
                      pattern = paste0(".", extension, "$"))
  
  for(i in 1:length(names)){
    assign(names[i], data.table::fread(paths[i], sep = sep), envir = .GlobalEnv)
  }
}


source_all <- function(dir = "./", encoding = "UTF-8"){
  sourcefile <- list.files(dir, full.names = TRUE)
  purrr::walk(sourcefile, ~source(., encoding = encoding))
}

save_df <- function(df, directory = "./"){
  name_df <- deparse(substitute(df))
  save(list = name_df, file = paste0(directory, name_df, ".RData"), envir = parent.frame())
}
