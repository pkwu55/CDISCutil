#' Read SAS data set in either sas7bdat or xpt
#' from a directory
#'
#' @param lib location for xpt/sas7bdat datasets
#' @param dst dataset name
#' @param ext extension, could be xpt or sas7bdat, if any then automatically select one
#' @return sas dataset read by haven::read_sas
#' @export
#'
#' @examples
#' \dontrun{
#' adsl <- import_sas(adam, "adsl")
#' }

import_sas <- function(lib, dst, ext = c("any")){
  if (!file.exists(lib)){
    print("library does not exist")
    return(0)
  }

  if (is.null(dst)){
    print("please provide dataset name")
    return(0)
  }

  if (ext != "any"){
    dst_name = paste(dst, ext, sep = ".")
  } else {
    ls_lib <- list.files(lib)
    dst_name <- ls_lib[grepl(paste0('^',dst), ls_lib)]
    dst_name <- dst_name[1] #sas7bdat will have priority
  }

  dst_fname <- file.path(lib, dst_name)

  if (!file.exists(dst_fname)){
    print(paste("dataset", dst, "does not exist in", lib))
    return(0)
  } else {
    if (grepl("\\.xpt$", dst_fname, ignore.case = TRUE)){
      return(haven::read_xpt(dst_fname))
    }else
      return(haven::read_sas(dst_fname))
  }
}
