#' Wrapper function to read SAS datasets in either sas7bdat or xpt
#' from a directory
#'
#' @param lib location for xpt/sas7bdat datasets
#' @param dst dataset name
#' @param ext extension, could be xpt or sas7bdat, if any then automatically select one
#' @return sas dataset read by haven::read_sas
#' @examples
#' \dontrun{
#' adsl <- import_sas(adam, adsl)
#' }
import_sas <- function(lib, dst, ext = c("any")){
  if (!file.exists(lib)){
    print("lib does not exist")
    break
  }

  if (is.null(dst)){
    print("please provide dataset name")
    break
  }

  if (ext != "any"){
    dst_name = paste(dst, ext, sep = ".")
  } else {
    ls_lib <- list.files(lib)
    dst_name <- ls_lib[grepl(dst, ls_lib)]
    dst_name <- dst_name[1] #sas7bdat will have priority
  }

  dst_fname <- file.path(lib, dst_name)

  if (!file.exists(dst_fname)){
    print(paste0("dataset", dst_name, "does not exist in", lib))
    break
  } else {
    #print(dst_fname)
    return(haven::read_sas(dst_fname))
  }
}
