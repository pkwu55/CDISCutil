#' Add a column with column name from the name_from column
#' and values from name_to column
#' Note that the value of the names_from column must be unique
#'
#' @param dst data set name
#' @param name_from column to get the new column name, values must be unique
#' @param name_to column to have the new name
#' @return data with new column added

newcol <- function(dst, name_from, name_to){
  new_name <- unlist(dplyr::select(dst, all_of(name_from)) %>%
                       dplyr::distinct())

  # check if values are inheritly numeric and convert to numeric if yes
  if (all(grepl("^[0-9]{1,}$", eval(expr(`$`(dst, !!name_to))))))
    {
    eval(expr(`$`(dst, !!name_to) <- as.numeric(`$`(dst, !!name_to))))
    }
  eval(expr(`$`(dst, !!new_name) <- `$`(dst, !!name_to)))
  return(dst)
}
