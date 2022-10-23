#' Build SDTMplus data from main and supp
#' domains
#' @import dplyr
#' @param lib location for xpt/sas7bdat datasets
#' @param dst data set name
#' @param ext extension, could be xpt or sas7bdat, if any then automatically select one
#' @return SDTMplus data
#' @export
#'
#' @examples
#' \dontrun{
#' dmplus <- build_sdtm(dm)
#' }

build_sdtm <- function (lib, dst, ext = c("any")){

  main_dm <- import_sas(lib, dst)
  supp_dm_name <- paste0("supp", dst)
  supp_dm <- import_sas(lib, supp_dm_name)

  if (is.numeric(supp_dm)){
    return(main_dm)
  } else {
    if (all(supp_dm$IDVAR != '')){
      supp_dm_gp <- dplyr::group_split(supp_dm, "IDVAR") # spliy by IDVAR

      by_id <- unlist(lapply(supp_dm_gp,
                             function(x) select(x, "IDVAR") %>%
                               distinct()),
                      use.names = FALSE) # vector for IDVARs

      supp_dm_gp_wide <- lapply(supp_dm_gp, function(x) # pivot
        tidyr::pivot_wider(x,
                           id_col = c("USUBJID", "IDVAR", "IDVARVAL"),
                           names_from = "QNAM",
                           values_from = "QVAL",
                           values_fill = ''))

      supp_dm_gp_wide <- lapply(supp_dm_gp_wide, function(x) newcol(x, "IDVAR", "IDVARVAL")) #add new col in each df

      comb_dm <- main_dm

      for (i in 1:length(supp_dm_gp)){
        comb_dm <- dplyr::left_join(comb_dm, supp_dm_gp_wide[[i]],
                                    by = c("USUBJID",
                                           purrr::map_chr(by_id[i], rlang::as_string))
                                    ) %>%
          select(-c("IDVAR", "IDVARVAL"))
      }
     }else {
       supp_dm_wide <- tidyr::pivot_wider(supp_dm,
                                          id_col = c("USUBJID", "IDVAR", "IDVARVAL"),
                                          names_from = "QNAM",
                                          values_from = "QVAL",
                                          values_fill = '')

      comb_dm <- dplyr::left_join(main_dm, supp_dm_wide, by = "USUBJID")
    }
  }
  return(comb_dm)
}
