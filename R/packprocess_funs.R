#' A function to document dataframe variables with their labels
#'
#' This function will taken any data.frame object with labelled variables and
#' automatically generate the Variables section of the documentation.
#'
#' @param dt a data.frame object with labelled variables
#' @param file_path a character of length 1, the file path to a ".R" script
#' to be created for the documentation (preferably in the R folder)
#' @param title character, the title of the dataset
#' @param description character, the description of the dataset
#' @param format character, the character of the dataset
#' @param source character, the source information for the data
#' @param dataname character, the name of the data.frame object stored within
#' the data folder now being documented
#'
#' @details See roxygen documentation for full information on the parameters
#'
#' @importFrom data.table data.table
#' @importFrom stringr str_replace_all
#' @importFrom labelled get_variable_labels
#' @importFrom dplyr %>% mutate
#'
#' @export
#'


gen_var_documentation <- function(dt,
                                  file_path,
                                  title = "Mat's Governance Participation Indicators Dataset",
                                  description = "This dataset contains various indicators related to democracy, governance, social participation and economic conditions across multiple countries and years.  The variables come from sources such as V-Dem, Gallup, and QoG, covering aspects like political engagement, corruption, civil liberties, and electoral integrity.",
                                  format = "Data frame with indicators, unit of observation country-year",
                                  source = "V-DEM, Gallup & VOG",
                                  dataname = "cloutier_dt"){


  labels_dt <- get_variable_labels(dt)


  labels_dt <- data.table(var = names(labels_dt),
                          labels = unlist(labels_dt, use.names = FALSE))



  labels_dt <-
    labels_dt %>%
    mutate(labels = stringr::str_replace_all(labels, "\\\\", "\\\\\\\\")) %>%
    mutate(roxygen = paste0("#' \\item{",
                            var,
                            "}{",
                            labels,
                            "}"))


  ### write to file
  top_blurb <- c(
    paste0("#' @title ", title),
    paste0("#'"),
    paste0("#' @description ", description),
    paste0("#'"),
    paste0("#' @format ", format),
    paste0("#'"),
    paste0("#' @source ", source),
    "#' \\describe{"
  )


  bottom_blurb <- paste0("#' } \n",
                         "#' @examples \n",
                         paste0("#' ", dataname, "\n"),
                         paste0('"', dataname, '"'))

  full_document <- c(top_blurb, labels_dt$roxygen, bottom_blurb)

  writeLines(full_document, file_path)


  return(labels_dt)


}

