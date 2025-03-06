################################################################################
######## PREPARE DATA FOR THE GALLUP PLUS OTHER SOURCES ANALYTICS ##############
################################################################################

#### -- package loading -- ####

#### make sure pacman is not installed before installing it
#### pacman is needed to load the other packages
if (sum(installed.packages()[,1] %in% "pacman") != 1){

  install.packages("pacman")

}

if (!requireNamespace("devtools", quietly = TRUE)){

  install.packages("devtools")

}


if (sum(installed.packages()[,1] %in% "vdemdata") != 1){

  devtools::install_github("vdeminstitute/vdemdata")

}

## load necessary packages
pacman::p_load(dplyr, data.table, sf, readxl, rio, labelled,
               vdemdata, haven, sjlabelled, tibble)

### read in the appropriate datasets

qogstd_dt <- haven::read_dta("https://www.qogdata.pol.gu.se/data/qog_std_ts_jan25.dta")


#### read in the gallup data
gallup_dt <- haven::read_dta("data-raw/GWP_variables_new.dta")


#### read in worldbank country information from API
wbiso_url <- "https://datacatalogapi.worldbank.org/ddhxext/ResourceDownload?resource_unique_id=DR0090755"

code_dt <- rio::import(wbiso_url) %>% tibble()

### create labels for the variables in code_dt
code_dt <-
  code_dt %>%
  rename(wbcountryname = Economy,
         wbcode = Code,
         wbregion = Region,
         wbincomegroup = `Income group`,
         wblendingcat = `Lending category`) %>%
  set_variable_labels(wbcountryname = "World Bank Country Name",
                      wbcode = "World Bank Country ISO-3 Code",
                      wbregion = "World Bank Region Name",
                      wbincomegroup = "World Bank Income Group",
                      wblendingcat = "World Bank Lending Category")

### create labels for the gallup data
gallup_dt <-
  gallup_dt %>%
  rename(year = YEAR_WAVE,
         countrycode = COUNTRY_ISO3,
         voiced_opinion = wp111d,
         margin_sexual = wp105d,
         margin_immigrant = wp106d,
         biz_corrupt = wp145d,
         gov_corrupt = wp146d,
         media_freedom = wp10251d,
         election_confidence = wp144d,
         voltime_org = wp109d,
         media_confidence = wp143d,
         natgov_confidence = wp139d,
         leadership_approval = wp150d) %>%
  set_variable_labels(countrynew = "Gallup Country Name",
                      year = "Year",
                      countrycode = "Country ISO-3 CODE",
                      voiced_opinion = "Level of engagement of citizens in the social and political life of the country",
                      margin_sexual = "Is the city or areas where you live a good place or not a good place to live for gay or lesbian people?",
                      margin_immigrant = "Is the city or area where you live a good place or not a good place to live for immigrants from other countries?",
                      biz_corrupt = "Is corruption widespread within businesses located in (country), or not?",
                      gov_corrupt = "Is corruption widespread throughout the government in (country), or not?",
                      media_freedom = "Media Freedom",
                      election_confidence = "Confidence in honesty of elections",
                      voltime_org = "Volunteered your time to an organization?",
                      media_confidence = "Confidence in Media",
                      natgov_confidence = "Confidence in National Government",
                      leadership_approval = "Approval of Country's Leadership")


#### combine all the data (vdem, gallup, qog and WB country info)
combine_dt <-
  vdem %>%
  left_join(gallup_dt %>%
              distinct(countrycode,
                       year,
                       .keep_all = TRUE),
            by = join_by(country_text_id == countrycode,
                         year == year)) %>%
  left_join(qogstd_dt %>%
              distinct(ccodealp,
                       year,
                       .keep_all = TRUE),
            by = join_by(country_text_id == ccodealp,
                         year == year)) %>%
  left_join(code_dt,
            by = join_by(country_text_id == wbcode),
            keep = TRUE)


# #### send combined data to Mathieu
haven::write_dta(combine_dt,
                 "data-raw/gallup_vdem_qogstdts.dta")

### create the labels for vdem variables

vdemlabel_dt <- lapply(X = colnames(vdem),
                       FUN = function(x) {
  info <- var_info(x)

  # Ensure all fields have a valid value
  dt <- data.table(
    var = x,
    name = ifelse(!is.null(info$name), info$name, NA),
    label = ifelse(!is.null(info$question), info$question, NA),
    clarification = ifelse(!is.null(info$clarification), info$clarification, NA),
    stringsAsFactors = FALSE
  )

  return(dt)
})

vdemlabel_dt <- rbindlist(vdemlabel_dt)

#### select the set of variables of interest

combine_dt <-
  combine_dt %>%
  dplyr::select(v2dlengage, "v2cagenmob", "voiced_opinion",
                "bti_sc", "v2xpe_exlpol", "v2xpe_exlecon", "v2xcl_rol",
                "v2x_egal", "v2xeg_eqprotec", "v2xeg_eqaccess",
                "v2xeg_eqdr", "v2clacjust", "v2clsocgrp", "v2clsnlpct",
                "margin_sexual", "margin_immigrant", "bti_eo", "bti_seb",
                "v2pepwrsoc", "v2pepwrses", "biz_corrupt", "gov_corrupt",
                "bti_poa", "bti_acp", "v2x_freexp_altinf", "v2caassemb",
                "media_freedom", "bti_aar", "bti_foe", "ciri_assn",
                "v2xel_frefair", "v2xel_locelec", "v2xel_regelec",
                "v2xdd_dd", "election_confidence", "v2cltrnslw",
                "bti_ffe", "ibp_cat", "egov_epar", "v2csprtcpt",
                "v2cseeorgs", "v2cscnsult", "voltime_org", "bti_aar",
                "bti_csp", "wbcountryname", "wbcode", "wbregion",
                "wbincomegroup", "wblendingcat", "year")

#### include the variable labels for the v2 variables in combine_dt that are
#### unlabelled and have labels in vdemlabel_dt
v2name_list <- colnames(combine_dt)[grepl("^v2", colnames(combine_dt))]

# Ensure that the labels are correctly matched to the variables
labels_to_assign <- vdemlabel_dt %>%
  filter(var %in% v2name_list) %>%
  select(var, label) %>%
  deframe()  # Convert to a named vector

# Apply the labels to the selected columns dynamically
combine_dt[,colnames(combine_dt)[grepl("^v2", colnames(combine_dt))]] <-
  combine_dt %>%
  dplyr::select(starts_with("v2")) %>%
  set_variable_labels(.labels = labels_to_assign)

cloutier_dt <-
  combine_dt %>%
  set_variable_labels(year = "Year") ## set variable label for variable "year"

# save(justpulse_dt, file = "data/justpulse_dt.rda") ### save the combined dataset

### save in convenient form for diffs
write.csv(cloutier_dt, "data-raw/cloutier_dt.csv")


### save as package data
usethis::use_data(cloutier_dt, overwrite = TRUE)




