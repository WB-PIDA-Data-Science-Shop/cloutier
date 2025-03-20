################################################################################
######## PREPARE DATA FOR THE GALLUP PLUS OTHER SOURCES ANALYTICS ##############
################################################################################
#### load all functions and necessary datasets

devtools::load_all()


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


# # #### send combined data to Mathieu
# haven::write_dta(combine_dt,
#                  "data-raw/gallup_vdem_qogstdts.dta")

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
                "bti_ig", "bti_csp", "wbcountryname", "wbcode", "wbregion",
                "wbincomegroup", "wblendingcat", "year", "natgov_confidence",
                "election_confidence")

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

###### ------------- enough cleaning, index computations time! --------------- #######

cloutier_dt <-
  cloutier_dt %>%
  mutate(social_capital = compute_transdices(dt = .,
                                             vars = c("v2dlengage", "bti_sc"),
                                             std_funs = scale,
                                             agg_fun = sum,
                                             index_fun = scale)) %>%
  mutate(absence_exclusion = compute_transdices(dt = .,
                                                vars = c("v2xeg_eqprotec", "bti_eo"),
                                                std_funs = scale,
                                                agg_fun = sum,
                                                index_fun = scale)) %>%
  mutate(absence_capture = compute_transdices(dt = .,
                                              vars = c("v2pepwrsoc", "v2pepwrses"),
                                              std_funs = scale,
                                              agg_fun = sum,
                                              index_fun = scale)) %>%
  mutate(vdeminformal = v2x_freexp_altinf + v2caassemb) %>%
  mutate(btiinformal = bti_aar + bti_foe) %>%
  mutate(informal_channels = compute_transdices(dt = .,
                                                vars = c("vdeminformal", "btiinformal"),
                                                std_funs = scale,
                                                agg_fun = sum,
                                                index_fun = scale)) %>%
  mutate(vdeminstitutional = v2xel_frefair + v2xel_locelec) %>%
  mutate(btiinstitutional = bti_ffe + ibp_cat) %>%
  mutate(institutional_channels = compute_transdices(dt = .,
                                                     vars = c("vdeminstitutional", "btiinstitutional"),
                                                     std_funs = scale,
                                                     agg_fun = sum,
                                                     index_fun = scale)) %>%
  mutate(vdemcso = v2csprtcpt + v2cscnsult) %>%
  mutate(bticso = bti_ig + bti_csp) %>%
  mutate(intermediary_channels = compute_transdices(dt = .,
                                                    vars = c("vdemcso", "bticso"),
                                                    std_funs = scale,
                                                    agg_fun = sum,
                                                    index_fun = scale)) %>%
  mutate(civil_capacity = scale(x = social_capital + absence_exclusion + absence_capture)) %>%
  mutate(quality_interface = scale(informal_channels + institutional_channels + intermediary_channels)) %>%
  mutate(percept_civicspace = compute_transdices(dt = .,
                                                 vars = c("election_confidence", "voiced_opinion"),
                                                 std_funs = scale,
                                                 agg_fun = sum,
                                                 index_fun = scale)) %>%
  mutate(resilience = scale(scale(natgov_confidence) + percept_civicspace + scale(v2cagenmob))) %>%
  mutate(across(c(vdeminformal, btiinformal, vdeminstitutional,
                  btiinstitutional, vdemcso, bticso),
         scale)) %>%
  mutate(across(where(is.numeric), preserve_labels_as_numeric)) %>%
  set_variable_labels(social_capital = "Social Capital",
                      absence_exclusion = "Absence of Exclusion",
                      absence_capture = "Absence of Elite Capture",
                      informal_channels = "Informal Channels",
                      institutional_channels = "Institutional Channels",
                      intermediary_channels = "Intermediary Channels",
                      civil_capacity = "Civil Capacity",
                      quality_interface = "Quality of the Citizen-State Interface",
                      vdeminformal = "Scaled sum of variables v2x_freexp_altinf and v2caassemb",
                      btiinformal = "Scaled sum of variables bti_aar and bti_foe",
                      vdeminstitutional = "Scaled sum of variables v2xel_frefair and v2xel_locelec",
                      btiinstitutional = "Scaled sum of variables bti_ffe and ibp_cat",
                      vdemcso = "Scaled sum of variables v2csprtcpt and v2cscnsult",
                      bticso = "Scaled sum of the variables bti_ig and bti_csp",
                      percept_civicspace = "Perceptions of the Civic Space",
                      resilience = "Resilience of the Social Contract")



index_list <- c("social_capital", "absence_exclusion", "absence_capture",
                "vdeminformal", "btiinformal", "informal_channels",
                "vdeminstitutional", "btiinstitutional", "institutional_channels",
                "vdemcso", "bticso", "intermediary_channels","civil_capacity",
                "quality_interface", "resilience", "v2cagenmob", "percept_civicspace",
                "natgov_confidence")


##### create regional and global comparators datasets now

regcomp_dt <-
  cloutier_dt %>%
  as.data.table() %>%
  .[, lapply(.SD, mean, na.rm = TRUE),
    .SDcols = index_list,
    by = c("year", "wbregion")] %>%
  filter(!is.na(wbregion))

globalcomp_dt <-
  cloutier_dt %>%
  as.data.table() %>%
  .[, lapply(.SD, mean, na.rm = TRUE), .SDcols = index_list, by = "year"]


### run automated data documentation
gen_var_documentation(dt = cloutier_dt,
                      file_path = "R/cloutier.R")

gen_var_documentation(dt = regcomp_dt,
                      file_path = "R/regcomp.R",
                      title = "Mat's Governance Participation Indicators Dataset (Regional Aggregates)",
                      description = "This data computes the regional aggregates from the Cloutier Dataset",
                      format = "Data frame with regional aggregate indicators, unit of observation region-year",
                      source = "V-DEM, Gallup & VOG indicators, regional aggregates",
                      dataname = "regcomp_dt")

gen_var_documentation(dt = globalcomp_dt,
                      file_path = "R/globalcomp.R",
                      title = "Mat's Governance Participation Indicators Dataset (Global Aggregates)",
                      description = "This data computes the global aggregates from the Cloutier Dataset",
                      format = "Data frame with global aggregate indicators, unit of observation year",
                      source = "V-DEM, Gallup & VOG indicators, global aggregates",
                      dataname = "globalcomp_dt")

### save in convenient form for diffs
write.csv(cloutier_dt, "data-raw/cloutier_dt.csv")
write.csv(regcomp_dt, "data-raw/regional_comparator.csv")
write.csv(globalcomp_dt, "data-raw/global_comparator.csv")


### save as package data
usethis::use_data(cloutier_dt, overwrite = TRUE)
usethis::use_data(regcomp_dt, overwrite = TRUE)
usethis::use_data(globalcomp_dt, overwrite = TRUE)



