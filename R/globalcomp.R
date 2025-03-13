#' @title Global Comparators Indicators Dataset for the Cloutier Dataset
#'
#' @description This dataset contains various indicators related to global comparators generated from the cloutier dataset.
#'
#' @format Data frame with indicators, unit of observation is year
#'
#' @source V-DEM, Gallup & VOG
#' \describe{
#' \item{year}{Year}
#' \item{social_capital}{Social Capital}
#' \item{absence_exclusion}{Absence of Exclusion}
#' \item{absence_capture}{Absence of Elite Capture}
#' \item{vdeminformal}{Scaled sum of variables v2x_freexp_altinf and v2caassemb}
#' \item{btiinformal}{Scaled sum of variables bti_aar and bti_foe}
#' \item{informal_channels}{Informal Channels}
#' \item{vdeminstitutional}{Scaled sum of variables v2xel_frefair and v2xel_locelec}
#' \item{btiinstitutional}{Scaled sum of variables bti_ffe and ibp_cat}
#' \item{institutional_channels}{Institutional Channels}
#' \item{vdemcso}{Scaled sum of variables v2csprtcpt and v2cscnsult}
#' \item{bticso}{Scaled sum of the variables bti_ig and bti_csp}
#' \item{intermediary_channels}{Intermediary Channels}
#' \item{civil_capacity}{Civil Capacity}
#' \item{quality_interface}{Quality of the Citizen-State Interface}
#' } 
#' @examples 
#' globalcomp_dt
"globalcomp_dt"
