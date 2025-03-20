#' @title Mat's Governance Participation Indicators Dataset (Global Aggregates)
#'
#' @description This data computes the global aggregates from the Cloutier Dataset
#'
#' @format Data frame with global aggregate indicators, unit of observation year
#'
#' @source V-DEM, Gallup & VOG indicators, global aggregates
#' \describe{
#' \item{year}{Social Capital}
#' \item{social_capital}{Absence of Exclusion}
#' \item{absence_exclusion}{Absence of Elite Capture}
#' \item{absence_capture}{Scaled sum of variables v2x_freexp_altinf and v2caassemb}
#' \item{vdeminformal}{Scaled sum of variables bti_aar and bti_foe}
#' \item{btiinformal}{Informal Channels}
#' \item{informal_channels}{Scaled sum of variables v2xel_frefair and v2xel_locelec}
#' \item{vdeminstitutional}{Scaled sum of variables bti_ffe and ibp_cat}
#' \item{btiinstitutional}{Institutional Channels}
#' \item{institutional_channels}{Scaled sum of variables v2csprtcpt and v2cscnsult}
#' \item{vdemcso}{Scaled sum of the variables bti_ig and bti_csp}
#' \item{bticso}{Intermediary Channels}
#' \item{intermediary_channels}{Civil Capacity}
#' \item{civil_capacity}{Quality of the Citizen-State Interface}
#' \item{quality_interface}{Resilience of the Social Contract}
#' \item{resilience}{To what extent are elections free and fair?}
#' \item{v2cagenmob}{Perceptions of the Civic Space}
#' \item{percept_civicspace}{Confidence in National Government}
#' \item{natgov_confidence}{Social Capital}
#' } 
#' @examples 
#' globalcomp_dt
"globalcomp_dt"
