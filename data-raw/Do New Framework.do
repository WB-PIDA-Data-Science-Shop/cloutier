clear all
set maxvar 10000
use "C:\Users\WB511679\OneDrive - WBG\Projects\Social Contract Assessment Framework Approach Paper\SC MAPS\Prosperity Factsheet\gallup_vdem_qogstdts.dta" 

keep countrynew cname_qog country_name year cname_year v2dlengage v2cagenmob voiced_opinion bti_sc v2xeg_eqprotec margin_immigrant margin_sexual bti_eo bti_seb v2pepwrsoc v2pepwrses biz_corrupt gov_corrupt bti_poa bti_acp v2x_freexp_altinf v2caassemb media_freedom bti_aar bti_foe v2xel_frefair v2xel_locelec v2xel_regelec v2xdd_dd election_confidence bti_ffe ibp_cat ibp_obi ibp_pub v2csprtcpt v2cscnsult voltime_org bti_ig bti_csp

keep if year>2004

************************************************************
*Civil Capacity*

egen std_v2dlengage=std(v2dlengage) //this standardizes relatively to the historical average and standard deviation to get both variables on a similar scale.
egen std_bti_sc=std(bti_sc) 
egen Social_Capital=std(std_v2dlengage+std_bti_sc) // This standardization is so that the final index has a simple interpretation. I am not standardizing by year because I want to see the world average every year compared to the historical average.
bys year: egen Social_Capital_World=mean(Social_Capital)

egen std_v2xeg_eqprotec=std(v2xeg_eqprotec)
egen std_bti_eo=std(bti_eo)
egen Absence_Exclusion=std(std_v2xeg_eqprotec+std_bti_eo)
bys year: egen Absence_Exclusion_World=mean(Absence_Exclusion)

egen std_vdempower=std(v2pepwrsoc+v2pepwrses)
egen std_bti_poa=std(bti_poa)
egen Absence_Capture=std(std_vdempower+std_bti_poa)
bys year: egen Absence_Capture_World=mean(Absence_Capture)

egen Civil_Capacity=std(Social_Capital+Absence_Capture+Absence_Exclusion)
bys year: egen Civil_Capacity_World=mean(Civil_Capacity)

twoway (line Social_Capital year if country_name=="Nigeria") (line Social_Capital_World year) 
twoway (line Absence_Exclusion year if country_name=="Nigeria") (line Absence_Exclusion_World year)
twoway (line Absence_Capture year if country_name=="Nigeria") (line Absence_Capture_World year)
twoway (line Civil_Capacity year if country_name=="Nigeria")(line Civil_Capacity_World year)

**************************************************************************
*Citizen-State Interface*

egen std_vdeminformal=std(v2x_freexp_altinf+v2caassemb)
egen std_btiinformal=std(bti_aar+bti_foe) 
egen Informal_Channels=std(std_vdeminformal+std_btiinformal) // This standardization is so that the final index has a simple interpretation. I am not standardizing by year because I want to see the world average every year compared to the historical average.
bys year: egen Informal_Channels_World=mean(Informal_Channels)

egen std_vdeminstitutional=std(v2xel_frefair+v2xel_locelec)
egen std_btiinstitutional=std(bti_ffe+ibp_cat)
egen Institutional_Channels=std(std_vdeminstitutional+std_btiinstitutional)
bys year: egen Institutional_Channels_World=mean(Institutional_Channels)

egen std_vdemcso=std(v2csprtcpt+v2cscnsult)
egen std_bticso=std(bti_ig + bti_csp)
egen Intermediary_Channels=std(std_vdemcso+std_bticso)
bys year: egen Intermediary_Channels_World=mean(Intermediary_Channels)

egen Quality_Interface=std(Informal_Channels+Institutional_Channels+Intermediary_Channels)
bys year: egen Quality_Interface_World=mean(Quality_Interface)

twoway (line Informal_Channels year if country_name=="Nigeria") (line Informal_Channels_World year) 
twoway (line Institutional_Channels year if country_name=="Nigeria") (line Institutional_Channels_World year)
twoway (line Intermediary_Channels year if country_name=="Nigeria") (line Intermediary_Channels_World year)
twoway (line Quality_Interface year if country_name=="Nigeria")(line Quality_Interface_World year)


/////////////////////////////////////

/// Tests and controls

//countrynew cname_qog country_name year cname_year
//v2dlengage v2cagenmob voiced_opinion bti_sc 
//v2xeg_eqprotec margin_immigrant margin_sexual bti_eo bti_seb 
//v2pepwrsoc v2pepwrses biz_corrupt gov_corrupt bti_poa bti_acp 
//v2x_freexp_altinf v2caassemb media_freedom bti_aar bti_foe
//v2xel_frefair v2xel_locelec v2xel_regelec v2xdd_dd election_confidence bti_ffe ibp_cat ibp_obi ibp_pub
//v2csprtcpt v2cscnsult voltime_org bti_ig bti_csp


keep if year==2023

summarize

corr v2dlengage v2cagenmob voiced_opinion bti_sc 
corr v2xeg_eqprotec margin_immigrant margin_sexual bti_eo bti_seb 
corr v2pepwrsoc v2pepwrses biz_corrupt gov_corrupt bti_poa bti_acp
corr v2x_freexp_altinf v2caassemb media_freedom bti_aar bti_foe
corr v2xel_frefair v2xel_locelec v2xel_regelec v2xdd_dd election_confidence bti_ffe ibp_cat ibp_obi ibp_pub
corr v2csprtcpt v2cscnsult voltime_org bti_ig bti_csp

corr voiced_opinion margin_immigrant margin_sexual biz_corrupt gov_corrupt media_freedom election_confidence voltime_org

corr voiced_opinion margin_immigrant margin_sexual biz_corrupt gov_corrupt media_freedom election_confidence voltime_org v2dlengage v2cagenmob v2xeg_eqprotec v2pepwrsoc v2pepwrses v2x_freexp_altinf v2caassemb v2xel_frefair v2xel_locelec v2xel_regelec v2xdd_dd v2csprtcpt v2cscnsult

corr v2xel_frefair v2xel_locelec bti_ffe ibp_cat ibp_obi ibp_pub  v2x_freexp_altinf v2caassemb bti_aar bti_foe v2pepwrsoc v2pepwrses bti_poa bti_acp v2dlengage v2xeg_eqprotec v2csprtcpt v2cscnsult bti_sc bti_eo bti_seb  bti_aar bti_foe bti_ig bti_csp


//////////////////////////////////////////////////////
