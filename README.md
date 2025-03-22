
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cloutier

<!-- badges: start -->

![R-CMD-check](https://github.com/WB-PIDA-Data-Science-Shop/cloutier/blob/master/.github/workflows/Rcmdcheck_overkill.yml)
<!-- badges: end -->

The goal of cloutier is to curate data from the Gallup, VDEM and the VoG
towards creating a social contract factsheet flexdashboard. This is a
product of the Public Institutions Data and Analytics Unit within the
Institutions Department upon the request of Mathieu Cloutier both of the
World Bank.

## Installation

You can install the development version of cloutier from Github as
follows:

``` r

# install.packages("remotes")
remotes::install_github("WB-PIDA-Data-Science-Shop/cloutier")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo WB-PIDA-Data-Science-Shop/cloutier@HEAD
#> 
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\wb559885\AppData\Local\Temp\RtmpWuZcgF\remotes4c431df5e8b\WB-PIDA-Data-Science-Shop-cloutier-157d8bd/DESCRIPTION' ...  ✔  checking for file 'C:\Users\wb559885\AppData\Local\Temp\RtmpWuZcgF\remotes4c431df5e8b\WB-PIDA-Data-Science-Shop-cloutier-157d8bd/DESCRIPTION' (951ms)
#>       ─  preparing 'cloutier': (425ms)
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts (646ms)
#>   ─  checking for empty or unneeded directories
#> ─  building 'cloutier_0.0.0.9000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/wb559885/AppData/Local/R/win-library/4.4'
#> (as 'lib' is unspecified)
```

The package can be loaded as follows:

``` r
library(cloutier)
## basic example code
```

You may reproduce all published datasets within this package by simply
running this ReadMe.Rmd file within the project environment (i.e. just
click the .Rproj file and run this Readme.Rmd) or you can run the
following while in the project environment:

``` r

source(here("data-raw/prepare_data.R"))
#> ℹ Loading cloutier
#> ✔ Setting active project to "C:/Users/wb559885/OneDrive -
#>   WBG/Documents/GitProjects/cloutier".
#> 
#> ✔ Saving "cloutier_dt" to "data/cloutier_dt.rda".
#> 
#> ☐ Document your data (see <https://r-pkgs.org/data.html>).
#> 
#> ✔ Saving "regcomp_dt" to "data/regcomp_dt.rda".
#> 
#> ☐ Document your data (see <https://r-pkgs.org/data.html>).
#> 
#> ✔ Saving "globalcomp_dt" to "data/globalcomp_dt.rda".
#> 
#> ☐ Document your data (see <https://r-pkgs.org/data.html>).
```

### country level data

``` r

head(cloutier_dt[cloutier_dt$year >= 2005,])
#>     v2dlengage v2cagenmob voiced_opinion bti_sc v2xpe_exlpol v2xpe_exlecon
#> 217      1.133      1.160             NA      5        0.296         0.711
#> 218      1.133      1.784             NA     NA        0.304         0.711
#> 219      1.275      0.946             NA      5        0.304         0.711
#> 220      1.275      0.853      0.1293613     NA        0.304         0.711
#> 221      1.275      0.951      0.2161508      5        0.304         0.711
#> 222      1.275      0.775      0.1875124     NA        0.304         0.711
#>     v2xcl_rol v2x_egal v2xeg_eqprotec v2xeg_eqaccess v2xeg_eqdr v2clacjust
#> 217     0.608    0.418          0.368          0.554      0.318     -0.049
#> 218     0.596    0.415          0.360          0.554      0.318     -0.049
#> 219     0.596    0.411          0.347          0.561      0.318     -0.049
#> 220     0.596    0.411          0.347          0.561      0.318     -0.049
#> 221     0.596    0.415          0.344          0.561      0.318     -0.049
#> 222     0.594    0.431          0.349          0.597      0.318     -0.049
#>     v2clsocgrp v2clsnlpct margin_sexual margin_immigrant bti_eo bti_seb
#> 217      -0.57     26.964            NA               NA      5       6
#> 218      -0.57     27.357            NA               NA     NA      NA
#> 219      -0.57     31.500            NA               NA      5       6
#> 220      -0.57     31.500     0.4535458        0.5433257     NA      NA
#> 221      -0.57     32.143     0.3838985        0.5447784      5       6
#> 222      -0.57     34.833     0.4484899        0.4643047     NA      NA
#>     v2pepwrsoc v2pepwrses biz_corrupt gov_corrupt bti_poa bti_acp
#> 217      0.907      0.102          NA          NA       5       3
#> 218      0.907      0.102          NA          NA      NA      NA
#> 219      0.907      0.102          NA          NA       5       4
#> 220      0.907      0.102   0.6922601   0.7799783      NA      NA
#> 221      0.907      0.102   0.6788964   0.7518478       5       4
#> 222      1.005      0.102   0.5937780   0.6782700      NA      NA
#>     v2x_freexp_altinf v2caassemb media_freedom bti_aar bti_foe ciri_assn
#> 217             0.799      2.344            NA       8       7         2
#> 218             0.791      2.191            NA      NA      NA         2
#> 219             0.800      2.350            NA       8       7         1
#> 220             0.791      2.350            NA      NA      NA         0
#> 221             0.788      2.350            NA       8       7         1
#> 222             0.786      2.350     0.6066148      NA      NA         2
#>     v2xel_frefair v2xel_locelec v2xel_regelec v2xdd_dd election_confidence
#> 217         0.869         0.991          0.99        0                  NA
#> 218         0.788         0.991          0.99        0                  NA
#> 219         0.718         0.991          0.99        0                  NA
#> 220         0.730         0.991          0.99        0           0.2596521
#> 221         0.747         0.991          0.99        0           0.3494920
#> 222         0.760         0.991          0.99        0           0.2575572
#>     v2cltrnslw bti_ffe ibp_cat egov_epar v2csprtcpt v2cseeorgs v2cscnsult
#> 217      0.325      10      NA        NA      0.476      1.931      1.087
#> 218      0.325      NA       3        NA      0.476      1.931      1.087
#> 219      0.280       8      NA   0.75000      0.476      1.893      1.087
#> 220      0.280      NA       3        NA      0.476      1.893      1.087
#> 221      0.280       9      NA   0.37142      0.476      1.893      1.087
#> 222      0.280      NA       3        NA      0.476      2.004      1.087
#>     voltime_org bti_ig bti_csp wbcountryname wbcode                  wbregion
#> 217          NA      7       6        Mexico    MEX Latin America & Caribbean
#> 218          NA     NA      NA        Mexico    MEX Latin America & Caribbean
#> 219          NA      7       5        Mexico    MEX Latin America & Caribbean
#> 220   0.1020561     NA      NA        Mexico    MEX Latin America & Caribbean
#> 221   0.1965002      7       5        Mexico    MEX Latin America & Caribbean
#> 222   0.2473430     NA      NA        Mexico    MEX Latin America & Caribbean
#>           wbincomegroup wblendingcat year natgov_confidence social_capital
#> 217 Upper middle income         IBRD 2005                NA      0.1480232
#> 218 Upper middle income         IBRD 2006                NA             NA
#> 219 Upper middle income         IBRD 2007                NA      0.2096525
#> 220 Upper middle income         IBRD 2008         0.4135532             NA
#> 221 Upper middle income         IBRD 2009         0.4461378      0.2096525
#> 222 Upper middle income         IBRD 2010         0.3719863             NA
#>     absence_exclusion absence_capture vdeminformal btiinformal
#> 217        -0.4441240       0.6467219     1.463622   0.6663669
#> 218                NA       0.6467219     1.378007          NA
#> 219        -0.4880800       0.6467219     1.467344   0.6663669
#> 220                NA       0.6467219     1.462558          NA
#> 221        -0.4943594       0.6467219     1.460963   0.6663669
#> 222                NA       0.6842590     1.459899          NA
#>     informal_channels vdeminstitutional btiinstitutional institutional_channels
#> 217         0.9373409          1.655210               NA                     NA
#> 218                NA          1.533240               NA                     NA
#> 219         0.9393275          1.427834               NA                     NA
#> 220                NA          1.445903               NA                     NA
#> 221         0.9359220          1.471502               NA                     NA
#> 222                NA          1.491077               NA                     NA
#>       vdemcso    bticso intermediary_channels civil_capacity quality_interface
#> 217 0.8009216 0.7972124             0.5500133    -0.09394207                NA
#> 218 0.8009216        NA                    NA             NA                NA
#> 219 0.8009216 0.5461003             0.4008272    -0.08655233                NA
#> 220 0.8009216        NA                    NA             NA                NA
#> 221 0.8009216 0.5461003             0.4008272    -0.08917794                NA
#> 222 0.8009216        NA                    NA             NA                NA
#>     percept_civicspace resilience
#> 217                 NA         NA
#> 218                 NA         NA
#> 219                 NA         NA
#> 220                 NA         NA
#> 221                 NA         NA
#> 222          0.3505614 -0.4407322
```

### regional comparators dataset

``` r

head(regcomp_dt[regcomp_dt$year >= 2005,])
#>     year                  wbregion social_capital absence_exclusion
#>    <num>                    <char>          <num>             <num>
#> 1:  2005 Latin America & Caribbean      0.3829375        0.04688286
#> 2:  2006 Latin America & Caribbean            NaN               NaN
#> 3:  2007 Latin America & Caribbean      0.5051043        0.09917469
#> 4:  2008 Latin America & Caribbean            NaN               NaN
#> 5:  2009 Latin America & Caribbean      0.4974782        0.13103348
#> 6:  2010 Latin America & Caribbean            NaN               NaN
#>    absence_capture vdeminformal btiinformal informal_channels vdeminstitutional
#>              <num>        <num>       <num>             <num>             <num>
#> 1:       0.8051488     1.183698   0.6570528         0.7703776          1.207745
#> 2:       0.8266793     1.181848         NaN               NaN          1.212383
#> 3:       0.8502328     1.145751   0.7222514         0.7808610          1.203709
#> 4:       0.8634497     1.157918         NaN               NaN          1.202445
#> 5:       0.8754223     1.121120   0.6849951         0.7453288          1.230332
#> 6:       0.8688726     1.121439         NaN               NaN          1.227742
#>    btiinstitutional institutional_channels  vdemcso    bticso
#>               <num>                  <num>    <num>     <num>
#> 1:              NaN                    NaN 1.040851 0.3786922
#> 2:              NaN                    NaN 1.078787       NaN
#> 3:              NaN                    NaN 1.086205 0.3667345
#> 4:              NaN                    NaN 1.101323       NaN
#> 5:              NaN                    NaN 1.082763 0.5102271
#> 6:              NaN                    NaN 1.051548       NaN
#>    intermediary_channels civil_capacity quality_interface resilience v2cagenmob
#>                    <num>          <num>             <num>      <num>      <num>
#> 1:             0.4158278      0.2655050               NaN        NaN    0.16200
#> 2:                   NaN            NaN               NaN        NaN    0.15756
#> 3:             0.4380203      0.3608929               NaN        NaN    0.01700
#> 4:                   NaN            NaN               NaN        NaN   -0.02196
#> 5:             0.5222622      0.3835640               NaN        NaN    0.03472
#> 6:                   NaN            NaN               NaN 0.05172981    0.02924
#>    percept_civicspace natgov_confidence
#>                 <num>             <num>
#> 1:                NaN               NaN
#> 2:                NaN               NaN
#> 3:                NaN               NaN
#> 4:                NaN         0.3899329
#> 5:                NaN         0.4513060
#> 6:          0.4179324         0.4361259
```

### global comparators dataset

``` r

head(globalcomp_dt[globalcomp_dt$year >= 2005,])
#>     year social_capital absence_exclusion absence_capture vdeminformal
#>    <num>          <num>             <num>           <num>        <num>
#> 1:  2005    -0.03422225       -0.02844243       0.7510298    0.6359092
#> 2:  2006            NaN               NaN       0.7539252    0.6279927
#> 3:  2007     0.03449749       -0.04676672       0.7519885    0.6021643
#> 4:  2008            NaN               NaN       0.7553611    0.6132090
#> 5:  2009     0.05219205        0.02922588       0.7569917    0.6148730
#> 6:  2010            NaN               NaN       0.7619142    0.6237010
#>    btiinformal informal_channels vdeminstitutional btiinstitutional
#>          <num>             <num>             <num>            <num>
#> 1:   0.1889808        0.15075303         0.6586577              NaN
#> 2:         NaN               NaN         0.6724581              NaN
#> 3:   0.1906782        0.12674318         0.6566483              NaN
#> 4:         NaN               NaN         0.6622632              NaN
#> 5:   0.1088422        0.08046003         0.6718766              NaN
#> 6:         NaN               NaN         0.6732973              NaN
#>    institutional_channels   vdemcso       bticso intermediary_channels
#>                     <num>     <num>        <num>                 <num>
#> 1:                    NaN 0.8611985 -0.026950428          -0.007836750
#> 2:                    NaN 0.8669653          NaN                   NaN
#> 3:                    NaN 0.8663880 -0.020408636          -0.007892807
#> 4:                    NaN 0.8776755          NaN                   NaN
#> 5:                    NaN 0.8812928  0.002353588           0.009174860
#> 6:                    NaN 0.9091751          NaN                   NaN
#>    civil_capacity quality_interface resilience   v2cagenmob percept_civicspace
#>             <num>             <num>      <num>        <num>              <num>
#> 1:   -0.007969594               NaN        NaN -0.122744318                NaN
#> 2:            NaN               NaN        NaN -0.147017045                NaN
#> 3:    0.010814973               NaN        NaN -0.149028249                NaN
#> 4:            NaN               NaN        NaN -0.120977401                NaN
#> 5:    0.048893324               NaN        NaN -0.096468927                NaN
#> 6:            NaN               NaN  0.1062079  0.001129944          0.4354377
#>    natgov_confidence
#>                <num>
#> 1:               NaN
#> 2:               NaN
#> 3:               NaN
#> 4:         0.4853225
#> 5:         0.4939237
#> 6:         0.4924376
```
