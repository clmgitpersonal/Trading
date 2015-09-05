
library(restimizeapi)
SetKey("Bbe7883")
companies <- GetCompanies ()
msftCompany <- GetCompany ("MSFT")
msftCompanyEstimates <- GetCompanyEstimates ("MSFT")
msftCompanyReleases <- GetCompanyReleases ("MSFT")
estimates <- GetEstimates ("2015-01-20", "2015-02-15")
releaseConsensus <- GetReleaseConsensus ("535c963053c804e0d50002a1")


devtools::install_github("bwlewis/networkD3")