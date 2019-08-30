rm(list=ls())
library(lubridate)
species="Red Grouper"
sedar="SEDAR 53"
run.name="rg11"
styr=1976                               #Starting year of model
endyr=2015                              #Ending year of model
styr_rec_dev=1976                       #Starting year to estimate recruitment deviation from S-R curve
endyr_rec_dev=2015                      #Ending year to estimate recruitment deviation from S-R curve
endyr_rec_phase=c(1977,2013)            #ending years of recruitment constraint phases
endyr_selex_phase=c(1983,1991)          #ending years of regulation periods
sizelim=c(304.8,508.0)                  #Size limits
start_age=1                             #used to generate number of and vector of ages
max_age=16                              #used to generate number of and vector of ages
plus_age=16                             #Maximum age used to match age comps
start_len=160                           #used to generate vector of length bins 'lenbins' and number of length bins 'nlenbins'
end_len=1180                            ##used to generate vector of length bins 'lenbins' and number of length bins 'nlenbins'
lenbins_width=30                        #width of length bins (mm)
max_F_spr_msy=1.0                       #Max F used in spr and msy calcs
n_iter_spr=10001                        #Total number of iterations for msy calcs
styr_rec_spr=1976                       #Starting year to compute arithmetic average recruitment for SPR-related values
endyr_rec_spr=2015                      #Ending year to compute arithmetic average recruitment for SPR-related values
nyrs_rec_spr=3                          #Number years at end of time series over which to average sector F's, for weighted selectivities
set_BiasCor=-1                             #bias correction (set to 1.0 for no bias correction or a negative value to compute from rec variance)
# .dat miscellaneous stuff section
wgtpar_a=0.000000008418                     #length-weight (TL-whole wgt) coefficients a , W=aL^b, (W in kg, TL in mm)--sexes combined
wgtpar_b=3.1                                #length-weight (TL-whole wgt) coefficients b , W=aL^b, (W in kg, TL in mm)--sexes combined
set_Dmort_cH=0.2                        #discard mortality for commercial handline
spawn_month=4                           #month for spawning
spawn_day=15                            #day of month for spawning
spawn_time_frac=yday(as.Date(paste("2015",spawn_month,spawn_day,sep="-")))/365  #2015 is just random non-leap year

#create r list of data elements stored as .csv files in .../data folder
#naming convetion for dat files saved as .csv
#first element is fleet acronym
#second element is dat type
#   land - landings (year, landings, cv)
#   disc - discards (year, discards, cv)
#   ac - age composition (year, n.fish, n.trip, bins)
#   lc - length composition (year, n.fish, n.trip, bins)
#   index - index (year)
#There are input files that are not fleet-specific and only have one part to filename
#   parms - parameters 
#     columns-parameter name, 
#     rows - i.guess	low.bound	up.bound	phase	prior.mean	prior.var.d.cv	prior.pdf
#   lh - age-specific life history information
#     columns - age, parameter acronym...e.g. maturity, proportion female, etc.
#     rows - age
#   age.error - ageing error matrix with no row or column labels



datin <- lapply(paste("data",list.files("data"),sep="/"), read.csv,header=T,check.names=F)
names(datin) <- gsub("\\.csv|./data/", "",list.files("data") )
#add model setup from header above
datin$setup=lapply(ls()[!ls()%in%c("datin")],get)
datin$setup=setNames(datin$setup,ls()[!ls()%in%c("datin")])
datin$setup$fleets=unique(sapply(strsplit(names(datin),"[.]"), `[`, 1))[!unique(sapply(strsplit(names(datin),"[.]"), `[`, 1))%in%c("parms","lh","setup","age")]

#save data object
save(datin,file="datIn.RData")
