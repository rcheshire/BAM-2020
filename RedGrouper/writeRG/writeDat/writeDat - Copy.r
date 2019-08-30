library(lubridate)

species="Red Grouper"
sedar="SEDAR 53"
run.name="rg11"
model.start.yr=1976                     #Starting year of model
model.end.yr=2015                       #Ending year of model
rec.start.yr=1976                       #Starting year to estimate recruitment deviation from S-R curve
rec.end.yr=2015                         #Ending year to estimate recruitment deviation from S-R curve
rec.dev.constraint.end.yrs=c(1977,2013) #ending years of recruitment constraint phases
reg.end.yrs=c(1983,1991)                #ending years of regulation period
size.lim=c(304.8,508.0)                 #Size limits
start.age=1
max.age=16
plus.age=16                             #Maximum age used to match age comps
start.len=160
end.len=1180
len.bin=30
max.f=1.0
n.iter=10001
avg.rec.start.yr=1976
avg.rec.end.yr=2015
avg.f.yrs=3
bias.cor=-1
# .dat miscellaneous stuff section
lw.a=0.000000008418                     #length-weight (TL-whole wgt) coefficients a , W=aL^b, (W in kg, TL in mm)--sexes combined
lw.b=3.1                                #length-weight (TL-whole wgt) coefficients b , W=aL^b, (W in kg, TL in mm)--sexes combined
disc.mort.ch=0.2                        #discard mortality for commercial handline
spawn.month=4                           #month for spawning
spawn.day=15                            #day of month for spawning
spawn.time.frac=yday(as.Date(paste("2015",spawn.month,spawn.day,sep="-")))/365  #2015 is just random non-leap year



#create r list of data elements
dat <- lapply(paste("data",list.files("data"),sep="/"), read.csv,header=T,check.names=F)
names(dat) <- gsub("\\.csv|./data/", "",list.files("data") )

fleets=unique(sapply(strsplit(names(dat),"[.]"), `[`, 1))[!unique(sapply(strsplit(names(dat),"[.]"), `[`, 1))%in%c("parms","lh")]


##############################################################################################################
# name of dat file to create
datfile.name=paste(run.name,"dat",sep=".")

# write out *.dat file
write(x=paste("# Data Input File"),file=datfile.name)
write(x=paste("#",sedar,"South Atlantic", species, sep=" "),file=datfile.name,append=T)
write(x="####################################################################",file=datfile.name,append=T)
write(x="# BAM: setup",file=datfile.name,append=T)
write(x="####################################################################",file=datfile.name,append=T)
write(x="#Starting and ending year of model",file=datfile.name,append=T)
write(x=model.start.yr,file=datfile.name,append=T)
write(x=model.end.yr,file=datfile.name,append=T)
write(x="#Starting year to estimate recruitment deviation from S-R curve",file=datfile.name,append=T)
write(x=rec.start.yr,file=datfile.name,append=T)
write(x="#Ending year to estimate recruitment deviation from S-R curve",file=datfile.name,append=T)
write(x=rec.end.yr,file=datfile.name,append=T)
write(x="#3 phases of constraints on recruitment deviations: 
#allows possible heavier constraint (weights defined later) in early and late period, with lighter constraint in the middle
#ending years of recruitment constraint phases",file=datfile.name,append=T)
write(x=rec.dev.constraint.end.yrs[1],file=datfile.name,append=T)
write(x=rec.dev.constraint.end.yrs[2],file=datfile.name,append=T)
write(x="#3 periods of size regs: yr1-83 no restrictions, 1984-91 12-inch TL, 1992-09 20-in TL
#ending years of regulation period",file=datfile.name,append=T)
write(x=reg.end.yrs[1],file=datfile.name,append=T)
write(x=reg.end.yrs[2],file=datfile.name,append=T)
write(x="#Size limits",file=datfile.name,append=T)
write(x=size.lim[1],file=datfile.name,append=T)
write(x=size.lim[2],file=datfile.name,append=T)
write(x="#Number of ages in population model(16 classes are 1,...,N+) //assumes last age is plus group",file=datfile.name,append=T)
write(x=max.age-start.age+1,file=datfile.name,append=T)
write(x="#Vector of agebins, last is a plus group",file=datfile.name,append=T)
write(x=start.age:max.age,file=datfile.name,append=T,ncol=max.age-start.age+1)
write(x="#Number of ages used to match age comps: first age must be same as popn, plus group may differ",file=datfile.name,append=T)
write(x=plus.age-start.age+1,file=datfile.name,append=T)
write(x="#Vector of agebins for fitting, last is a plus group",file=datfile.name,append=T)
write(x=start.age:plus.age,file=datfile.name,append=T,ncol=max.age-start.age+1)
write(x="#Number length bins used to match length comps and width of bins",file=datfile.name,append=T)
write(x=length(seq(start.len,end.len,by=len.bin)),file=datfile.name,append=T)
write(x=len.bin,file=datfile.name,append=T)
write(x="#Vector of length bins (mm)(midpoint of bin) used to match length comps and bins used to compute plus group",file=datfile.name,append=T)
write(x=seq(start.len,end.len,by=len.bin),file=datfile.name,append=T,ncol=length(seq(start.len,end.len,by=len.bin)))
write(x="#Max value of F used in spr and msy calculations",file=datfile.name,append=T)
write(x=max.f,file=datfile.name,append=T)
write(x="#Number of iterations in spr and msy calculations ",file=datfile.name,append=T)
write(x=n.iter,file=datfile.name,append=T)
write(x="#Starting year to compute arithmetic average recruitment for SPR-related values",file=datfile.name,append=T)
write(x=avg.rec.start.yr,file=datfile.name,append=T)
write(x="#Ending year to compute arithmetic average recruitment for SPR-related values",file=datfile.name,append=T)
write(x=avg.rec.end.yr,file=datfile.name,append=T)
write(x="#Number years at end of time series over which to average sector Fs, for weighted selectivities",file=datfile.name,append=T)
write(x=avg.f.yrs,file=datfile.name,append=T)
write(x="#Multiplicative bias correction of recruitment (may set to 1.0 for none or negative to compute from recruitment variance)",file=datfile.name,append=T)
write(x=bias.cor,file=datfile.name,append=T)
write(x="####################################################################",file=datfile.name,append=T)
write(x="# BAM: observed data",file=datfile.name,append=T)
write(x="####################################################################",file=datfile.name,append=T)
for(i in 1:length(fleets)){
  write(paste("##########",fleets[i],"############",sep=" "),file=datfile.name,append=T)
  write(x="####################################################################",file=datfile.name,append=T)
  if(paste(fleets[i],"index",sep=".")%in%names(dat)){
  write(x="#Starting and ending years of the index",file=datfile.name,append=T) 
  write(x=min(eval(parse(text=paste("dat$",fleets[i],".index$year",sep="")))),file=datfile.name,append=T)
  write(x=max(eval(parse(text=paste("dat$",fleets[i],".index$year",sep="")))),file=datfile.name,append=T)
  write(x="#Observed index and CVs",file=datfile.name,append=T)
  write(x=eval(parse(text=paste("dat$",fleets[i],".index$index",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".index$index",sep=""))))[1])
  write(x=eval(parse(text=paste("dat$",fleets[i],".index$cv",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".index$cv",sep=""))))[1])
  }
  if(paste(fleets[i],"land",sep=".")%in%names(dat)){
    write(x="#Starting and ending years for landings time series",file=datfile.name,append=T)  
    write(x=min(eval(parse(text=paste("dat$",fleets[i],".land$year",sep="")))),file=datfile.name,append=T) 
    write(x=max(eval(parse(text=paste("dat$",fleets[i],".land$year",sep="")))),file=datfile.name,append=T)
    write(x="#landings vector (1000 lb whole weight) and assumed CVs",file=datfile.name,append=T)
    write(x=eval(parse(text=paste("dat$",fleets[i],".land$landings",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".land$landings",sep=""))))[1])
    write(x=eval(parse(text=paste("dat$",fleets[i],".land$cv",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".land$cv",sep=""))))[1])
  }

  if(paste(fleets[i],"lc",sep=".")%in%names(dat)){
    write(x="#Number and vector of years of length compositions",file=datfile.name,append=T)
    write(x=length(eval(parse(text=paste("dat$",fleets[i],".lc$year",sep="")))),file=datfile.name,append=T) 
    write(x=eval(parse(text=paste("dat$",fleets[i],".lc$year",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".lc$year",sep=""))))) 
    write(x="#Sample size of length comp data (first row observed Ntrips, second row Nfish",file=datfile.name,append=T)
    write(x=eval(parse(text=paste("dat$",fleets[i],".lc$n.trip",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".lc$n.trip",sep=""))))) 
    write(x=eval(parse(text=paste("dat$",fleets[i],".lc$n.fish",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".lc$n.fish",sep=""))))) 
    write(x="#length comp",file=datfile.name,append=T)
    write.table(x=eval(parse(text=paste("dat$",fleets[i],".lc",sep="")))[,4:dim(eval(parse(text=paste("dat$",fleets[i],".lc",sep=""))))[2]],file=datfile.name,append=T,col.names=F,row.names=F)
  }
  
  if(paste(fleets[i],"ac",sep=".")%in%names(dat)){
    write(x="#Number and vector of years of age compositions",file=datfile.name,append=T)
    write(x=length(eval(parse(text=paste("dat$",fleets[i],".ac$year",sep="")))),file=datfile.name,append=T) 
    write(x=eval(parse(text=paste("dat$",fleets[i],".ac$year",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".ac$year",sep=""))))) 
    write(x="#Sample size of age comp data (first row observed Ntrips, second row Nfish",file=datfile.name,append=T)
    write(x=eval(parse(text=paste("dat$",fleets[i],".ac$n.trip",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".ac$n.trip",sep=""))))) 
    write(x=eval(parse(text=paste("dat$",fleets[i],".ac$n.fish",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".ac$n.fish",sep=""))))) 
    write(x="#age comp",file=datfile.name,append=T)
    write.table(x=eval(parse(text=paste("dat$",fleets[i],".ac",sep="")))[,4:dim(eval(parse(text=paste("dat$",fleets[i],".ac",sep=""))))[2]],file=datfile.name,append=T,col.names=F,row.names=F)
  }

  if(paste(fleets[i],"disc",sep=".")%in%names(dat)){
    write(x="#Starting and ending years for discards time series",file=datfile.name,append=T)  
    write(x=min(eval(parse(text=paste("dat$",fleets[i],".disc$year",sep="")))),file=datfile.name,append=T) 
    write(x=max(eval(parse(text=paste("dat$",fleets[i],".disc$year",sep="")))),file=datfile.name,append=T)
    write(x="#Observed discards (1000s) and assumed CVs",file=datfile.name,append=T)
    write(x=eval(parse(text=paste("dat$",fleets[i],".disc$discards",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".disc$discards",sep=""))))[1])
    write(x=eval(parse(text=paste("dat$",fleets[i],".disc$cv",sep=""))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".disc$cv",sep=""))))[1])
  }
}
write(x="###--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>-->" ,file=datfile.name,append=T)
write(x="###-- BAM DATA SECTION: parameter section" ,file=datfile.name,append=T)
write(x="###--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>-->" ,file=datfile.name,append=T)
write(x="###################Parameter values and initial guesses############################" ,file=datfile.name,append=T)
write(x="#####################################################" ,file=datfile.name,append=T)
write(x="###prior PDF (1=none, 2=lognormal, 3=normal, 4=beta)" ,file=datfile.name,append=T)
write(x="###############################################################" ,file=datfile.name,append=T)
write(x="##initial # lower # upper #       #  prior  # prior   # prior #" ,file=datfile.name,append=T)
write(x="## guess  # bound # bound # phase #  mean   # var/-CV #  PDF  #" ,file=datfile.name,append=T)
write(x="##--------#-------#-------#-------#---------#---------#-------#" ,file=datfile.name,append=T)
write(x="######Population                                               ###### Biological input ###########" ,file=datfile.name,append=T)
for(j in 2:dim(dat$parms)[2]){
#write(x=paste(dat$parms[j],"                 # ",names(dat$parms[j]),sep=""),file=datfile.name,append=T)
  x=c(t(unlist(dat$parms[j])),paste("#               ",names(dat$parms[j]),sep=""))
  x=x[is.na(x)==F]
  write(x,file=datfile.name,append=T,ncol=8)
}
write(x="############ F dev initial guesses #############",file=datfile.name,append=T)
for(i in 1:length(fleets)){
if(paste(fleets[i],"land",sep=".")%in%names(dat)){
  write(paste("##########",fleets[i],"landings ############",sep=" "),file=datfile.name,append=T)
  write(x=rep(0,length(eval(parse(text=paste("dat$",fleets[i],".land$year",sep=""))))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".land$year",sep="")))))
}
  if(paste(fleets[i],"disc",sep=".")%in%names(dat)){
    write(paste("##########",fleets[i]," discards############",sep=" "),file=datfile.name,append=T)
    write(x=rep(0,length(eval(parse(text=paste("dat$",fleets[i],".disc$year",sep=""))))),file=datfile.name,append=T,ncol=length(eval(parse(text=paste("dat$",fleets[i],".disc$year",sep="")))))
  }
}
write(x="########## rec devs ##########",file=datfile.name,append=T)
write(x=rep(0,rec.end.yr-rec.start.yr+1),file=datfile.name,append=T,ncol=rec.end.yr-rec.start.yr+1)
write(x="##########  initial N age devs, all ages but the first one ",file=datfile.name,append=T)
write(x=rep(0,max.age-start.age),file=datfile.name,append=T,ncol=max.age-start.age)
write(x="####################################################################",file=datfile.name,append=T)
write(x="#### -- BAM DATA SECTION: likelihood weights section  ######",file=datfile.name,append=T)
write(x="####################################################################",file=datfile.name,append=T)
write(x="###############  Likelihood Component Weighting  ###################",file=datfile.name,append=T)
write(x="1.0                #landings",file=datfile.name,append=T)
write(x="1.0                #discards",file=datfile.name,append=T)
for(i in 1:length(fleets)){
  if(paste(fleets[i],"index",sep=".")%in%names(dat)){ 
    write(x=paste("1.0          #",fleets[i],"index",sep= " "),file=datfile.name,append=T)
  } 
}
for(i in 1:length(fleets)){
  if(paste(fleets[i],"lc",sep=".")%in%names(dat)){ 
    write(x=paste("1.0          #",fleets[i],"length composition",sep= " "),file=datfile.name,append=T)
  } 
}
for(i in 1:length(fleets)){
  if(paste(fleets[i],"ac",sep=".")%in%names(dat)){ 
    write(x=paste("1.0          #",fleets[i],"age composition",sep= " "),file=datfile.name,append=T)
  } 
}
write(x="1.0	    #log N.age.dev residuals (initial abundance)",file=datfile.name,append=T)
write(x="1.0	    #S-R residuals",file=datfile.name,append=T)
write(x="0.0	    #constraint on early recruitment deviations",file=datfile.name,append=T)
write(x="0.0	    #constraint on ending recruitment deviations",file=datfile.name,append=T)
write(x="0.0     #penalty if F exceeds 3.0 (reduced by factor of 10 each phase, not applied in final phase of optimization) fULL F summed over fisheries",file=datfile.name,append=T)
write(x="0.0	    #weight on tuning F (penalty not applied in final phase of optimization)",file=datfile.name,append=T)
write(x="####################################################################",file=datfile.name,append=T)
write(x="##-- BAM DATA SECTION: miscellaneous stuff section",file=datfile.name,append=T)
write(x="####################################################################",file=datfile.name,append=T)
write(x="#length-weight (TL-whole wgt) coefficients a and b, W=aL^b, (W in kg, TL in mm)--sexes combined",file=datfile.name,append=T)
write(x=lw.a,file=datfile.name,append=T)
write(x=lw.b,file=datfile.name,append=T)
write(x="# vector of maturity-at-age for females (ages 1-16+)",file=datfile.name,append=T)
write(x=dat$lh$f.mat,file=datfile.name,append=T,ncol=length(dat$lh$f.mat))
write(x="# vector of maturity-at-age for males",file=datfile.name,append=T)
write(x=dat$lh$m.mat,file=datfile.name,append=T,ncol=length(dat$lh$m.mat))
write(x="#Proportion female by age",file=datfile.name,append=T)
write(x=dat$lh$ppt.f,file=datfile.name,append=T,ncol=length(dat$lh$ppt.f))
write(x="# time of year (as fraction) for spawning:",file=datfile.name,append=T)
write(x=spawn.time.frac,file=datfile.name,append=T)
write(x="#age-dependent natural mortality at age",file=datfile.name,append=T)
write(x=dat$lh$m,file=datfile.name,append=T,ncol=length(dat$lh$m))
write(x="# Discard mortality constants ",file=datfile.name,append=T)
write(x=paste(disc.mort.ch,"                  #ch"),file=datfile.name,append=T)
write(x="##Spawner-recruit parameters",file=datfile.name,append=T)
write(x="## SR function switch (integer 1=Beverton-Holt, 2=Ricker)",file=datfile.name,append=T)
write(x="1",file=datfile.name,append=T)
write(x="##switch for rate increase in q: Integer value (choose estimation phase, negative value turns it off)",file=datfile.name,append=T)
write(x="-1",file=datfile.name,append=T)
write(x="##annual positive rate of increase on all fishery dependent q's due to technology creep",file=datfile.name,append=T)
write(x="0.0",file=datfile.name,append=T)
write(x="## DD q switch: Integer value (choose estimation phase, negative value turns it off)",file=datfile.name,append=T)
write(x="-1",file=datfile.name,append=T)
write(x="##density dependent catchability exponent, value of zero is density independent, est range is (0.1,0.9)",file=datfile.name,append=T) 
write(x="0.0",file=datfile.name,append=T)
write(x="##SE of density dependent catchability exponent (0.128 provides 95% CI in range 0.5)",file=datfile.name,append=T)
write(x="0.128",file=datfile.name,append=T)
write(x="##Age to begin counting D-D q (should be age near full exploitation)",file=datfile.name,append=T)
write(x="4.0 ",file=datfile.name,append=T)
write(x="##Variance (sd^2) of fishery dependent random walk catchabilities (0.03 is near the sd=0.17 of Wilberg and Bence)",file=datfile.name,append=T)
write(x="0.03",file=datfile.name,append=T)
write(x="##Tuning F (not applied in last phase of optimization, or not applied at all if penalty weight=0)",file=datfile.name,append=T) 
write(x="0.35",file=datfile.name,append=T)
write(x="##Year for tuning F",file=datfile.name,append=T)
write(x="2008",file=datfile.name,append=T)
write(x="##threshold sample sizes ntrips (>=)for length comps (set to 99999.0 if sel is fixed): ",file=datfile.name,append=T)
write(x="1.0    #cH len comps",file=datfile.name,append=T)
write(x="1.0    #CVT len comps",file=datfile.name,append=T)
write(x="##threshold sample sizes ntrips (>=) for age comps (set to 99999.0 if sel is fixed)",file=datfile.name,append=T)
write(x="1.0    #cH age comps",file=datfile.name,append=T)
write(x="1.0    #CVT age comps",file=datfile.name,append=T)

write(x="## Projection input",file=datfile.name,append=T)
write(x="2025 #Projection end year, must be later than assessment endyr",file=datfile.name,append=T)
write(x="2017 #New management start year, must be later than assessment endyr",file=datfile.name,append=T)
write(x="3    #Switching indicating value to use for defining projection F: 1=Fcurrent, 2=Fmsy, 3=F30, 4=F40",file=datfile.name,append=T)
write(x="1.0  #Multiplier c applied to compute projection F, for example Fproj=cFmsy",file=datfile.name,append=T)
write(x="#Ageing error matrix (columns are true age 1-16+, rows are ages as read for age comps: columns should sum to one)",file=datfile.name,append=T)
write.table(x=dat$age.error[,2:dim(dat$age.error)[2]],file=datfile.name,append=T,col.names=F,row.names=F)
write(x="999 #end of data file flag",file=datfile.name,append=T)
