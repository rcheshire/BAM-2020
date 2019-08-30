#=================================================================================
# Optimal yield analyses and plots for RG assessment   
# KWS   December 2004
# Updated January 2017
#---------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
graphics.off()

##### Read in the data from the ASCII .rdat file: #####
spp <- dget("rg11.rdat")

yr=spp$t.series$year		
#Fmsy=spp$parms$Fmsy

#----compute F(30%SPR), F(40%SPR), F(50%SPR), F(%SPR at msy) and corresponding yields  ---------------

F.pr=spp$pr.series$F.spr
SPR.pr=spp$pr.series$SPR*100  #times 100 for percent
L.pr=spp$pr.series$ypr.lb.whole
Fmax.mult=as.numeric(F.pr[L.pr==max(L.pr)])
Fmax=median(Fmax.mult) #Take median, bc of possible multiple solutions if ypr has flat top
Fmsy=spp$parms$Fmsy
F20=F.pr[which.min(abs(SPR.pr-20.))]
F30=F.pr[which.min(abs(SPR.pr-30.))]
F40=F.pr[which.min(abs(SPR.pr-40.))]
F50=F.pr[which.min(abs(SPR.pr-50.))]

SPR.Fmsy=SPR.pr[which.min(abs(F.pr-Fmsy))]

F.eq=spp$eq.series$F.eq
L.eq=spp$eq.series$L.eq.wholeklb
D.eq=spp$eq.series$D.eq.knum
SSB.eq=spp$eq.series$SSB.eq
B.eq=spp$eq.series$B.eq

L.F20= L.eq[which.min(abs(F.eq-F20))]
L.F30= L.eq[which.min(abs(F.eq-F30))]
L.F40= L.eq[which.min(abs(F.eq-F40))]
L.F50= L.eq[which.min(abs(F.eq-F50))]
L.Fmax= L.eq[which.min(abs(F.eq-Fmax))]

# Fmsy65=0.65*Fmsy
# Fmsy75=0.75*Fmsy
# Fmsy85=0.85*Fmsy
# L.Fmsy65= L.eq[which.min(abs(F.eq-Fmsy65))]
# L.Fmsy75= L.eq[which.min(abs(F.eq-Fmsy75))]
# L.Fmsy85= L.eq[which.min(abs(F.eq-Fmsy85))]
# L.Fmsy= L.eq[which.min(abs(F.eq-Fmsy))]

#----plot of equilibrium yield ---------------
windows(width=8, height=10, record=TRUE)
par(mar=c(4.5, 5, 1, 1),mfrow=c(2,1),las=0, cex=1.1)

maxFplot=0.8
plot(F.eq[F.eq<=maxFplot],L.eq[F.eq<=maxFplot], type="l", lwd=2,
     xlab='Fishing mortality rate', ylab='Eq. landings (1000 lb whole)')
grid(lty=1, col="lightgray")
lines(F.eq[F.eq<=maxFplot],L.eq[F.eq<=maxFplot], lwd=2)
	#abline(v=median(F30),lwd=2,lty=4)
	#abline(v=median(F40),lwd=2,lty=2)
	#abline(v=median(Fmsy),lwd=2,lty=1)
	#leg=c('F35','F45','Fmsy')
	#legend(0.8,0.85*max(L.eq),legend=leg,lty=c(4,2,1),lwd=rep(2,3),bg="white")


plot(F.eq[F.eq<=maxFplot],SSB.eq[F.eq<=maxFplot], type="l", lwd=2,
     ylim=c(0,max(SSB.eq)),xlab='Fishing mortality rate',
     ylab='Eq. SSB (mt)')
grid(lty=1, col="lightgray")
lines(F.eq[F.eq<=maxFplot],SSB.eq[F.eq<=maxFplot], lwd=2)

	#abline(v=median(F30),lwd=2,lty=4)
	#abline(v=median(F40),lwd=2,lty=2)
	#abline(v=median(Fmsy),lwd=2,lty=1)
	#leg=c('F35','F45','Fmsy')
	#legend(0.8,0.85*max(SSB.eq)/1000,legend=leg,lty=c(4,2,1),lwd=rep(2,3),bg="white")

#savePlot(filename="spp-figs/spp-eq-L-SSB.pdf", type="pdf")


windows(width=8, height=10, record=TRUE)
par(mar=c(4.5, 4.5, 1, 1),mfrow=c(2,1),las=1, cex=1.1)
plot(B.eq/1000,L.eq, type="l", lwd=2,
     xlab='Eq. biomass (1000 mt)', ylab='Eq. landings (1000 lb whole)')
grid(lty=1, col="lightgray")
lines(B.eq/1000,L.eq)

plot(B.eq/1000,D.eq, type="l", lwd=2,
     xlab='Eq. biomass (1000 mt)', ylab='Eq. discards (1000 dead fish)')
grid(lty=1, col="lightgray")
lines(B.eq/1000,D.eq)

#savePlot(filename="spp-figs/spp-eq-BvLD.pdf", type="pdf")



######Equilibrium age structure #############################################
# fraction of year for mortality for SSB calcs
sp.frac=spp$parms$spawn.time

#model estimated SR stuff (not necessary if external.fit set to TRUE above)
steep=spp$parms$BH.steep
R0=spp$parms$BH.R0
autocorr=spp$parms$R.autocorr
Fmsy=spp$parms$Fmsy
SSBmsy=spp$parms$SSBmsy #in mt
msy.klb=spp$parms$msy.klb #in klb whole weight
Dmsy=spp$parms$Dmsy.knum   #in 1000 fish
Bmsy=spp$parms$Bmsy   #in mt
R.eq=spp$parms$Rmsy   #number fish
biascorr=spp$parms$BH.biascorr

spr.F0=spp$parms$BH.Phi0            # spr at F=0 
M=spp$a.series$M                    # vector of M-at-age
wgt.mt=spp$a.series$wgt.mt             # vector of weight at age in mt
wgt.klb=spp$a.series$wgt.klb             # vector of weight at age in klb
wgt.L.klb=spp$a.series$wholewgt.wgted.L.klb   #fishery-weighted mean weight of landings -- gutted
wgt.D.klb=spp$a.series$wholewgt.wgted.D.klb   #fishery-weighted mean weight of discards -- gutted
reprod=spp$a.series$reprod          # vector of reproductive output at age
#reprod=spp$reprod["2014",]          # vector of reproductive output at age

sel.L=spp$sel.age$sel.v.wgted.L    # selectivity at age for landed fish
sel.D=spp$sel.age$sel.v.wgted.D    # selectivity at age for discarded fish

ages=spp$a.series$age
nages=length(ages)
F.L=sel.L*Fmsy
F.D=sel.D*Fmsy
Z=F.L+F.D+M
N.pr=rep(1.0,nages)
N.pr.sp=N.pr
N.pr.sp=N.pr[1]*exp(-Z[1]*sp.frac)
for(a in 2:nages)
{
  N.pr[a]=N.pr[a-1]*exp(-Z[a-1])
  N.pr.sp[a]=N.pr[a]*exp(-Z[a]*sp.frac)
}
N.pr[nages]=N.pr[nages]/(1-exp(-Z[nages]))
N.pr.sp[nages]=N.pr.sp[nages]/(1-exp(-Z[nages]))

spr.Fmsy=sum(N.pr.sp*reprod)
  
R.eq=(R0/((5.0*steep-1.0)*spr.Fmsy))*(biascorr*4.0*steep*spr.Fmsy-spr.F0*(1.0-steep))

N.eq.knum=N.pr*R.eq/1000.
N.sp.knum=N.pr.sp*R.eq

mat.f=spp$a.series$mat.female
#prop.f=spp$a.series$prop.female
prop.f=spp$a.series$prop.female
N.pr.mat.f=sum(N.pr*mat.f*prop.f)
N.pr.mat.m=sum(N.pr*(1-prop.f))
prop.male.msy=N.pr.mat.m/(N.pr.mat.f+N.pr.mat.m)

windows(width=8, height=6, record=TRUE)
par(mar=c(4.5, 4.5, 1, 1),las=1, cex=1.1)
#palette(rainbow(seq(0,.7,len=7)))
yrs.sub=c("1976", "1985","1995","2005","2015")
N.age.sub=spp$N.age[yrs.sub,]
plot(ages,log(N.eq.knum*1000), type="o", lwd=3, pch=20,xlab="age", ylab="Natural log of abundance", 
     ylim=c(0.*min(log(N.eq.knum*1000)),1+max(log(N.age.sub))), col=1)
     
for (i in 1:length(yrs.sub)){
  lines(ages,log(N.age.sub[i,]),lty=i+1,col=i+1,lwd=2)
}
points(ages,log(N.age.sub[i,]),lty=i+1,col=i+1,lwd=2)
legend("bottomleft", legend=c("Fmsy equilibrium",yrs.sub), lty=1:7, lwd=c(3,rep(2,length(yrs.sub))), pch=c(20,rep(-1,length(yrs.sub)-1),1), col=1:(length(yrs.sub)+1))

savePlot(filename="spp-figs/spp.eq.agestructure.pdf", type="pdf")
