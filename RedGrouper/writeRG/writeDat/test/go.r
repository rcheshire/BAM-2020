##################################################################################
###   Sample R program for testing and demonstrating the "FishGraph" collection
###     of R graphics functions for analysis of stock-assessment results.
##################################################################################

##### Start fresh ###########
#rm(list=ls(all=TRUE))
graphics.off()
.SavedPlots <- NULL

library(FishGraph)

##### Read in the data from the ASCII .rdat file: #####
spp <- dget("rg11ch.rdat") 

##### Common arguments in FishGraph functions. Convenient to define them once.
ptype="pdf" #NULL (no quotes) for no plots saved; other options: "pdf", "wmf", "eps"
dtype=FALSE  #draft type
ctype=TRUE  #color type 
########## Open a graphics device ###########
windows(width = 8, height = 8, record = TRUE)
#pdf(file="RG11fits-web1.pdf", width = 8, height = 8)

#Round Effective sample sizes for more enjoyable plots
spp$t.series$lcomp.cH.neff<-round(spp$t.series$lcomp.cH.neff,1)
spp$t.series$lcomp.CVT.neff<-round(spp$t.series$lcomp.CVT.neff,1)

spp$t.series$acomp.cH.neff<-round(spp$t.series$acomp.cH.neff,1)
spp$t.series$acomp.CVT.neff<-round(spp$t.series$acomp.CVT.neff,1)

########## Call the functions #########################

Landings.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, 
               L.units=c("1000 lb","1000 lb","1000 fish","1000 fish"), D.units="1000 dead fish")

Index.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, resid.analysis=F)

windows(width = 8, height = 10, record = TRUE)
Comp.yearly.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, plot.neff=FALSE, print.neff=TRUE,
                  compact = TRUE, print.n=TRUE)
windows(width = 10, height = 8, record = TRUE)
Comp.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, c.min=0.2)

windows(width = 6, height = 4, record = TRUE, xpos = 10, ypos = 10)
Selectivity.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, plot.points=T, compact=TRUE)


#---------------------------------------------------------



Growth.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, plot.all = TRUE)

windows(width = 8, height = 6, record = TRUE)
PerRec.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, 
             user.PR = list("SPR", "ypr.lb.whole"), 
             F.references=list("Fmsy"))


#---------------------------------------------------------



Eq.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, 
         F.references=list("Fmsy"), user.Eq=list("L.eq.wholeklb", "L.eq.knum", "D.eq.knum"))    

StockRec.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, 
               draw.lowess = FALSE, start.drop = 0, units.rec="number age-1 fish")


#---------------------------------------------------------

CLD.total.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, first.year = "1976",
                units.CLD.w="1000 lb whole", CLD.w.references=list(NULL,"msy.klb",NULL),
                CLD.n.references=NULL, plot.proportion = TRUE)

NFZ.age.plots(spp,draft=dtype, use.color=ctype, graphics.type=ptype, 
              user.plots="N.age.mdyr", start.drop=0, plot.CLD=F)

BSR.time.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, legend.pos="top",
               BSR.references=list("Bmsy", "SSBmsy", "Rmsy"))
  
#---------------------------------------------------------


F.time.plots(spp, draft=dtype, use.color=ctype, graphics.type=ptype, 
            F.references=list(a="Fmsy"), F.additional=c("F.F30.ratio"))


#---------------------------------------------------------

Cohort.plots(spp,draft=dtype, graphics.type=ptype)
Parm.plots(spp, graphics.type=ptype)
Bound.vec.plots(spp, draft=dtype, graphics.type=ptype)


#---------------------------------------------------------

windows(width = 8, height = 8, record = TRUE)
Phase.plots(spp, start.drop=0, draft=dtype, use.color=ctype, graphics.type=ptype, Xaxis.F=F, year.pos=3,F.B.references=list("Fmsy","msst"))
#dev.off()

# windows(height=10, width=8, record=T)
# par(mfcol=c(5,2))
# theta.val=round(exp(spp$parm.cons$log_dm_cH_lc[8]),2)
# plot(spp$t.series$lcomp.cH.n[spp$t.series$lcomp.cH.n>0], spp$t.series$lcomp.cH.neff[spp$t.series$lcomp.cH.neff>0], 
#           xlab="N trips", ylab="Neff", main=substitute(paste("Len cH: ", theta,"=",theta.val), list(theta.val=theta.val)))
# theta.val=round(exp(spp$parm.cons$log_dm_cO_lc[8]),2)
# plot(spp$t.series$lcomp.cO.n[spp$t.series$lcomp.cO.n>0], spp$t.series$lcomp.cO.neff[spp$t.series$lcomp.cO.neff>0], 
#      xlab="N trips", ylab="Neff", main=substitute(paste("Len cO: ", theta,"=",theta.val), list(theta.val=theta.val)))
# theta.val=round(exp(spp$parm.cons$log_dm_HB_lc[8]),2)
# plot(spp$t.series$lcomp.HB.n[spp$t.series$lcomp.HB.n>0], spp$t.series$lcomp.HB.neff[spp$t.series$lcomp.HB.neff>0], 
#      xlab="N trips", ylab="Neff", main=substitute(paste("Len HB: ", theta,"=",theta.val), list(theta.val=theta.val)))
# theta.val=round(exp(spp$parm.cons$log_dm_GR_lc[8]),2)
# plot(spp$t.series$lcomp.GR.n[spp$t.series$lcomp.GR.n>0], spp$t.series$lcomp.GR.neff[spp$t.series$lcomp.GR.neff>0], 
#      xlab="N trips", ylab="Neff", main=substitute(paste("Len GR: ", theta,"=",theta.val), list(theta.val=theta.val)))
# theta.val=round(exp(spp$parm.cons$log_dm_HB_D_lc[8]),2)
# plot(spp$t.series$lcomp.HB.D.n[spp$t.series$lcomp.HB.D.n>0], spp$t.series$lcomp.HB.D.neff[spp$t.series$lcomp.HB.D.neff>0], 
#      xlab="N trips", ylab="Neff", main=substitute(paste("Len HB.D: ", theta,"=",theta.val), list(theta.val=theta.val)))
# theta.val=round(exp(spp$parm.cons$log_dm_CVT_lc[8]),2)
# plot(spp$t.series$lcomp.CVT.n[spp$t.series$lcomp.CVT.n>0], spp$t.series$lcomp.CVT.neff[spp$t.series$lcomp.CVT.neff>0], 
#      xlab="N trips", ylab="Neff", main=substitute(paste("Len CVT: ", theta,"=",theta.val), list(theta.val=theta.val)))
# 
# theta.val=round(exp(spp$parm.cons$log_dm_cH_ac[8]),2)
# plot(spp$t.series$acomp.cH.n[spp$t.series$acomp.cH.n>0], spp$t.series$acomp.cH.neff[spp$t.series$acomp.cH.neff>0], 
#      xlab="N trips", ylab="Neff", main=substitute(paste("Age cH: ", theta,"=",theta.val), list(theta.val=theta.val)))
# theta.val=round(exp(spp$parm.cons$log_dm_HB_ac[8]),2)
# plot(spp$t.series$acomp.HB.n[spp$t.series$acomp.HB.n>0], spp$t.series$acomp.HB.neff[spp$t.series$acomp.HB.neff>0], 
#      xlab="N trips", ylab="Neff", main=substitute(paste("Age HB: ", theta,"=",theta.val), list(theta.val=theta.val)))
# theta.val=round(exp(spp$parm.cons$log_dm_GR_ac[8]),2)
# plot(spp$t.series$acomp.GR.n[spp$t.series$acomp.GR.n>0], spp$t.series$acomp.GR.neff[spp$t.series$acomp.GR.neff>0], 
#      xlab="N trips", ylab="Neff", main=substitute(paste("Age GR: ", theta,"=",theta.val), list(theta.val=theta.val)))
# theta.val=round(exp(spp$parm.cons$log_dm_CVT_ac[8]),2)
# plot(spp$t.series$acomp.CVT.n[spp$t.series$acomp.CVT.n>0], spp$t.series$acomp.CVT.neff[spp$t.series$acomp.CVT.neff>0], 
#      xlab="N trips", ylab="Neff", main=substitute(paste("Age CVT: ", theta,"=",theta.val), list(theta.val=theta.val)))


####### object into an ASCII file (for spreadsheet users, etc.)
#rdat2ascii(gag)
#library(xlsReadWrite); 

F.pr=spp$pr.series$F.spr
SPR.pr=spp$pr.series$SPR*100  #times 100 for percent
SPR.Fmsy=SPR.pr[which.min(abs(F.pr-spp$parms$Fmsy))]

