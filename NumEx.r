#Spectrum example figures for three models presented: spectrum leasing, interference permit, and generalized interference permit.
#Final figures for IEEE TWC upload Feb 2018


#Need EPS for IEEE TWC: keep Quartz device open, before calling postscript. Else the size, placement etc are funny. ALso
#use Helvetica for LaTeX to show up properly.


#L's paras
BandWidth = 1 #W 1
McEta = .5 	#Cost per unit rate in primary market is .5*eta*r^2, mc = eta*r
chanNoise = .25 #0.25 #noise .25   Noisier channels should have lower mc?

#PU's and SU's parameters
ValPU = 2.5 # scale for util of rate 1  %2.5
ValSU = 2.5 #2.5						#2.5
sensPU = .5 # KPU energy sensitivity para PU .5
sensSU =  .5 #KSU  .5
maxPowPU = 1 #limit PU 1 (pbar)
maxPowSU = 1 #limit SU 1 (pbar_s)
CGhU = 1 #channel gain from PU to own receiver 1 (huu in paper)
CGgS = 1 # (hss in paper) gain from SU to own receiver 
CGhS = .2 #(hsu in paper) cross-user gain from SU's transmitter to PU's receiver  .15 and .3 for rate profit comps (fig4-6), .2 for basic eqm comparison Mon vs comp (Fig 3)
CGgU = .2 # (hus in paper) cross user gain from PU to Su's receiver .15 and .3



##############################################################
#1A: Baseline (with just a single equilibrium or a few finite ones for comparison) or to feed into the sharing scenarios

#Max rate (PU's demand when price=0)
rMax = BandWidth*log(( ((CGhU*maxPowPU)/(BandWidth*chanNoise)) +1)/(((sensPU)/(BandWidth*ValPU))  +1))

#Create a grid for the optimization
rUgrid = seq(0,rMax,rMax/800) 
elem = length(rUgrid)

#Call the baseline function to return inverse demand, marginal revenue, and marginal cost at each rate
baseVals<- baseline(W = BandWidth, eta = McEta, r=rUgrid,VPU=ValPU, KPU=sensPU, pBarPU=maxPowPU, hPU = CGhU,No = chanNoise)
demPU = baseVals[1:elem] #PU's inverse demand
mrPU = baseVals[(elem+1):(2*elem)] #marg rev
mcPU = baseVals[(2*elem+1):(3*elem)] #marg cost


MonEqm = max(which(mrPU>mcPU)) #Monopolistic
CompEqm = max(which(demPU>mcPU)) #Competitive
rMon = rUgrid[MonEqm] #Mon rate
rComp = rUgrid[CompEqm] #Comp rate
MonPrice = demPU[MonEqm] #Mon price
CompPrice = demPU[CompEqm] #Comp price


#plot to illustrate revenue and cost for L
plot(rUgrid, rUgrid*demPU, type="l", lwd=1.5,lty=1,col=2, xlab = "PU's rate (r)", ylab="Revenue and cost", xlim=c(0,1.5), ylim=c(0,2.1))
lines(rUgrid, 0.5*McEta*(rUgrid^2), type="l", lwd=1.5,lty=2,col=3)
 
legend(-.15,2.2,c(expression(paste("Revenue (", mr, " in [7])")), expression(paste("Cost (", psi, " in [7])"))), lty=c(1,2), col=c(2,3), bty="n")
xRev = c(rMon, rMon)
yRev = c(rUgrid[MonEqm]*demPU[MonEqm], 0.5*McEta*(rUgrid[MonEqm]^2))
lines(xRev,yRev,type="l", lty=2,lwd=1.5, col=1)

markers=seq(2,length(rUgrid),length=5)
# # Plot to illustrate basic demand, MR, MC and equilibrium. Just one or a few equilibria can be shown here. 


#while plot is open on quartz 
postscript("fig1.eps", width = 2*4.0, height = 2*3.0,
           horizontal = FALSE, onefile = FALSE, paper = "special",
           family = "Helvetica", encoding = "TeXtext.enc")

plot(rUgrid, demPU, type="l", lty=1,col=1, lwd=1.5, xlab="PU's rate (r)", ylab="Inv. Demand, Marg. Rev. and Marg. Cost", xlim=c(0,1.5), ylim=c(0,2.5))
lines(rUgrid, mrPU, type="l", lty=2,col=2, lwd=1.5)
points(rUgrid[markers],mrPU[markers], type="p", col=2,pch=1)
lines(rUgrid, mcPU, type="l", lty=3,col=3, lwd=1.5)
points(rUgrid[markers],mcPU[markers], type="p", col=3,pch=2)
x<-c(rMon, rComp)
y<-c(MonPrice, CompPrice)
points(x,y,type="p", pch=19, lwd=2)
lines(c(rMon,rMon), c(mrPU[MonEqm],MonPrice),type="l",lty=2,col="black")
points(rMon,mrPU[MonEqm],type="p", pch=4, lwd=4)
text(x[1], y[1]+.2,"Monopolistic")
text(x[2]-.05, y[2]-.2,"Competitive")
legend(-.025,1.6,c("Inverse Demand (LHS in (12))", "Marginal Revenue (LHS in (13))", expression(paste("Marginal Cost ", psi,"' (RHS in (12), (13))"))),col=c(1,2,3), lty=c(1,2,3), lwd=c(1.5,1.5,1.5), pch=c(NA_integer_, 1,2), bty="n")
grid()

dev.off() #not for quartz, use for postscript only

#free variables
rm(x,y,xRev, yRev, rUgrid, demPU, mrPU, mcPU)

##########################################################################################################################


#1B: Baseline (How equa changes as some parameter changes)

#The parameter to change
para = seq(.5,1.5,.05) #sensePU = seq(.8,1.4,.1), chanNoise = seq(.2,.3,.025),  McEta = seq(.15,.5,.0125)

#Max rate (PU's demand when price=0)
rMax = BandWidth*log(( ((CGhU*maxPowPU)/(BandWidth*chanNoise)) +1)/(((sensPU)/(BandWidth*ValPU))  +1))

#Create a grid for the optimization
rUgrid = seq(0,rMax,rMax/800)  #baseline 200 okay for monopoly, not for competive "price" 
elem = length(rUgrid)

#blank vectors for plots
rPuMon=rep(0,times=length(para))
rPuComp = rPuMon
mPuMon = rPuMon
mPuComp = rPuMon

for(i in 1:length(para)){
	#Call the baseline function to return inverse demand, marginal revenue, and marginal cost at each rate
	baseVals<- baseline(W = BandWidth, eta = McEta, r=rUgrid,VPU=ValPU, KPU=para[i], pBarPU=maxPowPU, hPU = CGhU,No =chanNoise )
	demPU = baseVals[1:elem] #PU's inverse demand
	mrPU = baseVals[(elem+1):(2*elem)] #marg rev
	mcPU = baseVals[(2*elem+1):(3*elem)] #marg cost	
	MonEqm = max(which(mrPU>mcPU)) #Monopolistic
	CompEqm = max(which(demPU>mcPU)) #Competitive
	rPuMon[i] = rUgrid[MonEqm] #Mon rate
	rPuComp[i] = rUgrid[CompEqm] #Comp rate
	mPuMon[i] = demPU[MonEqm] #Mon price
	mPuComp[i] = demPU[CompEqm] #Comp price
}

#The following vectors are just the comp and Mon equa with baseline paras. 
baseRComp = rPuComp  
baseMComp = mPuComp
baseRMon = rPuMon 
baseMMon = mPuMon 
#These will have to be modified (below) for each para variation as named below, before calling the plots (and modifying the names as needed)
#do not run the above with modified para vals, only the ones below

#will replace LHS turn by turn startign with mc25
c25RMon = rPuMon#mc25RMon, no15RMon, pB1p5RMon, V3RMon  
mc25MMon = mPuMon #mc25MMon, no15MMon, pB1p5MMon, V3MMon

markers = seq(2,length(para),4)


#while plot is open on quartz 
postscript("fig2.eps", width = 2*4.0, height = 2*3.0,
           horizontal = FALSE, onefile = FALSE, paper = "special",
           family = "Helvetica", encoding = "TeXtext.enc")


par(mfrow=c(1,2))

#plot for many para combos
plot(para, baseRComp,type="l", col=2,lty=2, lwd=1.5, xlab="PU's power sensitivity (k)", ylab= "Baseline rate (r*)", ylim=c(min(rPuMon),1.6))
lines(para,baseRMon ,type="l", col=1,lty=1, lwd=1.5)
lines(para,mc25RMon ,type="l", col=3,lty=3, lwd=1.5) #eta reduced from .5 to .25
points(para[markers],mc25RMon[markers] ,type="p", pch=1, col=3)
lines(para,no15RMon ,type="l", col=4,lty=4, lwd=1.5) #reduce noise from .25 to .15
points(para[markers],no15RMon[markers] ,type="p", pch=2, col=4)
lines(para,pB1p5RMon ,type="l", col=5,lty=5, lwd=1.5) #maxPow inc from 1 to 1.5
points(para[markers],pB1p5RMon[markers] ,type="p", pch=3, col=5)
lines(para,V3RMon ,type="l", col=6,lty=6, lwd=1.5) #V inc from 2.5 to 3
points(para[markers],V3RMon[markers] ,type="p", pch=4, col=6)
legend(min(para)-.06,1.65,c("Monopolistic","Competitive", expression(paste("Low marg. cost (",eta, "=.25)")), expression(paste("Low ch. noise (",N[0], "=.15)"))
, expression(paste("High pow. lim. (",bar(p), "=1.5)"))
, expression(paste("High rate pref. (",V, "=3)"))
), lty=c(1,2, 3, 4, 5, 6), col=c(1,2,3,4,5,6),lwd=c(1.5,1.5,1.5,1.5,1.5,1.5), pch=c(NA_integer_,NA_integer_,1,2,3,4), bty="n")


plot(para, baseMComp,type="l", col=2,lty=2, lwd=1.5, xlab="PU's power sensitivity (k)", ylab="Baseline price (m(r*))", ylim=c(.95*min(mPuComp),2.3))
lines(para,baseMMon ,type="l", col=1,lty=1, lwd=1.5)
lines(para,mc25MMon ,type="l", col=3,lty=3, lwd=1.5) #eta reduced from .5 to .25
points(para[markers],mc25MMon[markers] ,type="p", pch=1, col=3)
lines(para,no15MMon ,type="l", col=4,lty=4, lwd=1.5) #reduce noise from .25 to .15
points(para[markers],no15MMon[markers] ,type="p", pch=2, col=4)
lines(para,pB1p5MMon ,type="l", col=5,lty=5, lwd=1.5) #maxPow inc from 1 to 1.5
points(para[markers],pB1p5MMon[markers] ,type="p", pch=3, col=5)
lines(para,V3MMon ,type="l", col=6,lty=6, lwd=1.5) #V inc from 2.5 to 3
points(para[markers],V3MMon[markers] ,type="p", pch=4, col=6)
legend(min(para)-.06,1.55,c("Monopolistic","Competitive", expression(paste("Low marg. cost (",eta, "=.25)")), expression(paste("Low ch. noise (",N[0], "=.15)"))
, expression(paste("High pow. lim. (",bar(p), "=1.5)"))
, expression(paste("High rate pref. (",V, "=3)"))
), lty=c(1,2, 3, 4, 5, 6), col=c(1,2,3,4,5,6),lwd=c(1.5,1.5,1.5,1.5,1.5,1.5),pch=c(NA_integer_,NA_integer_,1,2,3,4), bty="n")


par(mfrow=c(1,1))

dev.off() #for postscript only

#free variables not needed any longer
rm(mc25RMon, no15RMon, pB1p5RMon, V3RMon,mc25MMon, no15MMon, pB1p5MMon, V3MMon, baseRCom, rPuComp,  baseMComp, mPuComp, baseRMon, rPuMon, baseMMon, mPuMon )


##########################################################################################################################
#1C How MC of leasing responds to changes in primary market paras

para =.1 #seq(.1,.3,.05)

#blank vectors for plots
rPuMon=rep(0,times=length(para))
omega = seq(0,.3,length=6)
mcOrtho = rPuMon
plot(omega, omega, type="l", col="white", ylim=c(0.2,1), xlab=expression(paste("Bandwidth leased (", omega, ")")), ylab="Marg. Cost")

for(i in 1:length(para)){
	
	#Max rate (PU's demand when price=0)
	rMax = BandWidth*log(( ((CGhU*maxPowPU)/(BandWidth*chanNoise)) +1)/(((sensPU)/(BandWidth*ValPU))  +1))

	#Create a grid for the optimization
	rUgrid = seq(0,rMax,rMax/800)  #baseline 200 okay for monopoly, not for competive "price" 
	elem = length(rUgrid)

	#Call the baseline function to return inverse demand, marginal revenue, and marginal cost at each rate
	baseVals<- baseline(W = BandWidth, eta = McEta, r=rUgrid,VPU=ValPU, KPU=sensPU, pBarPU=maxPowPU, hPU = CGhU,No =para[i])
	demPU = baseVals[1:elem] #PU's inverse demand
	mrPU = baseVals[(elem+1):(2*elem)] #marg rev
	mcPU = baseVals[(2*elem+1):(3*elem)] #marg cost	
	MonEqm = max(which(mrPU>mcPU)) #Monopolistic
	rPuMon[i] = rUgrid[MonEqm] #Mon rate
	baseCostOrtho = orthoMC(BW = BandWidth, VPU=ValPU,r=rPuMon[i], Omega=omega, KPU=sensPU,pBarPU=maxPowPU,hPU=CGhU, No = para[i])
	mcOrtho = baseCostOrtho[1:length(omega)]
	lines(omega, mcOrtho, col=i, lwd=1.5, lty=i)
	points(omega, mcOrtho, pch=i, col=i, lwd=1.5, lty=i)
	}
legend(0,1,c(expression(paste(N[0], "=", .1)),
expression(paste(N[0], "=", .15)),
expression(paste(N[0], "=", .2)),
expression(paste(N[0], "=", .25)),
expression(paste(N[0], "=", .3))), pch=c(1,2,3,4,5), lwd=c(1.5,1.5,1.5,1.5,1.5), col=c(1,2,3,4,5), lty=c(1,2,3,4,5), bty="n")

plot(para, rPuMon, type="l", col="blue", lty=1, lwd=1.5, xlab= expression(paste("Channel Noise (", N[0], ")")) , ylab="PU's rate (r*)")

######################################## 

#2: Figures for spectrum leasing model (orthogonal sharing).
#create a grid for bandwidth levels (to lease out to secondary user)
omega = seq(0.01,BandWidth,BandWidth/800) #grid from 0 to W

#Feed in PU baseline rate from above (e.g. monopoly, competitive, or combo) for r
rStar = rMon #.5*(rMon+rComp)
# mStar = MonPrice
# profStar= mStar*rStar-(.5*McEta*rStar^2)

#Obtain marginal cost for to maintain baseline
baseCostOrtho = orthoMC(BW = BandWidth, VPU=ValPU,r=rStar, Omega=omega, KPU=sensPU,pBarPU=maxPowPU,hPU=CGhU, No = chanNoise)

#WARNING: mc will be negative, and totCost NaN if PU power exceeds limit ####################
mcOrtho = baseCostOrtho[1:length(omega)]
totCostOrtho = baseCostOrtho[(length(omega)+1):(2*length(omega))]

#which SU paremter to change (fig4-6). Comment  out for fig3
para = seq(.5,3,.01) #WARNING: SU para only. otherwise need to redo baseline.

#uncomment for fig 3.
#para=.5

rL = rep(0,times=length(para))
leaseAmtMon = rL #eqm level of leasing (omega*) monopoly
leaseAmtComp = rL #competitive
leasePriceMon = rL #eqm price of leasing (pi*), monopoly
leasePriceComp = rL #competitive
rSuMonOrtho = rL #SU's rate monopoly
rSuCompOrtho = rL #rate comp
mSuMonOrtho = rL #effective price of rate for SU
mSuCompOrtho = rL
profMonOrtho = rL
profCompOrtho = rL

for (i in 1:length(para)){
	#call the function "share.r" which does the calculations for bandwidth leasing (orthogonal sharing)
	val = share(BW = BandWidth, VSU =para[i],Omega=omega,KSU= sensSU, pBarSU = maxPowSU ,gSU=CGgS, No = chanNoise)
	pi = val[1:length(omega)] #inverse demand
	mr = val[seq(length(omega)+1, 2*length(omega), 1)] #marginal revenue
	rSU = val[seq(2*length(omega)+1, 3*length(omega), 1)] # SU rate
	
    eqmMon = min(which(mr-mcOrtho<0))-1
    if(mcOrtho[eqmMon]<0){print("MC negative! (Monop lease)")}
    eqmComp = min(which(pi-mcOrtho<0))-1
    if(mcOrtho[eqmComp]<0){print("MC negative! (Comp lease)")}    
    leaseAmtMon[i] = omega[eqmMon]
	leaseAmtComp[i] = omega[eqmComp]
	leasePriceMon[i] = pi[eqmMon]
	leasePriceComp[i] = pi[eqmComp]
	rSuMonOrtho[i] = rSU[eqmMon]/rStar
	rSuCompOrtho[i] = rSU[eqmComp]/rStar
	mSuMonOrtho[i] = (pi[eqmMon]*omega[eqmMon])/(rSU[eqmMon]) #effective price of rate for SU, ratio
	mSuCompOrtho[i] = (pi[eqmComp]*omega[eqmComp])/(rSU[eqmComp])
	profMonOrtho[i] = (pi[eqmMon]*omega[eqmMon]) - totCostOrtho[eqmMon]
	profCompOrtho[i] = (pi[eqmComp]*omega[eqmComp]) - totCostOrtho[eqmComp]     
	}


#while plot is open on quartz 
postscript("fig3.eps", width = 2*4.0, height = 2*3.0,
           horizontal = FALSE, onefile = FALSE, paper = "special",
           family = "Helvetica", encoding = "TeXtext.enc")
par(mfrow=c(1,2))

#Plot for comparing monop and comp outcomes
piLease = pi
mrLease = mr
avCostOmega = totCostOrtho/omega

#Plot to show how monop and comp diverge based on mc or separation between demand and mr.
plot(omega[1:520], mcOrtho[1:520],type="l", lty=1, lwd = 1.5,col=1, xlab=expression(paste("Bandwidth leased (", omega, ")")), ylab="Inv. Demand, Marg. Rev, Marg. Cost, Avg. Cost", xlim=c(0,.5), ylim=c(0,6.5))
lines(omega[1:520],piLease[1:520], type="l", lty=2,lwd = 1.5,col=2)
lines(omega[1:520],mrLease[1:520], type="l", lty=4,lwd = 1.5,col=4)
points(omega[seq(1,520,length=5)],mrLease[seq(1,520,length=5)], type="p", pch=1,lwd = 1.5,col=4)
lines(omega[1:520],avCostOmega[1:520], type="l", lty=3,lwd = 1.5,col=3)
points(omega[seq(1,520,length=5)],avCostOmega[seq(1,520,length=5)], type="p", pch=2,lwd = 1.5,col=3)

points(omega[eqmMon],pi[eqmMon],type="p", pch=19, lwd=2)
text(omega[eqmMon],pi[eqmMon]+.25,"Mon.")
points(omega[eqmComp],pi[eqmComp],type="p", pch=19, lwd=2)
text(omega[eqmComp],pi[eqmComp]-.25,"Comp.")
legend(omega[1]-.01, 6.5, c("Inv. Dem. (20)", "Marg. Rev. (24)", "Marg. Cost (25)", "Avg. Cost (26)"), lty=c(2,4,1,3),lwd=c(1.5,1.5,1.5,1.5), col=c(2,4,1,3), pch = c(NA_integer_, 1, NA_integer_,2), bty="n")

lines(c(omega[eqmMon],omega[eqmMon]), c(avCostOmega[eqmMon],pi[eqmMon]),type="l",lty=2,col="black")
points(omega[eqmMon],mrLease[eqmMon],type="p", pch=4, lwd=4)
grid()

#free memory
rm(leaseAmtMon, leaseAmtComp, leasePriceMon,leasePriceComp,rSuMonOrtho,
rSuCompOrtho, mSuMonOrtho, mSuCompOrtho, profMonOrtho,  profCompOrtho, piLease, mrLease,avCostOmega, totCostOrtho, rSU)


#Plots of rates and profit are main.
plot(para,rSuCompOrtho, type="l", lwd=1.5, lty=1, col=1, xlab="KSU", ylab="Rate SU (Ortho)", ylim=c(min(rSuCompOrtho,rSuMonOrtho), max(rSuCompOrtho,rSuMonOrtho)))
lines(para,rSuMonOrtho, type="l", lwd=1.5, lty=2, col=2)

plot(para,profCompOrtho, type="l", lwd=1.5, lty=1, col=1, xlab="KSU", ylab="Profit (Ortho)", ylim=c(min(profCompOrtho,profMonOrtho), max(profCompOrtho,profMonOrtho)))
lines(para,profMonOrtho, type="l", lwd=1.5, lty=2, col=2)

#Plots of how much is leased/shared or effective price are secondary.
plot(para,mSuCompOrtho, type="l", lwd=1.5, lty=1, col=1, xlab="KSU", ylab="SU's Eff Price Ratio")
lines(para,mSuMonOrtho, type="l", lwd=1.5, lty=2, col=2)

plot(para,leaseAmtComp, type="l", lwd=1.5, lty=1, col=1, xlab="KSU", ylab="Eqm Leasing", ylim=c(0.2,.4))
lines(para,leaseAmtMon, type="l", lwd=1.5, lty=2, col=2)


##########################################################################################################################
#2a: Plots for interference pricing approach (no mitigation)
#create a grid for interference levels (to admit from secondary user)
I <- seq(0,.2,.2/800) #could be limited by powSU*hS

#Feed in PU baseline rate from above (either monopoly or competitive) for r
rStar = rMon #.5*(rMon+rComp)
# mStar = MonPrice
# profStar= mStar*rStar-(.5*McEta*rStar^2)
gammaStar = exp(rStar/BandWidth)-1

#the marginal and total costs of maintaining r* with iterference
baseCostNonOrtho = NonorthoMC(BW = BandWidth, VPU=ValPU,r=rStar, Int=I, KPU=sensPU,pBarPU=maxPowPU,hPU=CGhU, No = chanNoise)
mcNonOrtho = baseCostNonOrtho[1:length(I)]
totCostNonOrtho = baseCostNonOrtho[(length(I)+1):(2*length(I))]

#which SU paremter to change (fig4-6). Comment  out for fig3
para = seq(.5,3,.01) #WARNING: SU para only. otherwise need to redo baseline.

#Uncomment for fig3
#para=.5

rL = rep(0,times=length(para))
IntAmtMon = rL #eqm level of leasing (omega*) monopoly
IntAmtComp = rL #competitive
IntPriceMon = rL #eqm price of leasing (pi*), monopoly
IntPriceComp = rL #competitive
rSuMonNonOrtho = rL #SU's rate monopoly
rSuCompNonOrtho = rL #rate comp
mSuMonNonOrtho = rL #effective price of rate for SU
mSuCompNonOrtho = rL
profMonNonOrtho = rL
profCompNonOrtho = rL

for (i in 1:length(para)){
	#call the function "share.r" which does the calculations for bandwidth leasing (orthogonal sharing)
	val = permit(BW = BandWidth, VSU = para[i], gamma = gammaStar,Int=I, KSU=sensSU, pBarSU = maxPowSU, hPU = CGhU, gSU=CGgS,hSU = CGhS, gPU =CGgU, No = chanNoise)
	pi = val[1:length(I)] #inverse demand
	mr = val[seq(length(I)+1, 2*length(I), 1)] #marginal revenue
	rSU = val[seq(2*length(I)+1, 3*length(I), 1)] # SU rate
	
    eqmMon = min(which(mr-mcNonOrtho<0))-1
    if(mcNonOrtho[eqmMon]<0){print("MC negative! (Monop lease)")}
    eqmComp = min(which(pi-mcNonOrtho<0))-1
    if(mcNonOrtho[eqmComp]<0){print("MC negative! (Comp lease)")}    
    IntAmtMon[i] = I[eqmMon]
	IntAmtComp[i] = I[eqmComp]
	IntPriceMon[i] = pi[eqmMon]
	IntPriceComp[i] = pi[eqmComp]
	rSuMonNonOrtho[i] = rSU[eqmMon]/rStar
	rSuCompNonOrtho[i] = rSU[eqmComp]/rStar
	mSuMonNonOrtho[i] = (pi[eqmMon]*I[eqmMon])/(rSU[eqmMon]) #effective price of rate for SU, ratio
	mSuCompNonOrtho[i] = (pi[eqmComp]*I[eqmComp])/(rSU[eqmComp])
	profMonNonOrtho[i] = (pi[eqmMon]*I[eqmMon]) - totCostNonOrtho[eqmMon]
	profCompNonOrtho[i] = (pi[eqmComp]*I[eqmComp]) - totCostNonOrtho[eqmComp]     
	}



#For plotting fig 4-6. Run the variables below with high gains (=.3). And redo with low gains, with .15, where the following variables are no longer calculated.
rateHighGainComp = rSuCompNonOrtho 
rateHighGainMon = rSuMonNonOrtho
profHighGainComp = profCompNonOrtho
profHighGainMon = profMonNonOrtho
mHighGainComp = mSuCompNonOrtho
mHighGainMon = mSuMonNonOrtho

piInt = pi
mrInt = mr
avCostInt = totCostNonOrtho/I

#Plot to show how monop and comp diverge based on mc or separation between demand and mr.
plot(I, mcNonOrtho,type="l", lty=1, lwd = 1.5,col=1, xlab="Interference admitted (I)", ylab="Inv. Demand, Marg. Rev, Marg. Cost, Avg. Cost", ylim=c(1,20))
lines(I,piInt, type="l", lty=2,lwd = 1.5,col=2)
lines(I,mrInt, type="l", lty=4,lwd = 1.5,col=4)
points(I[seq(10,750,length=15)],mrInt[seq(10,750,length=15)], type="p", pch=1,lwd = 1.5,col=4)
legend(0.04, 21, c("Inv. Dem. (31)", "Marg. Rev. (34)", "Marg. Cost (35)", "Avg. Cost (36)"), lty=c(2,4,1,3),lwd=c(1.5,1.5,1.5,1.5),pch=c(NA_integer_, 1, NA_integer_,2), col=c(2,4,1,3), bty="n")
points(I[eqmMon],pi[eqmMon],type="p", pch=19, lwd=2)
text(I[eqmMon]+.03,pi[eqmMon],"Mon.")
points(I[eqmComp],pi[eqmComp],type="p", pch=19, lwd=2)
text(I[eqmComp],pi[eqmComp]+.4,"Comp.")
grid()
lines(I,avCostInt, type="l", lty=3,lwd = 1.5,col=3)
points(I[seq(10,750,length=5)],avCostInt[seq(10,750,length=5)], type="p", pch=2,lwd = 1.5,col=3)
lines(c(I[eqmMon],I[eqmMon]), c(avCostInt[eqmMon],piInt[eqmMon]),type="l",lty=2,col="black")
points(I[eqmMon],mrInt[eqmMon],type="p", pch=4, lwd=4)


dev.off() #turn off postscript

#free variables
rm(rL,IntAmtMon,IntAmtComp,IntPriceMon,IntPriceComp, rSuMonNonOrtho,rSuCompNonOrtho, mSuMonNonOrtho,
mSuCompNonOrtho, profMonNonOrtho,profCompNonOrtho, piInt, mrInt, mr, avCostInt,totCostNonOrtho )

#par(mfrow=c(2,2))
#Plots of rates and profit are main.
# plot(para,rSuCompNonOrtho, type="l", lwd=1.5, lty=1, col=1, xlab="KSU", ylab="Rate SU (Non Ortho)", ylim=c(min(rSuCompNonOrtho,rSuMonNonOrtho), max(rSuCompNonOrtho,rSuMonNonOrtho)))
# lines(para,rSuMonNonOrtho, type="l", lwd=1.5, lty=2, col=2)

# plot(para,profCompNonOrtho, type="l", lwd=1.5, lty=1, col=1, xlab="KSU", ylab="Profit (Non Ortho)", ylim=c(min(profCompNonOrtho,profMonNonOrtho), max(profCompNonOrtho,profMonNonOrtho)))
# lines(para,profMonNonOrtho, type="l", lwd=1.5, lty=2, col=2)

# #Plots of how much is leased/shared or effective price are secondary.
# plot(para,mSuCompNonOrtho, type="l", lwd=1.5, lty=1, col=1, xlab="KSU", ylab="SU's Eff Price Ratio")
# lines(para,mSuMonNonOrtho, type="l", lwd=1.5, lty=2, col=2)

# plot(para,IntAmtComp, type="l", lwd=1.5, lty=1, col=1, xlab="KSU", ylab="Eqm Int tol")
# lines(para,IntAmtMon, type="l", lwd=1.5, lty=2, col=2)



###################COMBO PLOTS ###########################
#Plots combining SU rate and L's profit for Mono/Comp/Ortho/NonOrtho
#while plot is open on quartz (for Fig4-6)
postscript("fig6.eps", width = 2*4.0, height = 2*3.0,
           horizontal = FALSE, onefile = FALSE, paper = "special",
           family = "Helvetica", encoding = "TeXtext.enc")
par(mfrow=c(1,2))

par(mfrow=c(1,2))

markers = seq(1,length(para),30)

plot(para,rSuCompOrtho, type="l", lwd=1.5, lty=1, col=1, xlab=expression(paste("SU's rate sensitivity  (",V[s], ")" )), ylab="SU's rate", ylim=c(min(rateHighGainMon,rSuMonNonOrtho), 1.4), xlim=c(0,max(para)))  #max(rSuCompOrtho,rSuCompNonOrtho)))
lines(para,rSuMonOrtho, type="l", lwd=1.5, lty=2, col=2)
lines(para,rSuCompNonOrtho, type="l", lwd=1.5, lty=3, col=3)
lines(para,rSuMonNonOrtho, type="l", lwd=1.5, lty=4, col=4)
lines(para,rateHighGainComp, type="l", lwd=1.5, lty=5, col=5)
lines(para,rateHighGainMon, type="l", lwd=1.5, lty=6, col=6)
points(para[markers],rSuCompNonOrtho[markers], type="p",pch=1, lwd=1.5, lty=3, col=3)
points(para[markers],rSuMonNonOrtho[markers], type="p",pch=2, lwd=1.5, lty=4, col=4)
points(para[markers],rateHighGainComp[markers], type="p", pch=3, lwd=1.5, lty=5, col=5)
points(para[markers],rateHighGainMon[markers], type="p", pch=4, lwd=1.5, lty=6, col=6)
legend(-.05,1.44,c("Ortho. Comp.", "Ortho. Mon.",  "N-O. Comp. (fav.)", "N-O. Mon. (fav.)", "N-O. Comp. (unfav.)", "N-O. Mon. (unfav.)"), lty=c(1,2,3,4, 5, 6), col=c(1,2,3,4, 5, 6), lwd=c(1.5,1.5,1.5,1.5, 1.5, 1.5), pch=c(NA_integer_,NA_integer_,1,2,3,4), bty="n")

plot(para,profCompOrtho, type="l", lwd=1.5, lty=1, col=1, xlab=expression(paste("SU's rate sensitivity (",V[s], ")" )), ylab="L's Profit", ylim=c(min(profHighGainComp,profCompNonOrtho), 1), xlim=c(0,max(para)))   #max(profMonOrtho,profMonNonOrtho)))
lines(para,profMonOrtho, type="l", lwd=1.5, lty=2, col=2)
lines(para,profCompNonOrtho, type="l", lwd=1.5, lty=3, col=3)
lines(para,profMonNonOrtho, type="l", lwd=1.5, lty=4, col=4)
lines(para,profHighGainComp, type="l", lwd=1.5, lty=5, col=5)
lines(para,profHighGainMon, type="l", lwd=1.5, lty=6, col=6)
points(para[markers],profCompNonOrtho[markers], type="p",pch=1, lwd=1.5, lty=3, col=3)
points(para[markers],profMonNonOrtho[markers], type="p",pch=2, lwd=1.5, lty=4, col=4)
points(para[markers],profHighGainComp[markers], type="p", pch=3, lwd=1.5, lty=5, col=5)
points(para[markers],profHighGainMon[markers], type="p", pch=4, lwd=1.5, lty=6, col=6)
legend(.05,1.05,c("Ortho. Comp.", "Ortho. Mon.",  "N-O. Comp. (fav.)", "N-O. Mon. (fav.)", "N-O. Comp. (unfav.)", "N-O. Mon. (unfav.)"), lty=c(1,2,3,4, 5, 6), col=c(1,2,3,4, 5, 6), lwd=c(1.5,1.5,1.5,1.5, 1.5, 1.5),pch=c(NA_integer_,NA_integer_,1,2,3,4), bty="n")

dev.off() #for PS

#not plotted in paper
plot(para,mSuCompOrtho, type="l", lwd=1.5, lty=1, col=1, xlab=expression(paste("SU's energy sensitivity (",k[s], ")" )), ylab="SU's effective price (per unit rate)", ylim=c(min(mSuCompOrtho,mSuCompNonOrtho), 2))
lines(para,mSuMonOrtho, type="l", lwd=1.5, lty=2, col=2)
lines(para,mSuCompNonOrtho, type="l", lwd=1.5, lty=3, col=3)
lines(para,mSuMonNonOrtho, type="l", lwd=1.5, lty=4, col=4)
lines(para,mHighGainComp, type="l", lwd=1.5, lty=5, col=5)
lines(para,mHighGainMon, type="l", lwd=1.5, lty=6, col=6)
points(para[markers],mSuCompNonOrtho[markers], type="p",pch=1, lwd=1.5, lty=3, col=3)
points(para[markers],mSuMonNonOrtho[markers], type="p",pch=2, lwd=1.5, lty=4, col=4)
points(para[markers],mHighGainComp[markers], type="p", pch=3, lwd=1.5, lty=5, col=5)
points(para[markers],mHighGainMon[markers], type="p", pch=4, lwd=1.5, lty=6, col=6)
legend(.51,2.05,c("Ortho. Comp.", "Ortho. Mon.",  "N-O. Comp. (fav.)", "N-O. Mon. (fav.)", "N-O. Comp. (non-fav.)", "N-O. Mon. (non-fav.)"), lty=c(1,2,3,4, 5, 6), col=c(1,2,3,4, 5, 6), lwd=c(1.5,1.5,1.5,1.5, 1.5, 1.5),pch=c(NA_integer_,NA_integer_,1,2,3,4), bty="n")


# paraV = para
# markersV = markers
# mSuCompOrthoV = mSuCompOrtho
# mSuMonOrthoV = mSuMonOrtho
# mSuCompNonOrthoV = mSuCompNonOrtho
# mSuMonNonOrthoV =mSuMonNonOrtho
# mHighGainCompV = mHighGainComp
# mHighGainMonV = mHighGainMon

# plot(paraV,mSuCompOrthoV, type="l", lwd=1.5, lty=1, col=1, xlab=expression(paste("SU's rate sensitivity (",V[s], ")" )), ylab="SU's effective price (per unit rate)", ylim=c(min(mSuCompOrthoV,mSuCompNonOrthoV), 1.8))
# lines(paraV,mSuMonOrthoV, type="l", lwd=1.5, lty=2, col=2)
# lines(paraV,mSuCompNonOrthoV, type="l", lwd=1.5, lty=3, col=3)
# lines(paraV,mSuMonNonOrthoV, type="l", lwd=1.5, lty=4, col=4)
# lines(paraV,mHighGainCompV, type="l", lwd=1.5, lty=5, col=5)
# lines(paraV,mHighGainMonV, type="l", lwd=1.5, lty=6, col=6)
# points(paraV[markersV],mSuCompNonOrthoV[markersV], type="p",pch=1, lwd=1.5, lty=3, col=3)
# points(paraV[markersV],mSuMonNonOrthoV[markersV], type="p",pch=2, lwd=1.5, lty=4, col=4)
# points(paraV[markersV],mHighGainCompV[markersV], type="p", pch=3, lwd=1.5, lty=5, col=5)
# points(paraV[markersV],mHighGainMonV[markersV], type="p", pch=4, lwd=1.5, lty=6, col=6)
# legend(.95,1.85,c("Ortho. Comp.", "Ortho. Mon.",  "N-O. Comp. (fav.)", "N-O. Mon. (fav.)", "N-O. Comp. (non-fav.)", "N-O. Mon. (non-fav.)"), lty=c(1,2,3,4, 5, 6), col=c(1,2,3,4, 5, 6), lwd=c(1.5,1.5,1.5,1.5,1.5, 1.5),pch=c(NA_integer_,NA_integer_,1,2,3,4), bty="n")


####################################
#par(mfrow=c(2,2)) 
#m: Primary market profitability (m), PU's energy sensitity (k), xlab=expression(paste("SU's energy sensitity (",k[S], ")")), xlab=expression(paste("Channel noise (",N[0], ")")), xlab=expression(paste("SU's power limit (",bar(p)[s], ")"))

##############################################
#Generalized interference tolerance (Hybrid approach) where owner reserves fraction alpha of Bandwidth for PU's exclusive use. So that part is orthogonal (for PU). The# #remaining bandwidth is non-orthogonal sharing between primary and secondary user.
#create a grid for interference levels (to admit from secondary user)
I <- seq(0,.9,.2/200) #could be limited by powSU*hS

#Feed in PU baseline rate from above (either monopoly or competitive) for r
rStar = rMon
gammaStar = exp(rStar/BandWidth)-1
para = c(0.01,.1,.2,.3) #seq(0,.7,.01)  #various alphas

x<-rep(0,times=length(para))
rateGIComp = x
rateGIMon = x
profComp = x
profMon = x

#PLOTS: for pX and PU as functions of interference. See lines and points (markers) and Legend below (relevant ones marked by #plot@pxPU)

#plot(I,I,type="l", col="white", ylim=c(0,.8),xlab= expression(paste("Interference admitted ", "(",I, ")")), ylab= expression(paste(p[X](I))  ) ) #plot@pxPU
#plot(I,I,type="l", col="white", ylim=c(0,.8),xlab= expression(paste("Interference admitted ", "(",I,")")), ylab= expression(paste(p[R](I)   )  ) ) #plot@pxPU


#while plot is open on quartz 
postscript("fig7.eps", width = 2*4.0, height = 2*3.0,
           horizontal = FALSE, onefile = FALSE, paper = "special",
           family = "Helvetica", encoding = "TeXtext.enc")



par(mfrow=c(1,2))


#change the channel gains as per favorabile unfavorable conditions
CGhS = .5; CGgU =.5;

#plot(I, I, type="l", col="white", xlab="Interference admitted (I)", ylab="L's Revenue (favorable gains)", ylim=c(0,1), xlim=c(.01,.16))
plot(I, I, type="l", col="white", xlab="Interference admitted (I)", ylab="L's Revenue (unfavorable gains)", ylim=c(.4,.5), xlim=c(.08, .15))


for (i in 1:length(para)){

	val = optPermit(alp = para[i], BW = BandWidth,VSU = ValSU, gamma =gammaStar,Int=I, KPU=sensPU, KSU=sensSU,pBarPU=maxPowPU, pBarSU=maxPowSU, hPU = CGhU, gSU=CGgS,hSU = CGhS, gPU =CGgU, No = chanNoise)
	#	value = c(piSU, rSU, mc, cost, pXI, pRI, cond)
	pi = val[1:length(I)] #inverse demand
	rSU = val[seq(length(I)+1, 2*length(I), 1)] #
	mc = val[seq(2*length(I)+1, 3*length(I), 1)] # marginal cost #WARNING:negative! When power exceeds PU Max		
	cost = val[seq(3*length(I)+1, 4*length(I), 1)]
	pX = val[seq(4*length(I)+1, 5*length(I), 1)]
	pR = val[seq(5*length(I)+1, 6*length(I), 1)]	
	cond = val[seq(6*length(I)+1, 7*length(I), 1)]
	
	IntLimitCond = max(which(cond>1))  
	
	
	#lines(I[1:IntLimitCond],pX[1:IntLimitCond], lty=i,lwd=1.5, col=i) #plot@pxPU 
	#lines(I[1:IntLimitCond],pR[1:IntLimitCond], lty=i,lwd=1.5, col=i) #plot@pxPU
	#markers = seq(1,IntLimitCond,40) #plot@pxPU
	#points(I[markers],pX[markers], pch=i,col=i) #plot@pxPU
	#points(I[markers],pR[markers], pch=i,col=i) #plot@pxPU

	#numerically calculate MR and MC (see warning about NaNs)
	Rev = pi*I
	mrTopDiff = Rev[2:length(I)]-Rev[1:(length(I)-1)]
	mrBotDiff = I[2:length(I)]-I[1:(length(I)-1)]
	mr = mrTopDiff/mrBotDiff
	#WARNING: this numerically derived marginal cost below will be NaN when the cost itself is NaN (when power exeeds PUmax)
	mc2Top =  cost[2:length(I)]-cost[1:(length(I)-1)]
	mc2 = mc2Top/mrBotDiff
	#lines for plotting revenue for various alpha and channel gains
	lines(I[2:180],Rev[2:180], type="l", col=i, lty=i, lwd=1.5)
	points(I[seq(2,180,30)],Rev[seq(2,180,30)], type="p", col=i, pch=i, lwd=1.5)
	
			
	eqm2 <- min(which(mc2>mr))-1 #MC being NaN should not be a problem as long as it becomes NaN after equilibrium
	
	if(cond[eqm2]<1){
		print("Cond less than 1 at monopolistic eqm")
		print(i)
		print("use alpha = ")
		print(para[i-1])
		print("breaking out ...")
		break
		}
	
	if(pR[eqm2]>pR[eqm2+1]){
		rateGIMon[i]=0
		profMon[i]=0
		print("Error: pR is decreasing for i =")
		print(i)
		print("use alpha = ")
		print(para[i-1])
		print("breaking out ...")
		break
	}else{
		if(mc2[eqm2]<0){
			print("Error: MC negative at eqm")
			}
		rateGIMon[i]<-rSU[eqm2]
		profMon[i]<-Rev[eqm2]-cost[eqm2]
		
		eqmComp <-min(which(mc>pi))-1
		if(cond[eqmComp]<1){
		print("Cond less than 1 at competitive eqm")
		print(i)
		print("use alpha = ")
		print(para[i-1])
		print("breaking out ...")
		break	
		}
		rateGIComp[i]<-rSU[eqmComp]
		profComp[i]<-Rev[eqmComp]-cost[eqmComp]

	}

}



###########revenue (inner problem for various alpha and good/bad cross channel gains)
#legend(0.01,1.03, c(expression(paste(alpha, " = ", 0)),expression(paste(alpha, " = ", 0.1)),expression(paste(alpha, " = ", 0.2)),expression(paste(alpha, " = ", 0.3))), col=c(1,2,3,4), lty=c(1,2,3,4), pch =c(1,2,3,4), lwd=c(1.5,1.5,1.5,1.5), bty="n")

legend(.08,.5, c(expression(paste(alpha, " = ", 0)),expression(paste(alpha, " = ", 0.1)),expression(paste(alpha, " = ", 0.2)),expression(paste(alpha, " = ", 0.3))), col=c(1,2,3,4), lty=c(1,2,3,4), pch =c(1,2,3,4), lwd=c(1.5,1.5,1.5,1.5), bty="n")

dev.off() #for PS

#Legend for #plot@pxPU
#legend(0,.8, c(expression(paste(alpha, " = ", 0)),expression(paste(alpha, " = ", 0.2)),expression(paste(alpha, " = ", 0.4)),expression(paste(alpha, " = ", 0.6))), col=c(1,2,3,4), lty=c(1,2,3,4), pch =c(1,2,3,4), lwd=c(1.5,1.5,1.5,1.5), bty="n")

plot(para[1:i-1], rateGIMon[1:i-1], type="l", lwd=1.5, ylab="SU's Rate",xlab= expression( paste("Frac. Bandwidth Res. for PU"," ",  (alpha))), ylim=c(0,.9))
lines(para[1:i-1], rateGIComp[1:i-1], type="l", lwd=1.5, col=2, lty=2)

plot(para[1:i-1], profMon[1:i-1], type="l", lwd=1.5, ylab="L's Profit", xlab= expression( paste("Frac. Bandwidth Res. for PU"," ",  (alpha))), ylim=c(0, .7))#
lines(para[1:i-1], profComp[1:i-1], type="l", lwd=1.5, col=2,lty=2)

####################################
##Gen int alternative version, where alpha is picked first, then grid for I is chosen based on alpha to make sure pR is increasing in I.


#Feed in PU baseline rate from above (either monopoly or competitive) for r
rStar = rMon
gammaStar = exp(rStar/BandWidth)-1

para = seq(0,.7,.025)  #various alphas

#start with either .5 (medium) or .75(high highly unfavor) saving those below first, end with .25 (favorable)
CGhS=.25; CGgU=.25;

x<-rep(0,times=length(para))
rateGIComp = x
rateGIMon = x
profComp = x
profMon = x
IntLimCond = x

for(i in 1: length(para)){
	#Limit interference so pR is monotonic in I
	maxI = min(maxPowSU*CGhS, (1-para[i])*BandWidth*chanNoise*(((1-para[i])*(1+gammaStar))^(1/para[i]) -1)) 
	IntLimCond[i] = max(0.000001,maxI)
	numIpoints = 600
	I = seq(0.000000001,IntLimCond[i],length=numIpoints) 
	
	val = optPermit(alp = para[i], BW = BandWidth,VSU = ValSU, gamma =gammaStar,Int=I, KPU=sensPU, KSU=sensSU,pBarPU=maxPowPU, pBarSU=maxPowSU, hPU = CGhU, gSU=CGgS,hSU = CGhS, gPU =CGgU, No = chanNoise)
	#	value = c(piSU, rSU, mc, cost, pXI, pRI, cond)
	pi = val[1:length(I)] #inverse demand
	rSU = val[seq(length(I)+1, 2*length(I), 1)] #
	mc = val[seq(2*length(I)+1, 3*length(I), 1)] # marginal cost #WARNING:negative! When power exceeds PU Max		
	cost = val[seq(3*length(I)+1, 4*length(I), 1)]
	pX = val[seq(4*length(I)+1, 5*length(I), 1)]
	pR = val[seq(5*length(I)+1, 6*length(I), 1)]	
	cond = val[seq(6*length(I)+1, 7*length(I), 1)]

	
	#Competitive eqm
	# prob 1: demand starts below mc, prob 2: mc always below demand
	if(mc[1]>pi[1]){
		print("mc starts above demand for alpha = ")
		print(para[i])
		}else{
			if(max(mc)<min(pi)){
				print("mc always below demand for alpha = ")
				print(para[i])		
				}else{
				eqmComp = min(which(mc>pi))-1   #lowest I where MC just passes demand minus 1
				rateGIComp[i] = rSU[eqmComp]
				profComp[i] = max(0,pi[eqmComp]*I[eqmComp]-cost[eqmComp]) 
			} 
		}
	
	#Monopolistic Eqm
	Rev = pi*I
	mrTopDiff = Rev[2:length(I)]-Rev[1:(length(I)-1)]
	mrBotDiff = I[2:length(I)]-I[1:(length(I)-1)]
	mr = mrTopDiff/mrBotDiff
	#WARNING: this numerically derived marginal cost below will be NaN when the cost itself is NaN (when power exeeds PUmax)
	mc2 =  mc[1:length(I)-1]
	
		if(mc2[1]>mr[1]){
		print("mc starts above MR for alpha = ")
		print(para[i])
		}else{
			if(max(mc2)< min(mr)){
				print("mc always below MR for alpha = ")
				print(para[i])		
				}else{
				eqmMon = min(which(mc2>mr))-1   #lowest I where MC just passes MR minus 1
				rateGIMon[i] = rSU[eqmMon]
				profMon[i] = max(0,pi[eqmMon]*I[eqmMon]-cost[eqmMon]) 
			} 
		}			
	
	}

rateGIMon[which(is.na(rateGIMon))]=0
rateGIComp[which(is.na(rateGIComp))]=0
profMon[which(is.na(profMon))]=0
profComp[which(is.na(profComp))]=0


#CGhS=.5; CGgU=.5;
rGMmid = rateGIMon
rGCmid = rateGIComp
pGMmid = profMon
pGCmid = profComp

#CGhS=.75 ;CGgU=.75;
rGMhigh = rateGIMon
rGChigh = rateGIComp
pGMhigh = profMon
pGChigh = profComp



#while plot is open on quartz 
postscript("fig8.eps", width = 2*4.0, height = 2*3.0,
           horizontal = FALSE, onefile = FALSE, paper = "special",
           family = "Helvetica", encoding = "TeXtext.enc")


markers = seq(1, length(para),length=7)


par(mfrow=c(1,2))


plot(para, rateGIMon, type="l", lwd=1.5, ylab="SU's Rate",xlab= expression( paste("Frac. Bandwidth Reserved"," ",  (alpha))), ylim=c(0,1.2))
lines(para, rateGIComp, type="l", lwd=1.5, col=2, lty=2)
lines(para, rGMmid, type="l", lwd=1.5, col=3, lty=3)
lines(para, rGCmid, type="l", lwd=1.5, col=4, lty=4)
lines(para, rGMhigh, type="l", lwd=1.5, col=5, lty=5)
lines(para, rGChigh, type="l", lwd=1.5, col=6, lty=6)
lines(para[markers], rGMmid[markers], type="p", pch=1, lwd=1.5, col=3, lty=3)
lines(para[markers], rGCmid[markers], type="p", pch=2,lwd=1.5, col=4, lty=4)
lines(para[markers], rGMhigh[markers], type="p", pch=3, lwd=1.5, col=5, lty=5)
lines(para[markers], rGChigh[markers], type="p", pch=4,lwd=1.5, col=6, lty=6)
legend(.025,1.27,c("Monop. (fav.)", "Comp. (fav.)", "Monop. (unfav.)", "Comp. (unfav.)", "Monop. (High unfav.)", "Comp. (High unfav.)" ), lty=c(1,2,3,4,5,6), col=c(1,2,3,4,5,6), lwd=c(1.5,1.5,1.5,1.5,1.5,1.5),pch=c(NA_integer_,NA_integer_,1,2,3,4), bty="n")

plot(para, profMon, type="l", lwd=1.5, ylab="L's Profit", xlab= expression( paste("Frac. Bandwidth Reserved"," ",  (alpha))), ylim=c(0, .8))#
lines(para, profComp, type="l", lwd=1.5, col=2,lty=2)
lines(para, pGMmid, type="l",  lwd=1.5, col=3, lty=3)
lines(para, pGCmid, type="l",  lwd=1.5, col=4, lty=4)
lines(para, pGMhigh, type="l",  lwd=1.5, col=5, lty=5)
lines(para, pGChigh, type="l",  lwd=1.5, col=6, lty=6)
lines(para[markers], pGMmid[markers], type="p", pch=1, lwd=1.5, col=3, lty=3)
lines(para[markers], pGCmid[markers], type="p", pch=2, lwd=1.5, col=4, lty=4)
lines(para[markers], pGMhigh[markers], type="p", pch=3, lwd=1.5, col=5, lty=5)
lines(para[markers], pGChigh[markers], type="p", pch=4, lwd=1.5, col=6, lty=6)
legend(.025,.85,c("Monop. (fav.)", "Comp. (fav.)", "Monop. (unfav.)", "Comp. (unfav.)", "Monop. (High unfav.)", "Comp. (High unfav.)"), lty=c(1,2,3,4,5,6), col=c(1,2,3,4,5,6), lwd=c(1.5,1.5,1.5,1.5,1.5,1.5),pch=c(NA_integer_,NA_integer_,1,2,3,4), bty="n")

dev.off()	