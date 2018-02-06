#All functions.
#No Spectrum Sharing [Baseline]
#Given paras, find baseline
baseline <- function(W = 1, eta = .5, r=.1,VPU=1, KPU=1, pBarPU=1, hPU = 1,No = 0.25){ 

#PU power as function of rate r and derivatives
powPu = (W*No/hPU)*(exp(r/W)-1)
powPuPrime = (No/hPU)*exp(r/W)
powPuPrime2 = (No/(W*hPU))*exp(r/W)

if(max(powPu)>pBarPU){
	print("Power exceeds cap")
}else{

#PU's inverse demand 
m = VPU - KPU*(powPuPrime/(pBarPU-powPu))

#L's MR for PU's demand [the first term below "m" already incorporates VPU]
MR = m - r*KPU*(( (pBarPU-powPu)*powPuPrime2 + (powPuPrime^2) )/((pBarPU-powPu)^2))

#L's marginal cost
MC = eta*r  #2*eta2*r + eta1

val = c(m, MR, MC)

return(val)
}
}




#Spectrum Sharing Example (Bandwidth leasing i.e. orthogonal sharing)
#For para choice returns lots of stuff (inverse demand for bandwodth, MR, MC, data rates) 
share <- function(BW = 1, VSU=1, Omega=.25, KSU=1,pBarSU=1,gSU=1, No = 0.25){ 
	
#channel gains hU, gS, normalized to 1
gS = gSU
omega = Omega 
W = BW #bandwidth also normalized to 1 (or could be changed)
noise = No 
powSU = pBarSU #power limit for SU
kSU = KSU  #SU power paramter 

aSU = kSU/VSU
bSU = (gS*powSU/noise)

if (aSU>bSU){
	print("Error, input(s) not valid. aSU should be < bSU")
}else{

	rSU = omega*log((omega+bSU)/(omega+aSU)) #SU's data rate
	piSU = VSU*(log((omega+bSU)/(omega+aSU)) - ((bSU-aSU)/(omega+bSU)) ) #SU's inverse demand for spectrum
	mrL	=  VSU*(log((omega+bSU)/(omega+aSU)) - ((bSU-aSU)/(omega+bSU))  - (omega/(omega+aSU))*((((bSU-aSU)/(omega+bSU)))^2) ) #L's marginal revenue from leasing spectrum
	
	value = c(piSU, mrL, rSU) #Profit "BL" is "baseline" not bw leasing
	return(value)
	
	}
}


#Spectrum Sharing Example (Bandwidth leasing i.e. orthogonal sharing)
#For para choice returns marginal cost and total opportunity cost leasing spectrum
orthoMC <- function(BW = 1, VPU=1,r=.5, Omega=.25, KPU=1,pBarPU=1,hPU=1, No = 0.25){ 
	
	pStar = ((BW*No)/hPU)*(exp(r/BW)-1)  #baseline power
	pOmega =(((BW-Omega)*No)/hPU)*(exp(r/(BW-Omega))-1) #power for omega
	
	mc1 = (KPU*No)/(hPU*(pBarPU - pOmega)) #first term in product for MC
	frac = r/(BW-Omega)
	mc2 = (frac-1)*exp(frac)+1 
	mCost = mc1*mc2 #mc of leasing omega #Negative when power exceeds limit
	totCost = KPU*log((pBarPU - pStar)/(pBarPU - pOmega))  #Will produce NaNs when power exceeds limit
	value = c(mCost, totCost)
	return(value)
}


#Spectrum Sharing Example for interference tolerance approach (i.e. non-orthogonal sharing)
#Given paras returns stuff: inverse demand, Marginal Revenue, data rates
permit <- function(BW = 1.5, VSU = 1, gamma = 1, Int=.1, KSU=.5,pBarSU=3, hPU = 1, gSU=1,hSU = .8, gPU =.8, No = .25){ 
	
#channel gains 
hU = hPU
gS = gSU
hS = hSU
gU = gPU

I = Int 
W = BW #bandwidth also normalized to 1
noise = No 
powSU = pBarSU #power limit for SU

kSU = KSU  #SU power paramter 

aSU = kSU
bSU = (gS*powSU/noise)

if (aSU>bSU){
	print("Error, input(s) not valid. aSU should be < bSU")
}else{
	A = (W*noise*(1 + gamma*(gU/hU)))/(gS/hS)
	B = (gamma*(gU/hU))/(gS/hS)
	
	rSU = W*log(1+I/(A+B*I)) #SU's data rate
	piSU = VSU*A*W/((A+B*I)*(A+(1+B)*I))  - (kSU/hS)/(powSU-I/hS)  #SU's inverse demand for permits
	mrL	=  VSU*A*W*(A^2 - B*(1+B)*I^2)/(((A+B*I)*(A+(1+B)*I))^2) - kSU*powSU/(hS*((powSU-I/hS)^2)) #L's marginal revenue from permits

	value = c(piSU, mrL, rSU)
	return(value)
		
	}
}

#Spectrum Sharing Example (Bandwidth leasing i.e. orthogonal sharing)
#For para choice returns marginal cost and total opportunity cost leasing spectrum
NonorthoMC <- function(BW = 1, VPU=1,r=.5, Int =.25, KPU=1,pBarPU=1,hPU=1, No = 0.25){ 
	
	pStar = ((BW*No)/hPU)*(exp(r/BW)-1)  #baseline power
	gammaStar = exp(r/BW)-1 #baseline SNR
	pR = (gammaStar*(BW*No + Int))/hPU
		
	mCost = (KPU*gammaStar)/((pBarPU*hPU) - gammaStar*(BW*No + Int))
	totCost = KPU*log((pBarPU - pStar)/(pBarPU - pR))  #Will produce NaNs when power exceeds limit
	value = c(mCost, totCost)
	return(value)
}


#Spectrum Sharing Example (Hybrid version with orthongal (for PU only) and non-orthogonal)

#returns inverse demand, rate SU, mc and total cost. Does not return marg rev, as there is no analytical expression for that
optPermit <- function(alp = 0.5, BW = 1.5, VSU = 1, gamma = 2,Int=.1, KPU=.5, KSU=.5,pBarPU=3, pBarSU=3, hPU = 1, gSU=1,hSU = .8, gPU =.8, No = .25){ 
	
#channel gains hU, gS, normalized to 1
hU = hPU
gS = gSU
hS = hSU
gU = gPU

I = Int 
W = BW #bandwidth 
alpha = alp #fraction of BW retained for exclusive use

noise = No 
powPU = pBarPU #normalize power PU to 1 as well.
powSU = pBarSU #power limit for SU

#need power cost parameter to be less than power limit scaled by inverse of noise which is 10 
kPU = KPU
kSU = KSU  #SU power paramter 


	pstar = gamma*W*noise/hU
		
	pXI = (alpha*W*noise/hU)*((1+gamma)*((1 + I/((1-alpha)*W*noise))^(1-alpha)) -1)
	pRI = (((1-alpha)*W*noise+I)/hU)*((1+gamma)*((1 + I/((1-alpha)*W*noise))^(-alpha))-1)
	
	#condition to pRI rises monotonically in I.
	cond= (1-alpha)*(1+gamma)*((1 + I/((1-alpha)*W*noise))^(-alpha))
	pRIprime = (1/hU)*(cond-1) 	#derivative of pRI wrt I
	
	gammaSU = ((gS*I)/hS)/(((1-alpha)*W*noise) + gU*pRI)
			 
	piSU = VSU*(1-alpha)*W*(gammaSU/(1+gammaSU))*((1/I) - (gU*pRIprime)/(((1-alpha)*W*noise) + gU*pRI))   - (kSU/hS)/(powSU-(I/hS))  #SU's inverse demand for permits
	
	#Marginal cost for L (WARNING: this does not check for negative values, when PU power exceeds max)
	mc = ((kPU/hU)/(powPU - pXI-pRI))*((1+gamma)*((1 + I/((1-alpha)*W*noise))^(-alpha))-1 )
	
	#WARNING: returns NaN when power exceeds powPU, which happens at high interference (non-eqm)
	cost = kPU*log((powPU-pstar)/(powPU - pXI-pRI)) #can be NaNs
				
	rSU = (1-alpha)*W*log(1+gammaSU)	

	value = c(piSU, rSU, mc, cost, pXI, pRI, cond)
	return(value)
	
}

