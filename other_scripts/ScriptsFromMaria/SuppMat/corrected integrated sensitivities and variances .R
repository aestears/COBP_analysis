##################################################################################################################

# R Script complementing the article "Beyond demographic buffering: Context dependence in demographic strategies across animals" 


# Here, we use Matrix Population Models for 31 animal populaiton to calculate the corrected standard deviation of the vital rates over the years, and correlate it with the corrected sensitivity (VSS) of the population growth rate (lambda) to changes in these vital rates (McDonald et al. 2017). 

# This R script calcualtes integrated sensitivities following van Tienderen (1995) Ecology 76:2482-2489.

#################################################################################################################

# Housekeeping
rm(list = ls())
setwd("")

# Libraries

library(car)
library(ggplot2)
library(plyr)
library(popbio) 
library(patchwork)


###############################
#
#   STRUCTURE OF THE CODE 
#                                                    
#    1. Preparation Steps                       
#      1a. Initial Steps                             
#      1b. Chionomys nivalis                                 
#      1c. Suricata suricatta                                  
#      1d. Anthropoides paradiseus
#      1e. Ovis aries (Soay sheep)
#      1f. Marmota flaviventris
#      1g. Umbonium costatum
#      1h. Orcinus orca
#      1i. Callospermophilus lateralis
#      1j. Pygoscelis adeliae
#      1k. Rangifer tarandus caribou
#      1l. Final Preparation Steps
#    2. Calculation of corrected Standard Deviations of Survival, Growth and Fecundity 
#      2a. Survival                                  
#      2b. Fecundity                                 
#      2c. Growth                                    
#    3. Sensitivity Calculations                     
#      3a. Survival                                   
#      3b. Fecundity                                 
#      3c. Growth                                    
#    4. Correlation Coefficients     
#    5. Plots (Fig. S1, Fig. 2, Fig. S2)     

#####################################

##### 1. PREPARATION STEPS #######################################################

# In this first section we load and prepare the data


#### 1a) Initial steps

# load the MPMs obtained from COMADRE, as well as a vector indicating to which species each MPM belongs to. The data is a subset of Comadre and had to follow various requirements (see "Methods" section of the paper). This subset of COMADRE is also available as supplementary material.
MPM       <- readRDS("comadre_MPM.rds")
MPM.names <- readRDS("comadre_MPM_names.rds")


# create object "animals" where we store the MPMs
animals       <- NULL
animals$mat   <- MPM
animals$names <- MPM.names

# vector with the names of the species that have been used
Species_name  <- unique(animals$names)


#### 1b) Chionomys nivalis - Snow vole (Bonnet et al. 2017)

# Demographic data coming from the work of Bonnet et al. 2017
nivalis <- readRDS("NivalisData")
# update vector of species name with the scientific name of the snow vole
Species_name[(length(Species_name)+1)] <- "Chionomys_nivalis" 
# Put them in alphabetical order
Species_name <- Species_name[order(Species_name)] 


#### 1c) Suricata suricatta - Meerkat (Paniw et al. 2019)

# Previously built annual MPMs, coming from the work of Paniw et al. 2019
meerkats <- readRDS("MeerkatData") 
Species_name[(length(Species_name)+1)] <- "Suricata_suricatta"
Species_name <- Species_name[order(Species_name)]


#### 1d) Anthropoides paradiseus - Blue Crane 

# Study present in COMADRE, but with mistakes in the MPMs, we therefore removed them from our subset and then added them separately, going back to the original pubblication to fix the mistakes) (Altwegg and Anderson 2009)
crane <- readRDS("BlueCrane")
Species_name[(length(Species_name)+1)] <- "Anthropoides_paradiseus"
Species_name <- Species_name[order(Species_name)]


### 1e) Ovis aries - Soay sheep

# also present in COMADRE, but dealt with separately because the data had males and females presented separately in the same MPM. We therefore had to extract and keep only the female data.
sheep <-readRDS("SoaySheep")
Species_name[(length(Species_name)+1)] <- "Ovis_aries"
Species_name <- Species_name[order(Species_name)]


### 1f) Marmota flaviventris - Yellow-bellied marmot (Ozgul et al. 2009). 

#Demographic data on the yellow-bellied marmot coming from Ozgul et al. 2009
marmot <- readRDS("Marmots")
Species_name[(length(Species_name)+1)] <- "Marmota_flaviventris"
Species_name <- Species_name[order(Species_name)]


### 1g) Umbonium costatum 

# Also present in COMADRE, but with tiny mistake, we had to go back to original pubblication to fix it. (Noda and Nakao 1996)
umbonium <- readRDS("Umbonium")
Species_name[(length(Species_name)+1)] <- "Umbonium_costatum"
Species_name <- Species_name[order(Species_name)]


### 1h) Orcinus orca - Killer whale 

# Also present in COMADRE, but also needed some "cleaning and preparation".
orca <- readRDS("SouthOrca")
Species_name[(length(Species_name)+1)] <- "Orcinus_orca"
Species_name <- Species_name[order(Species_name)]


### 1i) Callospermophilus lateralis - Golden-mantled ground squirrel

# Present in Comadre, but needed some "cleaning and preparation".
callospermophilus <- readRDS("Callospermophilus")
Species_name[(length(Species_name)+1)] <- "Callospermophilus_lateralis"
Species_name <- Species_name[order(Species_name)]


### 1j) Pygoscelis adeliae - Ad?lie penguin 

# Demographic data coming from an unreleased version of COMADRE, provided by Dr. Salguero-G?mez.
pygo <- readRDS("Pygo")
Species_name[(length(Species_name)+1)] <- "Pygoscelis_adeliae"
Species_name <- Species_name[order(Species_name)]


### ik) Rangifer tarandus caribou - Woodland caribou

# Demographic data available to be extracted from DeCesare et al. 2012)
rangifer <- readRDS("Rangifer")
Species_name[(length(Species_name)+1)] <- "Rangifer_tarandus_subsp._caribou"
Species_name <- Species_name[order(Species_name)]


### Combine all data in the same list
animals$mat   <- append(animals$mat, c(crane, meerkats, nivalis, sheep, marmot, umbonium, orca, callospermophilus, pygo, rangifer))
animals$names <- append(animals$names, c(rep("Anthropoides_paradiseus",11), rep("Suricata_suricatta", 20), rep("Chionomys_nivalis",9), rep("Ovis_aries",6), rep("Marmota_flaviventris",40), rep("Umbonium_costatum", 9), rep("Orcinus_orca",53), rep("Callospermophilus_lateralis", 19), rep("Pygoscelis_adeliae",30), rep("Rangifer_tarandus_subsp._caribou",10)))


#### 1l) FINAL PREPARATION STEPS

# Create empty vectors to store the correlation coefficient ("corr.all"), the deterministic pop.growth rate (lambdas) and the demographic strategy of each population (observed.demo). For each population we will get one value for each one of these vectors. Moreover, two empty lists have to be created: one to store the mean MPM of each population and one to store the plots of the correlations.
# Create empty vectors to store the correlation coefficient ("corr.all"), the deterministic pop.growth rate (lambdas) and the demographic strategy of each population (observed.demo). For each population we will get one value for each one of these vectors. Moreover, two empty lists have to be created: one to store the mean MPM of each population and one to store the plots of the correlations.

l <- length(Species_name) # number of populations used

vs.sd.vss=NULL

allVR=NULL

# Loop that goes through all the populations, and calculates for each one its correlation coefficient (i.e. either buffering or lability)
for (k in 1:l)
{
  
  # Get the MPMs by subsetting based on the name of the species
  id   <- which(animals$names == Species_name[k]) 
  Mat  <- animals$mat[id]
  
  Umat <- array(0, c(dim(Mat[[1]]$matA)[1],dim(Mat[[1]]$matA)[1],length(Mat))) # empty arrays
  Fmat <- array(0, c(dim(Mat[[1]]$matA)[1],dim(Mat[[1]]$matA)[1],length(Mat)))
  Amat <- array(0, c(dim(Mat[[1]]$matA)[1],dim(Mat[[1]]$matA)[1],length(Mat)))
  
  for(i in 1:length(Mat))
  {
    Umat[,,i] <- Mat[[i]]$matU # store corresponding matrices. Umat is a MPM with only survival and growth
    Fmat[,,i] <- Mat[[i]]$matF # Fmat contains only fecundity values
    Amat[,,i] <- Mat[[i]]$matA # Amat is the combination of Umat and Fmat
  }
  
  # In a couple of cases there are NAs values in the MPMs. the next step is to remove them from the analyses.
  
  
  # "out" will store the index of the U matrices that contain NAs
  out <- NULL 
  for (i in 1:length(Mat))
  {
    if(is.na(mean(Umat[,,i])))
    {
      out <- c(out,i)
    }
  }
  
  # "outF" will store the index of the F matrices that contain NAs
  outF <- NULL
  for (i in 1:length(Mat))
  {
    if(is.na(mean(Fmat[,,i])))
    {
      outF <- c(outF,i)
    }
  }
  
  # If - else loops to remove the matrices which have NAs
  if(length(out) == 0&length(outF) == 0)
  {
    Umat <- Umat
    Amat <- Amat
    Fmat <- Fmat
  }else{
    Umat <- Umat[,,-unique(c(out,outF))]
    Amat <- Amat[,,-unique(c(out,outF))]
    Fmat <- Fmat[,,-unique(c(out,outF))]
  }
  
  
  # in one case (killer whale) we have 2 MPMs with a 0 in survival in the first stage. To be able to calculate the correlation coefficients we have to remove them. Considering that we have 51 years of data on this population we believe that removing two years of data will not change considerably the results, and will enable us to conduct the analysis.
  if(Species_name[k]=="Orcinus_orca")
  {
    Umat <- Umat[,,-c(4,13)] 
    Amat <- Amat[,,-c(4,13)]
    Fmat <- Fmat[,,-c(4,13)]
  }
  
  
  ##### 2. CORRECTED STANDARD DEVIATIONS OF SURVIVAL; GROWTH AND FECUNDITY ##########################################
  
  # In this section we first extract the underlying vital rates, to then calculate their corrected standard deviation (according to McDonald et al. 2017). The corrections are required because we want to be able to include in the same analysis vital rates such as survival and growth (bounded between 0 and 1) and fecundity (bounded only by 0). The variance of 0-1 vital rates is constrained by a lower and upper limit, therefore these vital rates have to be transformed to free variance from this constraint. 
  
  #### 2a) Survival
  
  # Each entry in the Umat is not simply survival, but it is survival and either the chance to stay in the same stage, or proceed to the next one (in animals, retrogression is only rarely found, and it was not present in any of our study populations)
  surv    <- apply(Umat,c(2,3),sum) # Survival is still the sum of all matrix elements in a column of a U matrix
  surv.mu <- apply(surv,c(1),mean) # Get the mean value of the vital rate over all the MPMs (surv.mu is a vector with the mean survival rate for each age or stage class, for a study population)
  surv.sd <- apply(surv,c(1),sd)
  
  tempVR=t(surv)
  colnames(tempVR)=paste("s",1:ncol(tempVR))
  allVR=tempVR
  corr.surv.sd <- apply(logit(surv,adjust = 0.001),c(1),sd) # McDonald et al. (2017) used logit transformation on 0-1 vital rates
  
  #### 2b) Fecundity 
  # Same as surv.mu
  
  # Since the correction that we must apply to fecundities is the log-transformation, we encounter problems when the the vital rate has a value of 0. The following if-else loops are needed to deal with fecundity matrices with "0" entries where they should have a positive value (0s are a problem because log transformation can't be done on the value "0"). Therefore, we add a small value (0.01) to the fecundity value which is 0, over all the years of the study. This way, we keep a biological meaning, and we stay consistent.
  
  if(Species_name[k] == "Strix_occidentalis_subsp._occidentalis_2")
  {
    Fmat[1,2,] <- Fmat[1,2,]+0.01
  }
  
  if(Species_name[k] == "Chlorocebus_aethiops_2")
  {
    Fmat[1,2,] <- Fmat[1,2,]+0.01
  }
  
  if(Species_name[k] == "Macaca_mulatta_3")
  {
    Fmat[1,4,] <- Fmat[1,4,]+0.01
  }
  
  if(Species_name[k] == "Marmota_flaviventris")
  {
    Fmat[1,2,] <- Fmat[1,2,]+0.01
  }
  
  if(Species_name[k] == "Orcinus_orca")
  {
    Fmat[1,3,] <- Fmat[1,3,]+0.01
    Fmat[1,4,] <- Fmat[1,4,]+0.01
  }
  
  
  # Corrected standard deviation for fecundity rates (again according to McDonald et al. 2017)
  fec.mu <- apply(Fmat,c(1,2),mean)
  fec.sd=apply(Fmat, c(1,2), sd)
  corr.fec.sd <- apply(log(Fmat), c(1,2), sd) # log transformation is required 
  corr.fec.sd <- corr.fec.sd[which(corr.fec.sd!=0)]
  fec.sd <- fec.sd[which(fec.sd!=0)]
  
  tempVR=matrix(0,dim(Fmat)[3],dim(Fmat)[2])
  for(z in 1:dim(Fmat)[3]){
    Fmat.alt=Fmat[,,z]
    Fmat.alt[Fmat.alt%in%0]=NA
    mean=colMeans(Fmat.alt,na.rm=T)
    mean[is.na(mean)]=0
    tempVR[z,]=mean
  }
  
  
  colnames(tempVR)=paste("f",1:ncol(tempVR))
  
  allVR=cbind(allVR,tempVR[1:nrow(allVR),])
  
  # Mean matrices, necessary further on (for sensitivity calculations)
  MatMean  <- apply(Amat, c(1,2), mean) 
  MatMeanU <- apply(Umat, c(1,2), mean)
  MatMeanF <- apply(Fmat, c(1,2), mean) 
  
  #### 2c) Growth. In some studies (i.e. age-based studies) growth is 1.
  
  if(sum(diag(MatMeanU)[-dim(MatMeanU)[1]]) == 0)
  { # it checks the stasis transitions (except the last one), if they are all zero it means that stasis is zero and growth is always 1 (such is the case for age-class studies)
    MatMeanG <- matrix(1, dim(MatMean), dim(MatMean)) # if it is an age class study it creates a matrix full of 1s
  }else{
    
    # if the MPMs are stage-based, it's necessary to extract the growth values 
    Gmat <- array(0, c(dim(Mat[[1]]$matA)[1],dim(Mat[[1]]$matA)[1],dim(Umat)[3])) # empty array to store the growth matrices 
    for(j in 1:dim(Umat)[3])
    {
      for(i in 1:length(Mat[[1]]$matA[1,]))
      {
        Gmat[,i,j] <- Umat[,i,j]/surv[i,j]
      }
    }
    
    if(Species_name[k] == "Suricata_suricatta") # the meerkat populations must be considered separately because there is also growth directly from 1st to 3rd stage in the same year
    {
      growth <- matrix(0, length(Mat[[1]]$matA[1,]),length(Mat))
      for(i in 1:length(Mat))
      {
        growth[,i] <- Gmat[,,i][lower.tri(Gmat[,,i], diag=F)] 
      } 
      # get from the growth matrix only the values we really need
    }else{
      growth <- matrix(0, length(Mat[[1]]$matA[1,])-1,dim(Umat)[3])
      for(i in 1:dim(Umat)[3])
      {
        growth[,i] <- Gmat[,,i][lower.tri(Gmat[,,i], diag=F)& Gmat[,,i]>0]
      } 
    }
    
    corr.growth.sd <- apply(logit(growth, adjust=0.001), c(1), sd) # Corrected standard deviation of growth rates (McDonald et al. 2017)
    growth.sd <- apply(growth, c(1), sd) # Corrected standard deviation of growth rates (McDonald et al. 2017)
    growth.mean <- apply(growth, c(1), mean)
    # get mean matrix of growth
    MatMeanG <- apply(Gmat, c(1,2), mean)
    
    growthvector <- rep(0, length(MatMeanG[1,]))
    if(Species_name[k] == "Suricata_suricatta")
    {
      growthvector[1] <- MatMeanG[2,1]
      growthvector[2] <- MatMeanG[3,1]
      growthvector[3] <- MatMeanG[3,2]
    }else{
      for (i in 1:(length(MatMeanG[1,])-1))
      {
        growthvector[i] <- MatMeanG[i+1,i] # a vector with only the values of the mean growth matrix that we need
      }
    }
    tempVR=t(growth)
    colnames(tempVR)=paste("g",1:ncol(tempVR))
    
    allVR=cbind(allVR,tempVR)
  }
  
  
  
  # calculate vital-rate covariation and correlation
  
  cov.Mat=cov(allVR,method="spearman")
  rR=which(rowSums(cov.Mat)==0)
  rC=which(colSums(cov.Mat)==0)
  corr.Mat=cor(allVR,method="spearman")
  
  if(length(rR)>0){
    if(any(rR%in%grep("f",colnames(cov.Mat)))&length(rR)==length(grep("f",colnames(cov.Mat)))){
      
      cov.Mat<-cov.Mat[-rR,-rC]
      corr.Mat<-corr.Mat[-rR,-rC]
    }else if(any(rR%in%grep("f",colnames(cov.Mat)))&length(rR)!=length(grep("f",colnames(cov.Mat)))){
      
      cov.Mat[,rC[!rR%in%grep("f",colnames(cov.Mat))]]=0
      cov.Mat[rR[!rR%in%grep("f",colnames(cov.Mat))],]=0
      
      corr.Mat[,rC[!rR%in%grep("f",colnames(corr.Mat))]]=0
      corr.Mat[rR[!rR%in%grep("f",colnames(corr.Mat))],]=0
      
      cov.Mat<-cov.Mat[-rR[rR%in%grep("f",colnames(cov.Mat))],-rC[rR%in%grep("f",colnames(cov.Mat))]]
      corr.Mat<-corr.Mat[-rR[rR%in%grep("f",colnames(corr.Mat))],-rC[rR%in%grep("f",colnames(corr.Mat))]]
      
      
    }else{
      
      corr.Mat[rR,] =0
      corr.Mat[,rC] =0
    } 
  }
  
  ##### 3. SENSITIVITY ###################################################
  
  # Calculation of the corrected sensitivity. The first step is the calculation according to Silvertown and Franco (2004), and then we apply a correction according to McDonald et al. (2017)
  
  S <- sensitivity(MatMean, zero = T) # these are the uncorrected sensitivities, i.e. on the matrix elements, not on the underlying vital rates. It's necessary to calculate them on the underlying vital rates (Silvertown and Franco 2004)
  
  
  #### 3a) Survival
  # sensitivity of population growth rate (i.e. lambda) to changes in survival rates
  sens.surv <- rep(0, length(MatMean[,1]))
  elast.surv<-rep(0, length(MatMean[,1]))
  for(i in 1:length(MatMean[,1])-1)
  {
    sens.surv[i] <- S[i,i]*(1-MatMeanG[i+1,i]) + S[i+1,i]*MatMeanG[i+1,i]
    elast.surv[i]<-sens.surv[i]*surv.mu[i]/lambda(MatMean)
  }
  sens.surv[length(MatMean[,1])] <- S[length(MatMean[,1]),length(MatMean[,1])] 
  elast.surv[length(MatMean[,1])]<-sens.surv[length(MatMean[,1])]*surv.mu[length(MatMean[,1])]/lambda(MatMean)
  
  
  #### 3b) Fecundity
  # For fecundity rates the VSS transformation corresponds to the elasticities
  elast     <- elasticity(MatMean)
  elast.fec <- elast[which(fec.mu != 0)] # keep only the elasticities of fecundity values
  
  sens.fec     <- sensitivity(MatMean)
  sens.fec <- sens.fec[which(fec.mu != 0)] # keep only the elasticities of fecundity values
  
  
  #### 3c) Growth
  # sensitivity of lambda to growth according to Silvertown and Franco
  
  if(Species_name[k] == "Suricata_suricatta")
  {
    sens.growth <- rep(0,length(MatMean[,1]))
    elast.growth <- rep(0,length(MatMean[,1])-1)
    for(i in 1:length(MatMean[,1])-1)
    {
      sens.growth[i]<- abs(S[i,i]*(-surv.mu[i])+S[i+1,i]*surv.mu[i]) 
      elast.growth[i]<- sens.growth[i]*growth.mean[i]/lambda(MatMean)
    }
    sens.growth[3] <- sens.growth[2]
    elast.growth[3]<-sens.growth[2]*growth.mean[2]/lambda(MatMean)
    sens.growth[2] <- abs(S[1,1]*(-surv.mu[1])+S[3,1]*surv.mu[1]) # insert here growth from stage 1 to stage 3
    elast.growth[2]<-abs(S[1,1]*(-surv.mu[1])+S[3,1]*surv.mu[1])*growth.mean[1]/lambda(MatMean)
  }else{
    sens.growth <- rep(0,length(MatMean[,1])-1)
    elast.growth <- rep(0,length(MatMean[,1])-1)
    for (i in 1:length(MatMean[,1])-1)
    {
      gr.temp=tryCatch({growth.mean[i]},error = function(x) 0)
      sens.growth[i] <- abs(S[i,i]*(-surv.mu[i])+S[i+1,i]*surv.mu[i]) 
      elast.growth[i]<- sens.growth[i]*gr.temp/lambda(MatMean)
    }
    
  }
  
  
  ### INTEGRATED SENSITIVITIES
  
  sens.all=c(sens.surv,sens.fec)
  elast.all=c(elast.surv,elast.fec)
  CV=c(100*surv.sd/surv.mu,100*fec.sd/fec.mu[fec.mu>0])
  
  if(sum(diag(MatMeanU)[-dim(MatMeanU)[1]]) > 0){
    CV=c(CV,100*growth.sd/growth.mean)
    sens.all=c(sens.all,sens.growth)
    elast.all=c(elast.all,elast.growth)
  } 
  
  # survival
  
  int.sens.surv <- rep(0, length(MatMean[,1]))
  for(i in 1:length(int.sens.surv)){
    
    temp.surv=0
    
    for(j in 1:length(corr.Mat[,i])){
      
      if(is.na(sqrt(cov.Mat[j,j]/cov.Mat[i,i])))  temp.surv <- temp.surv+ 0 else temp.surv <- temp.surv+ corr.Mat[j,i]*sens.all[i]*sqrt(cov.Mat[j,j]/cov.Mat[i,i])
      
    }
    int.sens.surv[i]=temp.surv
  }
  
  VSS.int.surv <- rep(0, length(MatMean[,1]))
  
  for(i in 1:length(MatMean[,1]))
  {
    VSS.int.surv[i] <- int.sens.surv[i]*surv.mu[i]*(1-surv.mu[i])/lambda(MatMean)
  }
  
  
  temp=data.frame(mean=surv.mu,sd=surv.sd,sd.corr=corr.surv.sd,vss.int=VSS.int.surv,vr="surv")
  
  
  int.elast.fec=rep(0,length(elast.fec))
  
  for(i in 1:length(int.elast.fec)){
    
    temp.fec=0
    
    for(j in 1:length(corr.Mat[,(i+length(int.sens.surv))])){
      
      if(is.na(sqrt(CV[j]/CV[(i+length(int.sens.surv))]))) temp.fec <- temp.fec+0 else temp.fec <- temp.fec+ corr.Mat[j,(i+length(int.sens.surv))]*elast.all[(i+length(int.sens.surv))]*sqrt(CV[j]/CV[(i+length(int.sens.surv))])    
    }
    int.elast.fec[i]=temp.fec
  }
  
  VSS.int.fec=int.elast.fec
  
  
  temp=rbind(temp,data.frame(mean=fec.mu[fec.mu>0],sd=fec.sd,sd.corr=corr.fec.sd,vss.int=VSS.int.fec,vr="fec"))
  
  
  
  if(sum(diag(MatMeanU)[-dim(MatMeanU)[1]]) > 0){
    
    int.sens.growth <- rep(0, length(elast.growth))
    for(i in 1:length(int.sens.growth)){
      
      temp.gr=0
      
      for(j in 1:length(corr.Mat[,(i+length(int.sens.surv)+length(int.elast.fec))])){
        if(is.na(sqrt(cov.Mat[j,j]/cov.Mat[(i+length(int.sens.surv)+length(int.elast.fec)),(i+length(int.sens.surv)+length(int.elast.fec))]))) temp.gr <- temp.gr+0 else temp.gr <- temp.gr+ corr.Mat[j,(i+length(int.sens.surv)+length(int.elast.fec))]*sens.all[(i+length(int.sens.surv)+length(int.elast.fec))]*sqrt(cov.Mat[j,j]/cov.Mat[(i+length(int.sens.surv)+length(int.elast.fec)),(i+length(int.sens.surv)+length(int.elast.fec))])
        
      }
      int.sens.growth[i]=temp.gr
    }
    
    VSS.int.growth <- rep(0,length(MatMean[,1])-1)
    if(sum(MatMeanG) != (dim(MatMeanG)[1]*dim(MatMeanG)[2]))
    {
      
      if(Species_name[k] == "Suricata_suricatta")
      {
        VSS.int.growth    <- rep(0,3)
        VSS.int.growth[1] <- int.sens.growth[1]*growthvector[1]*(1-growthvector[1])/lambda(MatMean)
        VSS.int.growth[2] <- int.sens.growth[2]*growthvector[2]*(1-growthvector[2])/lambda(MatMean)
        VSS.int.growth[3] <- int.sens.growth[3]*growthvector[3]*(1-growthvector[3])/lambda(MatMean)
      }else{
        for(i in 1:length(MatMean[,1])-1)
        {
          VSS.int.growth[i] <- int.sens.growth[i]*growthvector[i]*(1-growthvector[i])/lambda(MatMean)
        }
      }
      
    }
    
    
    
    temp=rbind(temp,data.frame(mean=growth.mean,sd=growth.sd,sd.corr=corr.growth.sd,vss.int=VSS.int.growth,vr="gr")) 
    
  } 
  
  temp$species=as.character(Species_name[k])
  vs.sd.vss=rbind(vs.sd.vss,temp)
  
} # the warnings are due to 0 SD and resulting issues to calculate correlations (but this is dealt with in the code)


head(vs.sd.vss)

vs.sd.vss[is.na(vs.sd.vss$sd),]

### CORRELATIONS

corr=NULL

for(k in 1:l){
  # We calculate correlations only for vital rates where there are integrated vss
  sub=vs.sd.vss[vs.sd.vss$species%in%Species_name[k]&!is.na(vs.sd.vss$vss.int),] 
  
  # correlations using integrated vss
  correl.int=cor(sub$sd.corr,sub$vss.int,method="spearman",use = "na.or.complete")
  
  corr=rbind(corr,data.frame(corr.int=correl.int,species=Species_name[k]))
}


View(corr)

###### 5. Plots 

species=vs.sd.vss$species

species <- gsub("_", " ", species)
species <- gsub(" [0-9]", "", species)

species <- gsub("subsp.+","", species)
species <- gsub(" sp.", " platyceps", species) # species not defined yet, we use the most closely related one
species <- gsub("troglodytes ", "troglodytes schweinfurthii", species)
species <- gsub("occidentalis ", "occidentalis occidentalis", species)
species <- gsub("tarandus ", "tarandus caribou", species)

vs.sd.vss$species=species
species <- gsub("[0-9]+", "", species)

vs.sd.vss$animal=species

vss.data.integr=vs.sd.vss

vss.data.integr=vss.data.integr[!is.na(vss.data.integr$vss.int),] # remove NA entries

# write.csv(vss.data.integr,"vss.data.integr.csv",row.names = F) # already available 

vss.data.integr$vr[vss.data.integr$vr%in%"surv"] ="Survival"
vss.data.integr$vr[vss.data.integr$vr%in%"fec"] ="Reproduction"
vss.data.integr$vr[vss.data.integr$vr%in%"gr"] ="Growth"

vss.data.integr$species=factor(vss.data.integr$species)

# vss.data.integr=vss.data.integr[-which(vss.data.integr$vss.int==0),]

vss.data.integr$vr=factor(vss.data.integr$vr,levels = c("Survival","Reproduction","Growth"))

levels(vss.data.integr$species)[c(21,26)]=c("Pan troglodytes schw.", "Strix occidentalis occ.")


vss.sub=vss.data.integr

# Figure S3 
ggplot(vss.sub,aes(sd.corr,vss.int,col=vr))+
  geom_point(size=3)+
  scale_color_manual(values=c("grey70","orange","blue"),name="")+
  facet_wrap(species~.,ncol=5,scales="free")+
  theme_classic(base_size = 20)+
  theme(legend.position = "top",
        strip.text = element_text(face = "italic"))+
  ylab("Variance stabilized integrated sensitivity")+
  xlab("Corrected standard deviation")

