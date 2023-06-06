##################################################################################################################

# R Script complementing the article "Beyond demographic buffering: Context dependence in demographic strategies across animals" 

#  (based on Healy et al. 2019. https://doi.org/10.1038/s41559-019-0938-7)

# Here, we repurpose R scripts developed by Kevin Healy and coauthors (2019; https://github.com/healyke/Healy_et_al_2019_Animal_Life_History) 
# to fit phylogentically corrected GLMMs with two life hisotry traits (age at sexual maturity and Gini index) as responses
# and adult body mass and matrix dimension as fixed effects.

# Residuals from these models are used in GLMMs that model the relationship between corrected sensitivities and vital-rate variaition.

#################################################################################################################


rm(list=ls(all=TRUE))
library(popbio)
library(popdemo)
library(Matrix)
library(MASS)
library(maps)
library(scales)
library(devtools)
library(ape)
library(caper)
library(phytools)
library(ineq)

setwd("/Users/maria/Dropbox/demoBuff/analysesMaria")

# If you use the script below, please cite Kevin Healy's 2019 Nat Ecol Evol paper! 


# Download from Healy's GitHub repository

source("Healy_et_al_2019_Animal_Life_History-master/Calulate_pop_metrics/Demography_functions.R")

###############################
setwd()

load("MatMeanList.rda")
load("MatMeanUList.rda")
load("MatMeanFList.rda")

Species_name=read.csv("Species_name.csv")

#check
for(k in 1:length(MatMeanFList)){
 
  print(k)
  print(isErgodic(MatMeanList[[k]]))
  print(isPrimitive(MatMeanList[[k]]))
  print(isIrreducible(MatMeanList[[k]]))
  print(is.matrix_post_rep(MatMeanList[[k]]))
}

# The Orcas are not ergodic, primitive or irreducible because the last stage (which is post-reproductive), has no reproduction
# To alleviate this, I add a very small constant which does not change lambda

MatMeanList[[19]][1,5]=0.0000001

lh.data=NULL

for(k in 1:length(MatMeanFList)){
  
  matU=MatMeanUList[[k]]
  matF=MatMeanFList[[k]]
  matA=MatMeanList[[k]]
  
  matrix_size=dim(matA)[1]
  
  ##### age when 95% are dead
  surv_95 <- exceptionalLife(matU, startLife = 1)[1]
  
  ##### age when 99% are dead
  
  surv_99 <- which(log10(makeLifeTable(
    matU = matU, 
    matF = matF,
    startLife = 1, nSteps = 1000)$lx*1000) < 0)[2]
  
  #if the matrix dimension is larger then surv_99 we use that as the
  #lifespan cutoff instead.
  min_max <- max(surv_99,matrix_size)
  
  
  lxmx_curve <-  makeLifeTable(matU =  matU, 
                               matF =  matF, 
                               startLife = 1, nSteps = min_max)
  
  life_time_La <-	lifeTimeRepEvents(matU =   matU, 
                                    matF =  matF,
                                    startLife = 1)$La
  
  mean_life_expect <-	meanLifeExpectancy(matU = matU, 
                                         startLife= 1)
  
  M_rep_lif_exp <- lifeTimeRepEvents(matU =  matU, 
                                     matF =  matF,
                                     startLife = 1)$meanRepLifeExpectancy
  
  gen_time  <- generation.time(A =  matA)
  
  ### mean reproduction rate at stable state
  mean_repo_rate_stable_state <- meanRepo(matA = matA, 
                                          matF = matF)
  
  ### mean reproduction rate not at stable state
  mean_repo_rate <- mean(matF[1,matF[1,] > 0])
  
  ### mean reproduction rate
 
  gini <- Gini(lxmx_curve$lx*lxmx_curve$mx, corr= F)
  
  mxlxsd <- sd(lxmx_curve$lx*lxmx_curve$mx)
  
  sbins <- 100
  
  spline_curve <-  unlist(lx_spline(lxmx_curve$lx, lxmx_curve$x, bins = sbins)[1])
  
  La_sbins <- round((life_time_La/length(lxmx_curve$lx))*sbins)
  
  prop_la<- spline_curve[La_sbins]
  
  fx_curve=NULL
  
  for(z in 1:c(length(spline_curve)-1)){
    
    if(z == 1){fx_curve[1] <- 1 - c(spline_curve[z+1])}
    
    else{ fx_curve[z] <- c(spline_curve[z]) - spline_curve[z+1]
    
    }
  }
  
  surv_sd <- sd(fx_curve, na.rm = TRUE)
  
  lh.data=rbind(lh.data,data.frame(species=Species_name$x[k],
                                   mean_life_expect,life_time_La, mean_repo_rate,
                                   mean_repo_rate_stable_state,  gen_time, M_rep_lif_exp, matrix_size, surv_95, surv_99, gini,
                                   mxlxsd, surv_sd, prop_la, min_max))
}

head(lh.data)

cor(lh.data[,-1])

# write.csv(lh.data,"lh.data.csv",row.names = F)

### PHYLO GLMMS 

library(ape)
library(phytools)
library(MCMCglmm)
library(coda)
library(dplyr)
library(ggplot2)


# phylogeny

axis_tree=read.tree("AnimalPhylo.tree")

axis_tree$tip.label=gsub("_", " ", axis_tree$tip.label)
axis_tree$tip.label[axis_tree$tip.label%in%"Notamacropus eugenii"]="Macropus eugenii1"

axis_tree<-bind.tip(axis_tree, "Macropus eugenii2", where=which(axis_tree$tip.label=="Macropus eugenii1"))
axis_tree<-bind.tip(axis_tree, "Macropus eugenii3", where=which(axis_tree$tip.label=="Macropus eugenii2"))

axis_tree$node.label[axis_tree$node.label%in%"NA"]=c("mac1","mac2")

plot(axis_tree)

#check if ultrametric
is.ultrametric(axis_tree)

axis_tree=compute.brlen(axis_tree)

inv.phylo<-inverseA(axis_tree,nodes="TIPS",scale=TRUE)

# Traits: Use life time La and gini index

pop_data <- read.csv("lh.data.csv")

species=pop_data$species

species <- gsub("_", " ", species)
species <- gsub(" [0-9]", "", species)

species <- gsub("subsp.+","", species)
species <- gsub(" sp.", " platyceps", species) # species not defined yet, we use the most closely related one
species <- gsub("troglodytes ", "troglodytes schweinfurthii", species)
species <- gsub("occidentalis ", "occidentalis occidentalis", species)
species <- gsub("tarandus ", "tarandus caribou", species)
# species <- gsub("[0-9]+", "", species)

pop_data$animal=species

#add mass (obtained as described in the main text)

mass=read.csv("body_mass.csv")

mass$species=gsub("Notamacropus", "Macropus", mass$species)
mass$species=gsub("troglodytes", "troglodytes schweinfurthii", mass$species)
mass$species=gsub("Pygosciles adeliae", "Pygoscelis adeliae", mass$species)
mass$species=gsub("Strix occidentalis", "Strix occidentalis occidentalis", mass$species)

mass$species[mass$species%in%"Macropus eugenii"]=paste("Macropus eugenii",1:3,sep="")

pop_data$mass_g=left_join(pop_data,mass,by=c("animal"="species"))$adult_body_mass_g


log_list <- c("life_time_La",
              "mass_g",
              "matrix_size")


pop_data_log <- pop_data

pop_data_log[,log_list] <- sapply(pop_data[,log_list], function(x) log10(x))


mean_c_list  <- c( "life_time_La",
                   "matrix_size",
                   "gini",
                   "mass_g")

pop_data_log_mc <- pop_data_log
pop_data_log_mc[,mean_c_list] <- sapply(pop_data_log[,mean_c_list], function(x) mean_center(x))

# prior

prior<-list(R = list(V = 1/2, nu=0.002), 
            G = list(G1=list(V = 1/2,n = 1, alpha.mu=rep(0,1), alpha.V= diag(1)*10^3)))


# Trait models

#### La
m1=MCMCglmm(life_time_La ~ mass_g + matrix_size,random= ~animal,
            data = pop_data_log_mc,
            prior=prior,
            nitt=1100000, thin=500, burnin=100000,
            pedigree=axis_tree)

m2=MCMCglmm(life_time_La ~ mass_g + matrix_size,random= ~animal,
            data = pop_data_log_mc,
            prior=prior,
            nitt=1100000, thin=500, burnin=100000,
            pedigree=axis_tree)

m3=MCMCglmm(life_time_La ~ mass_g + matrix_size,random= ~animal,
            data = pop_data_log_mc,
            prior=prior,
            nitt=1100000, thin=500, burnin=100000,
            pedigree=axis_tree)


life_time_La.coda=mcmc.list(list(mcmc(m1$Sol),mcmc(m2$Sol),mcmc(m3$Sol)))
save(life_time_La.coda,file =  "/Users/maria/Dropbox/demoBuff/analysesMaria/life_time_La.coda.Sol.rda")

life_time_La.VCV=mcmc.list(list(mcmc(m1$VCV),mcmc(m2$VCV),mcmc(m3$VCV)))
save(life_time_La.VCV,file = "/Users/maria/Dropbox/collaborations/lemurs/mcmc_output/life_time_La.VCV.rda")

#### Gini
m1=MCMCglmm(gini ~ mass_g + matrix_size,random= ~animal,
            data = pop_data_log_mc,
            prior=prior,
            nitt=1100000, thin=500, burnin=100000,
            pedigree=axis_tree)

m2=MCMCglmm(gini ~ mass_g + matrix_size,random= ~animal,
            data = pop_data_log_mc,
            prior=prior,
            nitt=1100000, thin=500, burnin=100000,
            pedigree=axis_tree)

m3=MCMCglmm(gini ~ mass_g + matrix_size,random= ~animal,
            data = pop_data_log_mc,
            prior=prior,
            nitt=1100000, thin=500, burnin=100000,
            pedigree=axis_tree)


gini.coda=mcmc.list(list(mcmc(m1$Sol),mcmc(m2$Sol),mcmc(m3$Sol)))
save(gini.coda,file =  "gini.coda.Sol.rda")

gini.VCV=mcmc.list(list(mcmc(m1$VCV),mcmc(m2$VCV),mcmc(m3$VCV)))
save(gini.VCV,file = "gini.coda.VCV.rda")

