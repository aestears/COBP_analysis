##################################################################################################################

# R Script complementing the article "Beyond demographic buffering: Context dependence in demographic strategies across animals" 

# (based on Healy et al. 2019. https://doi.org/10.1038/s41559-019-0938-7)

# This R script using phylogenetically corrected generalized mixed effect models to model, for 31 animal populaitons,
# the variance stabilized sensitivity of the populaiton growth rate as a function of standardized vital-rate variation,
# while accounting for the effects of two life history traits on the splope of this relationship 

#################################################################################################################


rm(list = ls())

library(ape)
library(phytools)
library(MCMCglmm)
library(coda)
library(dplyr)
library(ggplot2)
library(caper)
library(paran)
library(SIBER)
library(hdrcde)
library(ggrepel)

# If you use the script below, please cite Kevin Healy's 2019 Nat Ecol Evol paper! 


# Download from Healy's GitHub repository

source("Healy_et_al_2019_Animal_Life_History-master/Calulate_pop_metrics/Demography_functions.R")


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

# Traits: Use life time La and gini index residuals
pop_data <- read.csv("lh.data.csv")

species=pop_data$species

species <- gsub("_", " ", species)
species <- gsub(" [0-9]", "", species)

species <- gsub("subsp.+","", species)
species <- gsub(" sp.", " platyceps", species) # species not defined yet, we use the most closely related one
species <- gsub("troglodytes ", "troglodytes schweinfurthii", species)
species <- gsub("occidentalis ", "occidentalis occidentalis", species)
species <- gsub("tarandus ", "tarandus caribou", species)

pop_data$species=species
species <- gsub("[0-9]+", "", species)

pop_data$animal=species

#add mass

mass=read.csv("body_mass.csv")

mass$species=gsub("Notamacropus", "Macropus", mass$species)
mass$species=gsub("troglodytes", "troglodytes schweinfurthii", mass$species)
mass$species=gsub("Pygosciles adeliae", "Pygoscelis adeliae", mass$species)
mass$species=gsub("Strix occidentalis", "Strix occidentalis occidentalis", mass$species)

sub=mass[!mass$species%in%"Macropus eugenii",]

pop_data$mass_g=left_join(pop_data,sub,by=c("animal"="species"))$adult_body_mass_g

pop_data$mass_g[pop_data$animal%in%"Macropus eugenii"]=unique(mass$adult_body_mass_g[mass$species%in%"Macropus eugenii"])

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

# Function to get residuals from life-history trait models

resids <- function(mcmc_output, trait_data, Y_data_col){
  n=length(mcmc_output)
  Expected_Y <- hdr(unlist(mcmc_output[1:n][,1]))$mode + 
    (trait_data$mass_g)*hdr(unlist(mcmc_output[1:n][,2]))$mode +
    (trait_data$matrix_size)*hdr(unlist(mcmc_output[1:n][,3]))$mode
  
  resids_mul <- trait_data[,Y_data_col]  - Expected_Y
  return(resids_mul)
  
}

#############  #Age at first reproduction

load("life_time_La.VCV.rda")

la_var = life_time_La.VCV

#phylogenetic signal
la_phlyo <- list()

#residual
la_unit <- list()

#extrace the random terms from across the models
for(i in 1:length(la_var)){
  
  la_phlyo[[i]] <-  la_var[[i]][,1] 
  la_unit[[i]] <-  la_var[[i]][,2] 
}

la_phlyo <- unlist(la_phlyo)

la_unit <- unlist(la_unit)

la_prop_phlyo <- la_phlyo/(la_phlyo + la_unit)
la_prop_residuals  <- la_unit/(la_phlyo +  la_unit)

#Phylogenetic signal
hdr(la_prop_phlyo)$mode


#Residual term
hdr(la_prop_residuals)$mode

load("life_time_La.coda.Sol.rda")

La_resids <- resids(mcmc_output =life_time_La.coda, 
                    trait_data = pop_data_log_mc, 
                    Y_data_col = c("life_time_La"))

#############  # Gini index

load("gini.coda.VCV.rda")

gini_var = gini.VCV

#phylogenetic signal
gini_phlyo <- list()

#residual
gini_unit <- list()

#extrace the random terms from across the models
for(i in 1:length(gini_var)){
  
  gini_phlyo[[i]] <-  gini_var[[i]][,1]  
  gini_unit[[i]] <-  gini_var[[i]][,2] 
}

gini_phlyo <- unlist(gini_phlyo)
gini_unit <- unlist(gini_unit)

gini_prop_phlyo <- gini_phlyo/(gini_phlyo + gini_unit)
gini_prop_residuals  <- gini_unit/(gini_phlyo + gini_unit)

#Phylogenetic signal
hdr(gini_prop_phlyo)$mode


#Residual term
hdr(gini_prop_residuals)$mode

load("gini.coda.Sol.rda")

gini_resids <- resids(mcmc_output =gini.coda, 
                      trait_data = pop_data_log_mc, 
                      Y_data_col = c("gini"))


traits=data.frame(species=pop_data_log_mc$species,animal=pop_data_log_mc$animal,
                  La_resids,gini_resids)

#############  #
##### MCMCglmm analyses

vss.data <- read.csv("vss.data.csv")

vss.data$animal[!vss.data$animal%in%axis_tree$tip.label]

vss.data$species=factor(vss.data$species)

vss.data=vss.data[-which(vss.data$vss==0),]

vss.data$vss=log(vss.data$vss)

# prior

priors2 <- list(
  B = list(mu = rep(0, 6), V = diag(1, 6)),
  G = list(G1 = list(V = diag(2), nu = 2, alpha.mu = c(0,0), alpha.V
                     = diag(2)*10^3)),
  R = list(V = 1, nu = 0.002)
)
####

m1=MCMCglmm(vss ~ sd.corr+gini_resids+La_resids+sd.corr*gini_resids++sd.corr*La_resids,random= ~ us(1 + sd.corr):animal,
            data = vss.data,
            prior=priors2,pr=T,
            nitt=1100000, thin=500, burnin=100000,
            ginverse=list(animal=inv.phylo$Ainv))

vss.data$pred_m1_fit=exp(predict(m1,interval="confidence",marginal=NULL)[,1])
vss.data$pred_m1_lrw=exp(predict(m1,interval="confidence",marginal=NULL)[,2])
vss.data$pred_m1_upr=exp(predict(m1,interval="confidence",marginal=NULL)[,3])

m2=MCMCglmm(vss ~ sd.corr+gini_resids+La_resids+sd.corr*gini_resids++sd.corr*La_resids,random= ~ us(1 + sd.corr):animal,
            data = vss.data,
            prior=priors2,pr=T,
            nitt=1100000, thin=500, burnin=100000,
            ginverse=list(animal=inv.phylo$Ainv))

vss.data$pred_m2_fit=exp(predict(m2,interval="confidence",marginal=NULL)[,1])
vss.data$pred_m2_lrw=exp(predict(m2,interval="confidence",marginal=NULL)[,2])
vss.data$pred_m2_upr=exp(predict(m2,interval="confidence",marginal=NULL)[,3])


m3=MCMCglmm(vss ~ sd.corr+gini_resids+La_resids+sd.corr*gini_resids++sd.corr*La_resids,random= ~ us(1 + sd.corr):animal,
            data = vss.data,
            prior=priors2,pr=T,
            nitt=1100000, thin=500, burnin=100000,
            ginverse=list(animal=inv.phylo$Ainv))

vss.data$pred_m3_fit=exp(predict(m3,interval="confidence",marginal=NULL)[,1])
vss.data$pred_m3_lrw=exp(predict(m3,interval="confidence",marginal=NULL)[,2])
vss.data$pred_m3_upr=exp(predict(m3,interval="confidence",marginal=NULL)[,3])


ggplot(vss.data,aes(sd.corr,exp(vss)))+
  geom_point()+
  geom_line(aes(sd.corr,pred_m2_fit))+
  theme_bw(base_size = 20)+
  facet_wrap(~species,ncol=5,scales = "free")

vss.coda.Sol=mcmc.list(list(mcmc(m1$Sol),mcmc(m2$Sol),mcmc(m3$Sol)))
save(vss.coda.Sol,file = "vss.coda.Sol.rda")

vss.coda.VCV=mcmc.list(list(mcmc(m1$VCV),mcmc(m2$VCV),mcmc(m3$VCV)))
save(vss.coda.VCV,file = "vss.coda.VCV.rda")


summary(vss.coda.Sol)

plot(vss.coda.Sol,smooth=F)

gelman.diag(vss.coda.Sol,multivariate=F)#

library(MCMCvis)

par(mfrow=c(1,1))

colnames(vss.coda.Sol[[1]])=gsub("flaviventris","flaviventer",colnames(vss.coda.Sol[[1]]))
colnames(vss.coda.Sol[[2]])=gsub("flaviventris","flaviventer",colnames(vss.coda.Sol[[2]]))
colnames(vss.coda.Sol[[3]])=gsub("flaviventris","flaviventer",colnames(vss.coda.Sol[[3]]))

names=c(colnames(vss.coda.Sol[[1]])[c(2,5:6)],colnames(vss.coda.Sol[[1]])[c(40:70)][order(colnames(vss.coda.Sol[[1]])[c(40:70)])])



MCMCplot(vss.coda.Sol,ref_ovl = T,params=names,xlab="Posterior stimate",
         labels = c("SD","SD:G","SD:La",paste("SD:",substring(names[4:length(names)], 16),sep="")),
         sz_labels = 0.9,sz_med = 0.8,
         sz_thick = 2,
         sz_thin = 1) 



### Variance components

load("vss.coda.VCV.rda")

vss_var = vss.coda.VCV

#phylogenetic signal
vss_phlyo_mean <- list()
vss_phlyo_slope <- list()

#residual
vss_unit <- list()

#extrace the random terms from across the models
for(i in 1:length(vss_var)){
  
  vss_phlyo_mean[[i]] <-  vss_var[[i]][,c(1)] 
  vss_phlyo_slope[[i]] <-  vss_var[[i]][,c(4)] 
  vss_unit[[i]] <-  vss_var[[i]][,5] 
}

vss_phlyo_mean <- unlist(vss_phlyo_mean)
vss_phlyo_slope <- unlist(vss_phlyo_slope)

vss_unit <- unlist(vss_unit)

vss_prop_phlyo_mean <- vss_phlyo_mean/(vss_phlyo_mean+vss_phlyo_slope+ vss_unit)
vss_prop_phlyo_slope <- vss_phlyo_slope/(vss_phlyo_mean+vss_phlyo_slope+ vss_unit)
vss_prop_residuals  <- vss_unit/(vss_phlyo_mean+vss_phlyo_slope+ vss_unit)

#Phylogenetic signal
hdr(vss_prop_phlyo_mean)$mode
hdr(vss_prop_phlyo_slope)$mode

#Residual term
hdr(vss_prop_residuals)$mode

df=data.frame(vss_prop_phlyo_mean,vss_prop_phlyo_slope,vss_prop_residuals)

ggplot(df,aes(vss_prop_phlyo_mean))+
  geom_density(fill="grey")+
  geom_density(aes(vss_prop_phlyo_slope),fill="orange")+
  geom_density(aes(vss_prop_residuals),fill="blue",alpha=0.5)+
  theme_classic(base_size = 20)+
  xlab("Proportion of random effect variance")+
  ylab("")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank())

