
# Newest COMADRE

load("/Users/maria/Dropbox/demoBuff/analysesMaria/oikos/SuppMat/COMADRE_v.4.21.8.0.RData")


temp <- subset(comadre$metadata,MatrixComposite == "Individual" & 
                           MatrixTreatment=="Unmanipulated" & 
                           StudyDuration > 2 & ProjectionInterval==1 )

unique(temp$SpeciesAuthor)
  
# species we used so far

sp=read.csv("/Users/maria/Dropbox/demoBuff/analysesMaria/oikos/SuppMat/Species_name.csv")

comadre$metadata[comadre$metadata$SpeciesAccepted%in%"Kobus ellipsiprymnus",]

temp2=temp[!temp$SpeciesAuthor%in%sp$x,]

unique(temp2$SpeciesAuthor)
