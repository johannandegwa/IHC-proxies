# 
# R script for creating Sankey diagram of reclassification between IHC proxy and PAM50,
#          using the alluvial package
# Author:  johanna.holm@ki.se
# Date:    2018-02-06
#


library(alluvial)

# Create data
PAM50=factor(rep(1:4,4), 
             levels=c(1:4),
             labels=c("Luminal A","Luminal B","HER2","Basal")) 

pred=factor(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4)), 
            levels=c(1:4),
            labels=c("Luminal A","Luminal B","HER2","Basal"))

# Set all IHC proxy variables to 'pred'
StGallen20=StGallen14=Prolif14=Prolif20=IHC3=pred

# Frequencies: Derived from cross-tabulations of PAM50 vs IHC proxies

# St gallen 14 in STO-3:
freqstgallen14=c(183,41,2,1,133,79,18,6,1,1,20,2,4,1,16,51)

# Prolif 14 in STO-3:
freqprolif14=c(272,72,10,2,44,48,10,5,1,1,20,2,4,1,16,51)

# St gallen 20 in STO-3:
freqstgallen20=c(199,48,2,1,117,72,18,6,1,1,20,2,4,1,16,51)

# Prolif 20 in STO-3:
freqprolif20=c(294,87,10,3,22,33,10,4,1,1,20,2,4,1,16,51)

# IHC3 in STO-3:
freqihc3 = c(314,117,8,5,3,4,12,1,1,1,20,2,4,1,16,51)

# Combine into data frame
dat <- data.frame(PAM50, 
                  StGallen14, 
                  StGallen20, 
                  Prolif14, 
                  Prolif20, 
                  IHC3, 
                  freqstgallen20, 
                  freqprolif20, 
                  freqprolif14, 
                  freqstgallen14, 
                  freqihc3)

###########
# Plotting: 
###########

# St Gallen 20
alluvial(dat[,c(1,3)], freq=dat$freqstgallen20, # Set nodes (PAM50, IHC proxy) and weights (frequency)
         col = ifelse(dat$PAM50=="Basal","wheat1",
                      ifelse(dat$PAM50=="HER2","darkorange1",
                             ifelse(dat$PAM50=="Luminal B", "skyblue1","darkorchid1"))), # Set color for flows
         alpha = 0.7, # Set opacity
         border = ifelse(dat$PAM50=="Basal","wheat1",
                         ifelse(dat$PAM50=="HER2","darkorange1",
                                ifelse(dat$PAM50=="Luminal B", "skyblue1","darkorchid1"))), # Set border color for flows
         layer = 1:4) # Determine what category of PAM50 should overlap: Lum A over Lum B over HER2 over Basal

# St Gallen 14
alluvial(dat[,1:2], freq=dat$freqstgallen14,
         col = ifelse(dat$PAM50=="Basal","wheat1",
                      ifelse(dat$PAM50=="HER2","darkorange1",
                             ifelse(dat$PAM50=="Luminal B", "skyblue1","darkorchid1"))),
         alpha = 0.7,
         border = ifelse(dat$PAM50=="Basal","wheat1",
                         ifelse(dat$PAM50=="HER2","darkorange1",
                                ifelse(dat$PAM50=="Luminal B", "skyblue1","darkorchid1"))),
         layer = 1:4)

# Prolif 14
alluvial(dat[,c(1,4)], freq=dat$freqprolif14,
                  col = ifelse(dat$PAM50=="Basal","wheat1",
                               ifelse(dat$PAM50=="HER2","darkorange1",
                                      ifelse(dat$PAM50=="Luminal B", "skyblue1","darkorchid1"))),
                  alpha = 0.7,
                  border = ifelse(dat$PAM50=="Basal","wheat1",
                                  ifelse(dat$PAM50=="HER2","darkorange1",
                                         ifelse(dat$PAM50=="Luminal B", "skyblue1","darkorchid1"))),
                  layer = 1:4)

# Prolif 20
alluvial(dat[,c(1,5)], freq=dat$freqprolif20,
         col = ifelse(dat$PAM50=="Basal","wheat1",
                      ifelse(dat$PAM50=="HER2","darkorange1",
                             ifelse(dat$PAM50=="Luminal B", "skyblue1","darkorchid1"))),
         alpha = 0.7,
         border = ifelse(dat$PAM50=="Basal","wheat1",
                         ifelse(dat$PAM50=="HER2","darkorange1",
                                ifelse(dat$PAM50=="Luminal B", "skyblue1","darkorchid1"))),
         layer = 1:4)

# IHC3 
alluvial(dat[,c(1,6)], freq=dat$freqihc3,
                  col = ifelse(dat$PAM50=="Basal","wheat1",
                               ifelse(dat$PAM50=="HER2","darkorange1",
                                      ifelse(dat$PAM50=="Luminal B", "skyblue1","darkorchid1"))),
                  alpha = 0.7,
                  border = ifelse(dat$PAM50=="Basal","wheat1",
                                  ifelse(dat$PAM50=="HER2","darkorange1",
                                         ifelse(dat$PAM50=="Luminal B", "skyblue1","darkorchid1"))),
                  layer = 1:4)
