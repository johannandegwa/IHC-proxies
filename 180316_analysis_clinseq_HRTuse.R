#'
#' Program to analyze associations between HRT use andrisk for breast cancer subtype
#' Analysis by different definitions of subtype as defined by IHC proxies st gallen, prolif and IHC3
#' 
#' Author: johanna.holm@ki.se
#' Date: 2018-03-16
#' 


#' Require packages

library(nnet)
library(gmodels)


#' Set up working directories 
WORKDIR = "P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies"
DATADIR = file.path(WORKDIR, "Data/Analysis data")


#' Read the analysis dataset for case control analysis of HRT use
fn = file.path(DATADIR, "180316_analysisdataset_IHCproxies_clinseq_casectrl.rds")
c = readRDS(fn)
head(c)
dim(c)

# subset of cases with random forest predictions available
d<-subset(c,pred_randforst != 'NoBC')

# restrict to subset of cases and controls with predictions available
c<-subset(c,!is.na(pred_stgallen14))

#' Add the helper function testHeterogenity
source("P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Programs/testHeterogenity_160310.R")

#' Create functions for extracting relevant output from multinomial regression models 

tab<-function(x){
  or = exp(coef(x))
  ci = exp(confint(x))
  tab = list(cbind(or[1,], ci[,,1]),cbind(or[2,], ci[,,2]), cbind(or[3,], ci[,,3]), cbind(or[4,], ci[,,4]))
  names(tab) = dimnames(ci)[[3]]
  print(tab)
}

pvalue<-function(x){
  z = with(summary(x), coefficients/standard.errors)
  p = 2*pnorm(-abs(z))
  print(p)
}


#'---------------------------------------------------------------------------
#'               Heterogeneity in associations to HRT use?
#'---------------------------------------------------------------------------



#' HRT use

CrossTable(c$hrtstatus,c$match_casestatus)
CrossTable(c$hrtstatus,c$pred_stgallen14)
CrossTable(c$hrtstatus,c$pred_stgallen20)
CrossTable(c$hrtstatus,c$pred_stgallenfree)
CrossTable(c$hrtstatus,c$pred_prolif14)
CrossTable(c$hrtstatus,c$pred_prolif20)
CrossTable(c$hrtstatus,c$pred_proliffree)
CrossTable(c$hrtstatus,c$pred_IHC3)

#' First the original design of cases vs healthy controls
glm.out <- glm(match_casestatus~ hrtstatus + x_parity + bmicat + soc_edu  + soc_born + age, data = c, family=binomial)
summary(glm.out)

#' look at estimate odds ratios
exp(coef(glm.out))
exp(confint(glm.out))


#' Start look into subtypes by proxies

#' St gallen 14
glm.out <- multinom(pred_stgallen14 ~ hrtstatus + x_parity + bmicat + soc_edu + soc_born  + age, data = c)
summary(glm.out)


#' look at estimate odds ratios
tab(glm.out)
pvalue(glm.out)
testHeterogenity(glm.out, param="hrtstatus")

#' St gallen 20
glm.out <- multinom(pred_stgallen20 ~ hrtstatus + x_parity + bmicat + soc_edu + soc_born  + age, data = c)
summary(glm.out)


tab(glm.out)
pvalue(glm.out)
testHeterogenity(glm.out, param="hrtstatus")

#' St gallen free (40)
glm.out <- multinom(pred_stgallenfree ~ hrtstatus + x_parity + bmicat + soc_edu + soc_born  + age, data = c)
summary(glm.out)


tab(glm.out)
pvalue(glm.out)
testHeterogenity(glm.out, param="hrtstatus")

#' Prolif 14
glm.out <- multinom(pred_prolif14 ~ hrtstatus + x_parity + bmicat + soc_edu + soc_born  + age, data = c)
summary(glm.out)


tab(glm.out)
pvalue(glm.out)
testHeterogenity(glm.out, param="hrtstatus")

#' Prolif 20
glm.out <- multinom(pred_prolif20 ~ hrtstatus + x_parity + bmicat + soc_edu + soc_born  + age, data = c)
summary(glm.out)

tab(glm.out)
pvalue(glm.out)
testHeterogenity(glm.out, param="hrtstatus")

#' Prolif free (40)
glm.out <- multinom(pred_proliffree ~ hrtstatus + x_parity + bmicat + soc_edu + soc_born  + age, data = c)
summary(glm.out)


tab(glm.out)
pvalue(glm.out)
testHeterogenity(glm.out, param="hrtstatus")

#' IHC3
glm.out <- multinom(pred_IHC3 ~ hrtstatus + x_parity + bmicat + soc_edu + soc_born  + age, data = c)
summary(glm.out)


tab(glm.out)
pvalue(glm.out)
testHeterogenity(glm.out, param="hrtstatus")

