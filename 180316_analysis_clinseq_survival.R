# Survival analysis in libro1 for study IV IHC proxies
# date 2018-02-09
# author johanna.holm@ki.se
# data input: dataset with er, pr, her2, ki67, date of diagnosis, date of study entry, date of death, cause of death, emigration date, study id: 180209_analysisdataset_medrec_full.rds

library(survival)
library(survminer)

#' Set up working directories 
WORKDIR = "P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies"
DATADIR = file.path(WORKDIR, "Data/Analysis data")


#' Read the analysis dataset for survival analysis
fn = file.path(DATADIR, "180316_analysisdataset_IHCproxies_clinseq_survival.rds")
dat = readRDS(fn)
head(dat)
dim(dat)

dat$endFU2<-"2017-06-30"

dat$studyentry_date<-as.Date(dat$studyentry_date,"%Y-%m-%d")
dat$emigration_date<-as.Date(as.character(dat$emigration_date),"%Y-%m-%d")
dat$deathdate<-as.Date(dat$deathdate,"%Y-%m-%d")
dat$bc_1stdiagdate<-as.Date(dat$bc_1stdiagdate,"%Y-%m-%d")
dat$endFU2<-as.Date(dat$endFU2,"%Y-%m-%d")


#' set emigration date as NA if before study entry date
dat$emigration_date<-ifelse(dat$emigration_date<dat$studyentry_date,NA,dat$emigration_date)

#' create last seen date
dat$endFU2<- ifelse(!is.na(dat$emigration_date),dat$emigration_date,
                   ifelse(!is.na(dat$deathdate),dat$deathdate,dat$endFU2))
                   
dat$endFU2<-as.Date(dat$endFU2,format="%Y-%m-%d", origin="1970-01-01")

#' Make as POSIX
dat$diagPOS<-as.POSIXlt(dat$bc_1stdiagdate)
dat$entryPOS<-as.POSIXlt(dat$studyentry_date)
dat$exitPOS<-as.POSIXlt(dat$endFU2)

dat$time2<-difftime(dat$exitPOS,dat$entryPOS,unit="days")


dat$time<-difftime(dat$entryPOS,dat$diagPOS,unit="days")

dat$FUdays<-difftime(dat$exitPOS,dat$diagPOS,unit="days")

#'event breast cancer specific death yes or no
dat$event <- ifelse(is.na(dat$deathcause_any),0, 
                    ifelse(dat$deathcause_bc==1, 1, 0))

dat$diagyr<-format(as.Date(dat$bc_1stdiagdate, format="%d/%m/%Y"),"%Y")

#' Two ways of limiting survivorship bias (we have left truncation, not left censored data 
#' - Im not sure left censoring approaches take care of problems with left truncation / Johanna)

#' 1. Diagnoses 2008 only
dat08<-subset(dat,diagyr=="2008")
#' 2 Diagnoses made within 2,5 years of studyentry date
datcut<-subset(dat,time<2.5*365.25)

#' Use Approach 2 (A2) (for now) of restricting to max 2.5 years before study entry
dat<-datcut

dat$pred_stgallen14<-relevel(as.factor(dat$pred_stgallen14),ref="LumA")
cox1<-coxph(Surv(time=FUdays,event=event)~pred_stgallen14, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)



kmstgal14<-survfit(Surv(time=FUdays,event=event)~pred_stgallen14, data=dat)
plot(kmstgal14, main="St Gallen 14")

dat$pred_stgallen20<-relevel(as.factor(dat$pred_stgallen20),ref="LumA")
cox1<-coxph(Surv(time=FUdays,event=event)~pred_stgallen20, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmstgal20<-survfit(Surv(time=FUdays,event=event)~pred_stgallen20, data=dat)
plot(kmstgal20, main="St Gallen 20")

dat$pred_stgallenfree<-relevel(as.factor(dat$pred_stgallenfree),ref="LumA")
cox1<-coxph(Surv(time=FUdays,event=event)~pred_stgallenfree, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmstgal40<-survfit(Surv(time=FUdays,event=event)~pred_stgallenfree, data=dat)
plot(kmstgal40, main="St Gallen 40")


pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/StGallen14KM_Libro1A2.pdf', width = 8, height= 8, onefile=FALSE)
print(ggsurvplot(kmstgal14, data = dat,
           conf.int = FALSE,
           pval = TRUE,
           fun = "pct",
           risk.table = TRUE,
           size = 1,
           xlim = c(0,3700),
           xscale=365.25,
           break.x.by=730.5,
           xlab="Time in years since diagnosis",
           title="StGallen 14",
           linetype = "strata",
           palette = c("#E7B800",
                       "#2E9FDF","magenta","red"),
           legend = "bottom",
           legend.title = "Subtype",
           legend.labs = c("LumA",
                           "Basal","HER2","LumB"))
)
dev.off()
pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/StGallen20KM_Libro1A2.pdf', width = 8, height= 8, onefile=FALSE)

print(ggsurvplot(kmstgal20, data = dat,
                 conf.int = FALSE,
                 pval = TRUE,
                 fun = "pct",
                 risk.table = TRUE,
                 size = 1,
                 xlim = c(0,3700),
                 xscale=365.25,
                 break.x.by=730.5,
                 xlab="Time in years since diagnosis",
                 title="StGallen 20",
                 linetype = "strata",
                 palette = c("#E7B800",
                             "#2E9FDF","magenta","red"),
                 legend = "bottom",
                 legend.title = "Subtype",
                 legend.labs = c("LumA",
                                 "Basal","HER2","LumB"))
)
dev.off()
pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/StGallen40KM_Libro1A2.pdf', width = 8, height= 8, onefile=FALSE)

print(ggsurvplot(kmstgal40, data = dat,
                  conf.int = FALSE,
                  pval = TRUE,
                  fun = "pct",
                  risk.table = TRUE,
                  size = 1,
                  xlim = c(0,3700),
                  xscale=365.25,
                  break.x.by=730.5,
                  xlab="Time in years since diagnosis",
                  title="StGallen 40",
                  linetype = "strata",
                  palette = c("#E7B800",
                              "#2E9FDF","magenta","red"),
                  legend = "bottom",
                  legend.title = "Subtype",
                  legend.labs = c("LumA",
                                  "Basal","HER2","LumB"))
)
dev.off()

dat$pred_prolif14<-relevel(as.factor(dat$pred_prolif14),ref="LumA")
cox1<-coxph(Surv(time=FUdays,event=event)~pred_prolif14, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmprol14<-survfit(Surv(time=FUdays,event=event)~pred_prolif14, data=dat)
plot(kmprol14, xscale=365.25,main="Prolif")

dat$pred_prolif20<-relevel(as.factor(dat$pred_prolif20),ref="LumA")
cox1<-coxph(Surv(time=FUdays,event=event)~pred_prolif20, data=dat)
summary(cox1)

zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmprol20<-survfit(Surv(time=FUdays,event=event)~pred_prolif20, data=dat)
plot(kmprol20, xscale=365.25,main="Prolif")


dat$pred_proliffree<-relevel(as.factor(dat$pred_proliffree),ref="LumA")
cox1<-coxph(Surv(time=FUdays,event=event)~pred_proliffree, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmprol40<-survfit(Surv(time=FUdays,event=event)~pred_proliffree, data=dat)
plot(kmprol40, xscale=365.25,main="Prolif")

pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/Prolif14KM_Libro1A2.pdf', width = 8, height= 8, onefile=FALSE)
print(ggsurvplot(kmprol14, data = dat,
           conf.int = FALSE,
           pval = TRUE,
           fun = "pct",
           risk.table = TRUE,
           size = 1,
           xlim = c(0,3700),
           xscale=365.25,
           break.x.by=730.5,
           xlab="Time in years since diagnosis",
           title="Prolif 14",
           linetype = "strata",
           palette = c("#E7B800",
                       "#2E9FDF","magenta","red"),
           legend = "bottom",
           legend.title = "Subtype",
           legend.labs = c("LumA",
                           "Basal","HER2","LumB"))
)
dev.off()
pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/Prolif20KM_Libro1A2.pdf', width = 8, height= 8, onefile=FALSE)

print(ggsurvplot(kmprol20, data = dat,
                 conf.int = FALSE,
                 pval = TRUE,
                 fun = "pct",
                 risk.table = TRUE,
                 size = 1,
                 xlim = c(0,3700),
                 xscale=365.25,
                 break.x.by=730.5,
                 xlab="Time in years since diagnosis",
                 title="Prolif 20",
                 linetype = "strata",
                 palette = c("#E7B800",
                             "#2E9FDF","magenta","red"),
                 legend = "bottom",
                 legend.title = "Subtype",
                 legend.labs = c("LumA",
                                 "Basal","HER2","LumB"))
)
dev.off()

pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/Prolif40KM_Libro1A2.pdf', width = 8, height= 8, onefile=FALSE)

print(ggsurvplot(kmprol40, data = dat,
                 conf.int = FALSE,
                 pval = TRUE,
                 fun = "pct",
                 risk.table = TRUE,
                 size = 1,
                 xlim = c(0,3700),
                 xscale=365.25,
                 break.x.by=730.5,
                 xlab="Time in years since diagnosis",
                 title="Prolif 40",
                 linetype = "strata",
                 palette = c("#E7B800",
                             "#2E9FDF","magenta","red"),
                 legend = "bottom",
                 legend.title = "Subtype",
                 legend.labs = c("LumA",
                                 "Basal","HER2","LumB"))
)
dev.off()

dat$pred_IHC3<-relevel(as.factor(dat$pred_IHC3),ref="LumA")
cox1<-coxph(Surv(time=FUdays,event=event)~pred_IHC3, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmihc<-survfit(Surv(time=FUdays,event=event)~pred_IHC3, data=dat)
plot(kmihc, xscale=365.25,main="IHC3")

pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/IHC3KM_Libro1A2.pdf', width = 8, height= 8, onefile=FALSE)
print(ggsurvplot(kmihc, data = dat,
           conf.int = FALSE,
           pval = TRUE,
           fun = "pct",
           risk.table = TRUE,
           size = 1,           
           xlim = c(0,3700),
           xscale=365.25,
           break.x.by=730.5,
           xlab="Time in years since diagnosis",
           title="IHC3",
           linetype = "strata",
           palette = c("#E7B800",
                       "#2E9FDF","magenta","red"),
           legend = "bottom",
           legend.title = "Subtype",
           legend.labs = c("LumA",
                           "Basal","HER2","LumB"))
)

dev.off()





#' Model survival from study entry only, still restricting to max 2.5 years before study entry


cox1<-coxph(Surv(time=time2,event=event)~pred_stgallen14, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmstgal14<-survfit(Surv(time=time2,event=event)~pred_stgallen14, data=dat)
plot(kmstgal14, main="St Gallen 14")

cox1<-coxph(Surv(time=time2,event=event)~pred_stgallen20, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmstgal20<-survfit(Surv(time=time2,event=event)~pred_stgallen20, data=dat)
plot(kmstgal20, main="St Gallen 20")
 
cox1<-coxph(Surv(time=time2,event=event)~pred_stgallenfree, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmstgal40<-survfit(Surv(time=time2,event=event)~pred_stgallenfree, data=dat)
plot(kmstgal40, main="St Gallen 40")


pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/StGallen14KM_Libro1A2_studyentry.pdf', width = 8, height= 8, onefile=FALSE)
print(ggsurvplot(kmstgal14, data = dat,
                 conf.int = FALSE,
                 pval = TRUE,
                 fun = "pct",
                 risk.table = TRUE,
                 size = 1,
                 xlim = c(0,2800),
                 xscale=365.25,
                 break.x.by=730.5,
                 xlab="Time in years since study entry",
                 title="StGallen 14",
                 linetype = "strata",
                 palette = c("#E7B800",
                             "#2E9FDF","magenta","red"),
                 legend = "bottom",
                 legend.title = "Subtype",
                 legend.labs = c("LumA",
                                 "Basal","HER2","LumB"))
)
dev.off()
pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/StGallen20KM_Libro1A2_studyentry.pdf', width = 8, height= 8, onefile=FALSE)

print(ggsurvplot(kmstgal20, data = dat,
                 conf.int = FALSE,
                 pval = TRUE,
                 fun = "pct",
                 risk.table = TRUE,
                 size = 1,
                 xlim = c(0,2800),
                 xscale=365.25,
                 break.x.by=730.5,
                 xlab="Time in years since study entry",
                 title="StGallen 20",
                 linetype = "strata",
                 palette = c("#E7B800",
                             "#2E9FDF","magenta","red"),
                 legend = "bottom",
                 legend.title = "Subtype",
                 legend.labs = c("LumA",
                                 "Basal","HER2","LumB"))
)
dev.off()
pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/StGallen40KM_Libro1A2_studyentry.pdf', width = 8, height= 8, onefile=FALSE)

print(ggsurvplot(kmstgal40, data = dat,
                 conf.int = FALSE,
                 pval = TRUE,
                 fun = "pct",
                 risk.table = TRUE,
                 size = 1,
                 xlim = c(0,2800),
                 xscale=365.25,
                 break.x.by=730.5,
                 xlab="Time in years since study entry",
                 title="StGallen 40",
                 linetype = "strata",
                 palette = c("#E7B800",
                             "#2E9FDF","magenta","red"),
                 legend = "bottom",
                 legend.title = "Subtype",
                 legend.labs = c("LumA",
                                 "Basal","HER2","LumB"))
)
dev.off() 

cox1<-coxph(Surv(time=time2,event=event)~pred_prolif14, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmprol14<-survfit(Surv(time=time2,event=event)~pred_prolif14, data=dat)
plot(kmprol14, xscale=365.25,main="Prolif")
 
cox1<-coxph(Surv(time=time2,event=event)~pred_prolif20, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmprol20<-survfit(Surv(time=time2,event=event)~pred_prolif20, data=dat)
plot(kmprol20, xscale=365.25,main="Prolif")


cox1<-coxph(Surv(time=time2,event=event)~pred_proliffree, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)

kmprol40<-survfit(Surv(time=time2,event=event)~pred_proliffree, data=dat)
plot(kmprol40, xscale=365.25,main="Prolif")

pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/Prolif14KM_Libro1A2_studyentry.pdf', width = 8, height= 8, onefile=FALSE)
print(ggsurvplot(kmprol14, data = dat,
                 conf.int = FALSE,
                 pval = TRUE,
                 fun = "pct",
                 risk.table = TRUE,
                 size = 1,
                 xlim = c(0,2800),
                 xscale=365.25,
                 break.x.by=730.5,
                 xlab="Time in years since study entry",
                 title="Prolif 14",
                 linetype = "strata",
                 palette = c("#E7B800",
                             "#2E9FDF","magenta","red"),
                 legend = "bottom",
                 legend.title = "Subtype",
                 legend.labs = c("LumA",
                                 "Basal","HER2","LumB"))
)
dev.off()
pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/Prolif20KM_Libro1A2_studyentry.pdf', width = 8, height= 8, onefile=FALSE)

print(ggsurvplot(kmprol20, data = dat,
                 conf.int = FALSE,
                 pval = TRUE,
                 fun = "pct",
                 risk.table = TRUE,
                 size = 1,
                 xlim = c(0,2800),
                 xscale=365.25,
                 break.x.by=730.5,
                 xlab="Time in years since study entry",
                 title="Prolif 20",
                 linetype = "strata",
                 palette = c("#E7B800",
                             "#2E9FDF","magenta","red"),
                 legend = "bottom",
                 legend.title = "Subtype",
                 legend.labs = c("LumA",
                                 "Basal","HER2","LumB"))
)
dev.off()

pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/Prolif40KM_Libro1A2_studyentry.pdf', width = 8, height= 8, onefile=FALSE)

print(ggsurvplot(kmprol40, data = dat,
                 conf.int = FALSE,
                 pval = TRUE,
                 fun = "pct",
                 risk.table = TRUE,
                 size = 1,
                 xlim = c(0,2800),
                 xscale=365.25,
                 break.x.by=730.5,
                 xlab="Time in years since study entry",
                 title="Prolif 40",
                 linetype = "strata",
                 palette = c("#E7B800",
                             "#2E9FDF","magenta","red"),
                 legend = "bottom",
                 legend.title = "Subtype",
                 legend.labs = c("LumA",
                                 "Basal","HER2","LumB"))
)
dev.off()

cox1<-coxph(Surv(time=time2,event=event)~pred_IHC3, data=dat)
summary(cox1)
zph<-cox.zph(cox1) 
zph
plot(zph[1])
abline(h=0, lty=3)
kmihc<-survfit(Surv(time=time2,event=event)~pred_IHC3, data=dat)
plot(kmihc, xscale=365.25,main="IHC3")

pdf('P:/LIBRO_1/Johanna/Surrogate subtypes/Validity of IHC proxies/Output/Clinseq/IHC3KM_Libro1A2_studyentry.pdf', width = 8, height= 8, onefile=FALSE)
print(ggsurvplot(kmihc, data = dat,
                 conf.int = FALSE,
                 pval = TRUE,
                 fun = "pct",
                 risk.table = TRUE,
                 size = 1,           
                 xlim = c(0,2800),
                 xscale=365.25,
                 break.x.by=730.5,
                 xlab="Time in years since study entry",
                 title="IHC3",
                 linetype = "strata",
                 palette = c("#E7B800",
                             "#2E9FDF","magenta","red"),
                 legend = "bottom",
                 legend.title = "Subtype",
                 legend.labs = c("LumA",
                                 "Basal","HER2","LumB"))
)

dev.off()