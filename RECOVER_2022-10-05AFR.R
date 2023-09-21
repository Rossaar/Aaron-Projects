# Script to pull data from RECOVER and prepare it for analysis
#
# Input: Qualtrics survey and data exports, as well as updated varmap
#  (See Set Up section assignment of import directories and filenames)
#   --Site to download TMB data: https://studies.testmybrain.org/recover/data_portal.html 
#     * logins are studyname_1stinitiallastname
#   --For Qualtrics data, download as: numeric values, remove line breaks, split multi-value fields into columns
#
# Output: data file saved as
#   "RECOVER_pulled2022-10-05.rds"
#
# Reference Files:
#   ATTEND and PREDICT Data Pull Scripts
#
# History:
#   2022-02-28 RCH wrote it

###############################
# I. Set up
###############################

# Library paths, as pulling deidentified data means working on the a VA computer
.libPaths("C:/LocalR/R-4.2.1/Library")

# Install packages if needed:
#setwd("C:/LocalR/")
#install.packages(c("devtools","data.table","ggplot2","reader","readxl","openxlsx","jsonlite","psych","psychTools"))

# Import directories and options
importoptions<-list(
  studies="RECOVER",
  studylettername="C",
  createNew_varmap=FALSE,
  update_varmap=FALSE,
  new_varmap_name="varmap_RECOVER_2022-03-31.rds",
  split_multiplevaluefields=TRUE # This will affect the way the survey is exported AND the way the varmap is created! (Qualtrics specific)
  # DDE settings are not relevant for purely qualtrics data
)
importdirectories<-list(
  #github="C:\\Users\\vhapughendrr\\GitHub\\",
  githubpath="U:\\My Documents\\GitHub\\",
  QualtricsData="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Qualtrics\\Data\\2022-10-05\\",
  QualtricsSurveys="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Qualtrics\\Surveys\\2022-03-31\\",
  TMBdata="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\TMB\\2022-10-05\\",
  TMBdatadictionary="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\TMB\\DataDictionary\\",
#  QualtricsLists="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Qualtrics\\List exports\\",
  workingdatasaves="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Analyses\\2022-10\\"
)
importfilenames<-list(
  QualtricsData="ALL",
  QualtricsSurveys="ALL",
  #QualtricsLists="ATTEND_Baseline_2020_12_29.csv",
  varmap="varmap_RECOVER_2022-03-28.rds"
#  surveymap="SurveyMap_ATTEND_2021-10-29.csv"
)
saveoptions=list(savetag="AASposter")
savefilename <- "RECOVER_pulled2022-10-05.rds"

# Libraries
#library(devtools)
library(data.table)
library(ggplot2)
library(ggpubr) # really only need for analysis part... err...
library(readr)
library(readxl)
library(openxlsx)
library(jsonlite)
library(psych) # need for factorization
library(jtools)
library(psychTools)
library(stringr)
library(lubridate)

# Just source these, because package version never up to date:
source(paste0(importdirectories$github,"HendricksonLab\\HLUtilities\\R\\Utilities.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\GeneralDataFunctions.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\qualtricsFunctions.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\VarMapFunctions.R"))

# Reminder of how to install packages that are part of our own github respository (need devtools loaded to do this):
#install(pkg=paste0(importdirectories$github,"\\","HendricksonLab\\HLUtilities"))
#install(pkg=paste0(importdirectories$github,"\\","HendricksonLab\\CoreInfastructure"))
# If you have fiddled with CoreInfastructre, you may need to reinstall it, as well - do this by hitting 'Ctrl + Shift + B'

#################################
# II. Load and process the full data set
#################################

# 1. Load the data in and clean / standardize it
# Initialize the data package
dp<-initializeDataPackage(importdirectories,importoptions)
# Load the survey info
dp <- loadQualtricsSurveys(dp,importdirectories,importfilenames)
# Load up the data!
dp <- loadQualtricsData(dp,importdirectories,importfilenames)
  # This standardizes the data dictionary, as well

# 2. Apply the varmap to standardize variable names, do common data calculations
#dp <- UpdateVarmap(dp,importdirectories,importoptions,importfilenames)
dp<-standardizeVariableNames(dp,importoptions,importdirectories)
  # Did you get the warning to hand-edit the varmap? Great! Once you've done it, run this code:
  # dp <- UpdateVarmap(dp,importdirectories,importoptions,importfilenames)
  # importoptions$createNew_varmap=FALSE
#  # This converts the variables to our "HL Standard"
dp<-standardDataCalculations(dp)
# This does standard calculations, like measure totals

# Figure out how to add in TMB stuff....
loadTMBdata=function(dp,importdirectories,importfilenames){
  fs <- list.files(importdirectories$TMBdata)
  dp$dataTMBraw <- as.data.table(read.csv(paste0(importdirectories$TMBdata,fs),
                                    stringsAsFactors=FALSE,
                                    header=TRUE,
                                    encoding="UTF-8",na.strings=c("-999","NA"),
                                    strip.white =TRUE))
  #setnames(dp$dataTMB,names(dp$dataTMB),paste0("TMB_",names(dp$dataTMB)))
  fs <- list.files(importdirectories$TMBdatadictionary)
  dp$dictionaryTMB <- as.data.table(read_excel(paste0(importdirectories$TMBdatadictionary,fs),sheet = "Report's Columns description"))
  #dp$dictionaryTMB[,VarID:=paste0("TMB_",Column)]
  dp$dictionaryTMB[,VarID:=Column]
  dp$dictionaryTMB[,Modality:="TMB"]
  dp$dictionaryTMB[,Category:="Cognitive"]
  return(dp)
}
extractsession=function(x){
  temp=fromJSON(x)
  session=temp$session[[1]]
  return(session)
}
removespacespunctuation=function(x){
  x=str_replace_all(x,"[^[:alnum:]]","")
}
processTMBdata=function(dp,importoptions){
  temp=dp$dataTMBraw[,.(user_id,start_time,test_name,score,URL_params)]
  temp[,TMB_date:=as.Date(start_time)]
  temp[,VisitID:=as.integer(NA)]
  temp$VisitID <- sapply(temp$URL_params,extractsession)
  temp[,PtID:=paste0(importoptions$studylettername,user_id)]
  temp[,test_name:=paste0("TMB_",removespacespunctuation(test_name))]
  temptemp=temp[,.(PtID,VisitID,test_name)]
  if(nrow(temptemp)!=nrow(unique(temptemp))){ # means somehow we have a redundant entry...
    warning("there are redundancies in the TMB data, unsure why, just taking 1st entry; look by hand?")
  } # as of 10-5-22 just one, PtID C373 for one test....
  dp$dataTMB <- dcast(temp,PtID+VisitID+TMB_date~test_name,value.var="score",fun.aggregate=function(x) x[1])
  return(dp)
}
dp <- loadTMBdata(dp,importdirectories,importfilenames)
dp <- processTMBdata(dp,importoptions)

###########################
# III. Bit of processing to merge...
###########################

# Make times into times
dp$dataQL$qaStartDate <- as.Date(dp$dataQL$StartDate)
dp$dataQL$qaEndDate <- as.Date(dp$dataQL$EndDate)

# gender should be factor not numeric:
if(is.numeric(dp$dataQL$gender)){
  map <- c("F","M","NB","O")
  dp$dataQL$gender <- map[dp$dataQL$gender]
}
# Alt version for when NB is too small N:
dp$dataQL[,gendernnb:=gender]
dp$dataQL[(gender=="NB"|gender=="O"),gendernnb:=NA]

# PtID does better if character (so treated as factor in regressions / by ggplot):
dp$dataQL[,PtID:=paste0(importoptions$studylettername,dp$dataQL$ExternalDataReference)]

# Deal with the fact we have two sets of some of the PROMIS pain measures, bc of the CRF update
dp$dataQL <- mergedupcol(dp$dataQL)

# Merge...
dp$data <- merge(dp$dataQL,dp$dataTMB,all.x=TRUE,all.y = FALSE,by=c("PtID","VisitID"))
warning("Not doing a good job merging the data dictionaries yet....")

### Extra still tentative fiddling:
dp$data[,asiNarrow:=sum(c(asi1i1,asi1i2+asi1i3,asi1i7,asi1i8,asi2i1,asi2i2,asi2i3,asi2i7,asi2i8,asi3total),na.rm = TRUE),by=1:nrow(dp$data)]
dp$data[,asiN:=sum(c(asi1i1,asi1i2+asi1i3,asi1i4,asi1i5,asi1i6,asi1i7,asi1i8,asi2i1,asi2i2,asi2i3,asi2i4,asi2i5,asi2i6,asi2i7,asi2i8,asi3total),na.rm = TRUE),by=1:nrow(dp$data)]
dp$data[,asiN2:=sum(c(asi1i1,asi1i2+asi1i3,asi1i7,asi1i8,asi2i1,asi2i2,asi2i3,asi2i7,asi2i8),na.rm = TRUE),by=1:nrow(dp$data)]

dp$data[,asi1totalnosleep:=(asi1i1+asi1i2+asi1i3+asi1i4+asi1i5+asi1i6+asi1i7+asi1i8+asi1i9+asi1i10+asi1i11+asi1i12+asi1i15+asi1i16+asi1i17+asi1i18+asi1i19+asi1i22+asi1i23+asi1i27+asi1i28+asi1i29+asi1i30+asi1i31),by=1:nrow(dp$data)]
dp$data[,asi2totalnosleep:=sum(c(asi2i1,asi2i2,asi2i3,asi2i4,asi2i5,asi2i6,asi2i7,asi2i8,asi2i9,asi2i10,asi2i11,asi2i12,asi2i15,asi2i16,asi2i17,asi2i18,asi2i19,asi2i22,asi2i23,asi2i27,asi2i28,asi2i29,asi2i30,asi2i31),na.rm=TRUE),by=1:nrow(dp$data)]
dp$data[,asi3totalnosleep:=sum(c(asi3i1,asi3i2,asi3i3,asi3i4,asi3i5,asi3i6,asi3i7,asi3i8,asi3i9,asi3i10),na.rm=TRUE),by=1:nrow(dp$data)]
dp$data[,asiSxTotalnosleep:=(asi1totalnosleep+asi2totalnosleep+2*asi3totalnosleep),by=1:nrow(dp$data)]
dp$data[,asiN1mostadrenergic:=sum(c(asi1i1,asi1i2+asi1i3,asi1i7,asi1i8,asi2i1,asi2i2,asi2i3,asi2i7,asi2i8,asi3total),na.rm = TRUE),by=1:nrow(dp$data)]
dp$data[,asiN2peripheral:=sum(c(asi1i1,asi1i2+asi1i3,asi1i4,asi1i5,asi1i6,asi1i7,asi1i8,asi1i27,asi1i28,asi1i28,asi2i1,asi2i2,asi2i3,asi2i4,asi2i5,asi2i6,asi2i7,asi2i8,asi2i27,asi2i28,asi2i29,asi3total),na.rm = TRUE),by=1:nrow(dp$data)]
dp$data[,asiN3mostadrenergicnotpostural:=sum(c(asi1i1,asi1i2+asi1i3,asi1i7,asi1i8,asi2i1,asi2i2,asi2i3,asi2i7,asi2i8),na.rm = TRUE),by=1:nrow(dp$data)]
dp$data[,asiN4peripheralnotpostural:=sum(c(asi1i1,asi1i2+asi1i3,asi1i4,asi1i5,asi1i6,asi1i7,asi1i8,asi1i27,asi1i28,asi1i28,asi2i1,asi2i2,asi2i3,asi2i4,asi2i5,asi2i6,asi2i7,asi2i8,asi2i27,asi2i28,asi2i29),na.rm = TRUE),by=1:nrow(dp$data)]

dp$data[,pcl5notE:=(pcl5clusterB+pcl5clusterC+pcl5clusterD)]

###################################

# Create normalized versions of the clusters/factors/covariates that we may need to use multiple places:
tempdt <- copy(dp$data)
classes <- sapply(tempdt,class)
#unique(classes)
var2norm <- names(classes[which(classes%in%c("numeric","logical","integer"))])
var2norm <- setdiff(var2norm,
                    c())
for(iv in 1:length(var2norm)){
  x <- tempdt[,((var2norm[iv])),with=FALSE]
  y <- scale(x,center=TRUE,scale=TRUE)
  tempdt[,(var2norm[iv]):=y]
}
dp$normed <- tempdt

#saveRDS(dp,paste0(importdirectories$workingdatasaves,savefilename))
dp=readRDS(paste0(importdirectories$workingdatasaves,savefilename))

#####################################
# IV. Stuff that we'll move to analysis once we know the data looks ok the first few shakes out of the box
#####################################

# Alt version for when NB is too small N:
dp$data[,gendernnb:=gender]
dp$data[(gender=="NB"|gender=="O"),gendernnb:=NA]
# Alt version for when NB is too small N:
dp$normed[,gendernnb:=gender]
dp$normed[(gender=="NB"|gender=="O"),gendernnb:=NA]


# Sneak: PASC rates
t=dp$data[COVID_categorization==TRUE]
dim(t)
t$PASCSxi1 # thru 9; count 3s or 4s
t[,P1:=PASCSxi1>2]
t[,P2:=PASCSxi2>2]
t[,P3:=PASCSxi3>2]
t[,P4:=PASCSxi4>2]
t[,P5:=PASCSxi5>2]
t[,P6:=PASCSxi6>2]
t[,P7:=PASCSxi7>2]
t[,P8:=PASCSxi8>2]
t[,P9:=PASCSxi9>2]

t[,pasc:=any(c(P1,P2,P3,P4,P5,P6,P7,P8,P9)),by=1:nrow(t)]

t$pasc

sum(t$pasc==TRUE,na.rm=TRUE)/sum(!is.na(t$pasc))
t[pasc==TRUE,.(PtID,COMPASS31Score)]

library(gridExtra)
p1=ggplot(data=dp$data,aes(x=COVID_categorization,y=NeuroQoLtotal))+geom_boxplot()+
  geom_jitter(color="black",size=.4,alpha=.7,width=.2)+theme_classic()+
  xlab("History of COVID-19")+ylab('NeuroQoL Total Score')
p2=ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=COMPASS31Score,y=NeuroQoLtotal))+
  geom_point()+stat_cor(method="pearson",label.x.npc=.5)+ 
  geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)+theme_classic()+
  xlab("Autonomic Symptom Burden\n(COMPASS-31 Total Score)")+ylab("NeuroQoL Total Score")

# Tag them with panel labels
p1=p1+labs("A")
p2=p2+labs("B")
ml <- grid.arrange(grobs=list(p1,p2),
                   nrow=1,ncol=2,
                   widths=c(.4,.6))

# Save, if going to
if(saveoptions$resave){
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_NeuroQoLvsCOVIDhxandCOMPASS.jpeg"),
         ml,width=6,height=4,units="in",dp=600)
}

# Version for us:
p1=ggplot(data=dp$data,aes(x=COVID_categorization,y=COMPASS31Score))+geom_boxplot(size=.3)+
  geom_jitter(color="black",size=.4,alpha=.7,width=.2)+theme_classic()+
  xlab("History of COVID-19")+ylab('COMPASS-31 Score')
p2=ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=COMPASS31Score,y=NeuroQoLtotal))+
  geom_point(size=1)+stat_cor(method="pearson",label.x.npc=0)+ 
  geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)+theme_classic()+
  xlab("COMPASS-31 Score")+ylab("NeuroQoL Total Score")

# Tag them with panel labels
p1=p1+labs("A")
p2=p2+labs("B")
ml <- grid.arrange(grobs=list(p1,p2),
                   nrow=1,ncol=2,
                   widths=c(.5,.5))

# Save, if going to
if(saveoptions$resave){
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_COMPASSvsCOVIDandNeuroQoL.jpeg"),
         ml,width=4,height=3,units="in",dp=600)
}

# Slight tangent to look the 'other direction':
dp$data[,lecAtotal3cat:=as.character(lecAtotal),by=1:nrow(dp$data)]
dp$data[lecAtotal>=2,lecAtotal3cat:="2+"]
p1=ggplot(data=dp$data,aes(x=COVID_categorization,y=COMPASS31Score))+geom_boxplot(size=.3)+
  geom_jitter(color="black",size=.4,alpha=.7,width=.2)+theme_classic()+
  xlab("History of COVID-19")+ylab('COMPASS-31 Score')+
  facet_wrap(~lecAtotal3cat)
p1
anovaout=aov(COMPASS31Score~COVID_categorization+lecAtotal3cat,data=dp$data)
summary(anovaout)
dp$data[,lecAtotalbinary:=as.logical(lecAtotal),by=1:nrow(dp$data)]
summary(lm(COMPASS31Score~age+gender+COVID_categorization*lecAtotalbinary,data=dp$data))

# Save, if going to
if(saveoptions$resave){
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_COMPASSvsCOVIDandTHx3cat.jpeg"),
         p1,width=4,height=3,units="in",dp=600)
}


# For comparison, the non-COVID group:
p3=ggplot(data=dp$data[COVID_categorization==FALSE],aes(x=COMPASS31Score,y=NeuroQoLtotal))+
  geom_point(size=1)+stat_cor(method="pearson",label.x.npc=0)+ 
  geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)+theme_classic()+
  xlab("COMPASS-31 Score")+ylab("NeuroQoL Total Score")

if(saveoptions$resave){
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_COMPASSvsCOVIDandNeuroQoLinNONCOVIDgroup.jpeg"),
         p3,width=4,height=3,units="in",dp=600)
}


# Ok - first, want to make a pile of (a) exposure variables, and (b) outcome variables to look at relationships between...

# General demographics, just for variable reference:
# - [age], [gender]

# Exposures / RFs:
# - Trauma hx (LEC [lecAtotal], PTSD dx [PHx_psych_PTSD], anxiety [PHx_psych_anxiety], panic [PHx_psych_panic])
# - TBI [VarID: nTBIwLOCmerged = nTBI with LOC; nTBInoLOCmerged = nTBI without LOC (mirecc soft def)]
# - Veteran [VarID: veteran]
# - prior autonomic dx [PHx_cards_POTS, PHx_cards_tachycardia, PHx_rheum_MCAS, PHx_gi_IBS]

# Outcomes: 
# (Reminder - priorities are: cognitive, new/worse MHSx, fatigue/MECFS, fcnl impairment, sleep, pain)
# - ASI Sx
# - COMPASS [COMPASS32Score vs COMPASS_total????]
# - ISI [isi_total]
# - dx of post-covid: PTSD, anx, panic, POTS... [NDx_psych_PTSD, NDx_psych_anxiety, NDx_psych_panic, 
#                     NDx_cards_POTS, NDx_cards_tachycardia, NDx_rheum_MCAS, NDx_rheum_MECFS, NDx_gi_IBS]
# - PROMIS measures: PROMIS_painInt3a_total, PROMIS_fatigue6a_total

summary(lm(PROMIS_painInt3a_total~asi1total+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(PROMIS_fatigue6a_total~asi1total+age+gender+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(PROMIS_painInt3a_total~age+gender+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(PROMIS_fatigue6a_total~age+gender+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))

summary(lm(COMPASS31Score~age+gender+COVID_categorization*lecAtotal,data=dp$normed))


# Look at stuff!
p1=ggplot(data=dp$data,aes(x=lecAtotal,y=COMPASS31Score,color=COVID_categorization))+
  geom_point()+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
p1
p2=ggplot(data=dp$data,aes(x=lecAtotal,y=COMPASS31Score))+
  geom_point()+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)+facet_wrap(~COVID_categorization)
p2
if(saveoptions$resave){
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_COMPASSvsLECAbyCOVIDhx.jpeg"),
         p2,width=7,height=5,units="in",dp=600)
}


ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=lecAtotal,y=COMPASS31Score))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==FALSE],aes(x=lecAtotal,y=COMPASS31Score))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

ggplot(data=dp$data,aes(x=lecAtotal,y=asiN))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=lecAtotal,y=asiNarrow))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==FALSE],aes(x=lecAtotal,y=asiNarrow))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

ggplot(data=dp$data,aes(x=asiN,y=COMPASS31Score))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=asiN,y=COMPASS31Score))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==FALSE],aes(x=asiN,y=COMPASS31Score))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)


ggplot(data=dp$data,aes(x=COMPASS31Score,y=asi1total,color=COVID_categorization))+
  geom_point()+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=COMPASS31Score,y=asiSxTotal))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=COMPASS31Score,y=asi3total,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

ggplot(data=dp$data,aes(x=lecAtotal,y=asi1total,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
# asi2 is sig but not 1?
ggplot(data=dp$data,aes(x=lecAtotal,y=asi2total,color=COVID_categorization))+ 
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=lecAtotal,y=asi3total,color=COVID_categorization))+ 
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=lecAtotal,y=asiSxTotal,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=lecAtotal,y=asi2total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

ggplot(data=dp$data,aes(x=lecAtotal,y=COMPASS31Score,color=COVID_categorization))+
  geom_point(aes())+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

ggplot(data=dp$data,aes(x=pcl5total,y=COMPASS31Score))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=pcl5total,y=COMPASS31Score))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==FALSE],aes(x=pcl5total,y=COMPASS31Score))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=pcl5total,y=asiSxTotal))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)




ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=lecAtotal,y=asiSxTotal))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=nTBI,y=COMPASS31Score,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)


ggplot(data=dp$data,aes(x=lecAtotal,y=PROMIS_fatigue6a_total,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=lecAtotal,y=PROMIS_painInt3a_total,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=lecAtotal,y=PROMIS_fatigue6a_total))+
  geom_point(aes(color=(SelfDescribeLongHaul==1)))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=lecAtotal,y=PROMIS_painInt3a_total))+
  geom_point(aes(color=(SelfDescribeLongHaul==1)))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)


ggplot(data=dp$data[SelfDescribeLongHaul==TRUE],aes(x=lecAtotal,y=PROMIS_fatigue6a_total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[SelfDescribeLongHaul==TRUE],aes(x=lecAtotal,y=PROMIS_painInt3a_total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)


# p1=ggplot(data=data[VisitID%in%VisitSpan],aes(x=caps5total,y=mabaHABindex))+
#   geom_point()+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)+
#   theme_classic()+
#   xlab("PTSD Symptom Severity\n(CAPS-5 Total)")+
#   ylab("HAB - reported concern")
# p1


summary(lm(COMPASS31Score~lecAtotal+COVID_categorization+age+gendernnb,data=dp$normed))
summary(lm(COMPASS31Score~lecAtotal+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS31Score~lecAtotal+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$data)) # <---
summary(lm(COMPASS31Score~Dx_psych_PTSD+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$data)) # <---
summary(lm(COMPASS31Score~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$data)) ## USE THIS ONE OR SOMETHING CLOSE

summary(lm(COMPASS31Score~Dx_psych_PTSD*COVID_categorization+nTBIwLOC+COVID_categorization+age+gender,data=dp$data)) # <---
summary(lm(COMPASS31Score~PHx_psych_PTSD*COVID_categorization+nTBIwLOC+COVID_categorization+age+gender,data=dp$data)) # <---

# in doc: ******************
summary(lm(COMPASS31Score~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asiSxTotal~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi1total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi2total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi3total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE

# even better are these 2:
summary(lm(COMPASS31Score~pcl5notE+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi1total~pcl5notE+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE



summary(lm(COMPASS31Score~pcl5total+nTBIwLOC+age+gendernnb,data=dp$data[COVID_categorization==TRUE])) ## USE THIS ONE OR SOMETHING CLOSE

summary(lm(COMPASS31Score~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi1total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asi1totalnoncog~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asi2total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asi2totalnoncog~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asi3total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asi3totalnoncog~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiSxTotal~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiSxTotalnoncog~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----



summary(lm(COMPASS31Score~pcl5notE+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi1total~pcl5notE+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiN2peripheral~pcl5notE+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----*****

summary(lm(asiN~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$data)) ## USE THIS ONE OR SOMETHING CLOSE <----*****
summary(lm(asiN~pcl5total+COVID_categorization+age+gendernnb,data=dp$data)) ## USE THIS ONE OR SOMETHING CLOSE <----*****


# Visualize (have to normalize first):
reltextsize=1.3
labelsize=9
compassfit <- lm(COMPASS31Score~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)
asi1fit <- lm(asi1total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)
asiN1mostadrenergic <- lm(asiN1mostadrenergic~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)
asiN2peripheral <- lm(asiN2peripheral~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)
asiN3mostadrenergicnotpostural <- lm(asiN3mostadrenergicnotpostural~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)
asiN4peripheralnotpostural <- lm(asiN4peripheralnotpostural~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)

plotsumms <- plot_summs(compassfit,asi1fit,asiN1mostadrenergic,asiN2peripheral,asiN3mostadrenergicnotpostural,asiN4peripheralnotpostural,
                        legend.title="Predicted Variable",
                        model.names=c("COMPASS Total","ASI1 Total","asiN1mostadr","asiN2periph","asiN3mostadrNotPost","asin4pierphNotPost"))+
  scale_y_discrete(name="",labels=rev(c("PCL5 Total","# TBI w/LOC","COVID hx","Age","Gender:\nMale")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms

if(saveoptions$resave){
  w=9
  h=9
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_whatpredictshyperadrenergicSx.jpeg"),
         plotsumms,width=w,height=h,units="in",dp=600)
}

# Similar with ISI, TMB data...
summary(lm(isi_total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) # <-- both end up sig
summary(lm(isi_total~pcl5clusterD+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed))

# When adjusting for age and gender, COVID_categorization (remember, this is a pretty gross categorization) only
# has a v weak maybe assoc with FastChoices, with P=.098
summary(lm(TMB_Continuousconcentration~age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_FastChoices~age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_QuickAdditionTask~age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_RememberingWordsRecall~age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_VisualPatterns~age+gendernnb+COVID_categorization,data=dp$normed))

# By contrast, QuickAddition and VisualPatterns have strong relationships to nTBIwLOC
summary(lm(TMB_Continuousconcentration~age+gendernnb+nTBIwLOC,data=dp$normed))
summary(lm(TMB_FastChoices~age+gendernnb+nTBIwLOC,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~age+gendernnb+nTBIwLOC,data=dp$normed))
summary(lm(TMB_QuickAdditionTask~age+gendernnb+nTBIwLOC,data=dp$normed))
summary(lm(TMB_RememberingWordsRecall~age+gendernnb+nTBIwLOC,data=dp$normed))
summary(lm(TMB_VisualPatterns~age+gendernnb+nTBIwLOC,data=dp$normed))

# By contrast, and pcl5 has impacts on almost everything (but not quite)!
summary(lm(TMB_Continuousconcentration~age+gendernnb+nTBIwLOC+pcl5total,data=dp$normed))
summary(lm(TMB_FastChoices~age+gendernnb+nTBIwLOC+pcl5total,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~age+gendernnb+nTBIwLOC+pcl5total,data=dp$normed))
summary(lm(TMB_QuickAdditionTask~age+gendernnb+nTBIwLOC+pcl5total,data=dp$normed))
summary(lm(TMB_RememberingWordsRecall~age+gendernnb+nTBIwLOC+pcl5total,data=dp$normed))
summary(lm(TMB_VisualPatterns~age+gendernnb+nTBIwLOC+pcl5total,data=dp$normed))

summary(lm(TMB_Continuousconcentration~asi1total+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(TMB_FastChoices~asi1total+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~asi1total+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(TMB_QuickAdditionTask~asi1total+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(TMB_RememberingWordsRecall~asi1total+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(TMB_VisualPatterns~asi1total+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))

summary(lm(TMB_Continuousconcentration~asi1totalnoncog+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(TMB_FastChoices~asi1totalnoncog+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~asi1totalnoncog+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(TMB_QuickAdditionTask~asi1totalnoncog+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(TMB_RememberingWordsRecall~asi1totalnoncog+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(TMB_VisualPatterns~asi1totalnoncog+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))

summary(lm(TMB_MatchingShapesandNumbers~asi1totalnoncog+COVID_categorization+age+gendernnb,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~asi1totalnoncog+age+gendernnb,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~asi1totalnoncog+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))

# ASI1totalnoncog is related to 3 of them
summary(lm(TMB_Continuousconcentration~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_FastChoices~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_QuickAdditionTask~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_RememberingWordsRecall~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_VisualPatterns~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))

# ASI2totalnoncog is related to all of them
summary(lm(TMB_Continuousconcentration~asi2totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_FastChoices~asi2totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~asi2totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_QuickAdditionTask~asi2totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_RememberingWordsRecall~asi2totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_VisualPatterns~asi2totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))

# ASI3totalnoncog is related to all of them also
summary(lm(TMB_Continuousconcentration~asi3totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_FastChoices~asi3totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~asi3totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_QuickAdditionTask~asi3totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_RememberingWordsRecall~asi3totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_VisualPatterns~asi3totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))

# ASI Sx Total: Cont Conc, Matching Shapes and Numbers, Quick Addition Task
summary(lm(TMB_Continuousconcentration~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_FastChoices~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_QuickAdditionTask~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_RememberingWordsRecall~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_VisualPatterns~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed))

# Visualize this one: ASI1totalnoncog is related to Continuous Concentration, Quick Addition, and trend for Matching Shapes and Numbers
CC=lm(TMB_Continuousconcentration~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)
FC=lm(TMB_FastChoices~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)
MS=lm(TMB_MatchingShapesandNumbers~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)
QA=lm(TMB_QuickAdditionTask~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)
RW=lm(TMB_RememberingWordsRecall~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)
VP=lm(TMB_VisualPatterns~asi1totalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)

plotsumms <- plot_summs(CC,FC,MS,QA,RW,VP,
                        legend.title="Predicted Variable",
                        model.names=c("Continuous Concentration","Fast Choices","Matching Shapes and Numbers","Quick Addition Task","Remembering Words Recall","Visual Patterns"))+
  scale_y_discrete(name="",labels=rev(c("ASI1 non-cognitive","Age","Gender:\nMale","COVID history")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms

if(saveoptions$resave){
  w=9
  h=9
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_HAsx2TMB.jpeg"),
         plotsumms,width=w,height=h,units="in",dp=600)
}

# Hone in on these 3:
CCfull=lm(TMB_Continuousconcentration~asi1totalnoncog+pcl5total+nTBIwLOC+age+gendernnb,data=dp$normed)
MSfull=lm(TMB_MatchingShapesandNumbers~asi1totalnoncog+pcl5total+nTBIwLOC+age+gendernnb,data=dp$normed)
QAfull=lm(TMB_QuickAdditionTask~asi1totalnoncog+pcl5total+nTBIwLOC+age+gendernnb,data=dp$normed)
plotsumms <- plot_summs(CCfull,MSfull,QAfull,
                        legend.title="Predicted Variable",
                        model.names=c("Continuous Concentration","Matching Shapes and Numbers","Quick Addition Task"))+
  scale_y_discrete(name="",labels=rev(c("ASI1 non-cognitive","PCL5","#TBIwLOC","Age","Gender:\nMale")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms

if(saveoptions$resave){
  w=9
  h=9
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_HAsvPCLvTBI2TMB.jpeg"),
         plotsumms,width=w,height=h,units="in",dp=600)
}

# Try with ASISxtotalnoncog is related to Continuous Concentration, Quick Addition, and trend for Matching Shapes and Numbers
CC=lm(TMB_Continuousconcentration~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)
FC=lm(TMB_FastChoices~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)
MS=lm(TMB_MatchingShapesandNumbers~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)
QA=lm(TMB_QuickAdditionTask~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)
RW=lm(TMB_RememberingWordsRecall~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)
VP=lm(TMB_VisualPatterns~asiSxTotalnoncog+age+gendernnb+COVID_categorization,data=dp$normed)

plotsumms <- plot_summs(CC,FC,MS,QA,RW,VP,
                        legend.title="Predicted Variable",
                        model.names=c("Continuous Concentration","Fast Choices","Matching Shapes and Numbers","Quick Addition Task","Remembering Words Recall","Visual Patterns"))+
  scale_y_discrete(name="",labels=rev(c("ASI1 non-cognitive","Age","Gender:\nMale","COVID history")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms

if(saveoptions$resave){
  w=9
  h=9
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_ASISxnoncogxTMB.jpeg"),
         plotsumms,width=w,height=h,units="in",dp=600)
}

# Try with ASISxtotalnoncog
CC=lm(TMB_Continuousconcentration~asiSxTotalnoncog+age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)
FC=lm(TMB_FastChoices~asiSxTotalnoncog+age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)
MS=lm(TMB_MatchingShapesandNumbers~asiSxTotalnoncog+age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)
QA=lm(TMB_QuickAdditionTask~asiSxTotalnoncog+age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)
RW=lm(TMB_RememberingWordsRecall~asiSxTotalnoncog+age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)
VP=lm(TMB_VisualPatterns~asiSxTotalnoncog+age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)

plotsumms <- plot_summs(CC,FC,MS,QA,RW,VP,
                        legend.title="Predicted Variable",
                        model.names=c("Continuous Concentration","Fast Choices","Matching Shapes and Numbers","Quick Addition Task","Remembering Words Recall","Visual Patterns"))+
  scale_y_discrete(name="",labels=rev(c("ASISx non-cognitive","Age","Gender:\nMale","COVID history","PCL5total","nTBIwLOC")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms

if(saveoptions$resave){
  w=9
  h=9
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_ASISxnoncogxTMBexp.jpeg"),
         plotsumms,width=w,height=h,units="in",dp=600)
}

# Try with ASISxtotalnoncog
CC=lm(TMB_Continuousconcentration~age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)
FC=lm(TMB_FastChoices~age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)
MS=lm(TMB_MatchingShapesandNumbers~age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)
QA=lm(TMB_QuickAdditionTask~age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)
RW=lm(TMB_RememberingWordsRecall~age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)
VP=lm(TMB_VisualPatterns~age+gendernnb+COVID_categorization+pcl5total+nTBIwLOC,data=dp$normed)

plotsumms <- plot_summs(CC,FC,MS,QA,RW,VP,
                        legend.title="Predicted Variable",
                        model.names=c("Continuous Concentration","Fast Choices","Matching Shapes and Numbers","Quick Addition Task","Remembering Words Recall","Visual Patterns"))+
  scale_y_discrete(name="",labels=rev(c("Age","Gender:\nMale","COVID history","PCL5total","nTBIwLOC")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms

if(saveoptions$resave){
  w=9
  h=9
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_ASISxnoncogxTMBexpMinusASISxnoncog.jpeg"),
         plotsumms,width=w,height=h,units="in",dp=600)
}


#summary(lm(asi2total~pcl5total+nTBIwLOC+COVID_categorization+age+gender,data=dp$data)) ## USE THIS ONE OR SOMETHING CLOSE <----


ggplot(data=dp$data,aes(x=lecAtotal,y=isi_total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
summary(lm(isi_total~lecAtotal+COVID_categorization+age+gendernnb,data=dp$data))

ggplot(data=dp$data,aes(x=NDx_rheum_MECFS,y=lecAtotal))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
summary(lm(NDx_rheum_MECFS~lecAtotal,data=dp$data[COVID_categorization==TRUE]))

## FOCUS HERE: trend for sig PHx_PTSD  to predict COMPASS score w/in COVID population... and nTBIwLOC only in non-covid? or just sig issue?
summary(lm(COMPASS31Score~PHx_psych_PTSD+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS31Score~PHx_psych_PTSD+age+gendernnb,data=dp$normed[COVID_categorization!=TRUE]))
summary(lm(asi1total~PHx_psych_PTSD+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS31Score~nTBIwLOC,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS31Score~PHx_psych_PTSD+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS31Score~Dx_psych_PTSD+age+gendernnb,data=dp$data[COVID_categorization==FALSE]))
summary(lm(COMPASS31Score~age+gendernnb+nTBIwLOC,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS31Score~age+gendernnb+nTBIwLOC,data=dp$data[COVID_categorization==FALSE]))

summary(lm(COMPASS31Score~age+gendernnb+nTBIwLOC,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS31Score~age+gendernnb+nTBIwLOC,data=dp$data[COVID_categorization==FALSE]))


m1=mean(dp$data[COVID_categorization==TRUE][PHx_psych_PTSD==TRUE]$COMPASS31Score,na.rm=TRUE)
m2=mean(dp$data[COVID_categorization==TRUE][PHx_psych_PTSD==FALSE]$COMPASS31Score,na.rm=TRUE)
s1=sd(dp$data[COVID_categorization==TRUE][PHx_psych_PTSD==TRUE]$COMPASS31Score,na.rm=TRUE)
s2=sd(dp$data[COVID_categorization==TRUE][PHx_psych_PTSD==FALSE]$COMPASS31Score,na.rm=TRUE)
stot=sd(dp$data[COVID_categorization==TRUE]$COMPASS31Score,na.rm=TRUE)

(m1-m2)/stot # effect size ~.42

mean(dp$data[COVID_categorization==TRUE]$asiN2peripheral,na.rm=TRUE)
sd(dp$data[COVID_categorization==TRUE]$asiN2peripheral,na.rm=TRUE)

summary(lm(COMPASS31Score~age+gendernnb+nTBIwLOC+pcl5notE+COVID_categorization,data=dp$normed))
summary(lm(whoQoLD1raw~age+gendernnb+nTBIwLOC+pcl5notE+COVID_categorization,data=dp$normed))
summary(lm(whoQoLD2raw~age+gendernnb+nTBIwLOC+pcl5notE+COVID_categorization,data=dp$normed))

# Still no evidence of interactive effects, per se?
summary(lm(COMPASS31Score~age+gendernnb+nTBIwLOC+pcl5notE*COVID_categorization,data=dp$normed))
summary(lm(whoQoLD1raw~age+gendernnb+nTBIwLOC+pcl5notE*COVID_categorization,data=dp$normed))
summary(lm(whoQoLD2raw~age+gendernnb+nTBIwLOC+pcl5notE*COVID_categorization,data=dp$normed))

# with dx
summary(lm(COMPASS31Score~age+gendernnb+Dx_psych_PTSD+COVID_categorization,data=dp$normed))
summary(lm(COMPASS31Score~age+gendernnb+nTBIwLOC+Dx_psych_PTSD+COVID_categorization,data=dp$normed))
summary(lm(whoQoLD1raw~age+gendernnb+nTBIwLOC+Dx_psych_PTSD+COVID_categorization,data=dp$normed))
summary(lm(whoQoLD2raw~age+gendernnb+nTBIwLOC+Dx_psych_PTSD+COVID_categorization,data=dp$normed))

summary(lm(COMPASS31Score~age+gendernnb+Dx_psych_PTSD*COVID_categorization,data=dp$normed))
summary(lm(COMPASS31Score~age+gendernnb+nTBIwLOC+Dx_psych_PTSD*COVID_categorization,data=dp$normed))
summary(lm(whoQoLD1raw~age+gendernnb+nTBIwLOC+Dx_psych_PTSD*COVID_categorization,data=dp$normed))
summary(lm(whoQoLD2raw~age+gendernnb+nTBIwLOC+Dx_psych_PTSD*COVID_categorization,data=dp$normed))



# Proportion of folks who consider vs don't consider themselves to have Long Covid who report a dx of PTSD:
# Using Fisher Exact probability test
smdata=dp$data[!is.na(SelfDescribeLongHaul)][!is.na(Dx_psych_PTSD)]
x=c(sum(smdata[SelfDescribeLongHaul==1]$Dx_psych_PTSD==TRUE),sum(smdata[SelfDescribeLongHaul!=1]$Dx_psych_PTSD==TRUE,na.rm = TRUE))
n=c(sum(!is.na(smdata[SelfDescribeLongHaul==1]$Dx_psych_PTSD)),sum(!is.na(smdata[SelfDescribeLongHaul!=1]$Dx_psych_PTSD)))
prop.test(x,
          n,
          p=c(NULL,NULL),
          alternative="two.sided",correct=TRUE)

# Hmm. Not sig. Try lm:
dp$data[,SDLHbinary:=as.logical(NA)]
dp$data[SelfDescribeLongHaul==1,SDLHbinary:=TRUE]
dp$data[SelfDescribeLongHaul==2,SDLHbinary:=FALSE]
dp$data[SelfDescribeLongHaul==3,SDLHbinary:=FALSE]
dp$data[,SDLHbinaryU:=as.logical(NA)]
dp$data[SelfDescribeLongHaul==1,SDLHbinaryU:=TRUE]
dp$data[SelfDescribeLongHaul==2,SDLHbinaryU:=TRUE]
dp$data[SelfDescribeLongHaul==3,SDLHbinaryU:=FALSE]
dp$data[,SDLHbinaryG:=as.integer(NA)]
dp$data[SelfDescribeLongHaul==1,SDLHbinaryG:=2]
dp$data[SelfDescribeLongHaul==2,SDLHbinaryG:=0]
dp$data[SelfDescribeLongHaul==3,SDLHbinaryG:=1]

# Nope. Not helpful.

summary(lm(SDLHbinary~PHx_psych_PTSD+nTBIwLOC+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(SDLHbinary~nTBIwLOC+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(SDLHbinary~lecAtotal+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(SDLHbinaryG~PHx_psych_PTSD+nTBIwLOC+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(SDLHbinaryG~nTBIwLOC+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(SDLHbinaryG~PHx_psych_PTSD+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))

# Soo... doesn't seem like it's that folks with hx PTSD aremore likely to REPORT longhaul covid - just to report Sx

ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=lecAtotal,y=COMPASS31Score))+
  geom_point(aes(color=PHx_psych_PTSD),position=position_jitter(w = 0.1, h = 0.1))+
  stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
summary(lm(NDx_rheum_MECFS~lecAtotal,data=dp$data[COVID_categorization==TRUE]))
summary(lm(NDx_rheum_MECFS~PHx_psych_PTSD,data=dp$data[COVID_categorization==TRUE]))
summary(lm(NDx_psych_PTSD~lecAtotal,data=dp$data[COVID_categorization==TRUE]))

#[1] "PtID"                         "VisitID"                      "TMB_date"                     "TMB_Continuousconcentration" 
#[5] "TMB_FastChoices"              "TMB_MatchingShapesandNumbers" "TMB_QuickAdditionTask"        "TMB_RememberingWordsRecall"  
#[9] "TMB_VisualPatterns" 


ggplot(data=dp$data,aes(x=lecAtotal,y=TMB_Continuousconcentration))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=lecAtotal,y=TMB_FastChoices))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=lecAtotal,y=TMB_MatchingShapesandNumbers))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=lecAtotal,y=TMB_QuickAdditionTask))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=lecAtotal,y=TMB_RememberingWordsRecall))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=lecAtotal,y=TMB_VisualPatterns))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

ggplot(data=dp$data,aes(x=pcl5total,y=TMB_Continuousconcentration,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=pcl5total,y=TMB_FastChoices,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=pcl5total,y=TMB_MatchingShapesandNumbers,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=pcl5total,y=TMB_QuickAdditionTask,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=pcl5total,y=TMB_RememberingWordsRecall,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=pcl5total,y=TMB_VisualPatterns,color=COVID_categorization))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

summary(lm(TMB_Continuousconcentration~COMPASS31Score+age+gendernnb,data=dp$data))
summary(lm(TMB_Continuousconcentration~COMPASS31Score,data=dp$data))
summary(lm(TMB_Continuousconcentration~SelfDescribeLongHaul,data=dp$data))

summary(lm(TMB_Continuousconcentration~asi2total+age+gendernnb,data=dp$data))
summary(lm(TMB_FastChoices~asi2total+age+gendernnb,data=dp$data))
summary(lm(TMB_MatchingShapesandNumbers~asi2total+age+gendernnb,data=dp$data))
summary(lm(TMB_QuickAdditionTask~asi2total+age+gendernnb,data=dp$data))
summary(lm(TMB_RememberingWordsRecall~asi2total+age+gendernnb,data=dp$data))
summary(lm(TMB_VisualPatterns~asi2total+age+gendernnb,data=dp$data))

summary(lm(TMB_Continuousconcentration~asi1totalnoncog+pcl5total+nTBIwLOC+age+gendernnbnnb,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~asi1totalnoncog+pcl5total+nTBIwLOC+age+gendernnbnnb,data=dp$normed))
summary(lm(TMB_QuickAdditionTask~asi1totalnoncog+pcl5total+nTBIwLOC+age+gendernnbnnb,data=dp$normed))



summary(lm(TMB_Continuousconcentration~age+gendernnb+nTBIwLOC,data=dp$data))
summary(lm(TMB_FastChoices~age+gendernnb+nTBIwLOC,data=dp$data))
summary(lm(TMB_MatchingShapesandNumbers~age+gendernnb+nTBIwLOC,data=dp$data))
summary(lm(TMB_QuickAdditionTask~age+gendernnb+nTBIwLOC,data=dp$data))
summary(lm(TMB_RememberingWordsRecall~age+gendernnb+nTBIwLOC,data=dp$data))
summary(lm(TMB_VisualPatterns~age+gendernnb+nTBIwLOC,data=dp$data))

summary(lm(asi1total~age+gendernnb+pcl5total+nTBIwLOC*lecAtotal*COVID_categorization,data=dp$data))
summary(lm(asi2total~age+gendernnb+pcl5total+nTBIwLOC*lecAtotal*COVID_categorization,data=dp$data))
summary(lm(asi3total~age+gendernnb+pcl5total+nTBIwLOC*lecAtotal*COVID_categorization,data=dp$data))
summary(lm(asiSxTotal~age+gendernnb+pcl5total+nTBIwLOC*lecAtotal*COVID_categorization,data=dp$data))
summary(lm(COMPASS31Score~age+gendernnb+pcl5total+nTBIwLOC*lecAtotal*COVID_categorization,data=dp$data))

summary(lm(asi1total~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asi2total~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asi3total~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asiSxTotal~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(COMPASS31Score~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))

summary(lm(TMB_Continuousconcentration~pcl5total+age+gendernnb,data=dp$data))
summary(lm(TMB_Continuousconcentration~nTBIwLOC+age+gendernnb,data=dp$data))
summary(lm(TMB_Continuousconcentration~pcl5total+age+gendernnb+asi2total,data=dp$data))
summary(lm(TMB_Continuousconcentration~nTBIwLOC+age+gendernnb+asi2total,data=dp$data))



summary(lm(phq9total~asi2total+age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(phq9total~asi2total+age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))

# ASI factorization:

asi1data <- dp$data[VisitID==1][!is.na(asi1total),.(asi1i1,asi1i2,asi1i3,asi1i4,asi1i5,asi1i6,asi1i7,asi1i8,asi1i9,asi1i10,asi1i11,asi1i12,asi1i13,asi1i14,asi1i15,asi1i16,asi1i17,asi1i18,asi1i19,asi1i20,asi1i21,asi1i22,asi1i23,asi1i24,asi1i25,asi1i26,asi1i27,asi1i28,asi1i29,asi1i30,asi1i31)]
asi2data <- dp$data[VisitID==1][!is.na(asi2total),.(asi2i1,asi2i2,asi2i3,asi2i4,asi2i5,asi2i6,asi2i7,asi2i8,asi2i9,asi2i10,asi2i11,asi2i12,asi2i13,asi2i14,asi2i15,asi2i16,asi2i17,asi2i18,asi2i19,asi2i20,asi2i21,asi2i22,asi2i24,asi2i25,asi2i26,asi2i27,asi2i28,asi2i29,asi2i30,asi2i31)]
asi2data[is.na(asi2data)]=0
asi1labels <- c("Sweating","Flushing","Tremors","Weakness","Heavy limbs","Nausea","Palpitations","Fast Heart Rate","Short of Breath","Chest Discomfort","Abdominal Discomfort","Lightheadedness or Dizziness",
               "Low Energy","Sleepiness","Problems with concentration of focus","Problems with Memory","Problems with Organization or Reasoning","Low Appetite","Headache","Trouble Falling Asleep","Trouble Staying Asleep",
               "Anxiety or Panic","Irritability or Anger","Moving or Thrashing During Sleep","Nightmares","Waking from Sleep Anxious or Scared","Muscle Tension","Pain in Joints or Muscles","Diarrhea or Loose Stools","Abdominal Pain or Cramping","Constipation")
asi2labels <- c("Sweating","Flushing","Tremors","Weakness","Heavy limbs","Nausea","Palpitations","Fast Heart Rate","Short of Breath","Chest Discomfort","Abdominal Discomfort","Lightheadedness or Dizziness",
                "Low Energy","Sleepiness","Problems with concentration of focus","Problems with Memory","Problems with Organization or Reasoning","Low Appetite","Headache","Trouble Falling Asleep","Trouble Staying Asleep",
                "Anxiety or Panic","Moving or Thrashing During Sleep","Nightmares","Waking from Sleep Anxious or Scared","Muscle Tension","Pain in Joints or Muscles","Diarrhea or Loose Stools","Abdominal Pain or Cramping","Constipation")

# Some initial looks:
describe(asi1data)
r <- lowerCor(asi1data)
corPlot(r)
describe(asi2data)
r <- lowerCor(asi2data)
corPlot(r)

# Internal reliability:
alpha(asi1data)
alpha(asi2data)

# Scree plot
fa.parallel(asi1data)
fa.parallel(asi2data)

# Implement
asi1f3fout <- fa(asi1data,nfactors=3,rotate="varimax")
asi1f3forplot <- asi1f3fout
rownames(asi1f3forplot$loadings) <- asi1labels
fa.diagram(asi1f3forplot)
dp$data <- addfactors(dp$data,asi1f3fout,"asi13f")

loadingsout <- as.matrix(asi1f3forplot$loadings[1:31,])
l_rnames <- rownames(loadingsout)
loadingsout <- as.data.table(loadingsout)
loadingsout[,VarID:=l_rnames]
loadingsout[,VarLabels:=asi1labels]

# fiddlingwithorder <- copy(loadingsout)
# orderchart <- data.table(order=1:13,list=c("ceaPTWi12","ceaPTWi10","ceaPTWi9","ceaPTWi6","ceaPTWi7","ceaPTWi11","ceaPTWi13","ceaPTWi8","ceaPTWi3","ceaPTWi1","ceaPTWi2","ceaPTWi5","ceaPTWi4"))
# fiddlingwithorder[,torder:=subfromkey(fiddlingwithorder$VarID,orderchart$list,orderchart$order)]
# fiddlingwithorder$torder <- as.integer(fiddlingwithorder$torder)
# setkey(fiddlingwithorder,torder)
# orderofitems <- fiddlingwithorder$VarLabels
# 
# loadingsout$VarLabels <- factor(loadingsout$VarLabels,levels=orderofitems)

loadings2plot <- melt(loadingsout,id.vars =c("VarID","VarLabels"),value.name="Weight",variable.name="Factor")
factorlabeledplot <-
  ggplot(data=loadings2plot,aes(x=Factor,y=VarLabels))+
  geom_tile(aes(fill=Weight))+
  geom_text(aes(label=round(Weight,2)))+
  scale_fill_continuous(low="white",high="Orange")+
  theme(axis.text=element_text(size=14))+
  ylab("")+xlab("")+
  theme(legend.position = "none")+
  scale_x_discrete(name="",labels=c("Factor 1","Factor 2","Factor 3"))
#nameslabeledplot <- factorlabeledplot+
#  scale_x_discrete(name="",labels=c("Volume","Demoralization","Risk"))

# two factor solution:
asi1f2fout <- fa(asi1data,nfactors=2,rotate="varimax")
asi1f2forplot <- asi1f2fout
rownames(asi1f3forplot$loadings) <- asi1labels
fa.diagram(asi1f2forplot)
dp$data <- addfactors(dp$data,asi1f2fout,"asi12f")

loadingsout <- as.matrix(asi1f2forplot$loadings[1:31,])
l_rnames <- rownames(loadingsout)
loadingsout <- as.data.table(loadingsout)
loadingsout[,VarID:=l_rnames]
loadingsout[,VarLabels:=asi1labels]

loadings2plot <- melt(loadingsout,id.vars =c("VarID","VarLabels"),value.name="Weight",variable.name="Factor")
factorlabeledplot <-
  ggplot(data=loadings2plot,aes(x=Factor,y=VarLabels))+
  geom_tile(aes(fill=Weight))+
  geom_text(aes(label=round(Weight,2)))+
  scale_fill_continuous(low="white",high="Orange")+
  theme(axis.text=element_text(size=14))+
  ylab("")+xlab("")+
  theme(legend.position = "none")+
  scale_x_discrete(name="",labels=c("Factor 1","Factor 2","Factor 3"))

summary(lm(asi1total~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asi12f_1~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+pcl5total*COVID_categorization,data=dp$data))
summary(lm(asi12f_2~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+pcl5total*COVID_categorization,data=dp$data))

#look here...
summary(lm(asi1total~age+gendernnb+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asi1total~age+gendernnb+nTBIwLOC+Dx_psych_PTSD+COVID_categorization,data=dp$data))
summary(lm(asi12f_1~age+gendernnb+nTBIwLOC+lecAtotal+nTBIwLOC*COVID_categorization,data=dp$data))
summary(lm(asi12f_2~age+gendernnb+nTBIwLOC+lecAtotal+nTBIwLOC*COVID_categorization,data=dp$data))


summary(lm(asi1total~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asi13f_1~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asi13f_2~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asi13f_3~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asiSxTotal~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(COMPASS31Score~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))

summary(lm(TMB_Continuousconcentration~pcl5total+age+gendernnb,data=dp$data))
summary(lm(TMB_Continuousconcentration~nTBIwLOC+age+gendernnb,data=dp$data))
summary(lm(TMB_Continuousconcentration~pcl5total+age+gendernnb+asi2total,data=dp$data))
summary(lm(TMB_Continuousconcentration~nTBIwLOC+age+gendernnb+asi2total,data=dp$data))
summary(lm(TMB_Continuousconcentration~pcl5total+age+gendernnb+asi13f_3,data=dp$data))
summary(lm(TMB_Continuousconcentration~nTBIwLOC+age+gendernnb+asi13f_2,data=dp$data))

summary(lm(TMB_Continuousconcentration~asi1i15+age+gendernnb,data=dp$data))
summary(lm(TMB_Continuousconcentration~asi1i16+age+gendernnb,data=dp$data))
summary(lm(TMB_Continuousconcentration~asi1i17+age+gendernnb,data=dp$data))
summary(lm(TMB_MatchingShapesandNumbers~asi1i15+age+gendernnb,data=dp$data))
summary(lm(TMB_MatchingShapesandNumbers~asi1i16+age+gendernnb,data=dp$data))
summary(lm(TMB_MatchingShapesandNumbers~asi1i17+age+gendernnb,data=dp$data))
summary(lm(TMB_QuickAdditionTask~asi1i15+age+gendernnb,data=dp$data))
summary(lm(TMB_QuickAdditionTask~asi1i16+age+gendernnb,data=dp$data))
summary(lm(TMB_QuickAdditionTask~asi1i17+age+gendernnb,data=dp$data))
summary(lm(TMB_FastChoices~asi1i15+age+gendernnb,data=dp$data))
summary(lm(TMB_FastChoices~asi1i16+age+gendernnb,data=dp$data))
summary(lm(TMB_FastChoices~asi1i17+age+gendernnb,data=dp$data))
summary(lm(TMB_RememberingWordsRecall~asi1i15+age+gendernnb,data=dp$data))
summary(lm(TMB_RememberingWordsRecall~asi1i16+age+gendernnb,data=dp$data))
summary(lm(TMB_RememberingWordsRecall~asi1i17+age+gendernnb,data=dp$data))
summary(lm(TMB_VisualPatterns~asi1i15+age+gendernnb,data=dp$data))
summary(lm(TMB_VisualPatterns~asi1i16+age+gendernnb,data=dp$data))
summary(lm(TMB_VisualPatterns~asi1i17+age+gendernnb,data=dp$data))

summary(lm(asi1i15~COVID_categorization+age+gendernnb,data=dp$data))
summary(lm(asi1i16~COVID_categorization+age+gendernnb,data=dp$data))
summary(lm(asi1i17~COVID_categorization+age+gendernnb,data=dp$data))

summary(lm(ESStotal~COVID_categorization+age+gendernnb,data=dp$data))
summary(lm(isi_total~COVID_categorization+age+gendernnb,data=dp$data))
summary(lm(ESStotal~COVID_categorization+asi1total+age+gendernnb,data=dp$data))
summary(lm(isi_total~COVID_categorization+asi1total+age+gendernnb,data=dp$data))
summary(lm(ESStotal~COVID_categorization+asiNarrow+age+gendernnb,data=dp$data))
summary(lm(isi_total~COVID_categorization+asiNarrow+age+gendernnb,data=dp$data))



summary(lm(TMB_Continuousconcentration~age+gendernnb+nTBIwLOC,data=dp$data))
summary(lm(TMB_FastChoices~age+gendernnb+nTBIwLOC,data=dp$data))
summary(lm(TMB_MatchingShapesandNumbers~age+gendernnb+nTBIwLOC,data=dp$data))
summary(lm(TMB_QuickAdditionTask~age+gendernnb+nTBIwLOC,data=dp$data))
summary(lm(TMB_RememberingWordsRecall~age+gendernnb+nTBIwLOC,data=dp$data))
summary(lm(TMB_VisualPatterns~age+gendernnb+nTBIwLOC,data=dp$data))

summary(lm(COMPASS31Score~age+gendernnb+COVID_categorization,data=dp$normed)) # covid trend
summary(lm(asiN2peripheral~age+gendernnb+COVID_categorization,data=dp$normed)) # covid trend
summary(lm(asi1total~age+gendernnb+COVID_categorization,data=dp$normed)) # covid trend

summary(lm(COMPASS31Score~age+gendernnb+asiN2peripheral,data=dp$normed)) # covid trend
summary(lm(COMPASS31Score~age+gendernnb+asi1total,data=dp$normed)) # covid trend



#Cog items
summary(lm(TMB_Continuousconcentration~age+gendernnb+COVID_categorization,data=dp$normed)) # covid trend
summary(lm(TMB_FastChoices~age+gendernnb+COVID_categorization,data=dp$normed)) # here covid
summary(lm(TMB_MatchingShapesandNumbers~age+gendernnb+COVID_categorization,data=dp$normed)) # tbi here
summary(lm(TMB_QuickAdditionTask~age+gendernnb+COVID_categorization,data=dp$normed)) # tbi here
summary(lm(TMB_RememberingWordsRecall~age+gendernnb+COVID_categorization,data=dp$normed)) # covid here
summary(lm(TMB_VisualPatterns~age+gendernnb+COVID_categorization,data=dp$normed)) # covid and tbi hre


summary(lm(TMB_Continuousconcentration~age+gendernnb+COVID_categorization,data=dp$normed))
summary(lm(TMB_FastChoices~age+gendernnb+COVID_categorization,data=dp$normed)) # here covid
summary(lm(TMB_MatchingShapesandNumbers~age+gendernnb+COVID_categorization,data=dp$normed)) 

summary(lm(TMB_Continuousconcentration~age+gendernnb+nTBIwLOC+COVID_categorization,data=dp$normed)) # covid trend
summary(lm(TMB_FastChoices~age+gendernnb+nTBIwLOC+COVID_categorization,data=dp$normed)) # here covid
summary(lm(TMB_MatchingShapesandNumbers~age+gendernnb+nTBIwLOC+COVID_categorization,data=dp$normed)) # tbi here
summary(lm(TMB_QuickAdditionTask~age+gendernnb+nTBIwLOC+COVID_categorization,data=dp$normed)) # tbi here
summary(lm(TMB_RememberingWordsRecall~age+gendernnb+nTBIwLOC+COVID_categorization,data=dp$normed)) # covid here
summary(lm(TMB_VisualPatterns~age+gendernnb+nTBIwLOC+COVID_categorization,data=dp$normed)) # covid and tbi hre

summary(lm(TMB_Continuousconcentration~age+gendernnb+nTBIwLOC+COVID_categorization+asi1totalnoncog,data=dp$normed)) # asi sig
summary(lm(TMB_FastChoices~age+gendernnb+nTBIwLOC+COVID_categorization+asi1totalnoncog,data=dp$normed))
summary(lm(TMB_MatchingShapesandNumbers~age+gendernnb+nTBIwLOC+COVID_categorization+asi1totalnoncog,data=dp$normed)) # asi sig
summary(lm(TMB_QuickAdditionTask~age+gendernnb+nTBIwLOC+COVID_categorization+asi1totalnoncog,data=dp$normed)) 
summary(lm(TMB_RememberingWordsRecall~age+gendernnb+nTBIwLOC+COVID_categorization+asi1totalnoncog,data=dp$normed)) # covid here
summary(lm(TMB_VisualPatterns~age+gendernnb+nTBIwLOC+COVID_categorization+asi1totalnoncog,data=dp$normed)) # covid and tbi hre

summary(lm(TMB_Continuousconcentration~age+gendernnb+asi1totalnoncog,data=dp$normed)) # asi sig
summary(lm(TMB_FastChoices~age+gendernnb+asi1totalnoncog,data=dp$normed)) # asi sig
summary(lm(TMB_MatchingShapesandNumbers~age+gendernnb+asi1totalnoncog,data=dp$normed)) # asi sig
summary(lm(TMB_QuickAdditionTask~age+gendernnb+asi1totalnoncog,data=dp$normed)) # NOT asi sig <-- becomes sig if asi1totalnoncog
summary(lm(TMB_RememberingWordsRecall~age+gendernnb+asi1totalnoncog,data=dp$normed)) # NOT asi sig
summary(lm(TMB_VisualPatterns~age+gendernnb+asi1totalnoncog,data=dp$normed)) # NOT asi sig

summary(lm(TMB_Continuousconcentration~age+gendernnb+asi1totalnoncog,data=dp$normed[COVID_categorization>0])) # asi sig
summary(lm(TMB_FastChoices~age+gendernnb+asi1totalnoncog,data=dp$normed[COVID_categorization>0])) # asi no longer sig
summary(lm(TMB_MatchingShapesandNumbers~age+gendernnb+asi1totalnoncog,data=dp$normed[COVID_categorization>0])) # asi sig
summary(lm(TMB_QuickAdditionTask~age+gendernnb+asi1totalnoncog,data=dp$normed[COVID_categorization>0])) # NOT asi sig <-- trends if asi1totalnoncog
summary(lm(TMB_RememberingWordsRecall~age+gendernnb+asi1totalnoncog,data=dp$normed[COVID_categorization>0])) # NOT asi sig
summary(lm(TMB_VisualPatterns~age+gendernnb+asi1totalnoncog,data=dp$normed[COVID_categorization>0])) # NOT asi sig


summary(lm(NeuroQoLtotal~COVID_categorization+age+gendernnb,data=dp$normed))
summary(lm(whoQoLD1raw~COVID_categorization+age+gendernnb,data=dp$normed))
summary(lm(whoQoLD2raw~COVID_categorization+age+gendernnb,data=dp$normed))
summary(lm(whoQoLD3raw~COVID_categorization+age+gendernnb,data=dp$normed))
summary(lm(whoQoLD4raw~COVID_categorization+age+gendernnb,data=dp$normed))

# Effect on whoqol-bref: physical >> psychological; no effect on social relationships and environment

summary(lm(NeuroQoLtotal~COVID_categorization+asi1total+age+gendernnb,data=dp$normed))
summary(lm(whoQoLD1raw~COVID_categorization+asi1total+age+gendernnb,data=dp$normed))
summary(lm(whoQoLD2raw~COVID_categorization+asi1total+age+gendernnb,data=dp$normed))
summary(lm(whoQoLD3raw~COVID_categorization+asi1total+age+gendernnb,data=dp$normed))
summary(lm(whoQoLD4raw~COVID_categorization+asi1total+age+gendernnb,data=dp$normed))
# asi1 takes all the weight, even with covid still in the mix...!

# Try mediation?
library(mediation)

X <- "COVID_categorization"
Y <- "PROMIS_fatigue6a_total"
Y <- "PROMIS_painInt3a_total"
Y <- "ESStotal"
Y <- "ISItotal"
Y <- "NeuroQoLtotal"
Y <- "whoQoLD1raw"
Y <- "whoQoLD2raw"
Y <- "TMB_Continuousconcentration"
M <- "asi1totalnoncog"
M <- "asi1total"
M <- "asi1totalnotsleep"
M <- "COMPASS31Score"
M <- "asiN2peripheral"
dt <- data.table(x=dp$normed[,get(X)],y=dp$normed[,get(Y)],m=dp$normed[,get(M)],age=dp$normed[,age],gendernnb=dp$normed[,gendernnbnnb]) # quite sure there is a more effic way to do this!
dt <- dt[!is.na(x)][!is.na(y)][!is.na(m)]
dt$x=as.numeric(dt$x)

res <- mediate(lm(m~x+age+gendernnb,data=dt),lm(y~x+m+age+gendernnb,data=dt),treat='x',mediator='m',boot=TRUE,sims=1000)
summary(res)

# Decode / Want to pull out:
# tau.coef, tau.ci, tau.p: Total effect=sum of mediation (indirect) effect and direct effect
# z0, z0.ci, z0.p: ADE=average direct effect = direct effect of X on Y after taking into account (removing) mediation from M
# d0, d0.ci, d0.p: ACME=average causal mediation effects=total effect minus direct effect
# n0, n0.ci, n0.p: proportion mediated
#mlist <- c("asi1total","asi1totalnoncog","asi1totalnosleep","asi2total","asi2totalnoncog","asi2totalnosleep","asi3total","asiSxTotal","asiSxTotalnoncog","asiSxTotalnosleep",
#           "asiN1mostadrenergic","asiN2peripheral","asiN3mostadrenergicnotpostural","asiN4peripheralnotpostural","COMPASS31Score")
mlist <- c("COMPASS31Score","asiN2peripheral","asi1total")
mlist <- c("asi1total","asiN2peripheral","COMPASS31Score")
#mlist <- c("asiSxTotalnoncog","asi3total")
#mlist <- c("asiNarrow","asiN","asiN2")
xlist <- c("COVID_categorization")
ylist <- c("PROMIS_fatigue6a_total","PROMIS_painInt3a_total","ESStotal","isi_total","NeuroQoLtotal","whoQoLD1raw","whoQoLD2raw",
           "TMB_Continuousconcentration","TMB_MatchingShapesandNumbers", "TMB_QuickAdditionTask", "TMB_FastChoices", "TMB_RememberingWordsRecall", "TMB_VisualPatterns")
ylist <- c("PROMIS_painInt3a_total","PROMIS_fatigue6a_total","isi_total","NeuroQoLtotal","whoQoLD2raw","whoQoLD1raw")
#grouplist <- c("All","HCW","FR")
grouplist <- c("All")
var2collect <- c("te","ade","acme","pm")
attributes2collect <- c("point","CI_lower","CI_upper","pvalue")
#subfromkey(vector2sub,key_from,key_to,nomatch="skip")
var2collectkey <-c("tau","z0","d0","n0")
attributes2collectkey <- c("",".ci",".ci",".p")
Nsims=2500

h_out <- data.table(m=character(),x=character(),y=character(),g=character(),
                    var=character(),attribute=character(),value=numeric())

for(im in 1:length(mlist)){
  for(ix in 1:length(xlist)){
    for(iy in 1:length(ylist)){
      for(ig in 1:length(grouplist)){
        X <- xlist[ix]
        Y <- ylist[iy]
        M <- mlist[im]
        dt <- data.table(x=dp$data[,get(X)],y=dp$data[,get(Y)],m=dp$data[,get(M)],age=dp$normed[,age],gendernnb=dp$normed[,gendernnbnnb]) # quite sure there is a more effic way to do this!
        dt <- dt[!is.na(x)][!is.na(y)][!is.na(m)]
        dt$x=as.numeric(dt$x)
        res <- mediate(lm(m~x+age+gendernnb,data=dt),lm(y~x+m+age+gendernnb,data=dt),treat='x',mediator='m',
                       boot=TRUE,sims=Nsims)
        ot <- summary(res)
        for(iv in 1:length(var2collect)){
          for(ia in 1:length(attributes2collect)){
            varnameintable <- paste0(subfromkey(var2collect[iv],var2collect,var2collectkey),
                                     subfromkey(attributes2collect[ia],attributes2collect,attributes2collectkey))
            # the odd one out...
            if(varnameintable=="tau") varnameintable <- "tau.coef"
            if(attributes2collect[ia]%in%c("CI_lower","CI_upper")){
              textend <- subfromkey(attributes2collect[ia],c("CI_lower","CI_upper"),c("[1]","[2]"))
              text2eval <- paste0("newvalue <- ot$",varnameintable,textend)
            }else{
              text2eval <- paste0("newvalue <- ot$",varnameintable)
            }
            eval(parse(text=text2eval))
            newoutline <- data.table(m=M,x=X,y=Y,g=grouplist[ig],var=var2collect[iv],
                                     attribute=attributes2collect[ia],value=newvalue)
            h_out <- rbind(h_out,newoutline)
          }
        }
      }
    }
  }
}

# Save results because they takea  while
saveRDS(h_out,paste0(importdirectories$workingdatasaves,"h_out3_",savefilename))
#dp=readRDS(paste0(importdirectories$workingdatasaves,savefilename))


# Put in a plot. Plan:
# Will facet wrap by x and m
# will make var the x-axis location (tau, ade, acme)

# Make the xaxis location
h_out[,xaxislocation:=(as.numeric(subfromkey(var,c("te","ade","acme","pm"),c(0,.15,.3,.45))))]
#h_out[,xaxislocation:=(as.numeric(subfromkey(m,unique(h_out$m),1:4))+
#                         as.numeric(subfromkey(var,c("te","ade","acme","pm"),c(0,.15,.3,.45))))]
ho <- copy(h_out)
#Pick subset if too many at once and diff axes
#ho=ho[(y%in%c("TMB_Continuousconcentration","TMB_MatchingShapesandNumbers", "TMB_QuickAdditionTask", "TMB_FastChoices", "TMB_RememberingWordsRecall", "TMB_VisualPatterns"))]
# flip the sign on where needed to make it consistent across all measures & pos effect directions
reversedirection_y=c("NeuroQoLtotal","whoQoLD1raw","whoQoLD2raw",
                     "TMB_Continuousconcentration","TMB_MatchingShapesandNumbers", "TMB_QuickAdditionTask",
                     "TMB_FastChoices", "TMB_RememberingWordsRecall", "TMB_VisualPatterns")
ho[y%in%reversedirection_y][attribute%in%c("point","CI_lower","CI_upper")]$value <-
  -ho[y%in%reversedirection_y][attribute%in%c("point","CI_lower","CI_upper")]$value
# cast to line up for plotting
hd <- as.data.table(dcast(ho,m+x+y+g+var+xaxislocation~attribute))
hd <- hd[g=="All"][var%in%c("te","ade","acme")]
# rename the options so the facet labeling is nice
#hd$y <- subfromkey(hd$y,
#                   key_from=c("PROMIS_painInt3a_total","PROMIS_fatigue6a_total","isi_total","NeuroQoLtotal","whoQoLD2raw","whoQoLD1raw"),
#                   key_to = c("Pain","Fatigue","Insomnia","Cognition","Mental Health","Physical Health"))
#hd$y <- factor(hd$y,levels=c("Physical Health","Mental Health","Cognition","Insomnia","Fatigue","Pain"))
hd$y <- subfromkey(hd$y,
                   key_from=c("PROMIS_painInt3a_total","PROMIS_fatigue6a_total","isi_total","NeuroQoLtotal","whoQoLD2raw","whoQoLD1raw"),
                   key_to = c("Pain (PROMIS-3a)","Fatigue (PROMIS-6a)","Insomnia (ISI)","Cognition (Neuro-QoL)","Mental Health (WHOQoL-D2)","Physical Health (WHOQoL-D1)"))
hd$y <- factor(hd$y,levels=c("Physical Health (WHOQoL-D1)","Mental Health (WHOQoL-D2)","Cognition (Neuro-QoL)","Insomnia (ISI)","Fatigue (PROMIS-6a)","Pain (PROMIS-3a)"))
#hd$m <- subfromkey(hd$m,
#                   key_from=c("asi1total","asiN2peripheral","COMPASS31Score"),
#                   key_to = c("ASI","ASI-peripheral","COMPASS"))
#hd$m <- factor(hd$m,levels=c("COMPASS","ASI-peripheral","ASI"))
hd$m <- subfromkey(hd$m,
                   key_from=c("asi1total","asiN2peripheral","COMPASS31Score"),
                   key_to = c("Adrenergic Symptoms (central&peripheral)","Adrenergic Symptoms (peripheral)","Autonomic Symptoms (COMPASS-31)"))
hd$m <- factor(hd$m,levels=c("Autonomic Symptoms (COMPASS-31)","Adrenergic Symptoms (peripheral)","Adrenergic Symptoms (central&peripheral)"))
# Adjust p-values bon fernoni for mult comparisons:
hd$pvalue <- hd$pvalue*length(unique(hd$m))*length(unique(hd$x))
hd[,yaxP:=max(c(CI_lower,CI_upper))+.75,by=1:nrow(hd)]
# Plot it!
p <- ggplot()+
  geom_point(data=hd,aes(x=xaxislocation,y=point),shape=1)+
  facet_grid(y~m,labeller=label_wrap_gen(10))+
  geom_linerange(data=hd,aes(x=xaxislocation,ymin=CI_lower,ymax=CI_upper))+
  geom_point(data=hd[pvalue<.05],aes(x=xaxislocation,y=yaxP),shape="*",size=3)+
  geom_point(data=hd[pvalue<.01],aes(x=xaxislocation,y=yaxP+.6),shape="*",size=3)+
  geom_point(data=hd[pvalue<.001],aes(x=xaxislocation,y=yaxP+1.2),shape="*",size=3)+
  #geom_point(data=hd[pvalue<.0001],aes(x=xaxislocation,y=yaxP),shape="+",size=3)+
  #geom_point(data=hd[pvalue<.0001],aes(x=xaxislocation,y=yaxP+.5),shape="+",size=3)+
  geom_hline(yintercept=0,linetype="dotdash")+
  theme_bw()+
  theme(legend.position = "none")+
  xlab("")+ylab("")+
  scale_x_continuous(breaks=c(0,.15,.3),labels=c("Total\nEffect","Direct\nEffect","Mediated\nEffect"),limits=c(-.05,.35))+
  scale_y_continuous(breaks=c(0,5,10),limits=c(-5,17))
  #scale_y_continuous(breaks=c(0,10,20),limits=c(-25,25))
p

ggsave(paste0(importdirectories$workingdatasaves,"Mediation1.png"),
       p,width=5,height=7,units="in",dp=600)
ggsave(paste0(importdirectories$workingdatasaves,"Mediation3.jpeg"),
       p,width=3.5,height=4.5,units="in",dp=600)

# Save h_out so can recreate figure without rerunning bootstrap (and results shiftin):
####saveRDS(h_out,file=paste0(importdirectories$workingdatasaves,"bootstrappeddata.rds"))
#h_out=readRDS(file=paste0(importdirectories$workingdatasaves,"bootstrappeddata.rds"))

# Finally, relationships:
ggplot(dp$dataQL)+geom_point(aes(x=cea_total,y=pcl5total))
ggplot(dp$dataQL)+geom_point(aes(x=cea_total,y=phq9total))
ggplot(dp$dataQL)+geom_point(aes(x=cea_total,y=gad7total))
ggplot(dp$dataQL)+geom_point(aes(x=cea_total,y=isi_total))
ggplot(dp$dataQL)+geom_point(aes(x=cea_total,y=pcl5clusterE))

library(ggpubr) #[attendDem_FR==TRUE]
ggscatter(dp$dataQL,x="cea_total",y="pcl5total",add="reg.line")+
  stat_cor(label.y=65)+xlab("COVID-19 professional exposure index")+ylab("Current PTSD symptoms (PCL total)")
