# Script to pull data from RECOVER and prepare it for analysis
#
# Input: Qualtrics survey and data exports, as well as updated varmap
#  (See Set Up section assignment of import directories and filenames)
#   --Site to download TMB data: https://studies.testmybrain.org/recover/data_portal.html 
#     * logins are studyname_1stinitiallastname
#   --For Qualtrics data, download as: numeric values, remove line breaks, split multi-value fields into columns
#
# Output: data file saved as
#   "RECOVER_pulled2022-12-14.rds"
#
# Reference Files:
#   ATTEND and PREDICT Data Pull Scripts
#
# History:
#   2022-02-28 RCH wrote it
#   2023-01-03 RCH adding longitudinal components
#
# ISSUES:
# [  ] WHY IS THE DATA DICTIONARY WRONG FOR THE SUBSTANCE USE QUESTIONS?????????????????????
# [  ] At "merge" step below, need to merge data dictionaries, too... write general function to handle?
# [  ] Data dictionary not being created correctly - basenames not picking up when try to add totals in StandardDataCalculation fields...
```{r}
`
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
  new_varmap_name="varmap_RECOVER_2023-01-10.rds",
  split_multiplevaluefields=TRUE # This will affect the way the survey is exported AND the way the varmap is created! (Qualtrics specific)
  # DDE settings are not relevant for purely qualtrics data
)
#studydatadirectoryname="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\"
#pulldate="2023-01-10"
importdirectories<-list(
  github="C:\\Users\\vhapughendrr\\GitHub\\",
  QualtricsData="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Qualtrics\\Data\\2023-01-10\\",
  QualtricsSurveys="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Qualtrics\\Surveys\\2023-01-10\\",
  TMBdata="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\TMB\\2023-01-10\\",
  TMBdatadictionary="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\TMB\\DataDictionary\\",
#  QualtricsLists="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Qualtrics\\List exports\\",
  workingdatasaves="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Analyses\\2023-01\\"
)
importfilenames<-list(
  QualtricsData="ALL",
  QualtricsSurveys="ALL",
  #QualtricsLists="ATTEND_Baseline_2020_12_29.csv",
  varmap="varmap_RECOVER_2023-01-10.rds"
#  surveymap="SurveyMap_ATTEND_2021-10-29.csv"
)
saveoptions=list(savetag="PUB")
savefilename <- "RECOVER_pulled2023-01-10.rds"

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
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\testmybrainFunctions.R"))

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
  # importoptions$update_varmap=FALSE
  # Now you need to re-run the standardizeVariableNames line, above (if you get a connection error, make sure the excel file is closed..)
#  # This converts the variables to our "HL Standard"

# Deal with duplicated columns before we do standard data calc's, because
# we know these are from different visits:
dp$dataQL <- mergedupcol(dp$dataQL)

dp<-standardDataCalculations(dp)
# This does standard calculations, like measure totals

# Add in TMB stuff....
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

##############
# Do some COVID episode processing that want to keep out of the more frequently re-run analysis script:
dctemplate=data.table(PtID=as.character(NA),
                      originalLetter=as.character(NA),
                      EvCovid1=as.logical(NA),EvCovid2=as.logical(NA),EvCovid3=as.logical(NA),EvCovid4=as.logical(NA),EvCovid5=as.logical(NA),EvCovid6=as.logical(NA),EvCovid7=as.logical(NA),EvCovid8=as.logical(NA),EvCovid8_TEXT=as.character(NA),
                      SxCovid1=as.logical(NA),SxCovid4=as.logical(NA),SxCovid5=as.logical(NA),SxCovid6=as.logical(NA),SxCovid7=as.logical(NA),SxCovid8=as.logical(NA),SxCovid9=as.logical(NA),SxCovid10=as.logical(NA),SxCovid11=as.logical(NA),SxCovid12=as.logical(NA),SxCovid13=as.logical(NA),SxCovid14=as.logical(NA),SxCovid14_TEXT=as.character(NA),
                      whenDiscreteLikelyCovid=as.character(NA))
dc=dctemplate[0,]
for(i in 1:nrow(dp$data)){
  for(l in c("A","B","C","D","E","F")){
    newrow=dctemplate
    newrow$PtID=dp$data[i,]$PtID
    newrow$originalLetter=l
    for(q in 1:8){
      evalstring=paste0("newrow$EvCovid",q,"=dp$data[i,]$EvCovid",l,q)
      eval(parse(text=evalstring))
    }
    evalstring=paste0("newrow$EvCovid8_TEXT=dp$data[i,]$EvCovid",l,"8_TEXT")
    eval(parse(text=evalstring))
    for(q in c(1,4:14)){
      evalstring=paste0("newrow$SxCovid",q,"=dp$data[i,]$SxCovid",l,q)
      eval(parse(text=evalstring))
    }
    evalstring=paste0("newrow$SxCovid14_TEXT=dp$data[i,]$SxCovid",l,"14_TEXT")
    eval(parse(text=evalstring))
    evalstring=paste0("newrow$whenDiscreteLikelyCovid=dp$data[i,]$whenDiscreteLikelyCovid",l)
    eval(parse(text=evalstring))
    
    dc=rbind(dc,newrow)
  }
}

dccopyforref=copy(dc)
dc=copy(dccopyforref)

# Turn the 1/NAs to TRUE/FALSE (do before below, so that aren't catching the dates/text entries!)
names2parse=setdiff(names(dc),c("PtID","originalLetter","whenDiscreteLikelyCovid",
                                "EvCovid8_TEXT","SxCovid14_TEXT","covidDate","nSx"))
dc[, (names2parse) := lapply(.SD, as.logical), .SDcols = names2parse]

# Before messing with the NAs, just use the TRUE columns to filter "empty" rows out:
dc=dc[rowSums(dc==TRUE,na.rm = TRUE)>=1]

# Now figure out how to deal with the NAs appropriately:
for(c in names2parse){
  evalstring=paste0("dc[is.na(",c,"),",c,":=FALSE]")
  eval(parse(text=evalstring))
}
# Now same for the text fields...
dc[SxCovid14_TEXT=="",SxCovid14_TEXT:=NA]
dc[EvCovid8_TEXT=="",EvCovid8_TEXT:=NA]

# Parse the dates:
dc[,covidDate:=as.Date(whenDiscreteLikelyCovid,"%m/%d/%Y")]
# For some reason one has dots instead of dashes(!?)
dc[,covidDateAlt:=as.Date(whenDiscreteLikelyCovid,"%m.%d.%Y")]
dc[!is.na(covidDateAlt),covidDate:=covidDateAlt]
dc[,covidDateAlt:=NULL]

# REF for questions:
# EvCovid: 1) rapid+; 2) PCR+; 3) fam/house contact+; 4) other contact+; 5) fam/close contact with Sx; 6) told by clinician; 7) Sx; 8) Other
# SxCovid: 1) fever/chills; 4) fatigue; 5) SOB/diff br; 6) dry cough; 7) taste/smell; 8) HA; 9) aches; 10) sore throat; 11) nasal cong; 12) nausea/appetite; 13) GI Sx; 14) Other

# Now do some by-episode and then by-participant accounting to port back to our main dataset...
dc[,testpos:=(EvCovid1|EvCovid2),by=1:nrow(dc)]
dc[,contactpos:=(EvCovid3|EvCovid4),by=1:nrow(dc)]
dc[,nSx:=sum(c(SxCovid1,SxCovid4,SxCovid5,SxCovid6,SxCovid7,SxCovid8,SxCovid9,SxCovid10,SxCovid11,SxCovid12,SxCovid13,SxCovid14)==TRUE),by=1:nrow(dc)]

dc[,testandSx:=(testpos&EvCovid7)]
dc[,contactposandSx:=(contactpos&EvCovid7)]
dc[,testorcontactandSx:=(testandSx|contactposandSx)]

# (Peaking briefly)
dc[(testandSx|contactposandSx)]

dcbyPt=dc[,.(covidDateLast=max(covidDate),covidDateFirst=min(covidDate),
             testandSx=max(testandSx),contactposandSx=max(contactposandSx),testorcontactandSx=max(testorcontactandSx),
             nSx=max(nSx),nLikelyCovid=length(covidDate)),by="PtID"]

dp$data=merge(dp$data,dcbyPt,by="PtID",all.x = TRUE)
dp$data[,VisitDate:=as.Date(EndDate)]
dp$data[,timesincefirstcovid:=(VisitDate-covidDateFirst)]
dp$data[,timesincelastcovid:=(VisitDate-covidDateLast)]

dp$covidEpisodeData=dc


### 
# Some fixes before we normalize:

# I don't understand what's going on with the entry without an externalreferenceID??? I'm going to cut it for now:
dp$data=dp$data[PtID!="CNA"]

# COVID_categorization is set after the COVID history block is completed, so people can end up miscategorized if they stop partway through this:
miscat=dp$data[(COVID_categorization==FALSE)&(nLikelyCovid>0)]$PtID # do in 2 steps to simplify fixing the normed version
dp$data[PtID%in%miscat,COVID_categorization:=TRUE]

# For now, skip the folks 2/9/22 and earlier (I think half are test runs anyways???)
dp$data$StartDate=as.Date(dp$data$StartDate)
notearlytests=dp$data[StartDate>as.Date("2022-02-09")]$PtID
# bc big deal, who are we losing?
setdiff(dp$data$PtID,notearlytests)
dp$data=dp$data[PtID%in%notearlytests]

# Not sure what's up here...
#> mean(dp$data$nTBIwoLOC,na.rm=TRUE)
#[1] 39374.06
#> max(dp$data$nTBIwoLOC,na.rm=TRUE)
#[1] 1e+07
dp$data[nTBIwoLOC>10000,nTBIwoLOC:=NA]
dp$data$nTBIwoLOC

# Various ways to look at trauma hx:
dp$data[,lecAtotal3cat:=paste0("Reported traumas: ",as.character(lecAtotal)),by=1:nrow(dp$data)]
dp$data[lecAtotal>=2,lecAtotal3cat:="Reported traumas: 2+"]


###################################

# Save a version before norming bc end up coming back here so often...
prenormsavefilename=paste0("prenorm_",savefilename)
saveRDS(dp,paste0(importdirectories$workingdatasaves,prenormsavefilename))
#dp=readRDS(paste0(importdirectories$workingdatasaves,prenormsavefilename))


# Create normalized versions of the clusters/factors/covariates that we may need to use multiple places:
tempdt <- copy(dp$data)
classes <- sapply(tempdt,class)
#unique(classes)
var2norm <- names(classes[which(classes%in%c("numeric","logical","integer"))])
var2norm <- setdiff(var2norm,
                    c("VisitID"))
for(iv in 1:length(var2norm)){
  x <- tempdt[,((var2norm[iv])),with=FALSE]
  y <- scale(x,center=TRUE,scale=TRUE)
  tempdt[,(var2norm[iv]):=y]
}
dp$normed <- tempdt

#########################
# Save:

saveRDS(dp,paste0(importdirectories$workingdatasaves,savefilename))
#dp=readRDS(paste0(importdirectories$workingdatasaves,savefilename))


