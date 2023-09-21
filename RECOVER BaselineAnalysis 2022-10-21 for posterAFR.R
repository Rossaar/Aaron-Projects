# Script to analyize the RECOVER baseline data
#
# Input: 
#   "RECOVER_pulled2022-10-05.rds" (made by RECOVER_2022-10-05 in Data Pull Scripts)
#
# Reference Files:
#   ATTEND and PREDICT Data Pull Scripts
#
# History:
#   2022-10-11 RCH pulled it out of the playing around at the bottom of the data pull script

###############################
# I. Set up
###############################

# Settings 
username="AFR"

if(username=="RebeccaVALT"){
  .libPaths("C:/LocalR/R-4.0.5/library")
  importdirectories<-list(
    github="C:\\Users\\vhapughendrr\\GitHub\\",
    workingdatasaves="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Analyses\\2022-10\\2022-10-21\\",
    plotsaves="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Analyses\\2022-10\\2022-10-21\\"
  )
}else{
  .libPaths("C:/LocalR/R-4.2.1/Library")
  importdirectories<-list(
    github="U:\\My Documents\\GitHub\\",
    workingdatasaves="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Analyses\\2022-10\\2022-10-21\\",
    plotsaves="R:\\MIRECC\\STUDIES\\Hendrickson_1629444_RECOVER\\Study Data\\Data\\Analyses\\2022-10\\2022-10-21\\")
}


importfilenames<-list(
  workingdata="RECOVER_pulled2022-10-21.rds"
)
saveoptions <- list(
  resave=TRUE,
  savetag="AASposterUpdates"
)

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
library(flextable)
library(mediation) # this overwrites the mediation of psych...

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
# II. Load the data, any initial fiddling
#################################

dp=readRDS(paste0(importdirectories$workingdatasaves,importfilenames$workingdata))

# # FIX!! Port backwards when next pull...
# setnames(dp$data,"SxCovidBD8","SxCovidD8")
# setnames(dp$data,"SxCovidED8","SxCovidE8")
# setnames(dp$data,"SxCovidFD8","SxCovidF8")

# I don't understand what's going on with the entry without an externalreferenceID??? I'm going to cut it for now:
dp$data=dp$data[PtID!="CNA"]

# COVID_categorization is set after the COVID history block is completed, so people can end up miscategorized if they stop partway through this:
miscat=dp$data[(COVID_categorization==FALSE)&(nLikelyCovid>0)]$PtID # do in 2 steps to simplify fixing the normed version
dp$data[PtID%in%miscat,COVID_categorization:=TRUE]
dp$normed[PtID%in%miscat,COVID_categorization:=max(dp$normed$COVID_categorization)]

# For now, skip the folks 2/9/22 and earlier (I think half are test runs anyways???)
dp$data$StartDate=as.Date(dp$data$StartDate)
notearlytests=dp$data[StartDate>as.Date("2022-02-09")]$PtID
# bc big deal, who are we losing?
setdiff(dp$data$PtID,notearlytests)
dp$data=dp$data[PtID%in%notearlytests]
dp$normed=dp$normed[PtID%in%notearlytests]

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

#####################################
# III. Characterize participants / exposure history, etc ("Table 1")
#####################################

########
# Pre-processing: this may actually take a bit; go through the less well defined
# things we want to include in the table and turn them into single variables!

# 2) #pasc on our assessments, self-ascription of LC, PMHx before/after covid
# 2) Means/SDs on the measures, by hx COVID, and ?LC desig'n?
# 3) Completion rate and scores for TMB data 

# PASC Sx direct characterization?
# PASCSxi# (Q100): 1) fatigue; 2) fast heart rate; 3) heart palpitations; 4) muscle and joint pain;
# 5) Headache; 6) Insomnia; 7) Anxiety; 8) Digestive symptoms; 9) Other
# to Q211: 9) shortness of breath; 10) chronic cough; 11) prob taste/smell; 12) brain fog; 
# 13) low energy or fatigue after exertion; 14) other 

# Also the before questions: Q102 (SxFreqB4i9)-->Q212 
# The before pandemic question is ONLY the early version: Q188

# Q208_1-4: persistentSxWorsened; Stable; Improved; Intermittent
# persistentSxWhenStart=Q207

# Sum of just the OLD options:
dp$data[,PASCSx_total:=sum((c(PASCSxi1,PASCSxi2,PASCSxi3,PASCSxi4,PASCSxi5,PASCSxi6,PASCSxi7,PASCSxi8,PASCSxi14)-1),na.rm=FALSE),by=1:nrow(dp$data)]
dp$data[,PASCSxB4_total:=sum((c(PASCSxB4i1,PASCSxB4i2,PASCSxB4i3,PASCSxB4i4,PASCSxB4i5,PASCSxB4i6,PASCSxB4i7,PASCSxB4i8,PASCSxB4i14)-1),na.rm=FALSE),by=1:nrow(dp$data)]
dp$data[,SxFreqB4_total:=sum((c(SxFreqB4i1,SxFreqB4i2,SxFreqB4i3,SxFreqB4i4,SxFreqB4i5,SxFreqB4i6,SxFreqB4i7,SxFreqB4i8,SxFreqB4i14)-1),na.rm=FALSE),by=1:nrow(dp$data)]
dp$data[,nPASCSx_total:=sum((c(PASCSxi1,PASCSxi2,PASCSxi3,PASCSxi4,PASCSxi5,PASCSxi6,PASCSxi7,PASCSxi8,PASCSxi14)>1),na.rm=FALSE),by=1:nrow(dp$data)]
dp$data[,nPASCSxB4_total:=sum((c(PASCSxB4i1,PASCSxB4i2,PASCSxB4i3,PASCSxB4i4,PASCSxB4i5,PASCSxB4i6,PASCSxB4i7,PASCSxB4i8,PASCSxB4i14)>1),na.rm=FALSE),by=1:nrow(dp$data)]
dp$data[,nSxFreqB4_total:=sum((c(SxFreqB4i1,SxFreqB4i2,SxFreqB4i3,SxFreqB4i4,SxFreqB4i5,SxFreqB4i6,SxFreqB4i7,SxFreqB4i8,SxFreqB4i14)>1),na.rm=FALSE),by=1:nrow(dp$data)]

#Employment: employment_a1 etc

# Need an audit total
dp$data[(auditc1==0),auditc2:=0]
dp$data[(auditc1==0),auditc3:=0]
dp$data[,auditc_total:=sum(c(auditc1,auditc2,auditc3)),by=1:nrow(dp$data)]

# ASInarrow: try for the smallest, most already accepted as adrenergic subset here, for now -
# Plan is: tremors, palpitations, fast HR, HA, muscle tension subset across all 3 forms. Ie,
# asi1i3, asi1i7, asi1i8, asi1i19, asi1i27; asi2 versions of same; asi3i3, asi3i7, asi3i8
dp$data[,asi5item:=sum(c(asi1i3,asi1i7,asi1i8,asi1i19,asi1i27,asi2i3,asi2i7,asi2i8,asi2i19,asi2i27,asi3i3,asi3i7,asi3i8),na.rm=TRUE),by=1:nrow(dp$data)]

########
## Settings & table setup for this section:

# How many sig digits this time?
d=1

# Group names and definitions
TableGroups=as.data.table(rbind(
  c("All",""),
  c("COVID Hx+","[COVID_categorization==TRUE]"),
  c("COVID Hx-","[COVID_categorization==FALSE]"),
  c("Self-identified LC: yes","[SelfDescribeLongHaul==1]"),
  c("Self-identified LC: unsure","[SelfDescribeLongHaul==3]"),
  c("Self-identified LC: no","[SelfDescribeLongHaul==2]")
))
names(TableGroups)=c("GroupName","GroupDef")
TableGroups[,N:=as.numeric(NA)]

# Row names, variable names, definitions and formats
# - if format is tmsd (table mean/SD), definition is just the variable name
# - if format is tpm (fraction matching a value), give variable name but after format need to give matching value
TableRows=as.data.table(rbind(
  # Basic demographics
  c("Age (y)","tage","age","tmsd",NA),
  c("Gender (F)","tgender","gender","tpm","\"F\""), # This is annoying but the most efficient I can find...
  c("Education (y)","teducation","education","tmsd", NA),
  c("Married","tmarried","maritalStatus","tpm",1),
  # COVID history
  c("Self-assessed likelihood had COVID (M+/-SD %)","tplikelycovid","pLikelyCovid","tmsd",NA),
  c("Assigned Likely COVID case","tCOVID_categorization","COVID_categorization","tpm",TRUE),
  c("# COVID episodes","nCovid","nLikelyCovid","tmsd",NA),
  c("# acute symptoms","tnSx","nSx","tmsd",NA),
  c("Days since 1st covid","ttimesincefirstcovid","timesincefirstcovid","tmsd",NA),
  c("Days since last covid","ttimesincelastcovid","timesincelastcovid","tmsd",NA),
  c("COVID: Required O2","tcovidO2","covidO2","tpm",1),
  c("COVID: Hospitalized","tcovidadmitted","covidAdmitted","tpm",1),
  c("COVID: ICU","tcovidicu","covidICU","tpm",1),
  c("COVID: Ventilator","tcovidvent","covidVentilator","tpm",1),
  c("COVID: Known cardiac damage","tcovidcardiac","covidCardiacDamage","tpm",1),
  c("COVID: Known Pulmonary damage","tcovidpulm","covidPulmDamage","tpm",1),
  c("COVID: known CNS damage","tcovidCNSdamage","covidCNSdamage","tpm",1),
  # covidAdmitted, covidO2, covidVentilator, covidCardiacDamage, covidICU, covidPulmDamage,covidCNSdamage
  # PASC direct report
  c("Self-describe as PASC: yes","tSelfDescribeLongHaulYes","SelfDescribeLongHaul","tpm",1),
  c("Self-describe as PASC: unsure","tSelfDescribeLongHaulUnsure","SelfDescribeLongHaul","tpm",3),
  c("N PASC Symptoms","tnPASCSx_total","nPASCSx_total","tmsd",NA),
  c("N PASC-like Symptoms prior to covid","tnPASCSxB4_total","nPASCSxB4_total","tmsd",NA),
  c("N PASC-like symptoms prior to pandemic","tnSxFreqB4_total","nSxFreqB4_total","tmsd",NA),
  c("PASC Symptoms Severity","tPASCSx_total","PASCSx_total","tmsd",NA),
  c("PASC-like Symptoms Severity prior to covid","tPASCSxB4_total","PASCSxB4_total","tmsd",NA),
  c("PASC-like symptoms Severity prior to pandemic","tSxFreqB4_total","SxFreqB4_total","tmsd",NA),
  c("PASC: fatigue","tPASCSxi1","PASCSxi1","tpoc",2),
  c("PASC: fast HR","tPASCSxi2","PASCSxi2","tpoc",2),
  c("PASC: palpitations","tPASCSxi3","PASCSxi3","tpoc",2),
  c("PASC: muscle/joint pain","tPASCSxi4","PASCSxi4","tpoc",2),
  c("PASC: headache","tPASCSxi5","PASCSxi5","tpoc",2),
  c("PASC: Insomnia","tPASCSxi6","PASCSxi6","tpoc",2),
  c("PASC: Anxiety","tPASCSxi7","PASCSxi7","tpoc",2),
  c("PASC: Digestive symptoms","tPASCSxi8","PASCSxi8","tpoc",2),
  c("PASC: Other","tPASCSxi14","PASCSxi14","tpoc",2),
  # MHx before/since COVID: nTBIwLOC, nTBIwoLOC, PHx_psych_PTSD, Dx_psych_PTSD
  c("TBI: N with LOC","tnTBIwLOC","nTBIwLOC","tmsd",NA),
  c("TBI w/o LOC: % > 0","tnTBIwoLOC","nTBIwoLOC","tpoc",1),
  c("TBI w/o LOC: % > 2","tnTBIwoLOC","nTBIwoLOC","tpoc",3),
  c("TBI w/o LOC: % > 10","tnTBIwoLOC","nTBIwoLOC","tpoc",11),
  c("History of Probable Trauma","tleca","lecAtotal","tmsd",NA),
  c("Prior diagnosis: PTSD","tPHx_psych_PTSD","PHx_psych_PTSD","tpm",TRUE),
  c("New diagnosis: PTSD","tNDx_psych_PTSD","NDx_psych_PTSD","tpm",TRUE),
  c("Current diagnosis: PTSD","tDx_psych_PTSD","Dx_psych_PTSD","tpm",TRUE),
  
  
  # Substances (comes with med hx?)
  c("Alcohol: AUDIT-C total","tauditc_total","auditc_total","tmsd",NA),
  c("Cannabis: use more days than not","tqcc7","qcc7","tpoc",4),
  c("Caffeine: use more days than not","tsubusei1","SubUsei1","tpoc",3),
  c("Nicotine: >half days","tsubuse2","SubUsei2","tpoc",3),
  c("Opiates: >half days","tsubuse3","SubUsei3","tpoc",3),
  c("Cocaine or Meth: >half days","tsubuse4","SubUsei4","tpoc",3),
  c("Other non rx stim: >half days","tsubuse5","SubUsei5","tpoc",3),
  
  # SRF measures: COMPASS_total, asiSxTotal, asi1total, asi2total, asi3total, auditc_total, ESStotal, 
  # gad7total, pcl5total, phq9total, phq9i9, isi_total, lecAtotal, PROMIS_fatigue6a_total, PROMIS_painFcn8a_total, PROMIS_painInt3a_total,
  # whoQoLDr1raw, whoQoLD2raw, whoQoLD3raw, whoQoLD4raw, NeuroQoLtotal
  # NEEDFOR: prmq
  c("COMPASS31 Total","tcompasstotal","COMPASS_total","tmsd",NA),
  c("ASI Total","tasiSxTotal","asiSxTotal","tmsd",NA),
  c("ASI1 Total","tasi1Total","asi1total","tmsd",NA),
  c("ASI2 Total","tasi2Total","asi2total","tmsd",NA),
  c("ASI3 Total","tasi3Total","asi3total","tmsd",NA),
  c("Adrenergic Symptom Score","tasi5Titem","asi5item","tmsd",NA),
  c("Epworth Total","tESStotal","ESStotal","tmsd",NA),
  c("ISI Total","tisitotal","isi_total","tmsd",NA),
  c("Anxiety: GAD7 total","tgad7total","gad7total","tmsd",NA),
  c("PTSD: PCL5 total","tpcl5total","pcl5total","tmsd",NA),
  c("Depression: PHQ9 total","tphq9total","phq9total","tmsd",NA),
  c("Suicidality: PHQ9 item 9","tphq9i9","phq9i9","tpoc",1),
  c("Fatigue: PROMIS 6a total","tfatigue","PROMIS_fatigue6a_total","tmsd",NA),
  c("Pain: Interference","tpainint","PROMIS_painInt3a_total","tmsd",NA),
  c("Pain: Function","tpainfunc","PROMIS_painFcn8a_total","tmsd",NA),
  c("Physical Health: WHOQoL-D1","twhoqold1","whoQoLD1raw","tmsd",NA),
  c("Mental Health: WHOQoL-D2","twhoqold2","whoQoLD2raw","tmsd",NA),
  c("Social Relationships: WHOQoL-D3","twhoqold3","whoQoLD3raw","tmsd",NA),
  c("Environment: WHOQoL-D4","twhoqold4","whoQoLD4raw","tmsd",NA),
  c("Cognition: Neuro-QoL","tneuroqol","NeuroQoLtotal","tmsd",NA),
  
  # TMB data: TMB_Continuousconcentration, TMB_FastChoices, TMB_MatchingShapesandNumbers, TMB_QuickAdditionTask, TMB_RememberingWordsRecall, TMB_VisualPatterns
  c("TMB: Continuous Concentration","tTMBcc","TMB_Continuousconcentration","tmsd",NA),
  c("TMB: Fast Choices","tTMBfc","TMB_FastChoices","tmsd",NA),
  c("TMB: Matching Shapes and Numbers","tTMBmsn","TMB_MatchingShapesandNumbers","tmsd",NA),
  c("TMB: Quick Addition Task","tTMBqa","TMB_QuickAdditionTask","tmsd",NA),
  c("TMB: Remembering Words","tTMBrw","TMB_RememberingWordsRecall","tmsd",NA),
  c("TMB: Visual Patterns","tTMBvp","TMB_VisualPatterns","tmsd",NA)
))
names(TableRows)=c("RowLabel","RowInternalName","RowVarID","RowFunction","RowOption")
# 2) #pasc on our assessments, self-ascription of LC, PMHx before/after covid
# 2) Means/SDs on the measures, by hx COVID, and ?LC desig'n?
# 3) Completion rate and scores for TMB data 

# Table settings:
includefraction=TRUE # give the fraction meeting a value for the percent-over-value ones?
compressed=TRUE # two default formatting options...
includeN=TRUE


##########
## Create the table!

o <- list()
for(ig in 1:nrow(TableGroups)){
  
  # Cut the data set to the relevant bit & tally the N:
  evalstring=paste0("data=dp$data[VisitID==1]",TableGroups$GroupDef[ig])
  eval(parse(text=evalstring))
  TableGroups[ig,N:=nrow(data)]
  
  # Fill in the rows
  for(ir in 1:nrow(TableRows)){
    if(TableRows$RowFunction[ir]=="tmsd"){
      evalstring=paste0("data[,",TableRows$RowInternalName[ir],":=tmsd(",TableRows$RowVarID[ir],",d,includeN=",includeN,")]")
      eval(parse(text=evalstring))
    }else if(TableRows$RowFunction[ir]=="tpm"){
      # if(class(TableRows$RowOption[ir])=="character"){
      #   evalstring=paste0("data[,",TableRows$RowInternalName[ir],":=tpm(",TableRows$RowVarID[ir],",\"",TableRows$RowOption[ir],"\",d,inclfraction=TRUE,compressed=TRUE)]")
      # }else{
      #   evalstring=paste0("data[,",TableRows$RowInternalName[ir],":=tpm(",TableRows$RowVarID[ir],",",TableRows$RowOption[ir],",d,inclfraction=TRUE,compressed=TRUE)]")
      # }
      evalstring=paste0("data[,",TableRows$RowInternalName[ir],":=tpm(",TableRows$RowVarID[ir],",",TableRows$RowOption[ir],",d,inclfraction=TRUE,compressed=TRUE)]")
      eval(parse(text=evalstring))
    }else if(TableRows$RowFunction[ir]=="tpoc"){
      evalstring=paste0("data[,",TableRows$RowInternalName[ir],":=tpoc(",TableRows$RowVarID[ir],",",TableRows$RowOption[ir],",d,inclfraction=TRUE,compressed=TRUE)]")
      eval(parse(text=evalstring))
    }
  }

  # What will we keep? Make shortened versions at will, just keep orig for convenience
  row_out=data[1,((TableRows$RowInternalName)),with=FALSE]
  o[[ig]] <- row_out
  
}

# Massage into basic table, for now
tableout=(o[[1]])
for(i in 2:length(o)){
  tableout=rbind(tableout,o[[i]])
}
tableout=as.data.table(t(tableout))
tableout=cbind(TableRows$RowLabel,tableout)
names(tableout)=c("rownames",TableGroups$GroupName)
tableout

# Make the formal column labels
ft <- flextable(as.data.frame(tableout))
# ft <- set_header_df(ft,
#                     mapping=data.frame(
#                       col_key=names(MHSxTable),
#                       Level1=colLabelsLevel1,
#                       #Level2=colLabelsLevel2,
#                       stringsAsFactors=FALSE),
#                     key="col_key")
ft <- autofit(ft)
ft <- theme_booktabs(ft)
ft <- flextable::align(ft,i=1,j=NULL,align="center",part="header")
Table1 <- ft

# Save, if going to
if(saveoptions$resave){
  #docx_file <- read_docx() # why was this here? I have no idea...
  save_as_docx("Table: Expanded"=ft,
               path=paste0(importdirectories$plotsaves,saveoptions$savetag,"_ExpandedTable.docx"))
  # Can see ok if switch to "tabloid" size in landscape :)
}

# Now do a smaller, more targeted one:
# tableout=(o[[1]])
# for(i in 2:length(o)){
#   tableout=rbind(tableout,o[[i]])
# }
# tableout=as.data.table(t(tableout))
# tableout=cbind(TableRows$RowLabel,tableout)
# names(tableout)=c("rownames",TableGroups$GroupName)
# tableout
tableoutsmall=tableout[rownames%in%c("Age (y)","Gender (F)","COVID: Hospitalized","COMPASS31 Total","Adrenergic Symptom Score","Cognition: Neuro-QoL","History of Probable Trauma","PTSD: PCL5 total"),]

# Make the formal column labels
fts <- flextable(as.data.frame(tableoutsmall))
fts <- autofit(fts)
fts <- theme_booktabs(fts)
fts <- flextable::align(fts,i=1,j=NULL,align="center",part="header")
Table1 <- fts

# Save, if going to
if(saveoptions$resave){
  save_as_docx("Table 1"=fts,
               path=paste0(importdirectories$plotsaves,saveoptions$savetag,"_Table1.docx"))
}


##############################
# Now, recreate the mediation analysis:
############

# Decode / Want to pull out:
# tau.coef, tau.ci, tau.p: Total effect=sum of mediation (indirect) effect and direct effect
# z0, z0.ci, z0.p: ADE=average direct effect = direct effect of X on Y after taking into account (removing) mediation from M
# d0, d0.ci, d0.p: ACME=average causal mediation effects=total effect minus direct effect
# n0, n0.ci, n0.p: proportion mediated
#mlist <- c("asi1total","asi1totalnoncog","asi1totalnosleep","asi2total","asi2totalnoncog","asi2totalnosleep","asi3total","asiSxTotal","asiSxTotalnoncog","asiSxTotalnosleep",
#           "asiN1mostadrenergic","asiN2peripheral","asiN3mostadrenergicnotpostural","asiN4peripheralnotpostural","COMPASS_total")
#mlist <- c("COMPASS_total","asiN2peripheral","asi1total")
mlist <- c("COMPASS_total","asi5item")
#mlist <- c("asiSxTotalnoncog","asi3total")
xlist <- c("COVID_categorization")
#ylist <- c("PROMIS_fatigue6a_total","PROMIS_painInt3a_total","ESStotal","isi_total","NeuroQoLtotal","whoQoLD1raw","whoQoLD2raw",
#           "TMB_Continuousconcentration","TMB_MatchingShapesandNumbers", "TMB_QuickAdditionTask", "TMB_FastChoices", "TMB_RememberingWordsRecall", "TMB_VisualPatterns")
ylist <- c("PROMIS_fatigue6a_total","NeuroQoLtotal","whoQoLD1raw","whoQoLD2raw")
#grouplist <- c("All","HCW","FR")
grouplist <- c("All")
var2collect <- c("te","ade","acme","pm")
attributes2collect <- c("point","CI_lower","CI_upper","pvalue")
#subfromkey(vector2sub,key_from,key_to,nomatch="skip")
var2collectkey <-c("tau","z0","d0","n0")
attributes2collectkey <- c("",".ci",".ci",".p")
Nsims=10000

h_out <- data.table(m=character(),x=character(),y=character(),g=character(),
                    var=character(),attribute=character(),value=numeric())

for(im in 1:length(mlist)){
  for(ix in 1:length(xlist)){
    for(iy in 1:length(ylist)){
      for(ig in 1:length(grouplist)){
        X <- xlist[ix]
        Y <- ylist[iy]
        M <- mlist[im]
        dt <- data.table(x=dp$data[,get(X)],y=dp$data[,get(Y)],m=dp$data[,get(M)],age=dp$normed[,age],gendernnb=dp$normed[,gendernnb]) # quite sure there is a more effic way to do this!
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
saveRDS(h_out,paste0(importdirectories$workingdatasaves,"h_out4"))
#h_out=readRDS(paste0(importdirectories$workingdatasaves,"h_out4_",savefilename))



# Put in a plot. Plan:
# Will facet wrap by x and m
# will make var the x-axis location (tau, ade, acme)

# Make the xaxis location
h_out[,xaxislocation:=(as.numeric(subfromkey(var,c("te","ade","acme","pm"),c(0,.15,.3,.45))))]
# Version of the yaxis not to mess with for later subsetting
h_out[,yhold:=y]
#h_out[,xaxislocation:=(as.numeric(subfromkey(m,unique(h_out$m),1:4))+
#                         as.numeric(subfromkey(var,c("te","ade","acme","pm"),c(0,.15,.3,.45))))]
ho <- copy(h_out)
#Pick subset if too many at once and diff axes
#ho=ho[(y%in%c("TMB_Continuousconcentration","TMB_MatchingShapesandNumbers", "TMB_QuickAdditionTask", "TMB_FastChoices", "TMB_RememberingWordsRecall", "TMB_VisualPatterns"))]
# flip the sign on where needed to make it consistent across all measures & pos effect directions
reversedirection_y=c("NeuroQoLtotal","whoQoLD1raw","whoQoLD2raw",
                     "TMB_Continuousconcentration","TMB_MatchingShapesandNumbers", "TMB_QuickAdditionTask",
                     "TMB_FastChoices", "TMB_RememberingWordsRecall", "TMB_VisualPatterns")
TMBentries=c("TMB_Continuousconcentration","TMB_MatchingShapesandNumbers", "TMB_QuickAdditionTask",
             "TMB_FastChoices", "TMB_RememberingWordsRecall", "TMB_VisualPatterns")
ho[y%in%reversedirection_y][attribute%in%c("point","CI_lower","CI_upper")]$value <-
  -ho[y%in%reversedirection_y][attribute%in%c("point","CI_lower","CI_upper")]$value
# cast to line up for plotting
hd <- as.data.table(dcast(ho,m+x+y+yhold+g+var+xaxislocation~attribute))
hd <- hd[g=="All"][var%in%c("te","ade","acme")]
# rename the options so the facet labeling is nice
hd$y <- subfromkey(hd$y,
                   key_from=c("PROMIS_painInt3a_total","PROMIS_fatigue6a_total","isi_total","NeuroQoLtotal","whoQoLD2raw","whoQoLD1raw",
                              "ESStotal","TMB_Continuousconcentration","TMB_FastChoices","TMB_MatchingShapesandNumbers","TMB_QuickAdditionTask",
                              "TMB_RememberingWordsRecall","TMB_VisualPatterns"),
                   key_to = c("Pain (PROMIS-3a)","Fatigue (PROMIS-6a)","Insomnia (ISI)","Cognition (Neuro-QoL)","Mental Health (WHOQoL-D2)","Physical Health (WHOQoL-D1)",
                              "Sleepiness (Epworth)","TMB Continuous Concentration","TMB Fast Choices","TMB Matching Shapes and Numbers","TMB Quick AdditionTask",
                              "TMB Remembering Words Recall","TMB Visual Patterns"))
hd$y <- factor(hd$y,levels=c("Physical Health (WHOQoL-D1)","Mental Health (WHOQoL-D2)","Cognition (Neuro-QoL)","Insomnia (ISI)","Fatigue (PROMIS-6a)","Pain (PROMIS-3a)",
                             "Sleepiness (Epworth)","TMB Continuous Concentration","TMB Fast Choices","TMB Matching Shapes and Numbers","TMB Quick AdditionTask",
                             "TMB Remembering Words Recall","TMB Visual Patterns"))
hd$m <- subfromkey(hd$m,
                   key_from=c("asi1total","asiN2peripheral",
                              "COMPASS_total","asi5item"),
                   key_to = c("Adrenergic Symptoms (central&peripheral)","Adrenergic Symptoms (peripheral)",
                              "Autonomic Symptoms (COMPASS-31)","Adrenergic Symptom Score"))
#hd$m <- factor(hd$m,
#               levels=c("Autonomic Symptoms (COMPASS-31)"),
#               labels=c("Direct effect of COVID-19 history\n & mediating effect of autonomic\nsymptoms (COMPASS-31) on\nself-reported outcomes"))
hd$m <- factor(hd$m,
               levels=c("Autonomic Symptoms (COMPASS-31)","Adrenergic Symptom Score"),
               labels=c("Autonomic Symptoms\n(COMPASS-31)","Adrenergic\nSymptom Score"))
# Adjust p-values bon fernoni for mult comparisons:
hd$pvalue <- hd$pvalue*length(unique(hd$m))*length(unique(hd$x))
hd[,yaxP:=max(c(CI_lower,CI_upper))+.75,by=1:nrow(hd)]
# Now we'll subdivide it, since the structure name is hardcoded in below:
hdhold=copy(hd)
hd=hd[!(yhold%in%TMBentries)]
# Plot it!
p1 <- ggplot()+
  geom_point(data=hd,aes(x=xaxislocation,y=point),shape=1)+
  facet_grid(y~m,labeller=labeller(y=label_wrap_gen(10)))+
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
  scale_y_continuous(breaks=c(0,5,10),limits=c(-2,10))
p1

ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"MediationSRF1.png"),
       p1,width=3.8,height=5,units="in",dp=600)

##########

# Also need to look at asi5item vs COMPASS:
ggplot(data=dp$data,aes(x=COMPASS_total,y=asi5item,color=COVID_categorization))+
  geom_point()+
  stat_cor(method="pearson")+ geom_smooth(method=lm)

ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=COMPASS_total,y=asi5item))+
  geom_point()+
  stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black")+
  theme_classic()+
  xlab("Autonomic Symptoms\n(COMPASS-31 Total Score)")+
  ylab("Adrenergic Symptom Score")
  

# Version for poster:
dataforplot=copy(dp$data[,.(PtID,COMPASS_total,asi5item,COVID_categorization)])
dataforplot$COVID_categorization <- factor(dataforplot$COVID_categorization,
               levels=c("FALSE","TRUE"),
               labels=c("COVID History: FALSE","COVID History: TRUE"))

p1=ggplot(data=dataforplot,aes(x=COMPASS_total,y=asi5item))+
  geom_point(position=position_jitter(w = 0.15, h = 0.15),alpha=.4)+
  stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black")+
  theme_classic()+
  xlab("Autonomic Symptoms\n(COMPASS-31 Total Score)")+
  ylab("Adrenergic Symptom Score")+facet_wrap(~COVID_categorization)
p1
  #  geom_point(aes(color=PHx_psych_PTSD),position=position_jitter(w = 0.1, h = 0.1))+
#  stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

if(saveoptions$resave){
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_ASIvsCOMPASSbyCOVIDstatus.jpeg"),
         p1,width=8,height=4,units="in",dp=600)
}

##########

#Note re: progress:
ggplot(dp$data,aes(x=COVID_categorization,y=Progress))+geom_boxplot(outlier.alpha=.5,position="dodge2")+theme_classic()

dp$data[,.(N=length(PtID)),by=c("Progress","COVID_categorization")]
datacopy=copy(dp$data)
datacopy[,ProgressRounded:=round(Progress,digits=-1)]
setkey(datacopy,"ProgressRounded")
dd=datacopy[,.(N=length(PtID)),by=c("ProgressRounded","COVID_categorization")]
datacopy[COVID_categorization==TRUE,.(N=length(PtID)),by=c("ProgressRounded")]
datacopy[COVID_categorization==FALSE,.(N=length(PtID)),by=c("ProgressRounded")]

ggplot(datacopy,aes(x=ProgressRounded))+geom_histogram()+facet_wrap(~COVID_categorization)+theme_classic()

#  geom_jitter(color="black",size=.4,alpha=.7,width=.2)+theme_classic()
#  geom_point(position=position_jitter(w=.1,h=.1))


###
# OOooooookay. Now, um, the acutal rest of the analyses...


# Stats for the direct comparisons below:
t.test(dp$data[COVID_categorization==TRUE]$COMPASS_total,dp$data[COVID_categorization==FALSE]$COMPASS_total)
 # p <1e-6
t.test(dp$data[COVID_categorization==TRUE]$NeuroQoLtotal,dp$data[COVID_categorization==FALSE]$NeuroQoLtotal)
 # p<1e-6
summary(lm(COMPASS_total~age+gender+COVID_categorization,data=dp$data))
 # p<1e-6
summary(lm(NeuroQoLtotal~age+gender+COVID_categorization,data=dp$data))
 # p<1e-5

library(gridExtra)
p1=ggplot(data=dp$data,aes(x=COVID_categorization,y=COMPASS_total))+geom_boxplot()+
  geom_jitter(color="black",size=.4,alpha=.7,width=.2)+theme_classic()+
  xlab("History of COVID-19")+ylab("Autonomic Symptom Burden\n(COMPASS-31 Total Score)")
p2=ggplot(data=dp$data,aes(x=COVID_categorization,y=NeuroQoLtotal))+geom_boxplot()+
  geom_jitter(color="black",size=.4,alpha=.7,width=.2)+theme_classic()+
  xlab("History of COVID-19")+ylab('Cognitive Functioning\n(NeuroQoL Total Score)')
p3=ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=COMPASS_total,y=NeuroQoLtotal))+
  geom_point()+
  #stat_cor(method="pearson",label.x.npc=.5)+ 
  stat_cor(method="pearson",label.x.npc=.5,label.y=37.5)+ 
  geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)+theme_classic()+
  xlab("Autonomic Symptom Burden\n(COMPASS-31 Total Score)")+ylab('Cognitive Functioning (NeuroQoL Total Score)')

# Tag them with panel labels
p1=p1+labs("A")
p2=p2+labs("B")
p3=p3+labs("C")
ml <- grid.arrange(grobs=list(p1,p2,p3),
                   nrow=1,ncol=3,
                   widths=c(.4,.4,.6))

# Save, if going to
if(saveoptions$resave){
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_NeuroQoLvsCOVIDhxandCOMPASS.jpeg"),
         ml,width=8,height=4,units="in",dp=600)
}



# Slight tangent to look the 'other direction':

p1=ggplot(data=dp$data,aes(x=COVID_categorization,y=COMPASS_total))+geom_boxplot(size=.3)+
  geom_jitter(color="black",size=.4,alpha=.7,width=.2)+theme_classic()+
  xlab("History of COVID-19")+ylab('Autonomic Symptom Burden\n(COMPASS-31 Total Score)')+
  facet_wrap(~lecAtotal3cat)
p1
anovaout=aov(COMPASS_total~COVID_categorization+lecAtotal3cat,data=dp$data)
summary(anovaout)
dp$data[,lecAtotalbinary:=as.logical(lecAtotal),by=1:nrow(dp$data)]
summary(lm(COMPASS_total~age+gender+COVID_categorization*lecAtotalbinary,data=dp$data))

# Save, if going to
if(saveoptions$resave){
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_COMPASSvsCOVIDandTHx3cat.jpeg"),
         p1,width=5,height=3,units="in",dp=600)
}

# Repeat with asi5item:
# Slight tangent to look the 'other direction':
p1=ggplot(data=dp$data,aes(x=COVID_categorization,y=asi5item))+geom_boxplot(size=.3)+
  geom_jitter(color="black",size=.4,alpha=.7,width=.2)+theme_classic()+
  xlab("History of COVID-19")+ylab('Adrenergic Symptom Score')+
  facet_wrap(~lecAtotal3cat)
p1
anovaout=aov(asi5item~COVID_categorization+lecAtotal3cat,data=dp$data)
summary(anovaout)
dp$data[,lecAtotalbinary:=as.logical(lecAtotal),by=1:nrow(dp$data)]
var2norm=c("lecAtotalbinary","asi5item")
for(iv in 1:length(var2norm)){
  x <- dp$data[,((var2norm[iv])),with=FALSE]
  y <- scale(x,center=TRUE,scale=TRUE)
  dp$normed[,(var2norm[iv]):=y]
}
summary(lm(asi5item~age+gendernnb+COVID_categorization*lecAtotalbinary,data=dp$data))

# Save, if going to
if(saveoptions$resave){
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_ASIvsCOVIDandTHx3cat.jpeg"),
         p1,width=5,height=3,units="in",dp=600)
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

# Both sig relate to pcl and COVID when done without asi:
summary(lm(PROMIS_painInt3a_total~age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(PROMIS_fatigue6a_total~age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
# But asi takes over when added:
summary(lm(PROMIS_painInt3a_total~asi1total+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(PROMIS_fatigue6a_total~asi1total+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
# any different with asiSx? Not really..
summary(lm(PROMIS_painInt3a_total~asiSxTotal+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(PROMIS_fatigue6a_total~asiSxTotal+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
# How about COMPASS?
summary(lm(PROMIS_painInt3a_total~COMPASS_total+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(PROMIS_fatigue6a_total~COMPASS_total+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed)) # only partly..
# ASi peripheral?
summary(lm(PROMIS_painInt3a_total~asiN2peripheral+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed))
summary(lm(PROMIS_fatigue6a_total~asiN2peripheral+age+gendernnb+pcl5total+nTBIwLOC+COVID_categorization,data=dp$normed)) # this time it gets all of covid at least..

# Try repeating with lecA instead of pcl5:
# Both sig relate to pcl and COVID when done without asi:
summary(lm(PROMIS_painInt3a_total~age+gendernnb+lecAtotal+nTBIwLOC+COVID_categorization,data=dp$normed)) # yes here
summary(lm(PROMIS_fatigue6a_total~age+gendernnb+lecAtotal+nTBIwLOC+COVID_categorization,data=dp$normed)) # no here!
# But asi takes over when added:
summary(lm(PROMIS_painInt3a_total~asi1total+age+gendernnb+lecAtotal+nTBIwLOC+COVID_categorization,data=dp$normed)) # asi only takes some of lecA...
summary(lm(PROMIS_fatigue6a_total~asi1total+age+gendernnb+lecAtotal+nTBIwLOC+COVID_categorization,data=dp$normed))

# Now it's a clean result: 
summary(lm(COMPASS_total~age+gendernnb+COVID_categorization*lecAtotal,data=dp$normed))
# Less clear for other outcomes, although still same diretcion/trend:
summary(lm(asiSxTotal~age+gendernnb+COVID_categorization*lecAtotal,data=dp$normed))
summary(lm(asiN2peripheral~age+gendernnb+COVID_categorization*lecAtotal,data=dp$normed))
# Doesn't work using pcl5, which, I think makes sense???
summary(lm(COMPASS_total~age+gendernnb+COVID_categorization*pcl5total,data=dp$normed))

# Other outcomes: with pcl5...
summary(lm(COMPASS_total~age+gendernnb+COVID_categorization*lecAtotal+pcl5total,data=dp$normed))
summary(lm(asiSxTotal~age+gendernnb+COVID_categorization*lecAtotal+pcl5total,data=dp$normed))
summary(lm(PROMIS_painInt3a_total~age+gendernnb+COVID_categorization*lecAtotal+pcl5total,data=dp$normed))
summary(lm(PROMIS_fatigue6a_total~age+gendernnb+COVID_categorization*lecAtotal+pcl5total,data=dp$normed))
summary(lm(NeuroQoLtotal~age+gendernnb+COVID_categorization*lecAtotal+pcl5total,data=dp$normed))
summary(lm(whoQoLD1raw~age+gendernnb+COVID_categorization*lecAtotal+pcl5total,data=dp$normed))
summary(lm(whoQoLD2raw~age+gendernnb+COVID_categorization*lecAtotal+pcl5total,data=dp$normed))
summary(lm(ESStotal~age+gendernnb+COVID_categorization*lecAtotal+pcl5total,data=dp$normed))
summary(lm(isi_total~age+gendernnb+COVID_categorization*lecAtotal+pcl5total,data=dp$normed))
# without pcl5...
summary(lm(COMPASS_total~age+gendernnb+COVID_categorization*lecAtotal,data=dp$normed))
summary(lm(asiSxTotal~age+gendernnb+COVID_categorization*lecAtotal,data=dp$normed))
summary(lm(PROMIS_painInt3a_total~age+gendernnb+COVID_categorization*lecAtotal,data=dp$normed))
summary(lm(PROMIS_fatigue6a_total~age+gendernnb+COVID_categorization*lecAtotal,data=dp$normed))
summary(lm(NeuroQoLtotal~age+gendernnb+COVID_categorization*lecAtotal,data=dp$normed))
summary(lm(whoQoLD1raw~age+gendernnb+COVID_categorization*lecAtotal,data=dp$normed))
summary(lm(whoQoLD2raw~age+gendernnb+COVID_categorization*lecAtotal,data=dp$normed))
# Other outcomes: with pcl5notE...
summary(lm(COMPASS_total~age+gendernnb+COVID_categorization*lecAtotal+pcl5notE,data=dp$normed))
summary(lm(asiSxTotal~age+gendernnb+COVID_categorization*lecAtotal+pcl5notE,data=dp$normed))
summary(lm(PROMIS_painInt3a_total~age+gendernnb+COVID_categorization*lecAtotal+pcl5notE,data=dp$normed))
summary(lm(PROMIS_fatigue6a_total~age+gendernnb+COVID_categorization*lecAtotal+pcl5notE,data=dp$normed))
summary(lm(NeuroQoLtotal~age+gendernnb+COVID_categorization*lecAtotal+pcl5notE,data=dp$normed))
summary(lm(whoQoLD1raw~age+gendernnb+COVID_categorization*lecAtotal+pcl5notE,data=dp$normed))
summary(lm(whoQoLD2raw~age+gendernnb+COVID_categorization*lecAtotal+pcl5notE,data=dp$normed))
summary(lm(ESStotal~age+gendernnb+COVID_categorization*lecAtotal+pcl5notE,data=dp$normed))
summary(lm(isi_total~age+gendernnb+COVID_categorization*lecAtotal+pcl5notE,data=dp$normed))


# Visualize: COMPASS_total, asiSxTotal, "PROMIS_fatigue6a_total","PROMIS_painInt3a_total",
# "ESStotal","isi_total","NeuroQoLtotal","whoQoLD1raw","whoQoLD2raw",

# Visualize (have to normalize first):
reltextsize=1.3
labelsize=9

# Flip the direction on the up is good ones:
dp$normed[,NeuroQoLtotalN:=-NeuroQoLtotal,by=1:nrow(dp$normed)]
dp$normed[,whoQoLD1rawN:=-whoQoLD1raw,by=1:nrow(dp$normed)]
dp$normed[,whoQoLD2rawN:=-whoQoLD2raw,by=1:nrow(dp$normed)]

COMPASS_total_fit=lm(COMPASS_total~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)
asiSxTotal_fit=lm(asiSxTotal~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)
PROMIS_fatigue6a_total_fit=lm(PROMIS_fatigue6a_total~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)
PROMIS_painInt3a_total_fit=lm(PROMIS_painInt3a_total~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)
ESStotal_fit=lm(ESStotal~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)
isi_total_fit=lm(isi_total~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)
NeuroQoLtotal_fit=lm(NeuroQoLtotalN~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)
whoQoLD1raw_fit=lm(whoQoLD1rawN~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)
whoQoLD2raw_fit=lm(whoQoLD2rawN~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)

plotsumms1 <- plot_summs(COMPASS_total_fit, asiSxTotal_fit, PROMIS_fatigue6a_total_fit, PROMIS_painInt3a_total_fit, ESStotal_fit, isi_total_fit,  #whoQoLD1raw_fit, whoQoLD2raw_fit,
                        legend.title="Predicted Variable",
                        model.names=c("COMPASS Total","ASI Total","Fatigue","Pain","ESS","ISI"))+ # ,"WHO QoL D1","WHO QoL D2"
  scale_y_discrete(name="",labels=rev(c("COVID Hx","Trauma Hx","PCL Total","Age","Gender:\nMale","COVID*Trauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms1

plotsumms2 <- plot_summs(NeuroQoLtotal_fit, whoQoLD1raw_fit, whoQoLD2raw_fit,
                        legend.title="Predicted Variable",
                        model.names=c("NeuroQoL","WHO QoL D1","WHO QoL D2"))+ 
  scale_y_discrete(name="",labels=rev(c("COVID Hx","Trauma Hx","PCL Total","Age","Gender:\nMale","COVID*Trauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms2

plotsumms3 <- plot_summs(COMPASS_total_fit, whoQoLD1raw_fit, whoQoLD2raw_fit, NeuroQoLtotal_fit, PROMIS_fatigue6a_total_fit,
                         legend.title="Predicted Variable",
                         model.names=c("Autonomic Symptoms\n(COMPASS 31 Total)","Physical Helath\n(WHOQoL-D1)","Mental Health\n(WHOQoL-D2)","Cognition\n(Neuro-QoL)","Fatigue\n(PROMIS-6a)"))+
  scale_y_discrete(name="",labels=rev(c("COVID\nHistory","Trauma\nHistory","PTSD\nsymptoms\n(PCL Total)","Age","Gender:\nMale","Interaction:\nCOVIDxTrauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms3

if(saveoptions$resave){
  w=9
  h=6
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions_PCLincluded.jpeg"),
         plotsumms3,width=w,height=h,units="in",dp=600)
}

# Without PCL:
COMPASS_total_fit=lm(COMPASS_total~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
asi5item_fit=lm(asi5item~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)
PROMIS_fatigue6a_total_fit=lm(PROMIS_fatigue6a_total~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
NeuroQoLtotal_fit=lm(NeuroQoLtotalN~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
whoQoLD1raw_fit=lm(whoQoLD1rawN~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
whoQoLD2raw_fit=lm(whoQoLD2rawN~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)

plotsumms4 <- plot_summs(COMPASS_total_fit, whoQoLD1raw_fit, whoQoLD2raw_fit, NeuroQoLtotal_fit, PROMIS_fatigue6a_total_fit,
                         legend.title="Predicted Variable",
                         model.names=c("Autonomic Symptoms\n(COMPASS 31 Total)","Physical Helath\n(WHOQoL-D1)","Mental Health\n(WHOQoL-D2)","Cognition\n(Neuro-QoL)","Fatigue\n(PROMIS-6a)"))+
  scale_y_discrete(name="",labels=rev(c("COVID\nHistory","Trauma\nHistory","Age","Gender:\nMale","Interaction:\nCOVID x\nTrauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms4

if(saveoptions$resave){
  w=9
  h=6
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions_PCLnotincluded.jpeg"),
         plotsumms4,width=w,height=h,units="in",dp=600)
}

# Just COMPASS
COMPASS_total_fit_wPCL=lm(COMPASS_total~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)
COMPASS_total_fit_woPCL=lm(COMPASS_total~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)

plotsumms5 <- plot_summs(COMPASS_total_fit_wPCL,
                         legend.title="Predicted Variable",
                         model.names=c("Autonomic Symptoms\n(COMPASS 31 Total)"))+
  scale_y_discrete(name="",labels=rev(c("COVID\nHistory","Trauma\nHistory","PTSD\nsymptoms\n(PCL Total)","Age","Gender:\nMale","Interaction:\nCOVIDxTrauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms5
plotsumms6 <- plot_summs(COMPASS_total_fit_woPCL,
                         legend.title="Predicted Variable",
                         model.names=c("Autonomic Symptoms\n(COMPASS 31 Total)"))+
  scale_y_discrete(name="",labels=rev(c("COVID\nHistory","Trauma\nHistory","Age","Gender:\nMale","Interaction:\nCOVID x\nTrauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms6

if(saveoptions$resave){
  w=6
  h=4.5
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions_COMPASSonly_PCLincluded.jpeg"),
         plotsumms5,width=w,height=h,units="in",dp=600)
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions_COMPASSonly_PCLnotincluded.jpeg"),
         plotsumms6,width=w,height=h,units="in",dp=600)
}

# In parallel,just ASI5item:
asi5item_fit_wPCL=lm(asi5item~COVID_categorization*lecAtotal+pcl5total+age+gendernnb,data=dp$normed)
asi5item_fit_woPCL=lm(asi5item~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)

plotsumms5 <- plot_summs(asi5item_fit_wPCL,
                         legend.title="Predicted Variable",
                         model.names=c("Adrenergic Symptom Total"))+
  scale_y_discrete(name="",labels=rev(c("COVID\nHistory","Trauma\nHistory","PTSD\nsymptoms\n(PCL Total)","Age","Gender:\nMale","Interaction:\nCOVIDxTrauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms5
plotsumms6 <- plot_summs(asi5item_fit_woPCL,
                         legend.title="Predicted Variable",
                         model.names=c("Adrenergic Symptom Total"))+
  scale_y_discrete(name="",labels=rev(c("COVID\nHistory","Trauma\nHistory","Age","Gender:\nMale","Interaction:\nCOVID x\nTrauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms6

if(saveoptions$resave){
  w=6
  h=4.5
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions_asi5itemsonly_PCLincluded.jpeg"),
         plotsumms5,width=w,height=h,units="in",dp=600)
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions_asi5itemsonly_PCLnotincluded.jpeg"),
         plotsumms6,width=w,height=h,units="in",dp=600)
}

# COMPASS and ASI
plotsumms9 <- plot_summs(COMPASS_total_fit_wPCL,asi5item_fit_wPCL,
                         legend.title="Predicted Variable",
                         model.names=c("Autonomic Symptoms\n(COMPASS 31 Total)","Adrenergic Symptom Total"))+
  scale_y_discrete(name="",labels=rev(c("COVID\nHistory","Trauma\nHistory","PTSD\nsymptoms\n(PCL Total)","Age","Gender:\nMale","Interaction:\nCOVIDxTrauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms9
plotsumms10 <- plot_summs(COMPASS_total_fit_woPCL,asi5item_fit_woPCL,
                         legend.title="Predicted Variable",
                         model.names=c("Autonomic Symptoms\n(COMPASS 31 Total)","Adrenergic Symptom Total"))+
  scale_y_discrete(name="",labels=rev(c("COVID\nHistory","Trauma\nHistory","Age","Gender:\nMale","Interaction:\nCOVID x\nTrauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms10

if(saveoptions$resave){
  w=9
  h=4.5
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions_compassANDasi5items_PCLincluded.jpeg"),
         plotsumms9,width=w,height=h,units="in",dp=600)
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions_compassANDasi5items_PCLnotincluded.jpeg"),
         plotsumms10,width=w,height=h,units="in",dp=600)
}


summary(COMPASS_total_fit)
summary(whoQoLD1raw_fit)
summary(whoQoLD2raw_fit)
summary(PROMIS_fatigue6a_total_fit)
summary(NeuroQoLtotal_fit)

#### If redo but with pcl5notE:

COMPASS_total_fit=lm(COMPASS_total~COVID_categorization*lecAtotal+pcl5notE+age+gendernnb,data=dp$normed)
asiSxTotal_fit=lm(asiSxTotal~COVID_categorization*lecAtotal+pcl5notE+age+gendernnb,data=dp$normed)
PROMIS_fatigue6a_total_fit=lm(PROMIS_fatigue6a_total~COVID_categorization*lecAtotal+pcl5notE+age+gendernnb,data=dp$normed)
PROMIS_painInt3a_total_fit=lm(PROMIS_painInt3a_total~COVID_categorization*lecAtotal+pcl5notE+age+gendernnb,data=dp$normed)
ESStotal_fit=lm(ESStotal~COVID_categorization*lecAtotal+pcl5notE+age+gendernnb,data=dp$normed)
isi_total_fit=lm(isi_total~COVID_categorization*lecAtotal+pcl5notE+age+gendernnb,data=dp$normed)
NeuroQoLtotal_fit=lm(NeuroQoLtotal~COVID_categorization*lecAtotal+pcl5notE+age+gendernnb,data=dp$normed)
whoQoLD1raw_fit=lm(whoQoLD1raw~COVID_categorization*lecAtotal+pcl5notE+age+gendernnb,data=dp$normed)
whoQoLD2raw_fit=lm(whoQoLD2raw~COVID_categorization*lecAtotal+pcl5notE+age+gendernnb,data=dp$normed)

plotsumms3 <- plot_summs(COMPASS_total_fit, asiSxTotal_fit, PROMIS_fatigue6a_total_fit, PROMIS_painInt3a_total_fit, ESStotal_fit, isi_total_fit,  #whoQoLD1raw_fit, whoQoLD2raw_fit,
                         legend.title="Predicted Variable",
                         model.names=c("COMPASS Total","ASI Total","Fatigue","Pain","ESS","ISI"))+ # ,"WHO QoL D1","WHO QoL D2"
  scale_y_discrete(name="",labels=rev(c("COVID Hx","Trauma Hx","PCL Total (not E)","Age","Gender:\nMale","COVID*Trauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms3

plotsumms4 <- plot_summs(NeuroQoLtotal_fit, whoQoLD1raw_fit, whoQoLD2raw_fit,
                         legend.title="Predicted Variable",
                         model.names=c("NeuroQoL","WHO QoL D1","WHO QoL D2"))+ 
  scale_y_discrete(name="",labels=rev(c("COVID Hx","Trauma Hx","PCL Total (not E)","Age","Gender:\nMale","COVID*Trauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms4


if(saveoptions$resave){
  w=9
  h=9
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions3notE.jpeg"),
         plotsumms3,width=w,height=h,units="in",dp=600)
}
if(saveoptions$resave){
  w=9
  h=9
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions4notE.jpeg"),
         plotsumms4,width=w,height=h,units="in",dp=600)
}

###### And without pcl:
COMPASS_total_fit=lm(COMPASS_total~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
asiSxTotal_fit=lm(asiSxTotal~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
PROMIS_fatigue6a_total_fit=lm(PROMIS_fatigue6a_total~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
PROMIS_painInt3a_total_fit=lm(PROMIS_painInt3a_total~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
ESStotal_fit=lm(ESStotal~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
isi_total_fit=lm(isi_total~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
NeuroQoLtotal_fit=lm(NeuroQoLtotal~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
whoQoLD1raw_fit=lm(whoQoLD1raw~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)
whoQoLD2raw_fit=lm(whoQoLD2raw~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)

plotsumms5 <- plot_summs(COMPASS_total_fit, asiSxTotal_fit, PROMIS_fatigue6a_total_fit, PROMIS_painInt3a_total_fit, ESStotal_fit, isi_total_fit,  #whoQoLD1raw_fit, whoQoLD2raw_fit,
                         legend.title="Predicted Variable",
                         model.names=c("COMPASS Total","ASI Total","Fatigue","Pain","ESS","ISI"))+ # ,"WHO QoL D1","WHO QoL D2"
  scale_y_discrete(name="",labels=rev(c("COVID Hx","Trauma Hx","Age","Gender:\nMale","COVID*Trauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms5

plotsumms6 <- plot_summs(NeuroQoLtotal_fit, whoQoLD1raw_fit, whoQoLD2raw_fit,
                         legend.title="Predicted Variable",
                         model.names=c("NeuroQoL","WHO QoL D1","WHO QoL D2"))+ 
  scale_y_discrete(name="",labels=rev(c("COVID Hx","Trauma Hx","Age","Gender:\nMale","COVID*Trauma")))+
  scale_x_continuous(name="Impact on Symptoms: Estimate and 95% CI",labels = NULL,breaks = NULL)+
  theme(axis.text=element_text(size=labelsize,face="bold"),
        axis.title.x = element_text(size=rel(reltextsize)),
        axis.text.y = element_text(size=rel(reltextsize)),
        legend.text=element_text(size=rel(reltextsize)),
        legend.title = element_text(size=rel(reltextsize)))
plotsumms6


if(saveoptions$resave){
  w=9
  h=9
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions5nopcl.jpeg"),
         plotsumms5,width=w,height=h,units="in",dp=600)
}
if(saveoptions$resave){
  w=9
  h=9
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_interactions6notpcl.jpeg"),
         plotsumms6,width=w,height=h,units="in",dp=600)
}

###

# Even with pcl5 in the model a positive interaction term, ish???
summary(lm(asiSxTotalnoncog~pcl5total+COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiSxTotalnoncog~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiSxTotalnoncog~pcl5notE+COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiSxTotalnoncog~pcl5clusterB+COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----

# Relationship to other diagnoses: just the first one I loked at...!
summary(lm(NDx_rheum_MECFS~lecAtotal,data=dp$data[COVID_categorization==TRUE]))
summary(lm(NDx_cards_tachycardia~lecAtotal,data=dp$data[COVID_categorization==TRUE]))
summary(lm(NDx_cards_hypotension~lecAtotal,data=dp$data[COVID_categorization==TRUE]))
summary(lm(NDx_cards_POTS~lecAtotal,data=dp$data[COVID_categorization==TRUE]))
summary(lm(NDx_rheum_fibromyalgia~lecAtotal,data=dp$data[COVID_categorization==TRUE]))


# Oh, and, relationship of neuroQoL to TMB?
tmb1=ggplot(data=dp$data,aes(x=get(TMBentries[1]),y=NeuroQoLtotal,color=COVID_categorization))+geom_point()+geom_smooth(method=lm)+stat_cor(method="pearson")
tmb2=ggplot(data=dp$data,aes(x=get(TMBentries[2]),y=NeuroQoLtotal,color=COVID_categorization))+geom_point()+geom_smooth(method=lm)+stat_cor(method="pearson")
tmb3=ggplot(data=dp$data,aes(x=get(TMBentries[3]),y=NeuroQoLtotal,color=COVID_categorization))+geom_point()+geom_smooth(method=lm)+stat_cor(method="pearson")
tmb4=ggplot(data=dp$data,aes(x=get(TMBentries[4]),y=NeuroQoLtotal,color=COVID_categorization))+geom_point()+geom_smooth(method=lm)+stat_cor(method="pearson")
tmb5=ggplot(data=dp$data,aes(x=get(TMBentries[5]),y=NeuroQoLtotal,color=COVID_categorization))+geom_point()+geom_smooth(method=lm)+stat_cor(method="pearson")
tmb6=ggplot(data=dp$data,aes(x=get(TMBentries[6]),y=NeuroQoLtotal,color=COVID_categorization))+geom_point()+geom_smooth(method=lm)+stat_cor(method="pearson")


ml <- grid.arrange(grobs=list(tmb1,tmb2,tmb3,tmb4,tmb5,tmb6),
                   nrow=2,ncol=3)
ml

# Save, if going to
if(saveoptions$resave){
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_NeuroQoLvsTMB.jpeg"),
         ml,width=20,height=14,units="in",dp=600)
}































# Look at stuff!
p1=ggplot(data=dp$data,aes(x=lecAtotal,y=COMPASS_total,color=COVID_categorization))+
  geom_point()+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
p1
p2=ggplot(data=dp$data,aes(x=lecAtotal,y=COMPASS_total))+
  geom_point()+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)+facet_wrap(~COVID_categorization)
p2
if(saveoptions$resave){
  ggsave(paste0(importdirectories$workingdatasaves,saveoptions$savetag,"_COMPASSvsLECAbyCOVIDhx.jpeg"),
         p2,width=7,height=5,units="in",dp=600)
}


ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=lecAtotal,y=COMPASS_total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==FALSE],aes(x=lecAtotal,y=COMPASS_total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

ggplot(data=dp$data,aes(x=lecAtotal,y=asiN))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=lecAtotal,y=asiNarrow))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==FALSE],aes(x=lecAtotal,y=asiNarrow))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

ggplot(data=dp$data,aes(x=asiN,y=COMPASS_total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=asiN,y=COMPASS_total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==FALSE],aes(x=asiN,y=COMPASS_total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)


ggplot(data=dp$data,aes(x=COMPASS_total,y=asi1total,color=COVID_categorization))+
  geom_point()+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=COMPASS_total,y=asiSxTotal))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=COMPASS_total,y=asi3total,color=COVID_categorization))+
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

ggplot(data=dp$data,aes(x=lecAtotal,y=COMPASS_total,color=COVID_categorization))+
  geom_point(aes())+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)

ggplot(data=dp$data,aes(x=pcl5total,y=COMPASS_total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=pcl5total,y=COMPASS_total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data[COVID_categorization==FALSE],aes(x=pcl5total,y=COMPASS_total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=pcl5total,y=asiSxTotal))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)




ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=lecAtotal,y=asiSxTotal))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
ggplot(data=dp$data,aes(x=nTBI,y=COMPASS_total,color=COVID_categorization))+
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


summary(lm(COMPASS_total~lecAtotal+COVID_categorization+age+gendernnb,data=dp$normed))
summary(lm(COMPASS_total~lecAtotal+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS_total~lecAtotal+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$data)) # <---
summary(lm(COMPASS_total~Dx_psych_PTSD+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$data)) # <---
summary(lm(COMPASS_total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$data)) ## USE THIS ONE OR SOMETHING CLOSE

summary(lm(COMPASS_total~Dx_psych_PTSD*COVID_categorization+nTBIwLOC+COVID_categorization+age+gender,data=dp$data)) # <---
summary(lm(COMPASS_total~PHx_psych_PTSD*COVID_categorization+nTBIwLOC+COVID_categorization+age+gender,data=dp$data)) # <---

# in doc: ******************
summary(lm(COMPASS_total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asiSxTotal~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi1total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi2total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi3total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE

# even better are these 2:
summary(lm(COMPASS_total~pcl5notE+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi1total~pcl5notE+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE



summary(lm(COMPASS_total~pcl5total+nTBIwLOC+age+gendernnb,data=dp$data[COVID_categorization==TRUE])) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(COMPASS_total~pcl5total+nTBIwLOC+age+gendernnb,data=dp$data[COVID_categorization==FALSE])) ## USE THIS ONE OR SOMETHING CLOSE

summary(lm(COMPASS_total~pcl5total+age+gendernnb,data=dp$data[COVID_categorization==TRUE])) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(COMPASS_total~pcl5total+age+gendernnb,data=dp$data[COVID_categorization==FALSE])) ## USE THIS ONE OR SOMETHING CLOSE


summary(lm(COMPASS_total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi1total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asi1totalnoncog~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asi2total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asi2totalnoncog~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asi3total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asi3totalnoncog~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiSxTotal~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiSxTotalnoncog~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----

# Even with pcl5 in the model a positive interaction term, ish???
summary(lm(asiSxTotalnoncog~pcl5total+COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiSxTotalnoncog~COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiSxTotalnoncog~pcl5notE+COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiSxTotalnoncog~pcl5clusterB+COVID_categorization*lecAtotal+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----


summary(lm(COMPASS_total~pcl5notE+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE
summary(lm(asi1total~pcl5notE+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----
summary(lm(asiN2peripheral~pcl5notE+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)) ## USE THIS ONE OR SOMETHING CLOSE <----*****

summary(lm(asiN~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$data)) ## USE THIS ONE OR SOMETHING CLOSE <----*****
summary(lm(asiN~pcl5total+COVID_categorization+age+gendernnb,data=dp$data)) ## USE THIS ONE OR SOMETHING CLOSE <----*****


# Visualize (have to normalize first):
reltextsize=1.3
labelsize=9
compassfit <- lm(COMPASS_total~pcl5total+nTBIwLOC+COVID_categorization+age+gendernnb,data=dp$normed)
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
summary(lm(COMPASS_total~PHx_psych_PTSD+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS_total~PHx_psych_PTSD+age+gendernnb,data=dp$normed[COVID_categorization!=TRUE]))
summary(lm(asi1total~PHx_psych_PTSD+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS_total~nTBIwLOC,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS_total~PHx_psych_PTSD+age+gendernnb,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS_total~Dx_psych_PTSD+age+gendernnb,data=dp$data[COVID_categorization==FALSE]))
summary(lm(COMPASS_total~age+gendernnb+nTBIwLOC,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS_total~age+gendernnb+nTBIwLOC,data=dp$data[COVID_categorization==FALSE]))

summary(lm(COMPASS_total~age+gendernnb+nTBIwLOC,data=dp$data[COVID_categorization==TRUE]))
summary(lm(COMPASS_total~age+gendernnb+nTBIwLOC,data=dp$data[COVID_categorization==FALSE]))


m1=mean(dp$data[COVID_categorization==TRUE][PHx_psych_PTSD==TRUE]$COMPASS_total,na.rm=TRUE)
m2=mean(dp$data[COVID_categorization==TRUE][PHx_psych_PTSD==FALSE]$COMPASS_total,na.rm=TRUE)
s1=sd(dp$data[COVID_categorization==TRUE][PHx_psych_PTSD==TRUE]$COMPASS_total,na.rm=TRUE)
s2=sd(dp$data[COVID_categorization==TRUE][PHx_psych_PTSD==FALSE]$COMPASS_total,na.rm=TRUE)
stot=sd(dp$data[COVID_categorization==TRUE]$COMPASS_total,na.rm=TRUE)

(m1-m2)/stot # effect size ~.42 --> .37

mean(dp$data[COVID_categorization==TRUE]$asiN2peripheral,na.rm=TRUE)
sd(dp$data[COVID_categorization==TRUE]$asiN2peripheral,na.rm=TRUE)

summary(lm(COMPASS_total~age+gendernnb+nTBIwLOC+pcl5notE+COVID_categorization,data=dp$normed))
summary(lm(whoQoLD1raw~age+gendernnb+nTBIwLOC+pcl5notE+COVID_categorization,data=dp$normed))
summary(lm(whoQoLD2raw~age+gendernnb+nTBIwLOC+pcl5notE+COVID_categorization,data=dp$normed))

# Still no evidence of interactive effects, per se?
summary(lm(COMPASS_total~age+gendernnb+nTBIwLOC+pcl5notE*COVID_categorization,data=dp$normed))
summary(lm(whoQoLD1raw~age+gendernnb+nTBIwLOC+pcl5notE*COVID_categorization,data=dp$normed))
summary(lm(whoQoLD2raw~age+gendernnb+nTBIwLOC+pcl5notE*COVID_categorization,data=dp$normed))

# with dx
summary(lm(COMPASS_total~age+gendernnb+Dx_psych_PTSD+COVID_categorization,data=dp$normed))
summary(lm(COMPASS_total~age+gendernnb+nTBIwLOC+Dx_psych_PTSD+COVID_categorization,data=dp$normed))
summary(lm(whoQoLD1raw~age+gendernnb+nTBIwLOC+Dx_psych_PTSD+COVID_categorization,data=dp$normed))
summary(lm(whoQoLD2raw~age+gendernnb+nTBIwLOC+Dx_psych_PTSD+COVID_categorization,data=dp$normed))

summary(lm(COMPASS_total~age+gendernnb+Dx_psych_PTSD*COVID_categorization,data=dp$normed))
summary(lm(COMPASS_total~age+gendernnb+nTBIwLOC+Dx_psych_PTSD*COVID_categorization,data=dp$normed))
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

ggplot(data=dp$data[COVID_categorization==TRUE],aes(x=lecAtotal,y=COMPASS_total))+
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

summary(lm(TMB_Continuousconcentration~COMPASS_total+age+gendernnb,data=dp$data))
summary(lm(TMB_Continuousconcentration~COMPASS_total,data=dp$data))
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
summary(lm(COMPASS_total~age+gendernnb+pcl5total+nTBIwLOC*lecAtotal*COVID_categorization,data=dp$data))

summary(lm(asi1total~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asi2total~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asi3total~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(asiSxTotal~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))
summary(lm(COMPASS_total~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))

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
summary(lm(COMPASS_total~age+gendernnb+pcl5total+nTBIwLOC+lecAtotal+COVID_categorization,data=dp$data))

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

summary(lm(COMPASS_total~age+gendernnb+COVID_categorization,data=dp$normed)) # covid trend
summary(lm(asiN2peripheral~age+gendernnb+COVID_categorization,data=dp$normed)) # covid trend
summary(lm(asi1total~age+gendernnb+COVID_categorization,data=dp$normed)) # covid trend

summary(lm(COMPASS_total~age+gendernnb+asiN2peripheral,data=dp$normed)) # covid trend
summary(lm(COMPASS_total~age+gendernnb+asi1total,data=dp$normed)) # covid trend



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
M <- "COMPASS_total"
M <- "asiN2peripheral"
dt <- data.table(x=dp$normed[,get(X)],y=dp$normed[,get(Y)],m=dp$normed[,get(M)],age=dp$normed[,age],gendernnb=dp$normed[,gendernnb]) # quite sure there is a more effic way to do this!
dt <- dt[!is.na(x)][!is.na(y)][!is.na(m)]
dt$x=as.numeric(dt$x)

res <- mediate(lm(m~x+age+gendernnb,data=dt),lm(y~x+m+age+gendernnb,data=dt),treat='x',mediator='m',boot=TRUE,sims=1000)
summary(res)

# Debugging 10/18/22:
library(mediation)
res <- mediate(lm(m~x,data=dt),lm(y~x+m,data=dt),treat='x',mediator='m',boot=TRUE,sims=1000)
summary(res)



# Decode / Want to pull out:
# tau.coef, tau.ci, tau.p: Total effect=sum of mediation (indirect) effect and direct effect
# z0, z0.ci, z0.p: ADE=average direct effect = direct effect of X on Y after taking into account (removing) mediation from M
# d0, d0.ci, d0.p: ACME=average causal mediation effects=total effect minus direct effect
# n0, n0.ci, n0.p: proportion mediated
#mlist <- c("asi1total","asi1totalnoncog","asi1totalnosleep","asi2total","asi2totalnoncog","asi2totalnosleep","asi3total","asiSxTotal","asiSxTotalnoncog","asiSxTotalnosleep",
#           "asiN1mostadrenergic","asiN2peripheral","asiN3mostadrenergicnotpostural","asiN4peripheralnotpostural","COMPASS_total")
mlist <- c("COMPASS_total","asiN2peripheral","asi1total")
mlist <- c("asi1total","asiN2peripheral","COMPASS_total")
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
        #Takes forever res <- mediate(lm(m~x+age+gendernnb,data=dt),lm(y~x+m+age+gendernnb,data=dt),treat='x',mediator='m',
        #               boot=TRUE,sims=Nsims)
        #ot <- summary(res)
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
#                   key_from=c("asi1total","asiN2peripheral","COMPASS_total"),
#                   key_to = c("ASI","ASI-peripheral","COMPASS"))
#hd$m <- factor(hd$m,levels=c("COMPASS","ASI-peripheral","ASI"))
hd$m <- subfromkey(hd$m,
                   key_from=c("asi1total","asiN2peripheral","COMPASS_total"),
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



#ASI Validation
asi<-data.table(
dp$dataQL$asi1i1, dp$dataQL$asi1i2, dp$dataQL$asi1i3, dp$dataQL$asi1i4, dp$dataQL$asi1i5, dp$dataQL$asi1i6, dp$dataQL$asi1i7,
dp$dataQL$asi1i8, dp$dataQL$asi1i9, dp$dataQL$asi1i10, dp$dataQL$asi1i11, dp$dataQL$asi1i12, dp$dataQL$asi1i13, dp$dataQL$asi1i14, 
dp$dataQL$asi1i15, dp$dataQL$asi1i16, dp$dataQL$asi1i17, dp$dataQL$asi1i18, dp$dataQL$asi1i19, dp$dataQL$asi1i20, dp$dataQL$asi1i21,
dp$dataQL$asi1i22, dp$dataQL$asi1i23, dp$dataQL$asi1i24, dp$dataQL$asi1i25, dp$dataQL$asi1i26, dp$dataQL$asi1i27,
dp$dataQL$asi1i28, dp$dataQL$asi1i29, dp$dataQL$asi1i30, dp$dataQL$asi1i31

#, 

#dp$dataQL$asi2i1, dp$dataQL$asi2i2, dp$dataQL$asi2i3, dp$dataQL$asi2i4, dp$dataQL$asi2i5, dp$dataQL$asi2i6, dp$dataQL$asi2i7, 
#dp$dataQL$asi2i8, dp$dataQL$asi2i9, dp$dataQL$asi2i10, dp$dataQL$asi2i11, dp$dataQL$asi2i12, dp$dataQL$asi2i13, dp$dataQL$asi2i14,
#dp$dataQL$asi2i15, dp$dataQL$asi2i16, dp$dataQL$asi2i17, dp$dataQL$asi2i18, dp$dataQL$asi2i19, dp$dataQL$asi2i20, dp$dataQL$asi2i21,
#dp$dataQL$asi2i22, dp$dataQL$asi2i23, dp$dataQL$asi2i24, dp$dataQL$asi2i25, dp$dataQL$asi2i26, dp$dataQL$asi2i27, dp$dataQL$asi2i28,
#dp$dataQL$asi2i29, dp$dataQL$asi2i30, dp$dataQL$asi2i31,

#dp$dataQL$asi3i1, dp$dataQL$asi3i2, dp$dataQL$asi3i3, dp$dataQL$asi3i4, dp$dataQL$asi3i5, dp$dataQL$asi3i6, dp$dataQL$asi3i7,
#dp$dataQL$asi3i8, dp$dataQL$asi3i9, dp$dataQL$asi3i10
)

colnames(asi) <- c("asi1i1","asi1i2" ,"asi1i3", "asi1i4", "asi1i5", "asi1i6", "asi1i7", "asi1i8", "asi1i9", "asi1i10", "asi1i11", "asi1i12", "asi1i13", "asi1i14",
                   "asi1i15", "asi1i16", "asi1i17", "asi1i18", "asi1i19", "asi1i20", "asi1i21", "asi1i22", "asi1i23", "asi1i24", "asi1i25", "asi1i26",
                   "asi1i27", "asi1i28", "asi1i29", "asi1i30", "asi1i31" 
                   #, " asi2i1",  "asi2i2", "asi2i3", "asi2i4", "asi2i5", "asi2i6",
                   #"asi2i7", "asi2i8", "asi2i9", "asi2i10", "asi2i11", "asi2i12", "asi2i13", "asi2i14", "asi2i15", "asi2i16",
                   #"asi2i17", "asi2i18", "asi2i19", "asi2i20", "asi2i21", "asi2i22", "asi2i23", "asi2i24", "asi2i25", "asi2i26", "asi2i27",
                   #"asi2i28", "asi2i29", "asi2i30", "asi2i31", "asi3i1", "asi3i2", "asi3i3", "asi3i4", "asi3i5", "asi3i6", "asi3i7",
                   #"asi3i8", "asi3i9", "asi3i10"
                   )
cronbach.alpha(na.omit(asi))

cor.test(dp$dataQL$asi1total, dp$dataQL$pcl5total)
cor.test(dp$dataQL$asi1total, dp$dataQL$COMPASS_total)

ggplot(data=dp$dataQL,aes(x=asi1total,y=dp$dataQL$pcl5total))+
  geom_point(aes(color=COVID_categorization))+stat_cor(method="pearson")+ geom_smooth(method=lm,se=FALSE,color="black",fullrange=TRUE)
