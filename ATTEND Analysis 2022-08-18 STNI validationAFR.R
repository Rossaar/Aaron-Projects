# ATTEND Analysis Script: Analysis of the STNI data, for a first pass validation / psychometrics
#
# Input: "ATTEND_allOO_pulled2022-07-18.rds"
#  * Produced by: ATTEND_allOO_2022-07-18.R in HendricksonLab / Data Pull Scripts
#
# Output:
#   XXXXXX
#
# History:
#   2022-08-18 RCH wrote it

###############################
# I. Set up
###############################

usersetting="Aaron" 
# Aaron, if you run this script and want to add default options for you that are different, just 
# swap this to "Aaron" and update away (I set up the library path portion this way as
# an example, but you may find other pLaces here that you want to swap things up a bit)

if(usersetting=="Rebecca"){
  .libPaths("C:/LocalR/R-4.0.5/library")
  githubpath="C:\\Users\\vhapughendrr\\GitHub\\"
}else if(usersetting=="Aaron"){
  # Put any library path you want to use instead here, and the path to your local github directory
  .libPaths("C:/LocalR/R-4.2.1/Library")
  githubpath="U:\\My Documents\\GitHub\\"
}else{
  warning("You'll need to set a githubpath for much of the code below to work...")
}

# Settings
importdirectories<-list(
  github=githubpath,
  workingdatasaves="R:\\MIRECC\\STUDIES\\Hendrickson_1588176-01881_ATTEND\\Study Data\\Data\\Analyses\\",
  plotsaves="R:\\MIRECC\\STUDIES\\Hendrickson_1588176-01881_ATTEND\\Study Data\\Data\\Analyses\\STNIplots\\"
)
importfilenames<-list(
  workingdata="ATTEND_allOO_pulled2022-07-18.rds"
)
saveoptions <- list(
  resave=TRUE,
  savetag="STNI"
)

# These you need for sure:
library(data.table)
library(ggplot2)
library(ggpubr)

# I can't remember which of these you actually will need for this
library(devtools)
library(readr)
library(readxl)
library(openxlsx)
library(htmlTable)
library(rvest) # for stripping html tags
library(flextable)
library(officer)
library(nlme)
library(psych)
library(psychTools)
library(jtools) # for visualizing lm output
library(ggstance) # needed by jtools
library(broom.mixed) # needed by ggstance!
library(mediation)
library(gridExtra)
library(nlme)

# These you almost certainly won't need
library(maps)
library(ggmap)

# Just source these, because package version never up to date:
source(paste0(importdirectories$github,"HendricksonLab\\HLUtilities\\R\\Utilities.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\GeneralDataFunctions.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\qualtricsFunctions.R"))

# Functions:


# Plot settings that carry through:
reltextsize=1.3
labelsize=9

#################################
# II. Load the prepared data set, initial prep
#################################

dp <- readRDS(paste0(importdirectories$workingdatasaves,importfilenames$workingdata))
# dp stands for data package, which has a couple of parts:
names(dp)

# The dataQL data table is the main data:
dp$data
# You can see the column names:
names(dp$dataQL)

# Other useful one:
dp$dictionaryQL
names(dp$dictionaryQL)
# You can look at the entry for a particular variable (column in dp$dataQL):
dp$dictionaryQL[VarID=="pcl5total"]

# Clean age:
dp$dataQL[age<21,.(age,attendDem_job)]
# the 3 that are "teens" are clearly legit responses, will change age to missing data
dp$dataQL[age<18,age:=NA]

###########################
# III. Basic characterizations
###########################

#############
# Basic demographics, profile of the responses we've gotten:

# how many responses do we have, with what completeness?
nBL <- length(dp$dataQL[VisitID=="BL"]$qaResponseID)
nFu <- length(dp$dataQL[VisitID!="BL"][trelative>0]$qaResponseID)

# Age
mAge <- mean(dp$dataQL$age,na.rm=TRUE)
sdAge <- sd(dp$dataQL$age,na.rm=TRUE)
rangeAge <- range(dp$dataQL$age,na.rm=TRUE)

# Field
nHCW <- sum(dp$dataQL[VisitID=="BL"]$attendDem_HCW)
nFR <- sum(dp$dataQL[VisitID=="BL"]$attendDem_FR)
nOverlap <- length(dp$dataQL[VisitID=="BL"][attendDem_HCW==TRUE][attendDem_FR==TRUE]$qaStartDate) # 6
nPolice <- sum(dp$dataQL[VisitID=="BL"]$attendDem_police)
nFire <- sum(dp$dataQL[VisitID=="BL"]$attendDem_fire)
nParamedic <- sum(dp$dataQL[VisitID=="BL"]$attendDem_emt)
nPhysician <- sum(dp$dataQL[VisitID=="BL"]$attendDem_physician)
nNurse <- sum(dp$dataQL[VisitID=="BL"]$attendDem_nurse)
nArnppa <- sum(dp$dataQL[VisitID=="BL"]$attendDem_arnppa)
fieldVector <- data.table(nHCW,nFR,nPolice,nFire,nParamedic,nPhysician,nNurse,nArnppa)
fieldVector

###########################dp$dataQL[VisitID=="BL"]$attendDem_HCW
# III. Pull out the STNI data to use, along with some demographics
###########################

# 1) What variables do we want to preserve?

# Demographic and basic ID variables:
var2keep=c("PtID","VisitID","age","gender")

# The STNI itself:
dp$dictionaryQL[Base=="STNI"]$VarID
var2keep=c(var2keep,dp$dictionaryQL[Base=="STNI"]$VarID)

# Let's remind ourselves why there's stni10 and stni10a (and same for 11):
dp$dictionaryQL[VarID=="stni10"]
dp$dictionaryQL[VarID=="stni10a"]
  # The answer is in the FieldLabel for stni10a: 
dp$dictionaryQL[VarID=="stni10a"]$FieldLabel
  # "Adjusted version of stni10 so monotonic"
  # Ie, if you look at the order of the answer choices:
dp$dictionaryQL[VarID=="stni10"]$answerChoices
  # ...they were out of order from the way it would make sense to
  # analyze them, ie, "never woken up sweaty" was the last=4 instead of first=0
  # So, stni10a is the same question but with the answer choices
  # rearranged so that never woken up sweaty is coded as 0
  # (stni11 vs 11a is the same)

# Make the actual tight data set:
data=dp$dataQL[,..var2keep] # note the ".." notation, which is very specific but handy; 
  # see this source for its function: https://stackoverflow.com/questions/12391950/select-assign-to-data-table-when-variable-names-are-stored-in-a-character-vect
dictionary=dp$dictionaryQL[VarID%in%var2keep]

# Pull the demographic variables forward now, because otherwise I forget all the time:
temp=data[VisitID=="BL",.(PtID,age,gender)]
data[,age:=NULL]
data[,gender:=NULL]
data=merge(data,temp,by="PtID",all.x=TRUE,all.y=FALSE)

# Drop any visits without a stniTotal, as we only want complete responses for this purpose:
data=data[!is.na(stniTotal)]
  # CONSIDER: do you want to go back and keep folks with NA for the bed partner dependent questions?

# How many responses does this leave?
dim(data) # 950 respodnses
length(unique(data$PtID)) # ...from 571 individual respondents 

#Write csv file for stni items 
write.csv(na.omit(data.table(stni1=dp$dataQL[VisitID=="BL"]$stni1,
           stni2=dp$dataQL[VisitID=="BL"]$stni2,
           stni3=dp$dataQL[VisitID=="BL"]$stni3,
           stni4=dp$dataQL[VisitID=="BL"]$stni4,
           stni5=dp$dataQL[VisitID=="BL"]$stni5,
           stni6=dp$dataQL[VisitID=="BL"]$stni6,
           stni7=dp$dataQL[VisitID=="BL"]$stni7,
           stni8=dp$dataQL[VisitID=="BL"]$stni8,
           stni9=dp$dataQL[VisitID=="BL"]$stni9,
           stni10=dp$dataQL[VisitID=="BL"]$stni10a,
           stni11=dp$dataQL[VisitID=="BL"]$stni11a)))
#Write csv file for shorter STNI
write.csv(na.omit(data.table(stni1=dp$dataQL[VisitID=="BL"]$stni1,
                             stni2=dp$dataQL[VisitID=="BL"]$stni2,
                             stni3=dp$dataQL[VisitID=="BL"]$stni3,
                             stni4=dp$dataQL[VisitID=="BL"]$stni4,
                             stni5=dp$dataQL[VisitID=="BL"]$stni5,
                             stni6=dp$dataQL[VisitID=="BL"]$stni6,
                             stni7=dp$dataQL[VisitID=="BL"]$stni7,
                             stni8=dp$dataQL[VisitID=="BL"]$stni8,
                             stni9=dp$dataQL[VisitID=="BL"]$stni9,
                             stni10=dp$dataQL[VisitID=="BL"]$stni10a,
                             stni11=dp$dataQL[VisitID=="BL"]$stni11a)))
#CSV file for STNI Short
write.csv(na.omit(data.table(stni1=dp$dataQL[VisitID=="BL"]$stni1,
                             stni2=dp$dataQL[VisitID=="BL"]$stni2,
                             stni3=dp$dataQL[VisitID=="BL"]$stni3,
                          #Possible add on for partner questions
                             #stni8=dp$dataQL[VisitID=="BL"]$stni8,
                             #stni9=dp$dataQL[VisitID=="BL"]$stni9,

                             stni11=dp$dataQL[VisitID=="BL"]$stni11a)))
#TEST
(data.table(na.omit(
            stni1=dp$dataQL[VisitID=="BL"]$stni1,
            stni2=dp$dataQL[VisitID=="BL"]$stni2,
            stni3=dp$dataQL[VisitID=="BL"]$stni3,
            stni4=dp$dataQL[VisitID=="BL"]$stni4,
            stni5=dp$dataQL[VisitID=="BL"]$stni5,
            stni6=dp$dataQL[VisitID=="BL"]$stni6,
            stni7=dp$dataQL[VisitID=="BL"]$stni7,
            stni10=dp$dataQL[VisitID=="BL"]$stni10a,
            stni11=dp$dataQL[VisitID=="BL"]$stni11a))&is.na(dp$dataQL$stni8))
            
#STNI and PCL datatable
View(data.table(na.omit(
  (stniTotal=dp$dataQL[VisitID=="BL"]$stniTotal) &
  (pclTotal=dp$dataQL[VisitID=="BL"]$pcl5total))))

(stniTotal=dp$dataQL[VisitID=="BL"]$stniTotal) &
  (pclTotal=dp$dataQL[VisitID=="BL"]$pcl5total)
#Wow did this actually work?
plot(na.omit(dp$dataQL[VisitID=="BL", .(stni9total, pcl5total)]))
ggplot(dp$dataQL[VisitID=="BL"], aes(x=stni9total, y=pcl5total))+geom_point()

dp$dataQL$pcl5total[1]
dp$dataQL$stni9total[1]
data.table(na.omit(dp$dataQL$stni9total))
data.table(na.omit(dp$dataQL$pcl5total))
dp$dataQL$trelative
(data.table(dp$dataQL[VisitID=="OO3mo"]$stni9total, dp$dataQL[VisitID=="BL"]$stni9total))
View(dp$dataQL)

#PTSD Diagnosis
#na.omit((dp$dataQL$pcl5i1>=2 & 
 #   dp$dataQL$pcl5i2>=2 |
  #  dp$dataQL$pcl5i3>=2 |
   # dp$dataQL$pcl5i4>=2 |
   # dp$dataQL$pcl5i5>=2) &
  #    (dp$dataQL$pcl5i6 |
   #    dp$dataQL$pcl5i7) &
  #        (if sum(
  #          dp$dataQL$pcl5i8>=2 |
  #          dp$dataQL$pcl5i9>=2 |
  #          dp$dataQL$pcl5i10>=2| 
  #          dp$dataQL$pcl5i11>=2| 
  #          dp$dataQL$pcl5i12>=2| 
  #          dp$dataQL$pcl5i13>=2|
  #          dp$dataQL$pcl5i14>=2)>=2))
    
#)

na.omit(sum(dp$dataQL$pcl5i8>2 |
  dp$dataQL$pcl5i9>2))

dp$dataQL$pcl5i8[1]



sum(
  sum(dp$dataQL$pcl5i8[1]>=2) +
  sum(dp$dataQL$pcl5i9[1]>=2) +
  sum(dp$dataQL$pcl5i10[1]>=2)+ 
  sum(dp$dataQL$pcl5i11[1]>=2)+ 
  sum(dp$dataQL$pcl5i12[1]>=2)+ 
  sum(dp$dataQL$pcl5i13[1]>=2)+
  sum(dp$dataQL$pcl5i14[1]>=2))>=2


#time PCL
na.omit(data.table(dp$dataQL[VisitID=="BL"]$PtID, dp$dataQL[VisitID=="BL"]$pcl5total, dp$dataQL[VisitID=="OO2wk"]$pcl5total))
