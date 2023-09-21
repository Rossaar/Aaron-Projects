
#
 loadpath="U:\\E070 Experiment R\\"
 savepath=paste0(loadpath,"ProcessingSaves\\")
 file_list = list.files(path=importdirectories$ActigraphyData,pattern="*.csv")
 filenames=file_list[1]

#process_raw_actigraphy_files(loadpath,savepath,overviewsavename = "NA",filenames=file_list[1:2])

process_raw_actigraphy_files <-
  function(loadpath,
           savepath,
           overviewsavename="NA",
           filenames="All",
           colnames=list(
             epochdata="\"Date\",\"Time\",\"Off-Wrist Status\",\"Activity\",\"Marker\",\"White Light\",\"Red Light\",\"Green Light\",\"Blue Light\",\"Sleep/Wake\",\"Interval Status\",",
             intervaldata="\"Interval Type\",\"Interval#\",\"Start Date\",\"Start Time\",\"End Date\",\"End Time\",",
             ),
           contpartialprocessing=TRUE
           #wrote countpartialprocessesing instead of contpartialprocessesing
           ){

    if(filenames[1]=="All"){
      files = list.files(path=loadpath,pattern="*.csv")
    }else{
      files=filename
    }

    if(overviewsavename=="NA"){
      overviewsavename=paste0("actigraphy_processing_overviewfile_",Sys.Date(),".rds")
    }

    # Are we continuing from a partial processing? Check whether there's an overview file
    ovf=list.files(path=savepath,pattern="*.rds")
    if((length(ovf)>0)&&(contpartialprocessing)){

      # Read in the partial overview file
      overview=readRDS(paste0(savepath,ovf))

      # Where were we??
      processedfilenames=list.files(path=savepath,pattern="*.csv")
      ilastfilestarted=max(which(!is.na(overview$rawfilename)))
      lastepochfilestarted=overview$epochdatafilename[ilastfilestarted]
      lastintervalfilestarted=overview$intervaldatafilename[ilastfilestarted]
      havelastepochfile=(length(grep(lastepochfilestarted,processedfilenames))>0)
      havelastintervalfile=(length(grep(lastintervalfilestarted,processedfilenames))>0)

      # Assign starting file
      if(havelastepochfile&&havelastintervalfile){
        istartfile=ilastfilestarted+1
      }else{
        istartfile=ilastfilestarted
      }

    }else{

      # Create overview output file:
      overview=data.table(
        #iFile=1:length(files),
        ifile=1:length(files),
        PtID=rep(as.character(NA),length(files)),
        loggingmode=rep(as.character(NA),length(files)),
        epochlength=rep(as.character(NA),length(files)),
        activitycalibrationfactor=rep(as.integer(NA),length(files)),
        startdate=rep(as.POSIXct(NA)),
        enddate=rep(as.POSIXct(NA)),
        rawfilename=rep(as.character(NA),length(files)),
        epochdatafilename=rep(as.character(NA),length(files)),
        intervaldatafilename=rep(as.character(NA),length(files))
      )

      istartfile=1

    }

    stripsetting=function(linesfile,grepstring){
      iL=grep(grepstring,linesfile)
      if(length(iL)>0){
        textline=linesfile[iL]
        # Pull out character class 2nd column:
        o1=strsplit(textline,",")[[1]][2]
        o2=strsplit(o1,"\"")[[1]][2]
        if(is.na(o2)){
          if(!is.na(o1)){
            o2=o1
          }
        }
      }else{
        o2=as.character(NA)
      }
      return(o2)
    }

    for(iFile in istartfile:length(files)){

      # Read in the automatically produced file
      d0 <- readLines(paste0(loadpath,files[iFile]), skipNul = T)
      # TESTING CODE:
      dC <- readLines(paste0(loadpath,files[1]), skipNul = T)
      
      # Pull out (& create) the stuff for the overviewfile
      overview[ifile==iFile]$PtID = stripsetting(d0,"Identity:")
      overview[ifile==iFile]$loggingmode = stripsetting(d0,"Logging Mode:")
      overview[ifile==iFile]$epochlength = as.integer(stripsetting(d0,"Epoch Length:"))
      overview[ifile==iFile]$activitycalibrationfactor = as.integer(stripsetting(d0,"Activity Calibration Factor:"))
      rawstartdate = stripsetting(d0,"Data Collection Start Date")
      rawstarttime = stripsetting(d0,"Data Collection Start Time")
      rawtimezone = stripsetting(d0,"Time Zone")
      if(!is.na(rawtimezone)){
        rawstartdatetime = paste0(rawstartdate," ",rawstarttime)
        if(rawtimezone!="(UTC-08:00) Pacific Time (US & Canada)"){
          warning("Was this actiwatch set up in a different time zone? Code isn't written to handle this yet. Fix it?")
        }
        processedstartdatetime = as.POSIXct(parse_date_time(rawstartdatetime,"mdY IMS p",tz="America/Los_Angeles"))
        overview[ifile==iFile]$startdate = processedstartdatetime
        rawstopdate = stripsetting(d0,"Data Collection End Date")
        rawstoptime = stripsetting(d0,"Data Collection End Time")
        rawstopdatetime = paste0(rawstopdate," ",rawstoptime)
        processedstopdatetime = as.POSIXct(parse_date_time(rawstopdatetime,"mdY IMS p",tz="America/Los_Angeles"))
      }
      overview[ifile==iFile]$enddate = processedstopdatetime
      overview[ifile==iFile]$rawfilename = files[iFile]
      overview[ifile==iFile]$epochdatafilename = paste0("epochdata_",files[iFile])
      overview[ifile==iFile]$intervaldatafilename = paste0("intervaldata_",files[iFile])

      # Pull out and save the datafiles
      # Epoch data:
      epoch_column_name <- "\"Date\",\"Time\",\"Off-Wrist Status\",\"Activity\",\"Marker\",\"White Light\",\"Red Light\",\"Green Light\",\"Blue Light\",\"Sleep/Wake\",\"Interval Status\","
      iLine_epochstarts = grep(epoch_column_name,d0)
      #TEST = grep(epoch_column_name,dC)
      if(length(iLine_epochstarts)<=0){
        epoch_column_name <- "Line,Date,Time,Off-Wrist Status,Activity,Marker,White Light,Red Light,Green Light,Blue Light,Sleep/Wake,Interval Statu"
        iLine_epochstarts = grep(epoch_column_name,d0)
      }
      if(length(iLine_epochstarts)>0){  # Occassionally epoch data didn't save, don't want processing to choke
        epochdata <- d0[c(iLine_epochstarts, iLine_epochstarts+2:length(d0))]
        writeLines(epochdata,paste0(savepath,overview[ifile==iFile]$epochdatafilename))
      }
      # Interval data:
      interval_column_name <- "\"Interval Type\",\"Interval#\",\"Start Date\","
      iLine_intervalstarts = grep(interval_column_name,d0)
      if(length(iLine_intervalstarts)<=0){
        epoch_column_name <- "Interval Type,Interval#,Start Date,Start Day,Start Time,End Date,End Day,End Time,Duration"
        iLine_intervalstarts = grep(epoch_column_name,d0)
      }
      iLine_intervalstops = grep("--- Marker/Score List ---",d0)-3
      if((length(iLine_intervalstarts)>0)&&(length(iLine_intervalstops)>0)){
        intervaldata <- d0[c(iLine_intervalstarts, iLine_intervalstarts+2:iLine_intervalstops)]
        writeLines(intervaldata,paste0(savepath,overview[ifile==iFile]$intervaldatafilename))
      }

      # Save the overviewfile (switch to RDS to protect eg datetime formats)
      # Note: include inside the loop to buffer against late loop fails given processing time
      saveRDS(overview,paste0(savepath,overviewsavename))

    }

  }

loadBasicActigraphyData <- function(dp,
                                    importdirectories,
                                    actigraphysettings=NA,
                                    overviewsavename=NA,
                                    overview=NA){

  # Have some default settings
  if(is.na(actigraphysettings[1])){
    actigraphysettings=list(
      rangearoundactigraphycenterdate=c(-6,0),
      roundrangeup2capturefullintervals=TRUE,
      maxInvalidTime4totals=.05,
      minDays4means=1,
      minDays4sds=3,
      visitDateChoice="mode"
    )
  }

  # for convenience
  savepath=paste0(importdirectories$ActigraphyData,importdirectories$ActigraphyProcessingSavesSubDirectory)

  # Load the overviewfile (see if can have alt later)
  if(!is.data.table(overview)){
    if(is.na(overviewsavename)){
      overviewsavename=list.files(path=savepath,pattern="*.rds")
    }
    if(length(overviewsavename)>1){
      stop("haven't yet written function to integrate overviewfiles. Do this now?")
    }
    overview=readRDS(paste0(importdirectories$ActigraphyData,importdirectories$ActigraphyProcessingSavesSubDirectory,overviewsavename))
  }

  # Sort the ActigraphyCenterDate to use
  if(actigraphysettings$visitDateChoice=="mode"){
    varWithVisitDates=c(names(dp$data)[grep("visit_date",names(dp$data))],
                        names(dp$data)[grep("VisitDate",names(dp$data))],
                        names(dp$data)[grep("qaDate",names(dp$data))],
                        names(dp$data)[grep("qaEndDate",names(dp$data))])
    dp$data[,actigraphyCenterDate:=as.POSIXct(NA)]
    # Ridiculous, but, for now going to do this in a loop... fix once better date handling overall
    for(iR in 1:nrow(dp$data)){
      r=dp$data[iR,((varWithVisitDates)),with=FALSE]
      # Just handle qualtrics and redcap data separately
      if("qaDate"%in%names(r)){
      #changed qaDate to "qaDate", allows for it to run, unknown if it messes anyhting important up
      #if(!is.na(qaDate)){  # <----- don't know why this was here or what it means????
        qaDate=parse_date_time(r$qaDate,"Ymd HMS",tz="America/Los_Angeles")
        qaEndDate=parse_date_time(r$qaEndDate,"Ymd HMS",tz="America/Los_Angeles")
        duration=as.duration(as.interval(qaDate,qaEndDate))
        centerdate=as.Date(qaDate+duration/2)
        dp$data$actigraphyCenterDate[iR]=centerdate
      #}
      }else{
        r=as.vector(as.data.table(lapply(r,as.Date)))
        rv=do.call(c,r)
        luse=!is.na(rv)
        dates2use=rv[luse]
        if(all(dates2use==dates2use[1])){
          dp$data$actigraphyCenterDate[iR]=dates2use[1]
        }else{
          warning("meant to write a function to pick the most common date, but haven't done it yet. Do this now? someother way to pick?")
        }
      }
    }
  }else{
    stop("Didn't yet write the code to pick based on a specific type of visitdate, but should! Do now?")
  }

  # Create the new columns for our data packages, as will add them in by row...
  newnumericcolumns=paste0("actRest_",c("waketime","percentWake","sleeptime","percentSleep","avgWhite","avgGreen","avgBlue","avgRed"))
  newnumericcolumns=c(newnumericcolumns,paste0("actSleep_",
                                               c("waketime","percentWake","sleeptime","percentSleep","sleepEfficiency","avgWhite","avgGreen","avgBlue","avgRed")))
  dp$data[,(newnumericcolumns):=as.numeric(NA)]
  newtimecolumns=paste0("actRest_",c("intervalstarttime","intervalstoptime"))
  newtimecolumns=c(newtimecolumns,paste0("actSleep_",c("intervalstarttime","intervalstoptime")))
  dp$data[,(newtimecolumns):=as.Date(NA)]

  # Create a unique ID for the dp$data construct for now, then delte again, just to decrease risk of error
  dp$data[,rID:=1:nrow(dp$data)]
  # Create list of actual extant files so don't throw an error if a file is missing
  extantdatafiles=list.files(path=savepath,pattern="*.csv")

  # Gather all data for a given Pt, first
  PtIDs=unique(dp$data[PtID!=""]$PtID)
  for(iPtID in 1:length(PtIDs)){

    # Compile all the data across that PtID first
    ifiles=overview[PtID==(PtIDs[iPtID])][intervaldatafilename%in%extantdatafiles]$ifile

    if(length(ifiles)>0){

      # Pull out and merge the right files
      intervalscompiled=list()
      for(iF in 1:length(ifiles)){
        intervaldata=read.csv(paste0(importdirectories$ActigraphyData,importdirectories$ActigraphyProcessingSavesSubDirectory,overview$intervaldatafilename[ifiles[iF]]),
                              header=TRUE,na.strings=c("NA","NaN"))
        # *************TO FIX: THERE IS SOMETHING WRONG WITH THE LINES SELECTION - cut down, but, may have lost some too??? ***FIX THIS!!!!!
        intervaldata=as.data.table(intervaldata)
        intervaldata=intervaldata[Interval.Type%in%c("REST","ACTIVE","SLEEP","DAILY")]
        intervaldata[,startdatetime:=parse_date_time(paste0(Start.Date," ",Start.Time),"mdY IMS p",tz="America/Los_Angeles")]
        intervaldata[,stopdatetime:=parse_date_time(paste0(End.Date," ",End.Time),"mdY IMS p",tz="America/Los_Angeles")]
        intervalscompiled[[iF]]=intervaldata
      }
      # sometimes there are differences in the columns!
      colnames=lapply(intervalscompiled,names)
      commonnames=Reduce(intersect,colnames)
      for(iF in 1:length(ifiles)){
        intervalscompiled[[iF]]=intervalscompiled[[iF]][,eval(commonnames),with=FALSE]
      }
      intervaldt=do.call("rbind",intervalscompiled)

      # For each correct timepoint, pull out the right interval data
      pullrightintervals=function(x,visitdate,actigraphysettings){
        # *****************NEEDS UPDATING: not controlling rounding etc as promised, yet
        desiredstart=visitdate+days(actigraphysettings$rangearoundactigraphycenterdate[1])
        desiredstop=visitdate+days(actigraphysettings$rangearoundactigraphycenterdate[2])
        x[,c("startok","stopok"):=FALSE]
        x[startdatetime>desiredstart,startok:=TRUE]
        x[stopdatetime<desiredstop,stopok:=TRUE]
        x[,include:=stopok&&startok,by=1:nrow(x)]
#        x[((startdatetime>desiredstart)&&(stopdatetime<desiredstop)),include:=TRUE,by=1:nrow(x)]
          # No idea why this line alone didn't work.....!
        y=x[include==TRUE]
        return(y)
      }
      # start debugging here..... :(

      rowids=dp$data[PtID==(PtIDs[iPtID])]$rID

      for(rowid in rowids){
        visitdate=dp$data[rID==rowid]$actigraphyCenterDate
        #print(rowid)
        #print(visitdate)
        if(!is.na(visitdate)){
          intervals2use=pullrightintervals(intervaldt,visitdate,actigraphysettings)
          if(nrow(intervals2use)>0){

            # fix the class assignments - this is foolish and should have been fixed by addressing the interval file save...
            intervals2use$Wake.Time=as.numeric(intervals2use$Wake.Time)
            intervals2use$X.Wake=as.numeric(intervals2use$X.Wake)
            intervals2use$Sleep.Time=as.numeric(intervals2use$Sleep.Time)
            intervals2use$X.Sleep=as.numeric(intervals2use$X.Sleep)
            intervals2use$Efficiency=as.numeric(intervals2use$Efficiency)

            # assign: newnumericcolumns, newtimecolumns
            dp$data[rID==rowid]$actRest_waketime=mean(intervals2use[Interval.Type=="REST"]$Wake.Time,na.rm = TRUE)
            dp$data[rID==rowid]$actRest_percentWake=mean(intervals2use[Interval.Type=="REST"]$X.Wake,na.rm = TRUE)
            dp$data[rID==rowid]$actRest_sleeptime=mean(intervals2use[Interval.Type=="REST"]$Sleep.Time,na.rm = TRUE)
            dp$data[rID==rowid]$actRest_percentSleep=mean(intervals2use[Interval.Type=="REST"]$X.Sleep,na.rm = TRUE)
            ##### SKIPPING LIGHTS FOR NOW TO SAVE TIME ADD LATER SAME WYA - ALSO INTERVAL START/STOPTIMES (do as sd??)
            dp$data[rID==rowid]$actSleep_waketime=mean(intervals2use[Interval.Type=="SLEEP"]$Wake.Time,na.rm = TRUE)
            dp$data[rID==rowid]$actSleep_percentWake=mean(intervals2use[Interval.Type=="SLEEP"]$X.Wake,na.rm = TRUE)
            dp$data[rID==rowid]$actSleep_sleeptime=mean(intervals2use[Interval.Type=="SLEEP"]$Sleep.Time,na.rm = TRUE)
            dp$data[rID==rowid]$actSleep_percentSleep=mean(intervals2use[Interval.Type=="SLEEP"]$X.Sleep,na.rm = TRUE)
            dp$data[rID==rowid]$actSleep_sleepEfficiency=mean(intervals2use[Interval.Type=="SLEEP"]$Efficiency,na.rm = TRUE)

 #           print(rowid)
  #          if(rowid%in%c(181)){
   #           print("1...")
    #        }else{
#              stop("testing!")
     #       }


          }# end ifnrow
        }# end if visitdate is there
      }# end for rowid
    }# end if files for this PtID
  }# end for ptID

  dp$data[,rID:=NULL]
#  stop("testing again")

  return(dp)
}


# # Scratch for reference:
#
# file_list = list.files(path=importdirectories$ActigraphyData,pattern="*.csv")
#
# # Look at single example to get setup
# d0 <- readLines(paste0(importdirectories$ActigraphyData, file_list[i]), skipNul = T) # read in the messy file
# #column_name <- "\"Date\",\"Time\",\"Activity\",\"Marker\",\"White Light\",\"Mobility\",\"Interval Status\",\"S/W Status\","
# column_name <- "\"Date\",\"Time\",\"Off-Wrist Status\",\"Activity\",\"Marker\",\"White Light\",\"Red Light\",\"Green Light\",\"Blue Light\",\"Sleep/Wake\",\"Interval Status\","
# dt <- grep(column_name, d0)
# d <- d0[c(dt, dt+2:length(d0))]
# name <- paste0(importdirectories$workingdatasaves, "pre_",file_list[i])
# writeLines(d, name)
# x <- read.csv(name)
#
# x=as.data.table(x)
# x[,newdate:=strptime(as.character(x$Date), "%m/%d/%y")]
# x[,datetime:=with(x, lubridate::ymd(newdate) + lubridate::hms(Time))]
# x[,activitycount:=as.integer(Activity)]
# xclean=x[Off.Wrist.Status==0][!is.na(Activity)]
# ggplot(xclean, aes(x=datetime,y=activitycount))+ geom_point()+
#   ggtitle(file_list[i])+
#   theme(plot.title = element_text(hjust = 0.5))+
#   xlab("Date/Time") + ylab("Activity")
#
# # x$newdate <- strptime(as.character(x$Date), "%m/%d/%y")
# # x$T<- with(x, lubridate::ymd(newdate) + lubridate::hms(Time))
# # x$C = as.integer(x$Activity)
# # rundata<-na.omit(x[c("T", "C")])
# # #savename<- paste(substring(file_list[i],1,10),".png")
# # ggplot(rundata, aes(x=T,y=C))+ geom_point()+
# #   ggtitle(file_list[i])+
# #   theme(plot.title = element_text(hjust = 0.5))+
# #   xlab("Date/Time") + ylab("Activity")
# # #ggsave(savename)
#
# # Try Ellen's code first
#
# #dat = read_xlsx('10000279_data_form.xlsx')
# dat=xclean
# #dat$date_time2 <-as.POSIXct(dat$date_time,format="%Y-%m-%d %H:%M:%S",
# #                            tz="PST8PDT")
# #dat$act <-dat$`Vector Magnitude`
#
# # #limit data
# # dat=as.data.table(dat)
# # dat2=dat[which(dat$T>"2020-10-19 00:00:00"&
# #                  dat$T<"2020-10-29 00:00:00"),]
# # #dat3=dat[intersect(which(dat$Off.Wrist.Status==0),which(!is.na(dat$Activity)))]
# # dat3=dat[Off.Wrist.Status==0][!is.na(dat$Activity)]
# #
# # ggplot(dat3, aes(x=T,y=Activity))+ geom_point()+
# #   ggtitle("Clean plot for 10000279")+
# #   theme(plot.title = element_text(hjust = 0.5))+
# #   xlab("Date/Time") + ylab("Activity")
# #ggsave("10000279_clean.png")
# #
# # # Zooming in on weird parts
# # dat3=dat[which(dat$date_time2>"2018-09-10 00:00:00"&
# #                  dat$date_time2<"2018-09-11 00:00:00"),]
# # ggplot(dat3, aes(x=date_time2,y=act))+ geom_point()+
# #   ggtitle("zoom plot for 10000279 \n 9/10/18 00:00 - 9/11/18 00:00")+
# #   theme(plot.title = element_text(hjust = 0.5))+
# #   xlab("Date/Time") + ylab("Activity")
# # ggsave("10000279_zoom_Sept10_11.png")
# #
# # dat3=dat[which(dat$date_time2>"2018-09-11 00:00:00"&
# #                  dat$date_time2<"2018-09-12 00:00:00"),]
# # ggplot(dat3, aes(x=date_time2,y=act))+ geom_point()+
# #   ggtitle("zoom plot for 10000279 \n 9/11/18 00:00 - 9/12/18 00:00")+
# #   theme(plot.title = element_text(hjust = 0.5))+
# #   xlab("Date/Time") + ylab("Activity")
# # ggsave("10000279_zoom_Sept11_12.png")
# #
# # dat3=dat[which(dat$date_time2>"2018-09-18 00:00:00"&
# #                  dat$date_time2<"2018-09-20 00:00:00"),]
# # ggplot(dat3, aes(x=date_time2,y=act))+ geom_point()+
# #   ggtitle("zoom plot for 10000279 \n 9/18/18 00:00 - 9/20/18 00:00")+
# #   theme(plot.title = element_text(hjust = 0.5))+
# #   xlab("Date/Time") + ylab("Activity")
# # ggsave("10000279_zoom_Sept18_20.png")
# #
# # dat3=dat[which(dat$date_time2>"2018-09-18 00:00:00"&
# #                  dat$date_time2<"2018-09-19 00:00:00"),]
# # ggplot(dat3, aes(x=date_time2,y=act))+ geom_point()+
# #   ggtitle("zoom plot for 10000279 \n 9/18/18 00:00 - 9/19/18 00:00")+
# #   theme(plot.title = element_text(hjust = 0.5))+
# #   xlab("Date/Time") + ylab("Activity")
# # ggsave("10000279_zoom_Sept18_19.png")
#
# # Analysis
# #new_id<-rep(1,57266)  # be sure to add the # of obs in dat2 for the 2nd num
# #dat2$id<-new_id
# #dat4 <- data.frame(dat2$date_time2, dat2$act, dat2$id)
# #dat4 <- data.frame(dat2$T, dat2$Activity, dat2$id)
#
# datRAR=xclean[,.(id=(1:nrow(xclean)),activitycount,datetime)]
# rar_ex<-RAR(df=datRAR,act_column = "activitycount",time_column = "datetime")
# rar_ex$parameters # parameter estimates
# rar_ex$messages #convergence message
#
# # View RAR bands plot.
# bands<-RAR_Bands_plot(rar_ex, t(c(0, 2/24)))
# bands$plots  #effect of filter
#
# #then save plot if you like it.
# png(file="10000279_RAR_Bands_plot.png")
# bands<-RAR_Bands_plot(rar_ex, t(c(0, 2/24)))
# bands$plots  #effect of filter
# dev.off()
#
# ### Run RAR_plot
# p<-RAR_plot(rar_ex)
# p$plot_log.act #on log scale
# p$plot_act # on natural scales
#
# # Save one of these plots
# png(file="10000279_RAR_plot.png")
# p$plot_log.act #on log scale
# dev.off()
#
# # nparACT functions
# dat5 <- data.frame(dat2$date_time2, dat2$act)
# r<-nparACT_base("dat5", SR=1/60, cutoff=1, plot=T, fulldays=F)
# print(r)
# ## save the plot that results
# dev.copy(png,"10000279_24hr.png")
# dev.off()
#
# # if you need to get rid of it. rm(SZrhythm.new)
# w<-data.frame(Id=c('10000279'))
# x <- cbind (rar_ex$parameters)
# y <- cbind(r)
# SZrhythm.new=as.data.frame(cbind(w,x,y))
# SZrhythm.dat=read.csv("SZ_circ_summ.csv")
# # get rid of dummy columns (doublecheck how many you are getting rid of
# # as written, getting rid of column 1)
# SZrhythm.dat<-SZrhythm.dat[-c(1)]
# newdf=as.data.frame(rbind(SZrhythm.dat,SZrhythm.new))
# # saving the new file.
# write.csv(newdf,file="SZ_circ_summ.csv")
#
#
#
# # The automated version:
#
# file_list = list.files(path=importdirectories$ActigraphyData,pattern="*.csv")
# len<-length(file_list)
#
# for(i in seq_along(file_list)){
#   d0 <- readLines(paste0(importdirectories$ActigraphyData, file_list[i]), skipNul = T) # read in the messy file
#   column_name <- "\"Date\",\"Time\",\"Activity\",\"Marker\",\"White Light\",\"Mobility\",\"Interval Status\",\"S/W Status\","
#   dt <- grep(column_name, d0)
#   d <- d0[c(dt, dt+2:length(d0))]
#   name <- paste0("pre_",file_list[i])
#   writeLines(d, name)
#   x <- read.csv(name)
#   x$newdate <- strptime(as.character(x$Date), "%m/%d/%y")
#   x$T<- with(x, lubridate::ymd(newdate) + lubridate::hms(Time))
#   x$C = as.integer(x$Activity)
#   rundata<-na.omit(x[c("T", "C")])
#   savename<- paste(substring(file_list[i],1,10),".png")
#   ggplot(rundata, aes(x=T,y=C))+ geom_point()+
#     ggtitle(file_list[i])+
#     theme(plot.title = element_text(hjust = 0.5))+
#     xlab("Date/Time") + ylab("Activity")
#   ggsave(savename)
# }
#
