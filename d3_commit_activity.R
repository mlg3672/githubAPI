# This script converts json files from github API results of
# repo commits to dataframe and then visualizes the output

library(rjson)
library(ggplot2)
library("lattice")
library(reshape2)
library(lubridate)

# import json file from data folder with commits by day
# open file with commits, put in list
doc <- fromJSON(file = "data/d3_commit_activity.json", method = 'C')
tab<-do.call("cbind",doc)



# transform data from table to dataframe
timelist=unlist(doc)
total=timelist[names(timelist)=="total"]
week=timelist[names(timelist)=="week"]
days1=timelist[names(timelist)=="days1"]
days2=timelist[names(timelist)=="days2"]
days3=timelist[names(timelist)=="days3"]
days4=timelist[names(timelist)=="days4"]
days5=timelist[names(timelist)=="days5"]
days6=timelist[names(timelist)=="days6"]
days7=timelist[names(timelist)=="days7"]
commits.frame=data.frame(total,week,days1,days2,days3,days4,days5,days6,days7)

# convert Unix Time to POSIX time, label day of week
commits.frame$date<-ymd(as.POSIXct(commits.frame$week,
	origin="1970-01-01", tz="GMT"))
commits.frame$month<-month(commits.frame$date,label=TRUE)

# change column names
cols<-c("WeekTotal",
	"UnixDate",
	"Sunday",
	"Monday",
	"Tuesday",
	"Wednesday", 
	"Thursday",
	"Friday",
	"Saturday",
	"ymd",
	"month")
colnames(commits.frame)<- cols

# reshape
new.frame<-commits.frame[, c(1, 3:9,11)]
df<-melt(new.frame,id=c("WeekTotal","month"))

# take out zero values
dz<-df[df$value>0,]

# create by month sum and plot
# this is a final output!
g<-lapply(split(dz$value,dz$month),sum)
tab3<-do.call("cbind",g)
g.frame<-melt(as.data.frame(tab3))
xyplot(value ~ variable, data=g.frame, type="l",xlab="Month",ylab="Total Commits",main="Total Repo Commits per Month")


# create by weekday sum and plot
# this is a final output
h<-lapply(split(dz$value,dz$variable),sum)
tab4<-do.call("cbind",h)
h.frame<-melt(as.data.frame(tab4))
barchart(value ~ variable, data=h.frame, col="wheat", ylab="Commits",main="Repo Commits by Day of the Week")

# create chronological graph of weeks by owner

barchart(ymd ~ WeekTotal, data=commits.frame, col="green",xlab="Total Commits",main="Total Repo Commits by Week")

