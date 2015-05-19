# This script converts json files from github API results of
# repo commits to dataframe and then visualizes the output
# updated by Michele Goe on May 13, 2015 
library(rjson)
library(ggplot2)
library("lattice")
library(reshape2)
# import json file from data folder with commits by author
document <- fromJSON(file = "data/d3_commits2.json", method = 'C')
Alist<-document[[1]]$weeks

tab<-do.call("cbind",document)
tab2<- do.call("rbind",document)

# obtains author list and commmits for week 1
# next task - iterate and combine for all weeks
# replace 1 with i
dat <- do.call(rbind, 
	lapply(document[[1]]$weeks, 
     function(x) data.frame(author=x$w, 
     		adds= x$c, 
     		deletes=x$d, 
     		commits=x$c, 
     		week=1)))
     		
# open file with commits only 
document <- fromJSON(file = "data/d3_commits_participation.json", method = 'C')
tab<-do.call("cbind",document)
dat<-as.data.frame(tab)    
dat$weeks<-c(1:52)       
dat$perown<-dat$owner/dat$all
dat$other<-dat$all-dat$owner
# convert json to dataframe
# reshape
df<-melt(dat,id=c("weeks","perown"))

# graph commits per week 
xyplot(value ~ weeks, group=variable,data=df,auto.key=list(space="right"), 
		jitter.x=TRUE, jitter.y=TRUE)
