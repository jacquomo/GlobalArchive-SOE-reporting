
###### Query MaxN and Length/3D points from EventMeasure and generic data tables downloaded from GlobalArchive ######


### Written by Tim Langlois 
### Any errors are due to Tim Langlois
### Please forward any updates and improvements to timothy.langlois@uwa.edu.au

# The following code forms an appendix to the manuscript:
#  "Langlois, T. J., L. M. Bellchambers, R. Fisher, G. R. Shiell, J. Goetze, L. Fullwood, S. N. Evans, N. Konzewitsch, E. S. Harvey, and M. B. Pember. 2016. Investigating ecosystem processes using targeted fisheries closures: can small-bodied invertivore fish be used as indicators for the effects of western rock lobster fishing? Marine and Freshwater Research."

# Please cite it if you like it

### Designed to: 

# 1. Import metadata, points, lengths and 3Dpoints for multiple Campaigns
# 2. Summarise to create MaxN and Length3Dpoint data
# 3. If PeriodTime has been set to zero at Time on Seabed in EM - this query will return the first PeriodTime of MaxN. Example included below of how to change to last PeriodTime.
# 4. Right-join these files with metadata - to result in MaxN and Length3Dpoint data that have Sample/OpCodes included that had no points or measures (i.e. a sample with no fish!)


# Naming conventions----
# data objects in lower case
# column names Capitalized


# Libraries required
detach("package:plyr", unload=TRUE)#will error - don't panic
detach("package:data.table", unload=TRUE)#will error - don't panic
detach("package:RCurl", unload=TRUE)#will error - don't panic
library(tidyr)
library(plyr) 
library(dplyr)
library(readr)
library(data.table)
library(magrittr)


# Study name----
rm(list=ls()) #clear memory
study<-"SOE"


# Set work directory----
#work.dir=("~/Google Drive/Analysis/Analysis_GlobalArchive_SOE_reporting") #Not workin on Jac's machine
work.dir=("C:/Users/jmonk1/Google Drive/Analysis_GlobalArchive_SOE_reporting")

# Set sub directories----
data.dir=paste(work.dir,"Data",sep="/")


# Import metadata and EM data tables for querying----
setwd(data.dir)
dir()


# Import metadata--
metadata.files <- list.files(pattern="_Metadata.txt")
metadata <- NULL
for (t in metadata.files) {
  dat <- read_tsv(t, col_names=T,na = c("", " "))
  dat$Date <- as.Date(as.POSIXct(dat$Date, format="%Y/%m/%d"))
  metadata <- plyr::rbind.fill(metadata, dat)
}
metadata%<>%select(CampaignID,Sample,Latitude,Longitude,Date,Time,Location,Status,Site,Depth,Observer,Successful)%>%
  data.frame()
head(metadata,2)

unique(metadata$Date)
unique(metadata$CampaignID)


# Import EventMeasure data tables----
# Import EM points---
points.files <- list.files(pattern="_Points.txt")
points <- NULL
for (t in points.files) {
  dat <- read_tsv(t, col_names=T,na = c("", " "))
  points <- rbind.fill(points, dat)
}
points%<>%data.frame()
head(points,2)

# Import EM lengths---
lengths.files <- list.files(pattern="_Lengths.txt")
lengths <- NULL
for (t in lengths.files) {
  dat <- read_tsv(t, col_names=T,na = c("", " "))
  lengths <- rbind.fill(lengths, dat)
}
lengths%<>%data.frame()
head(lengths,2)

# Import EM 3dpoints---
threedpoints.files <- list.files(pattern="_3DPoints.txt")
threedpoints <- NULL
for (t in threedpoints.files) {
  dat <- read_tsv(t, col_names=T,na = c("", " "))
  threedpoints <- rbind.fill(threedpoints, dat)
}
threedpoints%<>%data.frame()
head(threedpoints,2)


# Import generic data tables
# Import generic count---
count.files <- list.files(pattern="_Count.txt")
count <- NULL
for (t in count.files) {
  dat <- read_tsv(t, col_names=T,na = c("", " "))
  count <- rbind.fill(count, dat)
}
detach("package:plyr", unload=TRUE)
drop.cols <- c("Genus", "Species")
count%<>%data.frame()%>%
  select(-one_of(drop.cols))%>%
  separate(Genus_species, c("Genus", "Species"), sep="_",remove=TRUE)%>%
  rename(MaxN=Count)
head(count,2)



# Import generic length---
library(plyr)
length.files <- list.files(pattern="_Length.txt")
length <- NULL
for (t in length.files) {
  dat <- read_tsv(t, col_names=T,na = c("", " "))
  length <- rbind.fill(length, dat)
}
drop.cols <- c("Genus", "Species")
length%<>%data.frame()%>%
  select(-one_of(drop.cols))%>%
  separate(Genus_species, c("Genus", "Species"), sep="_",remove=TRUE)%>%
  mutate(Number=1) #add in missing variable
head(length,2)


# Run queries and write----
setwd(data.dir)
detach("package:plyr", unload=TRUE)#will error - don't panic - need to make sure it is not loaded as will interfer with dplyr() #TJL


# Query MaxN - at last Period time----
#This give the MaxN at the first Period time
head(points,10)
maxn.last.period<-points%>%
  group_by(OpCode,Frame,Period,PeriodTime,Family,Genus,Species,Code)%>%
  mutate(Number=as.numeric(Number))%>% #TJL
  summarise(SumNumber=sum(Number))%>%
  group_by(OpCode,Family,Genus,Species,Code)%>%
  summarise(MaxN=max(SumNumber),PeriodTime=max(PeriodTime))%>%
  ungroup()%>%
  filter(!is.na(MaxN))
head(maxn.last.period) #This give the MaxN with the last Period time


# Query MaxN from EM data tables and add in generic count- at first Period time----
#This give the MaxN at the first Period time
maxn<-points%>%
  group_by(OpCode,PeriodTime,Family,Genus,Species,Code)%>%
  mutate(Number=as.numeric(Number))%>% #TJL
  summarise(MaxN=sum(Number))%>%
  group_by(OpCode,Family,Genus,Species,Code)%>%
  slice(which.max(MaxN))%>%
  ungroup()%>%
  filter(!is.na(MaxN))%>%
  rename(Sample=OpCode)

# Add in the generic count data---
library(plyr)
maxn.count<-maxn%>%
  rbind.fill(count)%>%
  right_join(metadata,by=c("Sample"))%>%
  replace_na(list(PeriodTime = 0,MaxN=0))%>%
  data.frame()
head(maxn.count,2)#This give the MaxN with first Period time

# ADD a filter here to limit the dataset---
maxn.count%<>%
  filter(Species%in%c("rubescens","auratus"))
write_tsv(maxn.count, path=paste(study,"MaxN.txt",sep = "."))


# Query Length+3DPoints----
library(plyr)
length3dpoints<-lengths%>%
  rbind.fill(threedpoints) #this will fill in missing columns with NA's
detach("package:plyr", unload=TRUE)#will error - don't panic - need to make sure it is not loaded as will interfer with dplyr() #TJL
length3dpoints<-length3dpoints%>%
  rename(Sample=OpCode)
library(plyr)
length3dpoints<-length3dpoints%>%
  rbind.fill(length)
detach("package:plyr", unload=TRUE)
length3dpoints%<>%
  right_join(metadata, by=c("Sample"))

head(length3dpoints,2) 

# ADD a filter here to limit the dataset---
length3dpoints%<>%
  filter(Species%in%c("rubescens","auratus"))

write_tsv(length3dpoints, path=paste(study,"Len3DPoints.txt",sep = "."))


# GO TO SCRIPT 2