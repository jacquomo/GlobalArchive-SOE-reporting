
###### Check of MaxN and Length/3D points from EventMeasure data tables held in GlobalArchive ######


### Written by Tim Langlois 
### Any errors are due to Tim Langlois
### Please forward any updates and improvements to timothy.langlois@uwa.edu.au

# The following code forms an appendix to the manuscript:
#  "Langlois, T. J., L. M. Bellchambers, R. Fisher, G. R. Shiell, J. Goetze, L. Fullwood, S. N. Evans, N. Konzewitsch, E. S. Harvey, and M. B. Pember. 2016. Investigating ecosystem processes using targeted fisheries closures: can small-bodied invertivore fish be used as indicators for the effects of western rock lobster fishing? Marine and Freshwater Research."

# Please cite it if you like it

### Designed to: 
#   check data resulting in queries from GlobalArchive or EventMeasure. 
#   this script is designed to be used in an interative process to suggest corrections that can/should be made to original EventMeasure or GlobalArchive files.


### objective is to 
# 1. Import data and create Genus_species column
# 2. run BASIC data checks
# 3. Limit length data by range and precision rules
# 4. run SERIOUS checks against a master species list
# 5. Remove species that can never be ID'd
# 6. Visualise what MaxN are missing in the stereoMaxN
# 7. Write data for analysis that passes checks


# Naming conventions----
# data objects in lower case
# column names Capitalized


# Libraries required
detach("package:plyr", unload=TRUE)#will error - don't panic - need to make sure it is not loaded as will interfer with dplyr() #TJL

library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(magrittr)
library(data.table)
library(ggplot2)
library(googlesheets)
library(readr)
library(RCurl) #needed to download data from GitHub


# Study name----
rm(list=ls()) #clear memory
study<-"SOE"


# Set work directory----
work.dir=("~/Google Drive/Analysis/Analysis_GlobalArchive_SOE_reporting")

# Set sub directories----
data.dir=paste(work.dir,"Data",sep="/")
plots.dir=paste(work.dir,"Plots",sep="/")

# Load functions from GitHub----
gsr <- getURL("https://raw.githubusercontent.com/TimLanglois/Fuctions/master/gsr.r", ssl.verifypeer = FALSE)
eval(parse(text = gsr))


# Import MaxN and length/3d files----
setwd(data.dir)
dir()

# Import MaxN file---
maxn<-read_tsv(file=paste(study,"MaxN.txt",sep = "."),na = c("", " "))%>%
  mutate(MaxN=as.numeric(MaxN))%>%
  mutate(Genus = ifelse(Genus == "", Family,Genus))%>% #fill in any blank Genus names with family
  mutate(Genus_species = paste(Genus, Species, sep = ' '))%>% #paste Genus species together
  data.frame()
head(maxn,2)
str(maxn)
unique(maxn$CampaignID)


# Import length/3d file----
length<-read_tsv(file=paste(study,"Len3DPoints.txt",sep = "."),na = c("", " "))%>%
  mutate(Number=as.numeric(Number))%>%
  mutate(Length=as.numeric(Length))%>%
  mutate(Range=as.numeric(Range))%>%
  mutate(Genus = ifelse(Genus == "", Family,Genus))%>% #fill in any blank Genus names with family
  mutate(Genus_species = paste(Genus, Species, sep = ' '))%>% #paste Genus species together
  filter(!is.na(Number)) %>%#find and remove sync points that are not fish
  data.frame()
  head(length,2)
str(length)
unique(length$CampaignID)


# BASIC Checks----
setwd(data.dir)

# Check if we have 3d points (Number) in addition to length----
three.d.points<-length%>%
  filter(is.na(Length))%>%
  filter(!is.na(Number))
head(three.d.points,2) #Do we have 3d points?


# Check if we have schools associated with single length measures----
schools<-length%>%
  filter(Number>1) 
head(schools,2) #Do we have schools?


#Standardise for RANGE and Error for Length----
# To standardise for RANGE and Error we can remove any length observations outside Range and Error rules
# i.e. the length data, and any abundnance calculated from it, will be restricted by range

summary(length$Range)
out.of.range<-filter(length,Range>10000);head(out.of.range,2)
write.csv(out.of.range,file=paste(study,"check","length.out.of.range.csv",sep = "."), row.names=FALSE)
length %<>% filter(Range < 10000)


# Check on the BIG fish length data----
fish.greater.than.1.meter<-filter(length,Length>1000) #probrably sharks,
head(fish.greater.than.1.meter,2)
write.csv(fish.greater.than.1.meter,file=paste(study,"check","length.greater.than.1.meter.csv",sep = "."), row.names=FALSE)

# Plots to visually check length and range data----
# Plot to visualise length data---
setwd(plots.dir)
check.length<-ggplot(data=length, aes(as.numeric(Length))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="blue",
                 alpha = .2)
check.length
ggsave(check.length,file=paste(study,"check.length.png",sep = "."))

# Plot to visualise range data---
check.range<-ggplot(data=length, aes(as.numeric(Range))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="green",
                 alpha = .2)
check.range
ggsave(check.range,file=paste(study,"check.range.png",sep = "."))

# Plot to visualise length/range data---
check.range.vs.length<-ggplot(data=length, aes(Range,Length)) +
  geom_point()+
  geom_smooth()
check.range.vs.length
ggsave(check.range.vs.length,file=paste(study,"check.range.vs.length.png",sep = "."))


# SERIOUS data checking to compare taxa to Master list and correct any names that may have changed----
# # Read in Species list to compare against----
gs_ls()
Life_history <- gs_title("Life_history")#register a sheet
master<-Life_history%>%
  gs_read_csv(ws = "Life_history")%>%
  filter(grepl('Australia', Global.region))%>%
  filter(grepl('Ningaloo|Pilbara', Local.region))%>%
  data.table()
head(master,7)
str(master)
names(master)


# Update names of species that may have changed----
change<-filter(master,!Change.to=="No change")
head(change)
# For MaxN
maxn$Genus_species <- gsr(maxn$Genus_species, change$Genus_species, change$Change.to)
# For Length
length$Genus_species <- gsr(length$Genus_species, change$Genus_species, change$Change.to)


# Check for taxa.not.match----
setwd(data.dir)
x<-"check.maxn.taxa.not.match.life.history" #useful list of taxa that do not match by Sample
maxn.taxa.not.match.life.history<-
  master%>%
  select(Genus_species)%>%
  anti_join(maxn,.,by="Genus_species")%>%
  distinct(Genus_species,Sample)%>%
  select(Genus_species,Sample)
head(maxn.taxa.not.match.life.history,2)
write.csv(maxn.taxa.not.match.life.history,file=paste(study,x,"csv",sep = "."), row.names=FALSE)

x<-"check.length.taxa.not.match.life.history" #useful list of taxa that do not match by Sample
length.taxa.not.match<-
  master%>%
  select(Genus_species)%>%
  anti_join(length,.,by="Genus_species")%>%
  distinct(Genus_species,Sample)%>%
  select(Genus_species,Sample)
head(length.taxa.not.match,2)
write.csv(length.taxa.not.match,file=paste(study,x,"csv",sep = "."), row.names=FALSE)


### SERIOUS Check for Min Max Length compared to Master list----
x<-"check.wrong.length.vs.life.history"
# Before running the length check we must NOT have any non-matching taxa - so first remove these
keep<-select(master,Genus_species)

length.match.master<-length%>%
  semi_join(keep,by="Genus_species")%>%
  filter(!is.na(Length))

# Make a vector of names to compare against
Genus_species.Vec<- sort(unique(length.match.master$Genus_species)) #Need to order by name

# Make a dummy list for checking
wrong.length=vector('list',length=length(Genus_species.Vec))
names(wrong.length)=Genus_species.Vec
Matching.Species.Table=filter(master,Genus_species%in%Genus_species.Vec)
Matching.Species.Table=Matching.Species.Table[order(Matching.Species.Table$Genus_species),]

Min=Matching.Species.Table$Min_length #Vector of Min lengths
Max=Matching.Species.Table$Max_length #Vector of Max lengths
names(Min)=names(Max)=Matching.Species.Table$Genus_species #Add names to the Min and Max - very important vectors are in order

# Run the loop to check the length data---
test=NA
for(i in 1:length(Genus_species.Vec))
{
  Data=subset(length.match.master,Genus_species==Genus_species.Vec[i])
  Data=subset(Data,!is.na(Length))
  test=which(Data$Length  <Min[i])
  test=c(test,which(Data$Length  >Max[i]))
  test=Data[test,]
  wrong.length[[i]]=test
}
wrong.length2<-do.call(rbind,wrong.length)

# Merge with Matching.Species.Table
wrong.length.taxa<-wrong.length2%>%
  inner_join(Matching.Species.Table,by="Genus_species")%>%
  select(Sample, Genus_species,Length,Min_length,Max_length,everything())%>%
  data.frame()
head(wrong.length.taxa)
write.csv(wrong.length.taxa,file=paste(study,x,"csv",sep = "."), row.names=FALSE)




###
### CONGRATULATE yourself that you will not have to do checking of species and lengths using filter and sort functions in Excel----
##

# WRITE FINAL checked data----
setwd(data.dir)
dir()

x<-"checked.maxn" #MaxN of species that also occur in master list
maxn<-master%>%
  select(Genus_species)%>%
  semi_join(maxn,.,by="Genus_species")%>%
  data.frame()
head(maxn,2)
write.csv(maxn, file=paste(study,x,"csv",sep = "."), row.names=FALSE)


x<-"checked.length"
drop.length<-wrong.length.taxa %>% #TO REMOVE LENGTHS OUTSIDE THE MIN/MAX OF MASTER LIST
  distinct(Sample, Genus_species,Length)%>%
  select(Sample, Genus_species,Length)%>%
  mutate(key = paste(Sample, Genus_species, Length, sep = '_'))
length<-master%>%
  select(Genus_species)%>%
  semi_join(length,.,by="Genus_species")%>%
  mutate(key = paste(Sample, Genus_species, Length, sep = '_'))%>%
  anti_join(drop.length,by="key")%>%#for dropping wrong.lengths
  data.frame() 
head(length,2)
write.csv(length, file=paste(study,x,"csv",sep = "."), row.names=FALSE)


# GO TO SCRIPT 3


###########################################
# Additional check # # Check how many MaxN per Genus_species are missing from StereoMaxN----
# e.g. how many lengths are missing from the possible MaxN
#############################################




length.to.match.maxn<-master%>%
  select(Genus_species)%>%
  semi_join(length,.,by="Genus_species")%>%
  distinct(Genus_species)%>% 
  select(Genus_species)%>%
  semi_join(maxn,.,by="Genus_species")%>%
  semi_join(length,.,by="Genus_species")%>% 
  select(Family,Genus_species,Number,Sample)%>%
  mutate(Data = "StereoMaxN")

length.Sample <- length.to.match.maxn %>%
  distinct(Sample)%>% 
  select(Sample)


maxn.match.length<-master%>%
  select(Genus_species)%>%
  semi_join(maxn,.,by="Genus_species")%>%
  distinct(Genus_species)%>% 
  select(Genus_species)%>%
  semi_join(length,.,by="Genus_species")%>%
  semi_join(maxn,.,by="Genus_species")%>%
  semi_join(length.Sample,by="Sample")%>% # subset maxn to only those OpCode that match OpCodes from length
  select(Family,Genus_species,MaxN,Sample)%>%
  mutate(Data = "MaxN")%>%
  rename(Number = MaxN)%>%
  bind_rows(length.to.match.maxn)
head(maxn.match.length)


# Summarise the matched data by taxa
x<-"taxa.maxn.vs.stereo.summary"
taxa.maxn.vs.stereo.summary <- maxn.match.length %>%
  group_by(Genus_species,Family,Sample,Data) %>%
  summarise(MaxN = sum(Number))%>%
  spread(Data,MaxN)%>%
  mutate(Percent.diff = (MaxN-StereoMaxN)/MaxN)
head(taxa.maxn.vs.stereo.summary)
write.csv(taxa.maxn.vs.stereo.summary,file=paste( study,x,"csv",sep = "."), row.names=FALSE)

# Summarise the matched data by family
x<-"family.maxn.vs.stereo.summary"
family.maxn.vs.stereo.summary <- maxn.match.length %>%
  group_by(Family,Sample,Data) %>%
  summarise(MaxN = sum(Number))%>%
  spread(Data,MaxN)%>%
  mutate(Percent.diff = (MaxN-StereoMaxN)/MaxN)
head(family.maxn.vs.stereo.summary)
write.csv(family.maxn.vs.stereo.summary,file=paste( study,x,"csv",sep = "."), row.names=FALSE)


# Plot of MaxN versus StereoMaxN by family----

setwd(plots.dir)

head(family.maxn.vs.stereo.summary)

x<-"ggMaxNCheckzoomout"
ggMaxNCheckzoomout<-ggplot(data=family.maxn.vs.stereo.summary,aes(x=MaxN,y=StereoMaxN,colour=Family))+
  geom_point()+
  geom_text(aes(label=Sample),hjust=0, vjust=0)+
  theme(legend.direction = "horizontal", legend.position = "bottom")+
  geom_abline()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  ylab("StereoMaxN")+
  xlim(0, 45)+
  ylim(0, 45)
# coord_equal()
ggMaxNCheckzoomout
ggsave(ggMaxNCheckzoomout,file=paste( study,x,"png",sep = "."),width = 8, height = 8,units = "in")

x<-"ggMaxNCheckzoomin"
ggMaxNCheckzoomin<-ggplot(data=family.maxn.vs.stereo.summary,aes(x=MaxN,y=StereoMaxN,colour=Family))+
  geom_point()+
  geom_text(aes(label=Sample),hjust=0, vjust=0,angle=330)+
  theme(legend.direction = "horizontal", legend.position = "bottom")+
  geom_abline()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  ylab("StereoMaxN")+
  coord_cartesian(xlim = c(-2,16), ylim = c(-2,16))
ggMaxNCheckzoomin
ggsave(ggMaxNCheckzoomin,file=paste( study,x,"png",sep = "."),width = 8, height = 8,units = "in")


