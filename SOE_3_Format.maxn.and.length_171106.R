
###### Make complete.maxn and complete.length.number.mass data from Checked.maxn and Checked.length data created from EventMeasure data tables held in GlobalArchive######


### Written by Tim Langlois 
### Any errors are due to Tim Langlois
### Please forward any updates and improvements to timothy.langlois@uwa.edu.au

# The following code forms an appendix to the manuscript:
#  "Langlois, T. J., L. M. Bellchambers, R. Fisher, G. R. Shiell, J. Goetze, L. Fullwood, S. N. Evans, N. Konzewitsch, E. S. Harvey, and M. B. Pember. 2016. Investigating ecosystem processes using targeted fisheries closures: can small-bodied invertivore fish be used as indicators for the effects of western rock lobster fishing? Marine and Freshwater Research."

# Please cite it if you like it

### objective is to 

# 1. Import checked data
# 2. Make factors
# 3. Make complete.maxn data -  PeriodTime will represent the first PeriodTime of MaxN if PeriodTime has been set to zero at Time os Seabed in EM.
      # a. useful for abundance metrics - that do not account for body size or range/sample unit size
# 4. Make complete.length.number.mass data:
    # a. useful for calculating abundance/mass based on length rules - and that account for range/sample unit size
    # b. useful for length analyses (e.g. mean length, KDE, histograms) - after expansion
# 5. Make mass estimates from Length using a and b from Master list
# 6. Write complete data sets for further analysis


# Naming conventions----
# data objects in lower case
# column names Capitalized


# Libraries required
detach("package:plyr", unload=TRUE)#will error - don't panic
detach("package:data.table", unload=TRUE)#will error - don't panic
detach("package:RCurl", unload=TRUE)#will error - don't panic
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(ggplot2)
library(googlesheets)
library(readr)


# Study name----
rm(list=ls()) #clear memory
study<-"Example"


# Set work directory----
work.dir=("~/Google Drive/Analysis/Analysis_Example")

# Set sub directories----
data.dir=paste(work.dir,"Data",sep="/")
plots.dir=paste(work.dir,"Plots",sep="/")

# Read in the data----
setwd(data.dir)
dir()


# # Make Factors to merge back in after data is complete -----
# # Factors are in the data. Take them from the data
drop.cols <- c("Family","Genus","Species","MaxN","Genus_species","PeriodTime","Code","TIME.FED","TIME.FIRST.FED","Period","Stage")
maxn.factors<-read_csv(file=paste(study,"checked.maxn.csv",sep = "."),na = c("", " "))%>%
  select(-one_of(drop.cols))%>%
  distinct()%>%
  data.frame()
head(maxn.factors,2)

keep.cols <- c("Sample","Comment","CampaignID","Latitude","Longitude","Date","Location","Status","Site","Depth","Observer","Successful")
length.factors<-read_csv(file=paste(study,"checked.length.csv",sep = "."),na = c("", " "))%>%
  select(one_of(keep.cols))%>%
  # select(c(Sample,Comment,CampaignID,Latitude,Longitude,Date,Location,Status,Site,Depth,Observer,Successful))%>%
  distinct()%>%
  data.frame()
head(length.factors,2)
names(length.factors)

# Make complete.maxn from maxn and complete.length.number.mass from length3D----

# Make complete.maxn: fill in 0, make Total and Species Richness and join in factors----
dat<-read_csv(file=paste(study,"checked.maxn.csv",sep = "."),na = c("", " "))%>%
  select(c(Sample,Family,Genus,Species,MaxN,Genus_species,PeriodTime,Code))%>%
  complete(Sample,Genus_species) %>%
  replace_na(list(MaxN = 0))%>%
  select(Sample,Genus_species,MaxN)%>%
  group_by(Sample,Genus_species)%>%
  summarise(MaxN=sum(MaxN))%>%
  ungroup()%>% #always a good idea to ungroup() after you have finished using the group_by()!
  spread(Genus_species,MaxN, fill = 0)%>%
  mutate(Total=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))#Add in Totals
Presence.Absence <- dat[,2:(ncol(dat))-1]
for (i in 1:dim(Presence.Absence)[2]){
  Presence.Absence[,i] <- ifelse(Presence.Absence[,i]>0,1,0)
}
complete.maxn<-dat%>%
  mutate(Rich = rowSums(Presence.Absence,na.rm = TRUE))%>% #Add in Species Richness
  gather(key=Genus_species, value = MaxN,-Sample)%>%
  inner_join(maxn.factors,by="Sample")%>%
  data.frame()
head(complete.maxn,2)
unique(complete.maxn$CampaignID)


# Make complete.length.number.mass: fill in 0 and join in factors----
# This data is useful for calculating abundance based on length rules--
length.families<-read_csv(file=paste(study,"checked.length.csv",sep = "."),na = c("", " "))%>%
  select(Genus_species,Family)%>%
  distinct() #to join back in after complete
head(length.families,2)

complete.length.number<-read_csv(file=paste(study,"checked.length.csv",sep = "."),na = c("", " "))%>%
  select(Sample,Period,PeriodTime,Genus_species,Length,Number,Precision,RMS,Range)%>%
  complete(Sample,Genus_species) %>%
  replace_na(list(Number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  ungroup()%>%
  filter(!is.na(Number))%>% #this should not do anything
  inner_join(length.families,by="Genus_species")%>%
  inner_join(length.factors,by="Sample")%>%
  mutate(Length=as.numeric(Length))%>%
  data.frame()
head(complete.length.number,2)
str(complete.length.number)
unique(complete.length.number$CampaignID)

# MAKE mass data from number.length.complete----

# Import master from Life-history-
Life_history <- gs_title("Life_history")#register a sheet
master<-Life_history%>%
  gs_read_csv(ws = "Life_history")%>%
  filter(grepl('Australia', Global.region))%>%
  filter(grepl('Ningaloo|Pilbara', Local.region))%>%
  mutate(aLL=as.numeric(aLL))%>%
  mutate(bLL=as.numeric(bLL))%>%
  mutate(a=as.numeric(a))%>%
  mutate(b=as.numeric(b))%>%
  distinct(Genus_species,.keep_all = TRUE)%>%
  filter(!is.na(a))%>%
  select(Genus_species,Family,a,b,aLL,bLL)%>%
  data.frame()
head(master,2)
str(master)


# 1.Check if we have species length-weight relationship----
setwd(data.dir)
x<-"check.taxa.missing.lw.from.life.history"
taxa.missing.lw <- complete.length.number%>%
  distinct(Genus_species,.keep_all = TRUE)%>%
  anti_join(master, by="Genus_species")%>%
  select(Genus_species)
head(taxa.missing.lw,2)
write.csv(taxa.missing.lw,file=paste(study,x,"csv",sep = "."), row.names=FALSE)
#We have a few missing Taxa -   you can add these into the master table - or we can  use Family averages


#2. Check if any familys are missing?----
x<-"check.family.missing.lw.from.life.history"
family.missing.lw <- complete.length.number%>%
  distinct(Family)%>%
  anti_join(master, by="Family")%>%
  select(Family)
head(family.missing.lw)
write.csv(family.missing.lw,file=paste(study,x,"csv",sep = "."), row.names=FALSE)
# We have 2 familys missing - they will be dropped from Mass cal - until the LW are filled in


#3. Make a family average master table----
head(master)
master.Family <- master %>%
  group_by(Family) %>%
  summarise(a = mean(a,na.rm = T),
            b = mean(b,na.rm = T),
            aLL = mean(aLL,na.rm = T),
            bLL = mean(bLL,na.rm = T))
head(master.Family)


#4. Fill length data with relevant a and b and if blank use family?----
length.species.ab<-master%>% #done this way around to avoid duplicating Family coloum
  select(-Family)%>%
  inner_join(complete.length.number,., by="Genus_species")
head(length.species.ab,2)

length.family.ab<-complete.length.number%>%
  anti_join(master, by="Genus_species")%>%
  semi_join(master, by="Family")
head(length.family.ab,2)


complete.length.number.mass<-length.species.ab%>%
  bind_rows(length.family.ab)%>%
  filter(!is.na(a))%>% #this gets rid of species with no lw
  mutate(Length.cm = Length/10)%>%
  mutate(AdjLength = ((Length.cm*bLL)+aLL)) %>% # Adjusted length  accounts for a b not coming from for Fork length
  mutate(Mass = (AdjLength^b)*a*Number)%>%
  replace_na(list(Mass = 0))%>%
  select(c(Sample,Genus_species,Family,PeriodTime,Length,Number,Mass,CampaignID,Latitude,Longitude,Date,Location,Status,Site,Depth))
head(complete.length.number.mass,2)
str(complete.length.number.mass)


#5. Check the mass estimates across species - in kg's----
setwd(data.dir)


x<-"check.top.50.species.by.mean.mass"
top.mass<- complete.length.number.mass %>%
  group_by(Genus_species) %>%
  filter(Mass>0)%>%
  summarise(Mean = mean(Mass,na.rm = TRUE))%>%
  arrange(-Mean)
head(top.mass) #looks OK
write.csv(head(top.mass,50),file=paste(study,x,"csv",sep = "."), row.names=FALSE)



# WRITE FINAL complete and expanded data----
setwd(data.dir)
dir()

x<-"complete.maxn"
write.csv(complete.maxn, file=paste(study,x,"csv",sep = "."), row.names=FALSE)
head(complete.maxn,2)

x<-"complete.length.number.mass"
write.csv(complete.length.number.mass, file=paste(study,x,"csv",sep = "."), row.names=FALSE)
head(complete.length.number.mass,2)




# Additional Example of how to expand complete.length.number.mass for calculating hisotgrams, KDEs in further analyses----
  
  
  # Make expanded.length: expand by Number column---
  # This data is useful for length analyses (e.g. mean length, KDE, histograms)--
expanded.length<-complete.length.number.mass%>%
  select(Sample,Genus_species,Family,Length,Number,CampaignID,Latitude,Longitude,Date,Location,Status,Site,Depth)%>%
  ungroup()%>%
  filter(!is.na(Number))%>%
  filter(!is.na(Length))%>%
  .[rep(seq.int(1,nrow(.)), .$Number), 1:13]%>% #ensure number of columns is correct
    data.frame()
head(expanded.length,2)
str(expanded.length)
names(expanded.length)
  