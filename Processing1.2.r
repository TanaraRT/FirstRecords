##
## For updates of the code, check folder "Issues" for potential corrections
##

## Clean work space
graphics.off()
rm(list=ls())

## Call libraries
library(Hmisc)
library(stringr)
library(stringi)
## Changed "Taxonstand" to "U.Taxonstand", but deactivated because we will use the gbif function later
#library(U.Taxonstand)
library(rgbif)
library(worrms)
library(data.table)

## Dynamic read of the IntroData_raw.csv file.
directory <- system("pwd", intern = TRUE)
Raw_data <- file.path(directory, "Raw_data", "IntroData_raw.csv")
introdat <- fread(Raw_data, encoding="Latin-1")#[1:10000,] 

## delete information currently not used
introdat[,GrowthFormPlants:=NULL]
introdat[,Origin:=NULL]
introdat[,Habitat:=NULL]
introdat[,NextDataSet:=NULL]
# introdat[,'Remarks as provided in datasets':=NULL]

# all_sources <- unique(cbind.data.frame(introdat$Source,introdat$DataUsage))
# colnames(all_sources) <- c("Source","DataUsage")
# introdat[,DataUsage:=NULL]

# belg <- read.table("Data/BelgiumNames.csv",sep=";",stringsAsFactors=F,na.strings=NA)# authors removed
# belg <- apply(belg,1,function(s) paste(s,collapse=" "))
# belg <- gsub("^\\s+|\\s+$", "",belg) # trim leading and trailing whitespace
# introdat[grep("Verloove",introdat$Source),]$GenusSpecies <- belg # substitute long with short names

introdat <- subset(introdat,!(nchar(introdat$Genus)>0 & nchar(introdat$Species)==0)) # remove entries with missing species name
introdat <- introdat[!grepl("spec\\.",introdat$GenusSpecies),]
introdat <- introdat[introdat$Species!="x",]

introdat <- subset(introdat,!(grepl("DAISIE",Source) & FirstRecord1==1500)) # remove wrong entries from DAISIE
introdat <- subset(introdat,!(grepl("DAISIE",Source) & FirstRecord2==1500))

introdat$Species <- iconv(introdat$Species, from = "latin1", to = "UTF-8", sub = "byte")

introdat$Species <- gsub("×","x ",introdat$Species,perl=TRUE)

# grep("×",introdat$Species)

introdat <- subset(introdat,!grepl("\\?",Species))

## remove some native entries from Long book #################################
introdat <- subset(introdat,!Country=="not found")

introdat <- subset(introdat,!grepl("re-introduced",introdat$Pathway)) # remove re-introduced species
introdat <- subset(introdat,!(GenusSpecies=="Alces alces" & Country=="Germany")) # remove moose in Germany (native)
introdat <- subset(introdat,!(grepl("Capra ibex",introdat$GenusSpecies) & Country=="Slovenia")) # remove moose in Germany (native)
introdat <- subset(introdat,!(grepl("Cervus elaphus canadensis",introdat$GenusSpecies) & Country=="Canada")) # remove moose in Germany (native)
introdat <- subset(introdat,!(GenusSpecies=="Diceros bicornis" & Country=="Tanzania")) # remove moose in Germany (native)
introdat <- subset(introdat,!(GenusSpecies=="Enhydra lutris" & Country=="USA")) # remove moose in Germany (native)
introdat <- subset(introdat,!(GenusSpecies=="Felis lynx" & Country=="France")) # remove moose in Germany (native)
introdat <- subset(introdat,!(GenusSpecies=="Felis silvestris" & Country=="Germany")) # remove moose in Germany (native)
introdat <- subset(introdat,!(GenusSpecies=="Felis silvestris" & Country=="Switzerland")) # remove moose in Germany (native)
introdat <- subset(introdat,!(GenusSpecies=="Martes martes" & Country=="British Isles")) # remove moose in Germany (native)
introdat <- subset(introdat,!(GenusSpecies=="Martes martes" & Country=="Russian Federation")) # remove moose in Germany (native)
introdat <- subset(introdat,!(GenusSpecies=="Lycaon pictus" & Country=="South Africa")) # remove moose in Germany (native)
introdat <- subset(introdat,!GenusSpecies=="Bats, Species not known")

## concatenate spec + genus names ###################################
introdat$Species <- iconv(introdat$Species, from = "latin1", to = "UTF-8", sub = "byte") #row xxx was not readable
introdat$GenusSpecies <- iconv(introdat$GenusSpecies, from = "latin1", to = "UTF-8", sub = "byte") #row 182625 was not readable

introdat$GenusSpecies <- gsub("^\\s+|\\s+$", "",introdat$GenusSpecies) # trim leading and trailing whitespace

introdat$NewName <- introdat$GenusSpecies
ind <- nchar(introdat$GenusSpecies)==0
introdat$NewName[ind] <- paste(as.data.frame(introdat)[ind,2],as.data.frame(introdat)[ind,3])



# store original name
introdat$OrigName <- introdat$GenusSpecies
ind <- (introdat$Genus!="")
introdat$OrigName[ind] <- paste(introdat$Genus[ind],introdat$Species[ind],introdat$Author[ind])
introdat$OrigName <- gsub("^\\s+|\\s+$", "",introdat$OrigName,perl=TRUE) # trim leading and trailing whitespace

introdat[,Species:=NULL] # remove column as it is not needed anymore
introdat[,Genus:=NULL] # remove column as it is not needed anymore
# introdat[,Author:=NULL] # remove column as it is not needed anymore

ind <- grep("×[a-z]",introdat$NewName,perl=TRUE)
introdat$NewName[ind] <- gsub("×","x ",introdat$NewName[ind],perl=TRUE) # add space

introdat$NewName <- gsub("ë","e",introdat$NewName,perl=TRUE)

# introdat$NewName <- stri_replace_all(introdat$NewName," ",regex="[[:space:]]") # harmonise encoding of spaces
introdat$NewName <- gsub("[[:space:]]"," ",introdat$NewName,perl=TRUE) # harmonise encoding of spaces
# system.time({introdat$NewName1 <- stri_replace_all(introdat$NewName," ",regex="[[:space:]]") }) # harmonise encoding of spaces




## harmonise years ##################################################
introdat$DateNaturalisation[is.na(introdat$DateNaturalisation)] <- ""

### store original first records ########################################
introdat$FirstRecord_orig <- introdat$FirstRecord
ind_fr1 <- (introdat$FirstRecord1!="")
introdat$FirstRecord_orig[ind_fr1] <- introdat$FirstRecord1[ind_fr1]
ind_fr2 <- (introdat$FirstRecord2!="")
introdat$FirstRecord_orig[ind_fr2] <- introdat$FirstRecord2[ind_fr2]
ind_nat <- (introdat$DateNaturalisation!="")
introdat$FirstRecord_orig[ind_nat] <- introdat$DateNaturalisation[ind_nat]
ind_int <- (introdat$FirstRecord_intentional!="")
introdat$FirstRecord_orig[ind_int] <- introdat$FirstRecord_intentional[ind_int]
ind_frspan <- (introdat$FirstRecord1!="") & (introdat$FirstRecord2!="")
introdat$FirstRecord_orig[ind_frspan] <- paste(introdat$FirstRecord1[ind_frspan],"-",introdat$FirstRecord2[ind_frspan])
#########################################################################



ind <- grepl("\\?",introdat$FirstRecord,perl=TRUE) # remove unsure entries
introdat$FirstRecord[ind] <- ""
ind <- grepl("\\?",introdat$FirstRecord1,perl=TRUE) # remove unsure entries
introdat$FirstRecord1[ind] <- ""
ind <- grepl("\\?",introdat$FirstRecord2,perl=TRUE) # remove unsure entries
introdat$FirstRecord2[ind] <- ""
ind <- grepl("\\?",introdat$DateNaturalisation,perl=TRUE) # remove unsure entries
introdat$DateNaturalisation[ind] <- ""

ind <- which(introdat$FirstRecord1=="unknown") # remove unsure entries
introdat$FirstRecord1[ind] <- ""
ind <- which(introdat$FirstRecord1=="NULL") # remove unsure entries
introdat$FirstRecord1[ind] <- ""



introdat$FirstRecord <- gsub("^\\s+|\\s+$", "", introdat$FirstRecord,perl=TRUE) # trim leading and trailing whitespace
introdat$FirstRecord1 <- gsub("^\\s+|\\s+$", "", introdat$FirstRecord1,perl=TRUE) # trim leading and trailing whitespace
introdat$FirstRecord2 <- gsub("^\\s+|\\s+$", "", introdat$FirstRecord2,perl=TRUE) # trim leading and trailing whitespace

ind <- grep("publication",introdat$FirstRecord,perl=TRUE)
introdat[ind,]$DataQuality <- "Publication date"
introdat$FirstRecord <- gsub(" \\(publication\\)","",introdat$FirstRecord)



introdat$FirstRecord <- gsub("˂","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("<","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("\\[|\\]","",introdat$FirstRecord,perl=TRUE)
introdat <- introdat[!grepl("circa",introdat$FirstRecord,perl=TRUE),]
introdat$FirstRecord <- gsub(" \\(pre\\)","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("ca ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("Cir","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("ca\\. ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("ca\\.","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub(" ca ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("mid-","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("early ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("Early ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("late ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("before ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("Possibly ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("Around ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub(" or later","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub(" or earlier","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub(" Century","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("'","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("~","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("–","-",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("^.*by ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("since ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("prior to ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("prior","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("pre","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("around ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("mid ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub("Mid ","",introdat$FirstRecord,perl=TRUE)
introdat$FirstRecord <- gsub(" to ","-",introdat$FirstRecord,perl=TRUE)

introdat$FirstRecord <- gsub(" \\(.*?\\)","",introdat$FirstRecord) # remove brackets with letters

introdat$FirstRecord[introdat$FirstRecord=="-"] <- ""
introdat$FirstRecord[introdat$FirstRecord=="3,000 or more years ago"] <- "-1000"

introdat$FirstRecord <- gsub("Pre-","",introdat$FirstRecord)
introdat$FirstRecord1 <- gsub("Pre-","",introdat$FirstRecord1)
introdat$FirstRecord2 <- gsub("Pre-","",introdat$FirstRecord2)



ind <- grep("0s",introdat$FirstRecord)
for (i in 1:length(ind)){
  introdat$FirstRecord[ind[i]] <- gsub("0s",sample(0:9,1),introdat$FirstRecord[ind[i]],perl=TRUE) # assign random variable to ...0ies
  introdat$DataQuality[ind[i]] <- "NEW_random"
}
introdat$FirstRecord <- gsub("\t","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("\\([0-9]*\\)","",introdat$FirstRecord,perl=TRUE)

introdat$FirstRecord <- gsub(",.*$","",introdat$FirstRecord) # only select first entry if several dates are provided (mainly for Kraus herptile data)
introdat$FirstRecord <- gsub(";.*$","",introdat$FirstRecord) # only select first entry if several dates are provided (mainly for Kraus herptile data)
introdat$FirstRecord <- gsub(" and .*$","",introdat$FirstRecord) # remove " and" and everything afterwards
introdat$FirstRecord <- gsub(" or .*$","",introdat$FirstRecord) # remove " and" and everything afterwards
introdat$FirstRecord <- gsub(" & .*$","",introdat$FirstRecord) # remove " and" and everything afterwards

ind <- grep("-| - ",introdat$FirstRecord)
for (i in 1:length(ind)){ # calculate mean dates
  if (length(grep(",",introdat$FirstRecord[ind[i]],perl=TRUE))>0){ # check for long entries with several dates
    comma <- unlist(strsplit(introdat$FirstRecord[ind[i]],",",fixed=TRUE))
    ind_comma <- grep("-",comma,perl=TRUE)
    if (ind_comma==1)
      introdat$FirstRecord[ind[i]] <- comma[ind_comma] # write range only if range is at first position in long entry
  }
  FRsplit <- as.numeric(unlist(strsplit(introdat$FirstRecord[ind[i]],"-",fixed=TRUE))) # split range into upper and lower bound
  if (any(is.na(FRsplit))) # skip if NA occurrs
    next
  if (nchar(FRsplit[2])==2){
    FRsplit[2] <- FRsplit[2] + floor(FRsplit[1]/100)*100
  }
  if ((FRsplit[2] - FRsplit[1])>20){ # remove entry if time span is larger than ...
    introdat$FirstRecord[ind[i]] <- ""
  } else {
    introdat$FirstRecord[ind[i]] <- FRsplit[1] + sample(1:(FRsplit[2] - FRsplit[1]),1)
    if ((FRsplit[2] - FRsplit[1])>0) introdat$DataQuality[ind[i]] <- "NEW_random"
  } # add random number of range to lower bound
  
  if (i%%100==0) print(round(i/length(ind)*100))
}



## FirstRecord1
introdat$FirstRecord1[grepl("1981",introdat$FirstRecord1)] <- 1981 # remove full date, keep just year
introdat$FirstRecord1[grepl("1989",introdat$FirstRecord1)] <- 1989
introdat$FirstRecord1[grepl("1998",introdat$FirstRecord1)] <- 1998
introdat$FirstRecord1[grepl("1997",introdat$FirstRecord1)] <- 1997
introdat$FirstRecord1[grepl("1995",introdat$FirstRecord1)] <- 1995
introdat$FirstRecord1[grepl("1993",introdat$FirstRecord1)] <- 1993
introdat$FirstRecord1[grepl("2002",introdat$FirstRecord1)] <- 2002
introdat$FirstRecord1[grepl("1978",introdat$FirstRecord1)] <- 1978
## FirstRecord2
introdat$FirstRecord2[grepl("1998",introdat$FirstRecord2)] <- 1998
introdat$FirstRecord2[grepl("2006",introdat$FirstRecord2)] <- 2006
introdat$FirstRecord2[grepl("1993",introdat$FirstRecord2)] <- 1993
introdat$FirstRecord2[grepl("1982",introdat$FirstRecord2)] <- 1982
introdat$FirstRecord2[grepl("2003",introdat$FirstRecord2)] <- 2003


introdat$FirstRecord <- gsub(" \\(Hong Kong\\)","",introdat$FirstRecord) 
introdat$FirstRecord <- gsub(" \\(only one exemplar was found\\)","",introdat$FirstRecord) 
introdat$FirstRecord <- gsub("-Republic of Tyva","",introdat$FirstRecord)
introdat$FirstRecord <- gsub(" \\(Taiwan\\)","",introdat$FirstRecord) 
introdat$FirstRecord <- gsub("’s","",introdat$FirstRecord)
introdat$FirstRecord <- gsub(" \\(Shaanxi\\)","",introdat$FirstRecord)
introdat$FirstRecord <- gsub(" \\(first time in Guangdong\\)","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("Found in southern Yunnan in ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("Record in ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("up-","",introdat$FirstRecord)
introdat$FirstRecord <- gsub(" \\(Introduction of British Concession\\)","",introdat$FirstRecord) 
introdat$FirstRecord <- gsub(" \\(Shandong Yantai\\)","",introdat$FirstRecord) 
introdat$FirstRecord <- gsub(" \\(Kaohsiung","",introdat$FirstRecord) 
introdat$FirstRecord <- gsub(" \\(Taiwan\\)","",introdat$FirstRecord)
introdat$FirstRecord <- gsub(" earlier than the small phlox","",introdat$FirstRecord)
introdat$FirstRecord <- gsub(" introduction of Hainan","",introdat$FirstRecord)
introdat$FirstRecord <- gsub(" \\(Guangdong Shunde\\)","",introdat$FirstRecord)
introdat$FirstRecord <- gsub(" \\(Shenzhen\\)","",introdat$FirstRecord)

introdat$FirstRecord <- gsub("90ies","1990's",introdat$FirstRecord)
introdat$FirstRecord <- gsub(">","",introdat$FirstRecord)
introdat$FirstRecord <- gsub(" A.D.","",introdat$FirstRecord)
introdat$FirstRecord <- gsub(" AD","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("\\*","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("?","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("th","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("/3","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("after ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("After ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("AFTER ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("Before ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("Beforte ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("c. ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("end of ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("Late ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("Post ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("Pre ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("after","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("End ","",introdat$FirstRecord)

introdat$FirstRecord <- gsub("The ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("?","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("efore ","",introdat$FirstRecord)
introdat$FirstRecord <- gsub("\\.","",introdat$FirstRecord) 
introdat$FirstRecord <- gsub("\\(","",introdat$FirstRecord)



## calculate mean year ##########################################################
introdat$FirstRecord <- as.numeric(introdat$FirstRecord)
introdat$FirstRecord1 <- as.numeric(introdat$FirstRecord1)
introdat$FirstRecord2 <- as.numeric(introdat$FirstRecord2)
introdat$DateNaturalisation <- as.numeric(introdat$DateNaturalisation)
introdat$FirstRecord_intentional <- as.numeric(introdat$FirstRecord_intentional)

ind_dnat <- !is.na(introdat$DateNaturalisation) & is.na(introdat$FirstRecord) ## add first record of naturalisation
introdat$FirstRecord[ind_dnat] <- introdat$DateNaturalisation[ind_dnat]
introdat$DataQuality[ind_dnat] <- "DateNaturalisation"
ind_int <- !is.na(introdat$FirstRecord_intentional) & is.na(introdat$FirstRecord)
introdat$FirstRecord[ind_int] <- introdat$FirstRecord_intentional[ind_int]

ind <- is.na(introdat$FirstRecord1) & !is.na(introdat$FirstRecord2)
introdat$FirstRecord[ind] <- introdat$FirstRecord2[ind]
introdat$DataQuality[ind] <- "NEW_After"

ind <- is.na(introdat$FirstRecord2) & !is.na(introdat$FirstRecord1)
introdat$FirstRecord[ind] <- introdat$FirstRecord1[ind]
introdat$DataQuality[ind] <- "NEW_Before"



## randomise mean time of first record ###############
ind <- !is.na(introdat$FirstRecord2) & !is.na(introdat$FirstRecord1)
# ind <- (introdat$FirstRecord2!="") & (introdat$FirstRecord1!="")
ranges <- rep(0,dim(introdat)[1])
ranges[ind] <- introdat$FirstRecord2[ind] - introdat$FirstRecord1[ind]
introdat <- introdat[ranges<=20 & ranges>=0,]  ########### remove time spans > 20 years ##################
# introdat <- introdat[ranges>=0,]  ########### remove only time spans < 0 years ##################

ind <- which(!is.na(introdat$FirstRecord2) & !is.na(introdat$FirstRecord1))
span_all <- list()
new_firstrecord <- list()
if (length(ind)!=0){
  for (i in 1:length(ind)){#
    span <- introdat$FirstRecord2[ind[i]] - introdat$FirstRecord1[ind[i]]
    new_firstrecord[[i]] <- introdat$FirstRecord1[ind[i]] + sample(0:span,1) ## assign random year within time span
    span_all[[i]] <- span
    # if (span>0) introdat$DataQuality[ind[i]] <- "NEW_random"
  }
}
new_firstrecord <- unlist(new_firstrecord)
span_all <- unlist(span_all)
introdat$FirstRecord[ind] <- new_firstrecord
introdat$DataQuality[ind[span_all>0]] <- "NEW_random"


introdat$DataQuality[introdat$DataQuality=="New_\"Mean Before/After\""] <- "NEW_Mean"
introdat$DataQuality[introdat$DataQuality=="NEW_\"Before_only\""] <- "NEW_Before"

introdat$DataQuality[grep(c("Lavoie|Maroyi|Mosena"),introdat$Source)] <- "Herbarium record"

introdat <- subset(introdat,!is.na(FirstRecord))


introdat[,DateNaturalisation:=NULL] # remove column as it is not needed anymore
introdat[,FirstRecord_intentional:=NULL] # remove column as it is not needed anymore
introdat[,FirstRecord1:=NULL] # remove column as it is not needed anymore
introdat[,FirstRecord2:=NULL] # remove column as it is not needed anymore




## Countries ###########################################################

# introdat$Country_orig <- introdat$Country
introdat$Country <- gsub("^\\s+|\\s+$", "",introdat$Country) # trim leading and trailing whitespace

## Long book island names ###################################################
mainlands <- c("USA","Australia","Great Britain","France","Canada","Ecuador","Spain","Italy","Germany","Chile","Russian Federation","Denmark",
  "Netherlands","South Africa","Greece","Croatia","Madagascar Rep.","Tunisia","Tanzania","Azerbaijan","Mexico","Argentina","Sweden",
  "Brazil","Namibia","Vietnam","Panama","Hong Kong","Finland","Algeria","Venezuela","United Arab Emirates","Uganda","Timor Leste Island",
  "Timor Leste","Thailand","Singapore Rep.","Macedonia","Honduras","Greece and Turkey","Colombia","Chile and Argentina","Caribbean","British Isles")
ind <- which(grepl("Long",introdat$Source) & introdat$Island=="yes" & introdat$Country%in%mainlands)
introdat$Country[ind][grepl("Hawaii Island",introdat$Region[ind])] <- "Hawaiian Islands"
introdat$Country[ind][grepl("^Tasmania",introdat$Region[ind])] <- "Tasmania"
introdat$Country[ind][grepl("Crete",introdat$Region[ind])] <- "Crete"
introdat$Country[ind][grepl("Corsica",introdat$Region[ind])] <- "Corse"
introdat$Country[ind][grepl("Guam",introdat$Region[ind])] <- "Guam"
introdat$Country[ind][grepl("Virgin Islands",introdat$Region[ind]) & !grepl("British",introdat$Region[ind])] <- "Virgin Islands, US"
introdat$Country[ind][grepl("British Virgin Islands",introdat$Region[ind])] <- "British Virgin Islands"
introdat$Country[ind][grepl("Galapagos",introdat$Region[ind])] <- "Galapagos"
introdat$Country[ind][grepl("South Georgia",introdat$Region[ind])] <- "South Georgia"
introdat$Country[ind][grepl("Puerto Rico",introdat$Region[ind])] <- "Puerto Rico"
introdat$Country[ind][grepl("Bermuda",introdat$Region[ind])] <- "Bermuda"
introdat$Country[ind][grepl("Guadeloupe Island",introdat$Region[ind])] <- "Guadeloupe"
introdat$Country[ind][grepl("Faroe Islands",introdat$Region[ind])] <- "Faroe Islands"
# introdat$Country[ind][grepl("Netherlands Antilles",introdat$Region[ind])] <- "Netherlands Antilles"
introdat$Country[ind][grepl("Aruba",introdat$Region[ind])] <- "Aruba"
introdat$Country[ind][grepl("Martinique",introdat$Region[ind])] <- "Martinique"
introdat$Country[ind][grepl("Falkland Islands",introdat$Region[ind])] <- "Falkland Islands"
introdat$Country[ind][grepl("Zanzibar Island",introdat$Region[ind])] <- "Zanzibar Island"
introdat$Country[ind][grepl("Kodiak Island",introdat$Region[ind])] <- "Kodiak Island"
introdat$Country[ind][grepl("St\\. Helena Island",introdat$Region[ind])] <- "Saint Helena"
introdat$Country[ind][grepl("St\\. Helena Group",introdat$Region[ind])] <- "Saint Helena"
introdat$Country[ind][grepl("Kerguelen Islands",introdat$Region[ind])] <- "Kerguelen Islands"
introdat$Country[ind][grepl("Lord Howe Island",introdat$Region[ind])] <- "Lord Howe Island"
introdat$Country[ind][grepl("Ascension",introdat$Region[ind])] <- "Ascension"
introdat$Country[ind][grepl("Tristan da Cunha",introdat$Region[ind])] <- "Tristan da Cunha"

introdat$Country[ind][grepl("South Indian Ocean",introdat$Region[ind]) & introdat$Country[ind]=="France"] <- introdat$Region[ind][grepl("South Indian Ocean",introdat$Region[ind]) & introdat$Country[ind]=="France"]
introdat$Country[ind] <- gsub(", South Indian Ocean","",introdat$Country[ind])

introdat$Country[ind][grepl("Canary Islands",introdat$Region[ind])] <- "Canary Islands"
introdat$Country[ind][grepl("Sardinia",introdat$Region[ind])] <- "Sardinia"
introdat$Country[ind][grepl("Sicily",introdat$Region[ind])] <- "Sicily"
introdat$Country[ind][grepl("Réunion",introdat$Region[ind])] <- "Reunion"
# introdat$Country[ind][grepl("Mariana Islands",introdat$Region[ind])] <- "Mariana Islands"
introdat$Country[ind][grepl("The Northern Marianas",introdat$Region[ind])] <- "Northern Mariana Islands"
introdat$Country[ind][grepl("Pitcairn Island",introdat$Region[ind])] <- "Pitcairn Island"
introdat$Country[ind][grepl("Polynesia",introdat$Region[ind]) & introdat$Country[ind]=="France"] <- "French Polynesia"
introdat$Country[ind][grepl("Society Islands Group",introdat$Region[ind])] <- "French Polynesia"
introdat$Country[ind][grepl("Austral Islands Group",introdat$Region[ind])] <- "French Polynesia"
introdat$Country[ind][grepl("Tahiti",introdat$Region[ind])] <- "French Polynesia"
introdat$Country[ind][grepl("New Caledonia",introdat$Region[ind])] <- "New Caledonia"
introdat$Country[ind][grepl("Guadeloupe",introdat$Region[ind])] <- "Guadeloupe"
introdat$Country[ind][grepl("Wallis and Futuna",introdat$Region[ind])] <- "Wallis and Futuna"
# introdat$Country[ind][grepl("Jarvis Island",introdat$Region[ind])] <- "Jarvis Island"
introdat$Country[ind][grepl("Midway Islands",introdat$Region[ind])] <- "Midway Islands"
introdat$Country[ind][grepl("Hawaii islands",introdat$Region[ind])] <- "Hawaiian Islands"
introdat$Country[ind][grepl("American Samoa",introdat$Region[ind])] <- "American Samoa"
introdat$Country[ind][grepl("Greater Antilles",introdat$Region[ind])] <- "Greater Antilles"
introdat$Country[ind][grepl("Eniwetok Atoll",introdat$Region[ind])] <- "Eniwetok Atoll"
introdat$Country[ind][grepl("Bahamas Islands",introdat$Region[ind])] <- "Bahamas"
introdat$Country[ind][grepl("Cayman Islands",introdat$Region[ind])] <- "Cayman Islands"
introdat$Country[ind][grepl("Chagos Archipelago",introdat$Region[ind])] <- "Chagos Archipelago"
introdat$Country[ind][grepl("Peros Banhos Islands",introdat$Region[ind])] <- "Peros Banhos Islands"
introdat$Country[ind][grepl("Baleares|Balearic Islands",introdat$Region[ind])] <- "Balearic Islands"
introdat$Country[ind][grepl("Crozet",introdat$Region[ind])] <- "Crozet Island Group"
introdat$Country[ind][introdat$Region[ind]=="Vancouver Island"] <- "Vancouver Island"
introdat$Country[ind][grepl("Christmas Island",introdat$Region[ind])] <- "Christmas Island"
introdat$Country[ind][grepl("Shetland ",introdat$Region[ind])] <- "Shetland Islands"
introdat$Region[ind][grepl("Guernsey Island|Jersey Island",introdat$Region[ind])] <- "Channel Islands"

introdat$Island[introdat$Country=="British Isles"] <- ""
introdat$Country[introdat$Country=="British Isles"] <- "United Kingdom"
introdat$Island[introdat$Country%in%c("Azerbaijan","Uganda","United States","Vietnam","Tanzania","Sweden","Spain","Russia","Portugal","Norway","Netherlands","Namibia","Mexico","Greece","Germany","France","Denmark","Colombia","Chile","Canada","Brazil","Australia")] <- ""



## Kraus book island names ###################################################
ind <- grep("Kraus",introdat$Source)
introdat$Country[ind][introdat$Country[ind]=="US: Hawaii"] <- "Hawaiian Islands"
introdat$Country[ind][grep("US: ",introdat$Country[ind])] <- "United States"
introdat$Country[ind][grep("Canada: ",introdat$Country[ind])] <- "Canada"
introdat$Country[ind][grep("Malaysia: ",introdat$Country[ind])] <- "Malaysia"
introdat$Country[ind] <- gsub(".*: ","",introdat$Country[ind])
introdat$Country[ind] <- gsub("^\\s+|\\s+$", "",introdat$Country[ind]) # trim leading and trailing whitespace



## harmonise country names #####################################
introdat$Country <- tolower(introdat$Country)
simpleCap <- function(x) {
  s <- strsplit(x, " ",fixed=TRUE)[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),sep="", collapse=" ")
}
introdat$Country <- sapply(introdat$Country,simpleCap) # capitalize all words

introdat$Country <- gsub(" And "," and ",introdat$Country)
# introdat$Country <- gsub(" Rep."," Rep\\.",introdat$Country)
introdat$Country <- gsub(" Of"," of",introdat$Country)
introdat$Country <- gsub(" The"," the",introdat$Country)
introdat$Country <- gsub(" Independent"," independent",introdat$Country)
if (length(grep("\\?",introdat$Country))!=0) introdat <- introdat[-grep("\\?",introdat$Country),]

introdat$Country <- gsub("St ","Saint ",introdat$Country)
introdat$Country <- gsub("St\\. ","Saint ",introdat$Country)
introdat$Country[introdat$Country=="Saint Kitts"] <- "Saint Kitts and Nevis"
introdat$Country[introdat$Country=="Nevis"] <- "Saint Kitts and Nevis"
introdat$Country[grep("Saint-pierre  Et Miquelon",introdat$Country)] <- "Saint Pierre and Miquelon"
introdat$Country[grep("Saint Kitts",introdat$Country)] <- "Saint Kitts and Nevis"
introdat$Country[introdat$Country=="Saint Vincent and the Grenadines Rep."] <- "Saint Vincent and the Grenadines"
introdat$Country[introdat$Country=="Saint Vincent Republic"] <- "Saint Vincent and the Grenadines"
introdat$Country[introdat$Country=="Saint Vincent"] <- "Saint Vincent and the Grenadines"
introdat$Country[introdat$Country=="Saint Maarten"] <- "Sint Maarten"
introdat$Country[introdat$Country=="Wallis and Fortuna"] <- "Wallis and Futuna"
introdat$Country[introdat$Country=="Wallis Island"] <- "Wallis and Futuna"
introdat$Country[introdat$Country=="Mauritus"] <- "Mauritius"
introdat$Country[introdat$Country=="West Papua"] <- "Indonesia"
introdat$Country[introdat$Country=="Sulawesi"] <- "Indonesia"
introdat$Country[introdat$Country=="Dagestan Autonom. Republic"] <- "Russia"

introdat$Country[grep("Tasmania",introdat$Country)] <- "Tasmania"
introdat$Country[grep("Hawaii",introdat$Country)] <- "Hawaiian Islands"
introdat$Country[grep("Kerguelen",introdat$Country)] <- "Kerguelen Islands"

introdat$Country[grep("United States",introdat$Country)] <- "United States"
introdat$Country[grep("Usa Generally",introdat$Country)] <- "United States"
introdat$Country[grep("California",introdat$Country)] <- "United States"
introdat$Country[grep("Texas",introdat$Country)] <- "United States"
introdat$Country[grep("Florida",introdat$Country)] <- "United States"
introdat$Country[grep("Australia",introdat$Country)] <- "Australia"
introdat$Country[grep("Autralia",introdat$Country)] <- "Australia"
introdat$Country[grep("Paul",introdat$Country)] <- "Saint Paul (France)"
introdat$Country[grep("Fyrm",introdat$Country)] <- "Macedonia"
introdat$Country[grep("Pitcairn Island",introdat$Country)] <- "Pitcairn Islands"
introdat$Country <- gsub("Great britain","United Kingdom",introdat$Country)
introdat$Country <- gsub("Great Britain","United Kingdom",introdat$Country)
introdat$Country <- gsub("United kingdom","United Kingdom",introdat$Country)

introdat$Country[introdat$Country=="Uk"] <- "United Kingdom"
introdat$Country[introdat$Country=="United Kingdom (britain)"] <- "United Kingdom"
introdat$Country[introdat$Country=="Fsm"] <- "Micronesia, Federated States of"
introdat$Country[introdat$Country=="Fs Mikronesia"] <- "Micronesia, Federated States of"
introdat$Country[introdat$Country=="Mikronesia"] <- "Micronesia, Federated States of"
introdat$Country[introdat$Country=="Mainland, China"] <- "China"
introdat$Country[introdat$Country=="Hon Kong"] <- "Hong Kong"
introdat$Country[introdat$Country=="Anguilla"] <- "Anguilla"
introdat$Country[introdat$Country=="Anguilla Island"] <- "Anguilla"
introdat$Country[introdat$Country=="Antigua"] <- "Antigua Islands"
introdat$Country[introdat$Country=="Bosnia-hercegovina"] <- "Bosnia and Herzegovina"
introdat$Country[introdat$Country=="Bosnia"] <- "Bosnia and Herzegovina"
introdat$Country[introdat$Country=="Cap Verde Islands Rep."] <- "Cape Verde"
introdat$Country[introdat$Country=="Channel Island"] <- "Channel Islands"
introdat$Country[grep("Moicronesia|Mikronesia",introdat$Country)] <- "Micronesia, Federated States of"
introdat$Country[grep("Christmas Island",introdat$Country)] <- "Christmas Island"
introdat$Country[grep("Cnmi",introdat$Country)] <- "Northern Mariana Islands"
introdat$Country[grep("keeling|Keeling",introdat$Country)] <- "Cocos (Keeling) Islands"
introdat$Country[grep("Fiji",introdat$Country)] <- "Fiji"
introdat$Country[grep("Fuji",introdat$Country)] <- "Fiji"
introdat$Country[grep("Kiribati",introdat$Country)] <- "Kiribati"
introdat$Country[introdat$Country=="Phoenix Islands"] <- "Kiribati"
introdat$Country[grep("Lao People",introdat$Country)] <- "Laos"
introdat$Country[grep("Macedonia",introdat$Country)] <- "Macedonia"
introdat$Country[introdat$Country=="Curaçao"] <- "Curacao"
introdat$Country[introdat$Country=="Curaￃﾧao"] <- "Curacao"
introdat$Country[introdat$Country=="Surinam"] <- "Suriname"
introdat$Country[introdat$Country=="Jersey"] <- "Channel Islands"
introdat$Country[introdat$Country=="Korea, Republic of"] <- "South Korea"
introdat$Country[introdat$Country=="Korea, Democratic People's Republic of"] <- "North Korea"
# introdat$Country[introdat$Country=="Vancouver Island"] <- "Canada"
introdat$Country[introdat$Country=="Virgin Islands, Uk"] <- "British Virgin Islands"
introdat$Country[introdat$Country=="Virgin Islands Us"] <- "Virgin Islands, US"
introdat$Country[introdat$Country=="Us Virgin Islands"] <- "Virgin Islands, US"
introdat$Country[introdat$Country=="Wallis and Futuna Islands"] <- "Wallis and Futuna"
introdat$Country[introdat$Country=="Turks and Caicos Islands"] <- "Turks and Caicos"
introdat$Country[introdat$Country=="Terres Australes Et Antarctiques Françaises  - Iles Eparses"] <- "Scattered Islands"
introdat$Country[introdat$Country=="Terres Australes Et Antarctiques Fran?aises  - Iles Eparses"] <- "Scattered Islands"
introdat$Country[introdat$Country=="Terres Australes Et Antarctiques Fran?aises  - Iles Eparses"] <- "Scattered Islands"

introdat$Country[introdat$Country=="Corsica"] <- "Corse"
introdat$Country[introdat$Country=="Guinea-bissau"] <- "Guinea-Bissau"
introdat$Country[introdat$Country=="British Indian Ocean Territory"] <- "Chagos Archipelago"
introdat$Country[introdat$Country=="Cocos Islands" & introdat$Source=="CABI ISC"] <- "Cocos (Keeling) Islands"



introdat$Country <- gsub("South africa","South Africa",introdat$Country)
introdat$Country <- gsub("Rep\\. S\\. Africa","South Africa",introdat$Country)
introdat$Country <- gsub("England","United Kingdom",introdat$Country)
introdat$Country <- gsub("Wales","United Kingdom",introdat$Country)
introdat$Country[introdat$Country=="Prince of United Kingdom Island, Southern Tip of Alaska"] <- "Prince of Wales Island, Southern Tip of Alaska"
introdat$Country <- gsub("Scotland","United Kingdom",introdat$Country)
# introdat$Country <- gsub("Hawaii","Hawaiian Islands",introdat$Country)
introdat$Country <- gsub("Hawiian Islands","Hawaiian Islands",introdat$Country)
introdat$Country <- gsub("Russian federation","Russia",introdat$Country)
introdat$Country <- gsub("European part of russia","Russia",introdat$Country)
introdat$Country <- gsub("Usacanada","USACanada",introdat$Country) # Data from Andrew Liebhold
introdat$Country <- gsub("Usa","United States",introdat$Country)
introdat$Country <- gsub("Nz North Island","New Zealand",introdat$Country)
introdat$Country <- gsub("Nz South Island","New Zealand",introdat$Country)
introdat$Country <- gsub("New Zealand","New Zealand",introdat$Country)
introdat$Country <- gsub("Luxemburg","Luxembourg",introdat$Country)
introdat$Country <- gsub("Lichtenstein","Liechtenstein",introdat$Country)
introdat$Country <- gsub("Kyrgystan","Kyrgyzstan",introdat$Country)
# introdat$Country <- gsub(" \\(mainland\\)","",introdat$Country)
introdat$Country <- gsub(" \\(in europe\\)","",introdat$Country)
introdat$Country <- gsub(" \\(in Europe\\)","",introdat$Country)
introdat$Country[grep("corsica",introdat$Country)] <- "Corse"
introdat$Country[grep("anary Is",introdat$Country)] <- "Canary Islands"
introdat$Country[grep("crete",introdat$Country)] <- "Crete"
introdat$Country[grep("Greece",introdat$Country)] <- "Greece"
introdat$Country[grep("sicily",introdat$Country)] <- "Sicily"
introdat$Country[grep("Sicily",introdat$Country)] <- "Sicily"
introdat$Country[grep("Sicilia",introdat$Country)] <- "Sicily"
introdat$Country[grep("Seychelles",introdat$Country)] <- "Seychelles"
introdat$Country[grep("alearic Islands|Baleares",introdat$Country)] <- "Balearic Islands"
introdat$Country[grep("madeira",introdat$Country)] <- "Madeira"
introdat$Country[grep("^Canada",introdat$Country)] <- "Canada"
introdat$Country[grep("United States",introdat$Country)] <- "United States"
introdat$Country[grep("Quebec",introdat$Country)] <- "Canada"
introdat$Country[grep("Haiti",introdat$Country)] <- "Haiti"
introdat$Country[grep("Palau",introdat$Country)] <- "Palau"
introdat$Country[grep("Ukraine",introdat$Country)] <- "Ukraine"
introdat$Country[grep("Kazakhstan",introdat$Country)] <- "Kazakhstan"
introdat$Country[grep("Azerbaidschan",introdat$Country)] <- "Azerbaijan"
introdat$Country[grep("Aerbaidschan",introdat$Country)] <- "Azerbaijan"
introdat$Country[grep("Belarus",introdat$Country)] <- "Belarus"
introdat$Country[grep("Channel Is\\.",introdat$Country)] <- "Channel Islands"
introdat$Country[grep("Fiji Is",introdat$Country)] <- "Fiji"
introdat$Country[grep("Uzbekistan",introdat$Country)] <- "Uzbekistan"
introdat$Country[grep("Turkmenistan",introdat$Country)] <- "Turkmenistan"
introdat$Country[grep("Trinidad and Tobago",introdat$Country)] <- "Trinidad and Tobago"
introdat$Country[grep("Tanzania",introdat$Country)] <- "Tanzania"
introdat$Country[grep("Taiwan",introdat$Country)] <- "Taiwan"
introdat$Country[grep("Tajikistan",introdat$Country)] <- "Tajikistan"
introdat$Country[grep("Solomon Islands",introdat$Country)] <- "Solomon Islands"
introdat$Country[grep("Tonga Islands",introdat$Country)] <- "Tonga"
introdat$Country[grep("ivoire",introdat$Country)] <- "Cote D'Ivoire"

introdat$Country[grep("Russian Federation",introdat$Country)] <- "Russia"
introdat$Country[grep("Russian Federation",introdat$Country)] <- "Russia"
introdat$Country[grep("New Caledonia",introdat$Country)] <- "New Caledonia"
introdat$Country[grep("New Zealand",introdat$Country)] <- "New Zealand"
introdat$Country[grep("Mauritius",introdat$Country)] <- "Mauritius"
introdat$Country[grep("Maldives",introdat$Country)] <- "Maldives"
introdat$Country[grep("Madagascar",introdat$Country)] <- "Madagascar"
introdat$Country[grep("Lithuania",introdat$Country)] <- "Lithuania"
introdat$Country[grep("Lesser Antilles",introdat$Country)] <- "Lesser Antilles"
introdat$Country[grep("Latvia",introdat$Country)] <- "Latvia"
introdat$Country[grep("La Reunion",introdat$Country)] <- "Reunion"
introdat$Country[grep("La R?union",introdat$Country)] <- "Reunion"
introdat$Country[grep("Reunion",introdat$Country)] <- "Reunion"
introdat$Country[grep("La Réunion",introdat$Country)] <- "Reunion"
introdat$Country[grep("Réunion",introdat$Country)] <- "Reunion"
introdat$Country[grep("Reunion",introdat$Country)] <- "Reunion"
introdat$Country[grep("R?union",introdat$Country)] <- "Reunion"
introdat$Country[grep("Kirgizstan",introdat$Country)] <- "Kyrgyzstan"
introdat$Country[grep("Iran",introdat$Country)] <- "Iran"
introdat$Country[grep("Iceland",introdat$Country)] <- "Iceland"
introdat$Country[grep("Grenada Republic",introdat$Country)] <- "Grenada"
introdat$Country[grep("Georgia Republic",introdat$Country)] <- "Georgia"
introdat$Country[introdat$Country=="Georgia (republic of)"] <- "Georgia"
introdat$Country[grep("Galapagos Islands",introdat$Country)] <- "Galapagos"
introdat$Country[grep("Equatorial Guinea",introdat$Country)] <- "Equatorial Guinea"
introdat$Country[grep("Dominica Rep\\.",introdat$Country)] <- "Dominica"
introdat$Country[grep("Armenia",introdat$Country)] <- "Armenia"
introdat$Country[grep("Antigua and Barbuda",introdat$Country)] <- "Antigua and Barbuda"
introdat$Country[grep("Mongolia",introdat$Country)] <- "Mongolia"
introdat$Country[grep("Micronesia",introdat$Country)] <- "Micronesia, Federated States of"
introdat$Country[grep("Mongolia",introdat$Country)] <- "Mongolia"
introdat$Country[grep("Antigua Island",introdat$Country)] <- "Antigua and Barbuda"
introdat$Country[grep("Guiana Island",introdat$Country)] <- "Antigua and Barbuda"
introdat$Country[grep("Chatham Islands",introdat$Country)] <- "New Zealand"

introdat$Country[grep("zores",introdat$Country)] <- "Azores"
introdat$Country[grep("Ascension",introdat$Country)] <- "Ascension"
introdat$Country[grep("Ascencion",introdat$Country)] <- "Ascension"
introdat$Country[grep("Clipperton",introdat$Country)] <- "Clipperton Island"
introdat$Country[grep("ardinia",introdat$Country)] <- "Sardinia"
introdat$Country[grep("svalbard",introdat$Country)] <- "Svalbard and Jan Mayen"
introdat$Country[grep("Zanzibar",introdat$Country)] <- "Zanzibar Island"
introdat$Country[grep("Yemen",introdat$Country)] <- "Yemen"
introdat$Country[grep("Taiwan",introdat$Country)] <- "Taiwan"
introdat$Country[grep("Moldova",introdat$Country)] <- "Moldova"
# introdat$Country[grep("Fyrm",introdat$Country)] <- "Fyrm"
introdat$Country[grep("Falkland",introdat$Country)] <- "Falkland Islands"
introdat$Country[grep("British Columbia",introdat$Country)] <- "Canada"
introdat$Country[grep("Sao Tome",introdat$Country)] <- "Sao Tome and Principe"
introdat$Country[grep("India \\(south\\)",introdat$Country)] <- "India"
introdat$Country[introdat$Country=="Tunesia"] <- "Tunisia"
introdat$Country[introdat$Country=="Tunusia"] <- "Tunisia"
introdat$Country[introdat$Country=="Palestine"] <- "Palestine, State of"
introdat$Country[introdat$Country=="Palestine Authority"] <- "Palestine, State of"

introdat$Country[introdat$Country=="East Timor"] <- "Timor Leste"
introdat$Country[introdat$Country=="Norfolk Islands"] <- "Norfolk Island"
introdat$Country[introdat$Country=="Holland"] <- "Netherlands"
introdat$Country[introdat$Country=="U.s. Virgin Islands"] <- "Virgin Islands, US"
introdat$Country[introdat$Country=="Virgin Islands, US"] <- "Virgin Islands, US"
introdat$Country[introdat$Country=="Virgin Islands, Us"] <- "Virgin Islands, US"
introdat$Country[introdat$Country=="Virgin Islands, U.s"] <- "Virgin Islands, US"
introdat$Country[introdat$Country=="Us Minor Outlying Islands"] <- "US Minor Outlying Islands"
introdat$Country[introdat$Country=="Syrian Arab Republic"] <- "Syria"
introdat$Country[introdat$Country=="Libyan Arab Jamahiriya"] <- "Libya"
introdat$Country[introdat$Country=="Congo, the Democratic Republic of the"] <- "Congo, Democratic Republic of the"
introdat$Country[introdat$Country=="D.r. Congo"] <- "Congo, Democratic Republic of the"
introdat$Country[introdat$Country=="Congo, Democratic Republic of the"] <- "Congo, Democratic Republic of the"
introdat$Country[introdat$Country=="Congo Democratic Republic"] <- "Congo, Democratic Republic of the"
introdat$Country[introdat$Country=="Congo, Democratic Republic of"] <- "Congo, Democratic Republic of the"
introdat$Country[introdat$Country=="Congo"] <- "Congo, Democratic Republic of the"
introdat$Country[introdat$Country=="Sir Lanka"] <- "Sri Lanka"
introdat$Country[introdat$Country=="South Georgia"] <- "South Georgia and the South Sandwich Islands"
introdat$Country[introdat$Country=="Trinidad"] <- "Trinidad and Tobago"
introdat$Country[introdat$Country=="European Part of Russia"] <- "Russia"
introdat$Country[grep("Crozet",introdat$Country)] <- "Crozet Islands Group"
introdat$Country[grep("Midway Islands",introdat$Country)] <- "US Minor Outlying Islands"
introdat$Country[grep("Jarvis Island",introdat$Country)] <- "US Minor Outlying Islands"
introdat$Country[grep("Rsa",introdat$Country)] <- "South Africa"
# introdat$Country[introdat$Country=="Sea of Cortez Islands"] <- "United States"
introdat$Country[introdat$Country=="Nouvelle Amsterdam Island"] <- "Amsterdam Island"
introdat$Country[introdat$Country=="Amsterdam Islands"] <- "Amsterdam Island"
introdat$Country[introdat$Country=="North of Caucasus"] <- "Russia"
introdat$Country[introdat$Country=="Northern Caucasus"] <- "Russia"
introdat$Country[introdat$Country=="Northern Ireland"] <- "United Kingdom"
introdat$Country[introdat$Country=="Mongolei"] <- "Mongolia"
introdat$Country[introdat$Country=="Loyalty Islands"] <- "New Caledonia"
introdat$Country[introdat$Country=="Lampedusa"] <- "Italy"
introdat$Country[introdat$Country=="Kodiak Island"] <- "United States"
introdat$Country[introdat$Country=="Klein Curaçao"] <- "Curacao"
introdat$Country[introdat$Country=="Eniwetok Atoll"] <- "Marshall Islands"
introdat$Country[introdat$Country=="Culebra"] <- "Puerto Rico"
introdat$Country[introdat$Country=="Cozumel"] <- "Mexico"
introdat$Country[introdat$Country=="Corfu"] <- "Greece"
introdat$Country[introdat$Country=="Comoro Islands Republic"] <- "Comoros"
introdat$Country[introdat$Country=="Carriacou"] <- "Saint Vincent and the Grenadines"
introdat$Country[introdat$Country=="Canouan"] <- "Saint Vincent and the Grenadines"
introdat$Country[introdat$Country=="Bequia"] <- "Saint Vincent and the Grenadines"
introdat$Country[introdat$Country=="Union Island"] <- "Saint Vincent and the Grenadines"
# introdat$Country[introdat$Country=="Bonaire"] <- "Leeward Islands Group"
introdat$Country[introdat$Country=="Barbuda Island"] <- "Antigua and Barbuda"
# introdat$Country[introdat$Country=="Anticosti Island"] <- "Canada"
introdat$Country[introdat$Country=="Rￃﾩunion"] <- "Reunion"
introdat$Country[introdat$Country=="Cayman Island"] <- "Cayman Islands"
introdat$Country[introdat$Country=="Antigua and Barbados"] <- "Antigua and Barbuda"
introdat$Country[introdat$Country=="Wake Island"] <- "US Minor Outlying Islands"
introdat$Country[introdat$Country=="Walpole Island"] <- "New Caledonia"
introdat$Country[introdat$Country=="Zaire"] <- "Congo, Democratic Republic of the"
introdat$Country[introdat$Country=="Saint Barts"] <- "Saint Barthelemy"
introdat$Country[introdat$Country=="Rￃﾼgen Island"] <- "Germany"
introdat$Country[introdat$Country=="Åland Islands"] <- "Aland Islands"
introdat$Country[introdat$Country=="?land Islands"] <- "Aland Islands"
introdat$Country[introdat$Country=="Aland Islands"] <- "Aland Islands"

introdat$Country[introdat$Country=="Argentine"] <- "Argentina"
introdat$Country[introdat$Country=="New Hebrides"] <- "United Kingdom"
introdat$Country[introdat$Country=="New Ireland"] <- "Papua New Guinea"
introdat$Country[introdat$Country=="New Britain"] <- "Papua New Guinea"
introdat$Country[introdat$Country=="Burma"] <- "Myanmar"
introdat$Country[introdat$Country=="Mustique"] <- "Saint Vincent and the Grenadines"
introdat$Country[introdat$Country=="Tahiti"] <- "French Polynesia"
introdat$Country[introdat$Country=="Antilles Puerto Rico"] <- "Puerto Rico"
introdat$Country[introdat$Country=="Tristan Da Cunha"] <- "Tristan da Cunha"
introdat$Country[introdat$Country=="Andaman Islands"] <- "Andaman and Nicobar Islands"
introdat$Country[introdat$Country=="Nicobar Islands"] <- "Andaman and Nicobar Islands"
introdat$Country[introdat$Country=="Schweiz"] <- "Switzerland"
introdat$Country[grep("Helena",introdat$Country)] <- "Saint Helena"


## remove regions (be careful when new terms as grep also idenifies partial matches!)
introdat <- introdat[!grepl("Caucasus|Serbia and Montenegro|Ussr|Netherlands Antilles",introdat$Country),]
introdat <- introdat[!grepl("New Guinea|Pacific Island|India and Pakistan|Malaya",introdat$Country),]
introdat <- introdat[!grepl("South America|Caribbean|Lesser Antilles|Iles Subantarctiques|Amyahn Place Not Found",introdat$Country),]
introdat <- introdat[!grepl("Trust Territory of Pacific Islands|Carpathian Mountains|Europe$",introdat$Country),]
introdat <- introdat[!grepl("Yugoslavia|French West Indies|West Indies|Indonesia and Timor Leste Rep",introdat$Country),]
introdat <- introdat[!grepl("French Southern Territories|South-western Africa, Kalahari Desert|Borneo",introdat$Country),]
introdat <- introdat[!grepl("Oceania|Yugoslavia|Yougaslavia|Hispaniola|Leeward Islands Group|Czechoslovakia",introdat$Country),]

introdat <- introdat[!grepl("Aegean Sea|Asia|Black Sea|Sweden, Denmark",introdat$Country),]
introdat <- introdat[!grepl("Bosnia And Herzegovina|Central America/ Carribean|India And Pakistan|Indonesia And Timor Leste Rep.",introdat$Country),]
introdat <- introdat[!grepl("Macaronesia|Mediterranean|New World Tropics|North America|Serbia And Montenegro",introdat$Country),]
introdat <- introdat[!grepl("Tropical  S America|Tropical S America|Tropics of Southeast Asia|Unknown",introdat$Country),]

## remove exact matches
introdat <- subset(introdat,!Country%in%c("Mariana Islands","America","Korea","Virgin Islands","Antilles"))
introdat <- subset(introdat,Country!="")

introdat$Country <- gsub(" \\(mainland\\)","",introdat$Country)


introdat$Country[introdat$Country=="Andaman and Nicobar Islands"] <- "Andaman and Nicobar Islands"
introdat$Country[introdat$Country=="Andaman And Nicobar Islands"] <- "Andaman and Nicobar Islands"
introdat$Country[introdat$Country=="Antigua And Barbados"] <- "Antigua and Barbuda"
introdat$Country[introdat$Country=="Antigua And Barbuda"] <- "Antigua and Barbuda"
introdat$Country[introdat$Country=="Antigua And Barbuda Islands"] <- "Antigua and Barbuda"
introdat$Country[introdat$Country=="Arkansas"] <- "United States"
# introdat$Country[introdat$Country=="British Virgin Islands"] <- "Virgin Islands (British)"
# introdat$Country[introdat$Country=="Cocos (Keeling) Islands"] <- "Cocos Islands"
introdat$Country[introdat$Country=="Columbia"] <- "Colombia" # misspelling in Jones et al. about ferns
introdat$Country[introdat$Country=="Cura?ao"] <- "Curacao"
introdat$Country[introdat$Country=="Guadalupe Island"] <- "Guadeloupe"
introdat$Country[introdat$Country=="Guadeloupe Island"] <- "Guadeloupe"
introdat$Country[introdat$Country=="Guadeloupe Islands"] <- "Guadeloupe"
introdat$Country[introdat$Country=="Hawaaiian Islands"] <- "Hawaiian Islands"
introdat$Country[introdat$Country=="Hong-kong"] <- "Hong Kong"
introdat$Country[introdat$Country=="Iran"] <- "Iran, Islamic Republic of"
introdat$Country[introdat$Country=="Java"] <- "Indonesia"
introdat$Country[introdat$Country=="Juan Fernandez Islands"] <- "Chile"
introdat$Country[introdat$Country=="Kzn"] <- "South Africa"
introdat$Country[introdat$Country=="La R?union"] <- "Reunion"
introdat$Country[introdat$Country=="Marion Island"] <- "Canada"
introdat$Country[introdat$Country=="Marquesas Islands"] <- "French Polynesia"
introdat$Country[introdat$Country=="New Caledoina"] <- "New Caledonia"
introdat$Country[introdat$Country=="Palestinian Territory, Occupied"] <- "Palestine, State of"
introdat$Country[introdat$Country=="Peurto Rico"] <- "Puerto Rico"
introdat$Country[introdat$Country=="Prince Edward Island"] <- "Canada"
introdat$Country[introdat$Country=="Prince Edward Islands"] <- "Canada"
introdat$Country[introdat$Country=="R?union"] <- "Reunion"
introdat$Country[introdat$Country=="Reunion"] <- "Reunion"
introdat$Country[introdat$Country=="Saint Pierre And Miquelon"] <- "Saint Pierre and Miquelon"
introdat$Country[introdat$Country=="Saint Vincent And the Grenadines"] <- "Saint Vincent and the Grenadines"
introdat$Country[introdat$Country=="Saint Vincent And the Grenadines Rep."] <- "Saint Vincent and the Grenadines"
introdat$Country[introdat$Country=="Society Islands"] <- "French Polynesia"
introdat$Country[introdat$Country=="Society Islands Group"] <- "French Polynesia"
introdat$Country[introdat$Country=="South Georgia And the South Sandwich Islands"] <- "South Georgia and the South Sandwich Islands"
introdat$Country[introdat$Country=="South Georgia Island"] <- "South Georgia and the South Sandwich Islands"
introdat$Country[introdat$Country=="Spain,"] <- "Spain"
introdat$Country[introdat$Country=="Svalbard"] <- "Svalbard and Jan Mayen"
introdat$Country[introdat$Country=="Swaziland"] <- "Eswatini"
# introdat$Country[introdat$Country=="Terres Australes Et Antarctiques Fran?aises  - Iles Eparses"] <- "French Southern Territories"
introdat$Country[introdat$Country=="Trinidad and Tabago"] <- "Trinidad and Tobago"
introdat$Country[introdat$Country=="Trinidad And Tobago"] <- "Trinidad and Tobago"
introdat$Country[introdat$Country=="Trinidad And Tobago Rep."] <- "Trinidad and Tobago"
introdat$Country[introdat$Country=="Turks And Caicos"] <- "Turks and Caicos"
introdat$Country[introdat$Country=="Turks And Caicos Islands"] <- "Turks and Caicos"
introdat$Country[introdat$Country=="United Kingdom of United Kingdom and Northern Ireland"] <- "United Kingdom"
#introdat$Country[introdat$Country=="Unknown"] <- ""
# introdat$Country[introdat$Country=="US Minor Outlying Islands"] <- "United States Minor Outlying Islands"
introdat$Country[introdat$Country=="Us Minor Outlying Territories"] <- "US Minor Outlying Islands"
# introdat$Country[introdat$Country=="Vancouver Island"] <- "Canada"
introdat$Country[introdat$Country=="Wallis And Fortuna"] <- "Wallis and Futuna"
introdat$Country[introdat$Country=="Wallis And Futuna"] <- "Wallis and Futuna"
introdat$Country[introdat$Country=="Wallis And Futuna Islands"] <- "Wallis and Futuna"
introdat$Country[introdat$Country=="Wallis And Futuna Islands"] <- "Wallis and Futuna"
introdat$Country[introdat$Country=="United States of America"] <- "United States"
introdat$Country[introdat$Country=="Tristan Group"] <- "Tristan da Cunha"
introdat$Country[introdat$Country=="Transantarctic Mountains"] <- "Antarctica"
introdat$Country[introdat$Country=="South Victoria Land"] <- "Antarctica"
# introdat$Country[introdat$Country=="South Orkney Islands"] <- "Antarctica"
introdat$Country[introdat$Country=="South Antarctic Peninsula"] <- "Antarctica"
introdat$Country[introdat$Country=="Enderby Land"] <- "Antarctica"
introdat$Country[introdat$Country=="Larsen Ice Shelf"] <- "Antarctica"
introdat$Country[introdat$Country=="Ellsworth Mountains"] <- "Antarctica"
introdat$Country[introdat$Country=="East Antarctic Ice Sheet"] <- "Antarctica"
introdat$Country[introdat$Country=="East Antarctica"] <- "Antarctica"
introdat$Country[introdat$Country=="Ross Ice Shelf"] <- "Antarctica"
introdat$Country[introdat$Country=="Filchner Ice Shelf"] <- "Antarctica"
introdat$Country[introdat$Country=="North Victoria Land"] <- "Antarctica"
introdat$Country[introdat$Country=="Brunt Ice Shelf"] <- "Antarctica"
introdat$Country[introdat$Country=="Adelie Land"] <- "Antarctica"

introdat$Country[introdat$Country=="North-west Antarctic Peninsula"] <- "Antarctica"
introdat$Country[introdat$Country=="Snares"] <- "New Zealand"
introdat$Country[introdat$Country=="Serbia, Fyrom"] <- "Serbia"
introdat$Country[introdat$Country=="Peninsular Malaysia"] <- "Malaysia"
introdat$Country[introdat$Country=="Klein Cura?ao"] <- "Curacao"
introdat$Country[introdat$Country=="Cura?ao"] <- "Curacao"
introdat$Country[introdat$Country=="Czech Repoublic"] <- "Czech Republic"
introdat$Country[introdat$Country=="Auckland"] <- "New Zealand"
introdat$Country[introdat$Country=="Antipodes"] <- "New Zealand"

# sort(unique(introdat$Country))



## Taxon types ###########################################################
ind_tax <- nchar(introdat$LifeForm)==0 & !is.na(introdat$TaxonType)
introdat$LifeForm[ind_tax] <- introdat$TaxonType[ind_tax]
introdat$LifeForm <- gsub("^\\s+|\\s+$", "",introdat$LifeForm) # trim leading and trailing whitespace

introdat$LifeForm[grepl("Animalia",introdat$LifeForm)] <- introdat$TaxonType[grepl("Animalia",introdat$LifeForm)]
introdat[,TaxonType:=NULL] # remove column which is not used anymore

introdat$LifeForm[introdat$LifeForm=="Animals"] <- ""
introdat$LifeForm[introdat$LifeForm=="Arthropods"] <- ""
introdat$LifeForm[introdat$LifeForm=="Arthropoda"] <- ""
introdat$LifeForm[introdat$LifeForm=="Arthropod"] <- ""
introdat$LifeForm[introdat$LifeForm=="Microbes"] <- ""
introdat$LifeForm[introdat$LifeForm=="Other chordates"] <- ""
introdat$LifeForm[introdat$LifeForm=="Chordata"] <- ""
introdat$LifeForm2 <- NA

LifeForm_data <- file.path(directory, "Supporting_data", "Life_form_taxonomy.csv")
tax <- read.table(LifeForm_data,sep=";",header=T,stringsAsFactors=F)
for (i in 1:dim(tax)[1]){
  introdat$LifeForm2[introdat$LifeForm==tax[i,1]] <- tax[i,5]
  introdat$LifeForm[introdat$LifeForm==tax[i,1]] <- tax[i,4]
}

introdat$LifeForm[introdat$LifeForm=="incertae sedis" & introdat$Source=="GISD"] <- "Vascular plants" # wrong entries


#### substitute herptiles life form ################################
Herptiles_LifeForm_data <- file.path(directory, "Supporting_data", "Herptiles.csv")
repamph <- read.table(Herptiles_LifeForm_data,sep=";",stringsAsFactors = F,header=T)
repamph$Group[grepl("Crotaphytus|Gymnophthalmus|Hemidactylus|Gekko",repamph$Species)] <- "Reptilia"
repamph$Group[grepl("Eleutherodactylus|Triturus",repamph$Species)] <- "Amphibia"
repamph <- repamph[!duplicated(repamph),]

repamph$Group <- gsub("Reptilia","Reptiles",repamph$Group)
repamph$Group <- gsub("Amphibia","Amphibians",repamph$Group)

introdat <- merge(introdat,repamph,by.x="NewName",by.y="Species",all.x=T)
introdat[!is.na(introdat$Group),]$LifeForm <- introdat[!is.na(introdat$Group),]$Group

introdat[introdat$LifeForm=="Herptiles" & grepl("Bufo|Rana|Ambystoma|Fejervarya|Hoplobatrachus|Microhyla|Physalaemus",introdat$NewName,perl=TRUE),]$LifeForm <- "Amphibians"
introdat[introdat$LifeForm=="Herptiles" & grepl("Amphiesma|Anolis|Chlamydosaurus|Conolophus|Emydura|Eulamprus|Gallotia|Geochelone|Japalura|Laudakia|Lophognathus|Mediodactylus|Pantherophis|Phyllodactylus|Protobothrops|Sauromalus|Speleomantes|Teira|Trachemys|Trioceros|Vipera|Zamenis",introdat$NewName,perl=TRUE),]$LifeForm <- "Reptiles"

# out_lifeform <- sort(table(paste(introdat$LifeForm_orig,"_",introdat$LifeForm,sep="")))
# lf <- strsplit(rownames(out_lifeform),"_")
# lf2 <- do.call("rbind",lf)
# lf3 <- cbind(lf2,as.numeric(out_lifeform))
# # # write.table(lf3,"Data/LifeFormGrouping.csv",quote=F,sep=";",row.names=F,col.names=F)

introdat$LifeForm[introdat$LifeForm=="Fish"] <- "Fishes"


# ## remove no-species information
# introdat <- subset(introdat,!grepl(" spp\\.$",introdat$NewName))
# introdat <- subset(introdat,!grepl(" spec\\.$",introdat$NewName))
# introdat <- subset(introdat,!grepl(" sp$",introdat$NewName))
# 
# introdat$NewName <- gsub("\\[.*?\\]","",introdat$NewName) #
# introdat$NewName <- gsub("ssp","subsp",introdat$NewName)
# introdat$NewName <- gsub(" - ","-",introdat$NewName)

## replace taxon names with obvious wrong entries ####################


Wrong_Species <- file.path(directory, "Supporting_data", "Wrong_species_entries.csv")
newentries <- read.table(Wrong_Species,sep=";",stringsAsFactors = F,header=T)
for (i in 1:nrow(newentries)){
  introdat$NewName[introdat$NewName==newentries$WrongEntry[i]] <- newentries$NewEntry[i]
}




## check plant synonyms ######################################################

introdat$NewName <- gsub("^\\(","",introdat$NewName,perl=TRUE)
introdat$NewName <- gsub("^\\s+|\\s+$", "",introdat$NewName,perl=TRUE)  # trim leading and trailing whitespace
introdat$NewName <- gsub("  "," ",introdat$NewName,perl=TRUE)
introdat$NewName <- gsub("\\*","",introdat$NewName)


# write.table(introdat,"Data/IntroData_beforeTPL.csv",sep=";")
fwrite(introdat, file.path(directory, "Outputs", "IntroData_beforeTPL.gz"))
# saveRDS(introdat,"Data/IntroData_beforeTPL.rds")
# introdat <- read.table("Data/IntroData_beforeTPL.csv",stringsAsFactors=F,sep=";")
# write.xlsx(IntroData_beforeTPL,file=file.path("Data","IntroData_beforeTPL.xlsx"))


introdat <- as.data.frame(introdat)


# plants <- which(introdat$LifeForm=="Vascular plants")
introdat$TPLindex <- ""
introdat$AccName <- ""
introdat$scientificName <- NA

# introdat <- as.data.frame(fread("Data/Introdat_inTPL25445.gz"))
allplantspec <- unique(subset(introdat,LifeForm=="Vascular plants")$NewName)
# backup <- introdat
for (i in 1:length(allplantspec)){ #length(allplantspec)
  
  if (i%%100==0) print(round((i/length(allplantspec))*100,2))
  
  if (grepl("^x",allplantspec[i])) next # cannot be resolved by TPL
  ind_introdat <- which(introdat$NewName==allplantspec[i])

  newname <- try(TPLck(sp=allplantspec[i],corr=T),silent=T) # takes some time...
  # newname <- try(TPLck("Brunnera macrophylla (Adams) I.M.Johnst.",corr=T),silent=T) # takes some time...
  
  if (class(newname)=="try-error"){ # check if call was successful
    if (grepl("\\(",allplantspec[i])){
      allplantspec[i] <- gsub("\\(.*?\\)","",allplantspec[i]) # remove brackets because TPL has sometimes problems with brackets
      newname <- try(TPLck(allplantspec[i],corr=F,abbrev=T),silent=T)
    }
  }
  if (class(newname)=="try-error") next  # check if error persists

  introdat$TPLindex[ind_introdat] <- newname$Plant.Name.Index
  if (newname$Plant.Name.Index) introdat$AccName[ind_introdat] <- "TPL"
  if (!newname$Plant.Name.Index & grepl(" x ",allplantspec[i])) next # hybrids sometimes falsely removed by TPL

  if (newname$New.Hybrid.marker==""){
    introdat[ind_introdat,]$NewName <- paste(newname$New.Genus,newname$New.Species)
  } else {
    introdat[ind_introdat,]$NewName <- paste(newname$New.Genus,newname$New.Hybrid.marker,newname$New.Species)
  }

  if (newname$New.Authority!="") introdat[ind_introdat,]$scientificName <- paste(introdat[ind_introdat,]$NewName,newname$New.Authority)
  if (newname$Family!="") introdat[ind_introdat,]$Family <- newname$Family
  
  introdat[ind_introdat,]$NewName <- gsub(" \\(.*?\\)","",introdat[ind_introdat,]$NewName) # remove brackets
  introdat[ind_introdat,]$NewName <- gsub(" NA","",introdat[ind_introdat,]$NewName)

  # fwrite(introdat,"Data/Introdat_inTPL25445.gz")
}

introdat$scientificName <- str_trim(introdat$scientificName) # trim leading and trailing whitespace
introdat$NewName <- gsub("  "," ",introdat$NewName,perl=TRUE)

# write.table(introdat,"Data/IntroData_afterTPL.csv",sep=";")
fwrite(introdat,file.path(directory, "Outputs", "IntroData_afterTPL.gz"))

# introdat <- fread("Data/IntroData_afterTPL.gz")
# introdat <- read.table("Data/IntroData_afterTPL.csv",stringsAsFactors=F)

# backup <- introdat
# # introdat <- backupq
# introdat <- introdat[1:100000,]


# # remove subspecies etc ##################################################################
# introdat$NewName <- str_trim(introdat$NewName)
# introdat$NewName <- gsub(" subsp\\..*$","",introdat$NewName) # remove subspecies
# introdat$NewName <- gsub(" ssp\\..*$","",introdat$NewName) # remove subspecies
# introdat$NewName <- gsub(" spp\\..*$","",introdat$NewName) # remove subspecies
# introdat$NewName <- gsub(" subsp\\..*$","",introdat$NewName) # remove subspecies
# introdat$NewName <- gsub(" subssp\\..*$","",introdat$NewName) # remove subspecies
# introdat$NewName <- gsub(" subspp\\..*$","",introdat$NewName) # remove subspecies
# introdat$NewName <- gsub(" var\\. .*$","",introdat$NewName) # remove subspecies
# introdat$NewName <- gsub(" cf\\. .*$","",introdat$NewName) # remove subspecies
# introdat$NewName <- gsub(" f\\. .*$","",introdat$NewName) # remove subspecies
# 
# introdat$NewName <- gsub(" aff\\.","",introdat$NewName) #
# introdat$NewName <- gsub(" kl\\. "," ",introdat$NewName) #
# introdat$NewName <- gsub(" c\\. "," ",introdat$NewName) #
# introdat$NewName <- gsub(" cf\\. "," ",introdat$NewName) #
# introdat$NewName <- gsub(" cf "," ",introdat$NewName) #
# introdat$NewName <- gsub(" v\\. "," ",introdat$NewName) #
# introdat$NewName <- gsub(" \\? "," ",introdat$NewName) #
# introdat$NewName <- gsub(" s\\.l\\.","",introdat$NewName) #
# introdat$NewName <- gsub(" group","",introdat$NewName) #
# introdat$NewName <- gsub(" Group","",introdat$NewName) #
# introdat$NewName <- gsub(" sensu lato","",introdat$NewName) #

## check if only genus exists
# genusonly_split <- strsplit(introdat$NewName," ",fixed=TRUE)
# lengths <- unlist(lapply(genusonly_split,length))
lengths <- str_count(introdat$NewName, "\\S+")
introdat <- introdat[lengths!=1,]



introdat <- subset(introdat,!grepl("sp\\.| kl\\.",introdat$NewName,perl=TRUE))

## wrong entries in TPL
introdat$NewName[introdat$NewName=="× Crataegomespilus NA NA"] <- "Crataegomespilus gillotii"
introdat$NewName[introdat$NewName=="Epilobium novae-civitatis x Smejkal"] <- "Epilobium x novae-civitatis"
introdat$NewName[introdat$NewName=="Amsinckia tesselata"] <- "Amsinckia tessellata"
introdat$NewName[introdat$NewName=="Aristida longespica"] <- "Aristida longispica"
introdat$NewName[introdat$NewName=="Berberis hybrido-gagnepainii"] <- "Berberis x hybrido-gagnepainii"
introdat$NewName[introdat$NewName=="Solanum sarachoides"] <- "Solanum sarrachoides"

introdat$NewName <- gsub("hunter-gully","Hunter-Gully",introdat$NewName,perl=TRUE)
introdat$NewName <- gsub("gully","Gully",introdat$NewName,perl=TRUE)
introdat$NewName <- gsub("mawsons-blue","Mawsons-Blue",introdat$NewName,perl=TRUE)
introdat$NewName <- gsub("blue","Blue",introdat$NewName,perl=TRUE)
introdat$NewName <- gsub("herbstfreude","Herbstfreude",introdat$NewName,perl=TRUE)
introdat$NewName <- gsub("dorothy-perkins","Dorothy-Perkins",introdat$NewName,perl=TRUE)
introdat$NewName <- gsub("madame-plantier","Madame-Plantier",introdat$NewName,perl=TRUE)
introdat$NewName <- gsub("sunshine","Sunshine",introdat$NewName,perl=TRUE)
introdat$NewName <- gsub("purple","Purple",introdat$NewName,perl=TRUE)

introdat$NewName <- gsub("× ","x ",introdat$NewName,perl=TRUE)



## misspellings ##########################################
GloNAF_names <- file.path(directory, "Supporting_data", "GloNAF_names_2015.csv")
Correct_plants <- read.table(GloNAF_names,sep=",",stringsAsFactors = F)[,1]
plantnames_noTPLmatch <- unique(subset(introdat,LifeForm=="Vascular plants" & TPLindex!=TRUE)$NewName)

levDist_noTPLmatch <- adist(plantnames_noTPLmatch,Correct_plants)
minLevDist <- apply(levDist_noTPLmatch,1,min)
allplantnames_noTPLmatch <- matrix(NA,nr=length(plantnames_noTPLmatch),nc=20)
allplantnames_noTPLmatch[,1] <- plantnames_noTPLmatch

for (i in 1:length(plantnames_noTPLmatch)){
  if (minLevDist[i]>2) next # correct only names with a difference of max 2 characters
  nams <- Correct_plants[levDist_noTPLmatch[i,]==minLevDist[i]]
  allplantnames_noTPLmatch[i,2] <- minLevDist[i]
  allplantnames_noTPLmatch[i,1:length(nams)+2] <- nams
}
write.table(allplantnames_noTPLmatch, file.path(directory, "Outputs","PlantNamesCorrection.csv"),quote=F,sep=";",row.names=F)
# # which(Correct_plants=="Tilia japonica")



## remove years + authors and add them to author column
ind1 <- (gregexpr("[0-9]",introdat$NewName))
ind2 <- which(unlist(lapply(ind1,function(s) length(s)>1)))
for (i in 1:length(ind2)){
  name <- introdat$NewName[ind2[i]]
  name_split <- strsplit(name," ",fixed=TRUE)[[1]]
  if (grepl("[A-Z]",name_split[1]) & grepl("[A-Z]",name_split[2])) next # special case for insects
  nsplit <- length(name_split)
  introdat$NewName[ind2[i]] <- paste(name_split[1],name_split[2])
  introdat$scientificName[ind2[i]] <- paste(name_split[1],name_split[2],name_split[3:nsplit],collapse=" ")
}



## remove years + authors and add them to author column, now the special cases for insects
ind1 <- (gregexpr("[0-9]",introdat$NewName))
ind2 <- which(unlist(lapply(ind1,function(s) length(s)>1)))
for (i in 1:length(ind2)){
  name <- introdat$NewName[ind2[i]]
  name_split <- strsplit(name," ",fixed=TRUE)[[1]]
  if (!(grepl("[A-Z]",name_split[1]) & grepl("[A-Z]",name_split[2]))) next # special case for insects
  nsplit <- length(name_split)
  introdat$NewName[ind2[i]] <- paste(name_split[1],name_split[2],name_split[3])
  introdat$scientificName[ind2[i]] <- paste(name_split[1],name_split[2],name_split[3],name_split[4:nsplit],collapse=" ")
}



## check non-plant names with global taxonomy databases, only GBIF worked well.... ####################################
introdat <- subset(introdat,!(NewName==" " | NewName==""))


introdat$Phylum <- NA
introdat$confidence <- NA
allspecies <- sort(unique(introdat$NewName))
GBIFSpeciesLowConfidence <- c()

# allspecies <- sort(unique(introdat$NewName[introdat$AccName!="TPL"])) # do not check (but TPL does not provide taxonomic tree!)
for (i in 1:length(allspecies)){#[30720:length(nonplants)]
  if (i%%100==0) print(round(i/length(allspecies)*100,2))
  
  ind_introdat <- which(introdat$NewName==allspecies[i])
  db_all <- name_backbone_verbose(allspecies[i])
  # db_all <- name_backbone_verbose("Parentucellia latifolia")
  
  db <- db_all[["data"]]
  alternatives <- db_all$alternatives
  if (dim(db)[1]==0 | db$matchType=="NONE") next # | (all(colnames(db)!="species") & db$kingdom!="Plantae") 
  if (any(db$status=="ACCEPTED" & db$matchType=="EXACT" & db$rank=="SPECIES") & any(colnames(db)=="species")){ # select only accepted names and exact matches
    introdat$NewName[ind_introdat] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$species[1]
    introdat$AccName[ind_introdat] <- "GBIF"
    introdat$scientificName[ind_introdat]  <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$scientificName[1]
    if ("family"%in%colnames(db)) introdat$Family[ind_introdat] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$family[1]
    if ("class"%in%colnames(db))  introdat$Class[ind_introdat]  <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$class[1]
    if ("order"%in%colnames(db))  introdat$Order[ind_introdat]  <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$order[1]
    if ("phylum"%in%colnames(db)) introdat$Phylum[ind_introdat] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$phylum[1]
    introdat$confidence[ind_introdat] <- db$confidence
  } else if (any(max(db$confidence)>=90 & any(colnames(db)=="species"))){  # if name matches are not exact, select name with close match
    introdat$NewName[ind_introdat] <- db[db$confidence>=90,]$species[1] # plant names provided by ThePlantList
    introdat$AccName[ind_introdat] <- "GBIF"
    if (db$status!="SYNONYM") introdat$scientificName[ind_introdat]  <- db[db$confidence>=90,]$scientificName[1] # if synonym, scientificname reports name of synonym
    if ("family"%in%colnames(db) & all(unique(introdat$LifeForm[ind_introdat]) != "Vascular plants")) introdat$Family[ind_introdat] <- db[db$confidence>=90,]$family[1]
    if ("class"%in%colnames(db))  introdat$Class[ind_introdat]  <- db[db$confidence>=90,]$class[1]
    if ("order"%in%colnames(db))  introdat$Order[ind_introdat]  <- db[db$confidence>=90,]$order[1]
    if ("phylum"%in%colnames(db)) introdat$Phylum[ind_introdat] <- db[db$confidence>=90,]$phylum[1]
    introdat$confidence[ind_introdat] <- db[db$confidence>=90,]$confidence
  } else if (db$rank=="GENUS" & any(max(db$confidence)>=90) & any(colnames(db)=="kingdom")){ # just add taxonomic information for plants
    if (db$kingdom=="Plantae"){
      if ("family"%in%colnames(db))  introdat$Family[ind_introdat]  <- db[db$confidence>=90,]$family[1]
      if ("class"%in%colnames(db))  introdat$Class[ind_introdat]  <- db[db$confidence>=90,]$class[1]
      if ("order"%in%colnames(db))  introdat$Order[ind_introdat]  <- db[db$confidence>=90,]$order[1]
      if ("phylum"%in%colnames(db)) introdat$Phylum[ind_introdat] <- db[db$confidence>=90,]$phylum[1]
      introdat$confidence[ind_introdat] <- db[db$confidence>=90,]$confidence
    }
  } else if (dim(alternatives)[1]>0){ # if all previous not match, check for alternative names (synonyms)
    if (max(alternatives$confidence)>=90  & any(alternatives$rank=="SPECIES")){
      introdat$NewName[ind_introdat] <- alternatives[which.max(alternatives$confidence),]$species # plant names provided by ThePlantList
      introdat$AccName[ind_introdat] <- "GBIF"
      introdat$scientificName[ind_introdat]  <- alternatives[which.max(alternatives$confidence),]$scientificName
      if ("family"%in%colnames(db)) introdat$Family[ind_introdat] <- alternatives[which.max(alternatives$confidence),]$family
      if ("class"%in%colnames(db))  introdat$Class[ind_introdat]  <- alternatives[which.max(alternatives$confidence),]$class
      if ("order"%in%colnames(db))  introdat$Order[ind_introdat]  <- alternatives[which.max(alternatives$confidence),]$order
      if ("phylum"%in%colnames(db)) introdat$Phylum[ind_introdat] <- alternatives[which.max(alternatives$confidence),]$phylum
      introdat$confidence[ind_introdat] <- db[db$confidence>=90,]$confidence
    }
  }
  ## store species names with low confidence
  if (!all(is.na(introdat$confidence[ind_introdat]))){
    if (introdat$confidence[ind_introdat][1]<94){
      GBIFSpeciesLowConfidence <- c(GBIFSpeciesLowConfidence,allspecies[i])
      print(allspecies[i]) # check matches
    } 
  }
}

# introdat_taxtree <- introdat[,c("NewName","AccName","scientificName","Family","Class","Order","Phylum","LifeForm","confidence",)]

write.table(GBIFSpeciesLowConfidence, file.path(directory, "Outputs","GBIFSpeciesLowConfidence.csv"))
write.table(introdat,file.path(directory, "Outputs","IntroData_afterGBIF.csv"))
# introdat <- read.table("Data/IntroData_afterGBIF.csv",stringsAsFactors = F)

introdat$NewName[is.na(introdat$NewName)] <- introdat$OrigName[is.na(introdat$NewName)] # unsure entry in GBIF

# introdat$scientificName[is.na(introdat$scientificName)] <- introdat$NewName[is.na(introdat$scientificName)]

### remove some strange entries
introdat <- subset(introdat,!grepl(" cf\\.$",introdat$NewName))
introdat$NewName <- gsub(" e\\.","",introdat$NewName)


# ### remove inconsistencies etc. ############################################## 
ind <- grepl(" \\(.*?\\)",introdat$NewName) & !introdat$LifeForm%in%c("Viruses","Insects","Vascular plants","Bacteria and protozoans")
introdat$NewName[ind] <- gsub(" \\(.*?\\)","",introdat$NewName[ind]) # remove brackets from molluscs and other invertebrates

spl <- strsplit(introdat$NewName," ",fixed=TRUE)
# ind1 <- which(unlist(lapply(spl,length))>3 & introdat$LifeForm%in%c("Vascular plants") & grepl(" x ",introdat$NewName))#"Mammals","Reptiles","Crustaceans","Algae","Amphibians","Fishes","Molluscs","Birds"
# introdat$NewName[ind1] <- paste(lapply(strsplit(introdat$NewName[ind1]," "),"[[",1),lapply(strsplit(introdat$NewName[ind1]," "),"[[",3))
# ind1 <- which(unlist(lapply(spl,length))>2 & introdat$LifeForm%in%c("Crustaceans") & grepl("\\(",introdat$NewName))#"Mammals","Reptiles","Crustaceans","Algae","Amphibians","Fishes","Molluscs","Birds"
# introdat$NewName[ind1] <- paste(lapply(strsplit(introdat$NewName[ind1]," "),"[[",1),lapply(strsplit(introdat$NewName[ind1]," "),"[[",3))
ind1 <- which(unlist(lapply(spl,length))>2 & introdat$LifeForm%in%c("Mammals","Reptiles","Algae","Amphibians","Fishes","Molluscs","Birds")) # remove subspecies
introdat$NewName[ind1] <- paste(lapply(strsplit(introdat$NewName[ind1]," ",fixed=TRUE),"[[",1),lapply(strsplit(introdat$NewName[ind1]," ",fixed=TRUE),"[[",2))
# introdat$NewName <- gsub("\\( ","\\(",introdat$NewName) # one entry with an extra space...

spl <- strsplit(introdat$NewName," ",fixed=TRUE)
ind2 <- which(unlist(lapply(spl,length))>2 & introdat$LifeForm=="Insects")
ind3 <- grep("^\\([A-Z]",unlist(lapply(spl[ind2],"[[",2)))
introdat$NewName[ind2][ind3] <- paste(lapply(spl[ind2][ind3],"[[",1),lapply(spl[ind2][ind3],"[[",3))

spl <- strsplit(introdat$NewName," ",fixed=TRUE)
ind2 <- which(unlist(lapply(spl,length))>2 & introdat$LifeForm=="Insects" & !grepl(" nr ",introdat$NewName))
introdat$NewName[ind2] <- paste(lapply(spl[ind2],"[[",1),lapply(spl[ind2],"[[",2))

introdat$NewName <- gsub(" \\(.*?\\).*?$","",introdat$NewName) # remove brackets and after everything thereafter
introdat$NewName <- gsub(" .*?\\.$","",introdat$NewName) # remove endings with dot (eg " poss.")
ind <- grep(" .*?\\.$",introdat$NewName)

spl <- strsplit(introdat$NewName," ",fixed=TRUE) 
introdat <- introdat[!unlist(lapply(spl,length))==1,] # remove genus-only entries 

introdat <- subset(introdat,!grepl("\\?",NewName,perl=TRUE))

ind <- introdat$LifeForm==""
sort(table(introdat[ind,]$Order))

## re-check life forms using taxonomic information #####################

introdat$LifeForm[introdat$Class=="Mammalia"] <- "Mammals"
introdat$LifeForm[introdat$Class=="Aves"] <- "Birds"
introdat$LifeForm[introdat$Class%in%c("Cephalaspidomorphi","Actinopterygii","Elasmobranchii","Sarcopterygii")] <- "Fishes"
introdat$LifeForm[introdat$Order%in%c("Carcharhiniformes","Heterodontiformes","Perciformes","Cypriniformes","Salmoniformes","Siluriformes","Cyprinodontiformes","Characiformes","Acipenseriformes","Tetraodontiformes","Scorpaeniformes","Clupeiformes","Mugiliformes","Anguilliformes","Osteoglossiformes","Beloniformes","Atheriniformes","Syngnathiformes","Pleuronectiformes","Esociformes","Beryciformes","Osmeriformes","Aulopiformes","Gadiformes","Synbranchiformes","Gasterosteiformes","Gonorynchiformes","Zeiformes","Notacanthiformes","Lophiiformes","Elopiformes","Polypteriformes","Petromyzontiformes","Lepisosteiformes","Ceratodontiformes")] <- "Fishes"
introdat$LifeForm[introdat$Class=="Reptilia"] <- "Reptiles"
introdat$LifeForm[introdat$Order%in%c("Squamata","Testudines")] <- "Reptiles"
introdat$LifeForm[introdat$Class=="Amphibia"] <- "Amphibians"
introdat$LifeForm[introdat$Class=="Insecta"] <- "Insects"
introdat$LifeForm[introdat$Class=="Arachnida"] <- "Spiders"
introdat$LifeForm[introdat$Class%in%c("Branchiopoda","Maxillopoda","Ostracoda","Malacostraca","Hexanauplia")] <- "Crustaceans"
introdat$LifeForm[introdat$Order%in%c("Calanoida","Cyclopoida","Harpacticoida","Monstrilloida","Siphonostomatoida")] <- "Crustaceans"
introdat$LifeForm[introdat$Class%in%c("Haplosporea")] <- "Bacteria and protozoans"
introdat$LifeForm[introdat$Class%in%c("Ascidiacea")] <- "Invertebrates (excl. Arthropods, Molluscs)"
introdat$LifeForm[introdat$Class%in%c("Diplopoda","Entognatha","Pycnogonida","Chilopoda","Euchelicerata")] <- "Arthropods p.p. (Myriapods, Diplopods etc.)"
introdat$LifeForm[introdat$Order%in%c("Poduromorpha","Entomobryomorpha","Symphypleona","Xiphosurida",
                                      "Neelipleona","Protura","Diplura")] <- "Arthropods p.p. (Myriapods, Diplopods etc.)"
introdat$LifeForm[introdat$Class%in%c("Monocots","Eudicots")] <- "Vascular plants"

introdat$LifeForm[introdat$Phylum=="Mollusca"] <- "Molluscs"
introdat$LifeForm[introdat$Phylum=="Bryozoa"] <- "Bryozoa"
introdat$LifeForm[introdat$Phylum%in%c("Tracheophyta")] <- "Vascular plants"
introdat$LifeForm[introdat$Phylum%in%c("Bryophyta","Anthocerotophyta","Marchantiophyta")] <- "Bryophytes"
introdat$LifeForm[introdat$Phylum%in%c("Rhodophyta","Chlorophyta","Charophyta","Cryptophyta","Euglenozoa","Haptophyta","Foraminifera","Ciliophora","Ochrophyta","Myzozoa","Cercozoa")] <- "Algae"
introdat$LifeForm[introdat$Phylum%in%c("Ascomycota","Chytridiomycota","Basidiomycota","Microsporidia","Oomycota","Zygomycota")] <- "Fungi"
introdat$LifeForm[introdat$Phylum%in%c("Actinobacteria","Chlamydiae","Cyanobacteria","Firmicutes","Proteobacteria")] <- "Bacteria and protozoans"
introdat$LifeForm[introdat$Phylum%in%c("Echinodermata","Nematoda","Sipuncula","Chaetognatha","Acanthocephala","Ascidiacea","Cnidaria","Ctenophora","Platyhelminthes","Annelida","Porifera","Entoprocta")] <- "Invertebrates (excl. Arthropods, Molluscs)"
introdat$LifeForm[introdat$Order%in%c("Crassiclitellata")] <- "Invertebrates (excl. Arthropods, Molluscs)"
introdat$LifeForm[introdat$Family%in%c("Elminiidae")] <- "Crustaceans"



## not recognised by GBIF
introdat[introdat$GenusSpecies=="Ophiosaurus apodus",]$LifeForm <- "Reptiles"
introdat[introdat$GenusSpecies=="Rana esculenta",]$LifeForm <- "Amphibians"
introdat$LifeForm[introdat$LifeForm=="Hemmicriptophytes"] <- "Vascular plants"
introdat$LifeForm[introdat$NewName=="Axionicis insignis"] <- "Insects"
introdat$LifeForm[introdat$NewName=="Mallambyx raddei"] <- "Insects"
introdat$LifeForm[introdat$NewName=="Cheiracus sulcatus"] <- "Spiders"
# introdat[grep("Salvelinus forntinalis",introdat$NewName),] 



## invasion status ####################################################
### Birds
GAVIA <- file.path(directory, "Supporting_data", "GAVIA_first_records.csv")
birds_est <- read.table(GAVIA,sep=";",header=T,stringsAsFactors = F) # add another information
introdat[introdat$Source=="GAVIA",]$PresentStatus <- "casual"
introdat[introdat$Source=="GAVIA" & introdat$GenusSpecies%in%birds_est$Binomial,]$PresentStatus <- "established"

introdat <- subset(introdat,PresentStatus!="Cryptogenic")

IS_grouping <- file.path(directory, "Supporting_data", "Invasion_status_grouping.csv")
invstat <- read.table(IS_grouping,sep=";",header=T,stringsAsFactors=F)
invstat[,1] <- gsub("â€¦","…",invstat[,1])
introdat$PresentStatus <- gsub("^\\s+|\\s+$", "",introdat$PresentStatus) # trim leading and trailing whitespace

for (i in 1:dim(invstat)[1]){
  introdat$PresentStatus[introdat$PresentStatus==invstat[i,1]] <- invstat[i,4]
}
introdat <- subset(introdat,PresentStatus!="##delete")
introdat$PresentStatus[introdat$PresentStatus=="##missing entry"] <- ""


# out_status <- sort(table(paste(introdat$PresentStatus_orig,"_",introdat$PresentStatus,sep="")))
# lf <- strsplit(rownames(out_status),"_")
# lf2 <- do.call("rbind",lf)
# lf3 <- cbind(lf2,as.numeric(out_status))
# # write.table(lf3,"Data/InvasionStatusGrouping.csv",quote=F,sep=";",row.names=F,col.names=F)





#### check duplicates for insects in USACanada + USA + Canada #################################
ind_USAinsects <- which(introdat$LifeForm=="Insects" & grepl("USACanada|United States",introdat$Country))

# table(introdat$Country[ind_USAinsects])
dupl_insectsUSA <- duplicated(introdat$NewName[ind_USAinsects])
rm_i <- vector()
for (i in unique(introdat$NewName[ind_USAinsects][dupl_insectsUSA])){
  ind <- which(introdat$NewName[ind_USAinsects]==i)
  
  if (any(grepl("USACanada",introdat$Country[ind_USAinsects][ind]))){
    rm_i <- c(rm_i,which(introdat$NewName==i & introdat$Country=="USACanada"))
  }
}
if (length(rm_i)>0){
  introdat <- introdat[-rm_i,]
}

##################

ind_Canadainsects <- which(introdat$LifeForm=="Insects" & grepl("USACanada|Canada",introdat$Country))

# table(introdat$Country[ind_Canadainsects])
dupl_insectsCanada <- duplicated(introdat$NewName[ind_Canadainsects])
rm_i <- vector()
for (i in unique(introdat$NewName[ind_Canadainsects][dupl_insectsCanada])){
  ind <- which(introdat$NewName[ind_Canadainsects]==i)
  
  if (any(grepl("USACanada",introdat$Country[ind_Canadainsects][ind]))){
    rm_i <- c(rm_i,which(introdat$NewName==i & introdat$Country=="USACanada"))
  }
}
if (length(rm_i)>0){
  introdat <- introdat[-rm_i,]
}
#################################################################################


## remove duplicates: species in a country ###################################################

introdat <- subset(introdat,!(LifeForm=="Insects" & Source=="CABI ISC" & Country=="New Zealand")) # Data are considered of poor quality according to Eckehard Brockerhoff


dupl <- duplicated(introdat[,c("NewName","Country")])
spec_dupl <- unique(introdat[dupl,c("NewName","Country")])
################## store duplicates ##################################################
# ind_dupl_all <- vector() # export duplicates 
# for (i in 1:dim(spec_dupl)[1]){#Hippophae rhamnoides
#   ind_dupl <- which(introdat$NewName==spec_dupl[i,1] & introdat$Country==spec_dupl[i,2])
#   ind_dupl_all <- c(ind_dupl_all,ind_dupl)
# }
# sub <- introdat[ind_dupl_all,]
# 
# dupl_sub <- duplicated(sub[,c("NewName","Country","FirstRecord")])
# spec_dupl_sub <- unique(sub[dupl_sub,c("NewName","Country","FirstRecord")])
# ind_dupl_sub <- vector() # export duplicates 
# for (i in 1:dim(spec_dupl_sub)[1]){
#   ind_sub <- which(sub$NewName==spec_dupl_sub[i,1] & sub$Country==spec_dupl_sub[i,2])
#   ind_dupl_sub <- c(ind_dupl_sub,ind_sub)
# }
# # write.table(sub[-ind_dupl_sub,],"Data/InvRecords_Duplicates.csv",sep=";",row.names=F)
########################################################################################
# which(spec_dupl[,1]=="Bergenia crassifolia" & spec_dupl[,2]=="United Kingdom")


rm_inds <- vector()
xx <- 0
for (i in 1:dim(spec_dupl)[1]){#Hippophae rhamnoides, 
  rm_i <- vector()
  
  ind_dupl <- which(introdat$NewName==spec_dupl[i,1] & introdat$Country==spec_dupl[i,2])
  orig_len <- length(ind_dupl)
  
  ## select oldest from the same source
  dupl_source <- duplicated(introdat[ind_dupl,]$Source)
  if (any(dupl_source)){ # check for duplicates
    unique_dupl_source <- unique(introdat[ind_dupl,]$Source[dupl_source]) # identify duplicated databases
    rm_dupl <- vector()
    for (j in 1:length(unique_dupl_source)){ # for loop over duplicated database entries
      min_dupl <- min(introdat[ind_dupl,][introdat[ind_dupl,]$Source==unique_dupl_source[j],]$FirstRecord,na.rm=T) # select minimum
      ind_min <- which(introdat[ind_dupl,]$Source==unique_dupl_source[j] & introdat[ind_dupl,]$FirstRecord==min_dupl) # identify position of minimum
      keep <- ind_dupl[ind_min][1] # keep first entry of minimum
      ind_source <- ind_dupl[which(introdat[ind_dupl,]$Source==unique_dupl_source[j])]
      removes <- which(ind_dupl%in%ind_source[ind_source!=keep]) # identify other entries
      rm_dupl <- c(rm_dupl,removes) # add position of entries to be removed
    }
    rm_i <- c(rm_i,ind_dupl[rm_dupl]) # add position of youngest entries of duplicated entries from the same database to be removed
    ind_dupl <- ind_dupl[-rm_dupl] # remove entries
  }
  
  if (length(ind_dupl)>1){ # check for additional duplicates; ######### Was ist mit Quelle "???" ?????????????????????????????????
    gooddata <- grepl("GAVIA|Global alien spiders database|Brockerhoff|Verloove|Medveck|Charles Darwin Foundation|Celesti|Rossinelli|Tokarska|Arianoutsou|Rabitsch|Fuentes|Schaeffer|Hulme|Pyšek |Maroyi|Roy H|Franz Essl|Piero Genovesi|An Abridged|alien flora of Albania|Castro et al|César Capinha|Franz_additions|Ingolf Kühn|CAB International, Wallingford|Lazkov G A|Reynolds|Sykes et al|Wasowicz P|Wester in Tunison|Wu et al|\\?\\?\\?",introdat[ind_dupl,]$Source)
    if (any(gooddata)){
      rm_i <- c(rm_i,ind_dupl[!gooddata])
      ind_dupl <- ind_dupl[gooddata]
      still_dupl <- grepl("GAVIA|Global alien spiders database|Brockerhoff|Verloove|Medveck|Charles Darwin Foundation|Celesti|Rossinelli|Tokarska|Arianoutsou|Rabitsch|Fuentes|Schaeffer|Hulme|Pyšek |Maroyi|Roy H|Franz Essl|Piero Genovesi|An Abridged|alien flora of Albania|Castro et al|César Capinha|Franz_additions|Ingolf Kühn|CAB International, Wallingford|Lazkov G A|Reynolds|Sykes et al|Wasowicz P|Wester in Tunison|Wu et al|\\?\\?\\?",introdat[ind_dupl,]$Source)
      if (length(which(still_dupl))>1){ # selected oldest entry from gooddata
        ind_min <- which.min(introdat[ind_dupl,][still_dupl,]$FirstRecord)
        rm_i <- unique(c(rm_i,ind_dupl[still_dupl][-ind_min]))
      }
    }
    # if (any(introdat[ind_dupl,]$Source=="Long (book) via Sven Bacher") & any(introdat[ind_dupl,]$Source!="Long (book) via Sven Bacher")){
    #   long <- which(introdat[ind_dupl,]$Source=="Long (book) via Sven Bacher") # Source "Long" is uncertain; remove if better data are available
    #   rm_i <- unique(c(rm_i,ind_dupl[long]))
    # }
    if (any(introdat[ind_dupl,]$Source=="GISD") & any(grepl("Lavoie",introdat[ind_dupl,]$Source))){
      ind_GL <- grepl("GISD|Lavoie",introdat[ind_dupl,]$Source)#%in%c("DAISIE","GISD")
      Lav <- grepl("Lavoie",introdat$Source[ind_dupl][ind_GL])
      rm_i <- unique(c(rm_i,ind_dupl[ind_GL][Lav]))
    }
    if (any(introdat[ind_dupl,]$Source=="DAISIE") & any(introdat[ind_dupl,]$Source=="GISD")){
      ind_DG <- introdat[ind_dupl,]$Source%in%c("DAISIE","GISD")
      DG <- which.min(introdat[ind_dupl,]$FirstRecord[introdat[ind_dupl,]$Source%in%c("DAISIE","GISD")]) # Source "Long" is uncertain; remove if better data are available
      rm_i <- unique(c(rm_i,ind_dupl[ind_DG][-DG]))
    }
    if (any(introdat[ind_dupl,]$Source%in%c("GISD","DAISIE")) & any(introdat[ind_dupl,]$Source=="DAISIE (updated by Helen Roy)")){
      ind_DH <- introdat[ind_dupl,]$Source%in%c("GISD","DAISIE","DAISIE (updated by Helen Roy)")
      DH <- which(introdat[ind_dupl,]$Source=="DAISIE (updated by Helen Roy)")
      rm_i <- unique(c(rm_i,ind_dupl[ind_DH][-DH]))
    }
    if (any(introdat[ind_dupl,]$Source%in%c("GISD","DAISIE")) & any(introdat[ind_dupl,]$Source=="DAISIE (Jan Pergl)")){
      ind_DH <- introdat[ind_dupl,]$Source%in%c("GISD","DAISIE","DAISIE (Jan Pergl)")
      DH <- which(introdat[ind_dupl,]$Source=="DAISIE (Jan Pergl)")
      rm_i <- unique(c(rm_i,ind_dupl[ind_DH][-DH]))
    }
    # check whether it works!!! 
    if (any(grepl("Permission",introdat[ind_dupl,]$DataUsage))){ # check whether entry without permission exists
      ind_PM <- grepl("Permission",introdat[ind_dupl,]$DataUsage) # select the entry with permission to be removed
      # DH <- which(introdat[ind_dupl,]$Source=="DAISIE (Jan Pergl)")
      rm_i <- unique(c(rm_i,ind_dupl[ind_PM]))
    }
    if (length(rm_i)!=(orig_len-1)){ # still duplicates left? select oldest entry
      ind_min <- which.min(introdat[ind_dupl,]$FirstRecord)
      rm_i <- unique(c(rm_i,ind_dupl[-ind_min]))
    }
  }
  rm_inds <- unique(c(rm_inds,rm_i))
}  
speccount_dupl <- paste(spec_dupl[,1],"_",spec_dupl[,2],sep="")
introrm_dupl <- paste(introdat$NewName[rm_inds],"_",introdat$Country[rm_inds],sep="")
if (any(!introrm_dupl%in%speccount_dupl)) print("Wrong duplicated entries removed!")

if (length(rm_inds)!=0){
  introdat <- introdat[-rm_inds,]
}


## pathway #########################################################
introdat$Pathway <- gsub("^\\s+|\\s+$", "",introdat$Pathway) # trim leading and trailing whitespace
introdat$Pathway <- gsub("D","intentional",introdat$Pathway)
introdat$Pathway <- gsub("A","unintentional",introdat$Pathway)
introdat$Pathway <- gsub("del","intentional",introdat$Pathway)
introdat$Pathway <- gsub("acc","unintentional",introdat$Pathway)
introdat$Pathway <- gsub("Intentional","intentional",introdat$Pathway)
introdat$Pathway <- gsub("Unintentional","unintentional",introdat$Pathway)
introdat$Pathway[introdat$Pathway=="U"] <- "unintentional"
introdat$Pathway[introdat$Pathway=="Ui"] <- "unintentional"
introdat$Pathway[introdat$Pathway%in%c("Cr","Fi","Fd","Fr","Gr","Md","O","Ti")] <- "intentional"
introdat$Pathway[grep("Fd, ",introdat$Pathway)] <- "intentional"
introdat$Pathway[grep("Cr, ",introdat$Pathway)] <- "intentional"
introdat$Pathway[grep("O, ",introdat$Pathway)] <- "intentional"


## extract habitat type from WoRMS for marine species ##########################

uni_species <- unique(introdat$NewName)
introdat$Habitat_marine <- NA
introdat$Habitat_freshwater <- NA
introdat$Habitat_terrestrial <- NA

introdat$Phylum[introdat$Phylum==""] <- NA
introdat$Order[introdat$Order==""] <- NA
introdat$Class[introdat$Class==""] <- NA
introdat$Family[introdat$Family==""] <- NA

entry <- data.frame()
for (i in 1:length(uni_species)){#
  
  if (i%%100==0) print(round(i/length(uni_species)*100,2))
  
  
  # try({entry <- checklist(scientificname=uni_species[i])},silent=T) # OBIS
  entry <- try(wm_records_name(name = uni_species[i],marine=F),silent=T) # WoRMS
  
  if (class(entry)[1]=="try-error") next  # if database did not provide info, jump to next species
  if (all(is.na(entry$scientificname))) next # empty entries....
  if (nrow(entry)>1) next # avoid homonyms, which cannot be resolved here
  
  if (any(entry$status=="accepted")){
    
    ind_worms <- which(entry$status=="accepted")[1]
    
    ind_spec <- introdat$NewName==uni_species[i]
    
    if ("family"%in%colnames(entry) & all(is.na(introdat$Family[ind_spec]))) introdat$Family[ind_spec] <- entry$family[ind_worms]
    if ("class"%in%colnames(entry) & all(is.na(introdat$Class[ind_spec])))  introdat$Class[ind_spec]  <- entry$class[ind_worms]
    if ("order"%in%colnames(entry) & all(is.na(introdat$Order[ind_spec])))  introdat$Order[ind_spec]  <- entry$order[ind_worms]
    if ("phylum"%in%colnames(entry) & all(is.na(introdat$Phylum[ind_spec]))) introdat$Phylum[ind_spec] <- entry$phylum[ind_worms]

    if ("isMarine"%in%colnames(entry)) introdat$Habitat_marine[ind_spec] <- entry$isMarine[ind_worms]
    if ("isFreshwater"%in%colnames(entry)) introdat$Habitat_freshwater[ind_spec] <- entry$isFreshwater[ind_worms]
    if ("isTerrestrial"%in%colnames(entry)) introdat$Habitat_terrestrial[ind_spec] <- entry$isTerrestrial[ind_worms]
  }
}
write.table(introdat,file.path(directory, "Outputs","IntroData_afterWoRMS.csv"))
# introdat <- read.table("Data/IntroData_afterWoRMS.csv")

## island/mainland ###########################################################
introdat[grep("sland",introdat$Country),]$Island <- "yes"

Islands_list <- file.path(directory, "Supporting_data", "Islands_list.csv")
islands <- read.table(Islands_list,stringsAsFactors = F)[,1]
# islands <- c("Azores","Ascension","Madeira","Cyprus","Crete","Corse","Ireland","New Zealand","Iceland","French Polynesia","Taiwan","New Caledonia","Sicily",
#              "Reunion","Sardinia","Malta","Saint Helena","Mauritius","Martinique","Indonesia","Saint Pierre and Miquelon","Barbados",
#              "Seychelles","Jamaica","Trinidad and Tobago","Bahamas","Bermuda","Guadeloupe","Northern Ireland","Dominican Republic","Guam",
#              "Netherlands Antilles","British Indian Ocean Territory","Vanuatu","Papua New Guinea","Malaysia","Philippines","Cape Verde",
#              "Haiti","Madagascar","Antigua and Barbuda","Sri Lanka","Palau","Saint Vincent and The Grenadines","Iles Subantarctiques",
#              "Greenland","Aruba","Micronesia, Federated States of","French Southern Territories","Balearic Islands","British Isles","Cuba","Cyprus",
#              "Wallis and Futuna","South Georgia","South Georgia and The South Sandwich Islands","Kiribati","Maldives","Mayotte","Sulawesi",
#              "Terres Australes Et Antarctiques Françaises  - Iles Eparses","Montserrat"," Macao","Galapagos","Grenada","American Samoa",
#              "Chagos Archipelago","Comoros","Curacao","Dominica","Fiji","Japan","Hong Kong","Mustique","Nauru","Niue","Saint Kitts and Nevis",
#              "Saint Lucia","Saint Vincent and the Grenadines","Samoa","Sao Tome and Principe","Sint Maarten","Svalbard and Jan Mayen","Tahiti","Tasmania",
#              "Tokelau","Tonga","Tristan da Cunha","Turks and Caicos","Tuvalu","United Kingdom","Zanzibar Island")


introdat[introdat$Country%in%islands,]$Island <- "yes"

# names(table(subset(introdat,Island!="yes")$Country))

introdat$Island <- gsub("^\\s+|\\s+$", "",introdat$Island) # trim leading and trailing whitespace



introdat <- subset(introdat,introdat$NewName!="") # remove empty entries
introdat <- subset(introdat,FirstRecord<=as.integer(format(Sys.Date(), "%Y"))) # remove first records before today
introdat <- subset(introdat,!Country=="Not Found")
introdat <- subset(introdat,LifeForm!="unknown")


# introdat <- merge(introdat,all_sources,by="Source",all=T)


colnames(introdat)[colnames(introdat)=="NewName"] <- "TaxonName"
colnames(introdat)[colnames(introdat)=="Country"] <- "Region"

## "DataQuality" removed because not consistent
introdat <- introdat[,c("Region","LifeForm","TaxonName","OrigName","scientificName","PresentStatus","FirstRecord","FirstRecord_orig",
                        "Family","Order","Class","Phylum","AccName","Source","DataUsage","Pathway",
                        "Island","Habitat_terrestrial","Habitat_freshwater","Habitat_marine")]

introdat$Habitat_freshwater[is.na(introdat$Habitat_freshwater)] <- ""
introdat$Habitat_terrestrial[is.na(introdat$Habitat_terrestrial)] <- ""
introdat$Habitat_marine[is.na(introdat$Habitat_marine)] <- ""

introdat$TaxonName <- gsub("\"","'",introdat$TaxonName)
introdat$scientificName <- gsub("\"","'",introdat$scientificName)
introdat$OrigName <- gsub("\"","'",introdat$OrigName)

oo <- order(introdat$Region,introdat$LifeForm,introdat$TaxonName)
introdat <- introdat[oo,]

introdat <- introdat[!is.na(introdat$TaxonName),]


## output #####################################################################
today <- Sys.Date() #get today's date
filename <- paste0("First_records_database_", today, ".csv") # Build the filename with the date
write.table(introdat,file.path(directory, "Outputs", filename),sep=";",row.names=F)#[,c(dim(introdat)[2],1:(dim(introdat)[2]-1))]
# write.table(introdat[,c("NewName","LifeForm","LifeForm2","Country","PresentStatus","FirstRecord","FirstRecord_orig","DataQuality","Pathway","Origin","Island","Source","DataUsage")],"Data/DataFirstRecord_140817.csv",sep=";",row.names=F,na="")
# write.table(introdat[,c(dim(introdat)[2],1:(dim(introdat)[2]-1))],"Data/IntroDat_260116_AllTimeSpans.csv",sep=";",row.names=F,na="")
# write.table(introdat[,c("NewName","LifeForm","LifeForm2","Country","PresentStatus","FirstRecord","FirstRecord_orig","DataQuality","Pathway","Origin","Island","Source")],"Data/DataFirstRecord_260116_AllTimeSpans.csv",sep=";",row.names=F,na="")
