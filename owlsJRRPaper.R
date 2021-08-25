#Analysis of colorimetric, climatic, and geographic data for Great Horned Owls in the Museum of Southwest Biology (University of New Mexico, Albuquerque, NM)
#This script created the analysis in the 2021 Journal of Raptor Research paper: ECOGEOGRAPHY OF PLUMAGE PIGMENTATION IN GREAT HORNED OWLS
#
#Owl Map
library(ggmap)
library(raster)
library(geosphere)
library(maps)
library(mapdata)
library(rgdal)
library(usmap)
library(ggplot2)
library(colorspace)
library(sf)
library(ade4)
install_github("vqv/ggbiplot")
library(devtools)
library(ggbiplot)
#library(ggsn) load this later to avoid errors/conflicts with raster
#nonparametric stats
library(mgcv)
#AICc calculation and tabluation package
library(AICcmodavg)
#model averaging package
library(MuMIn)
#Multimodal distribution test
library(diptest)
#VIF calculations
library(car)
#Import climate data from local directory (data from WorldClim 2.0, 30 second resolution)
setwd("/Users/ibis/Documents/Maps/wc2large")
#temperature
climt<-raster("wc2.0_bio_30s_01.tif")
#precipitation
climp<-raster("wc2.0_bio_30s_12.tif")
#Import solar data (data from WorldClim 2.0, 30 second resolution) for months of January to August
setwd("/Users/ibis/Documents/Maps/wc2srad")
srad1<-raster("wc2.1_30s_srad_01.tif")
srad2<-raster("wc2.1_30s_srad_02.tif")
srad3<-raster("wc2.1_30s_srad_03.tif")
srad4<-raster("wc2.1_30s_srad_04.tif")
srad5<-raster("wc2.1_30s_srad_05.tif")
srad6<-raster("wc2.1_30s_srad_06.tif")
srad7<-raster("wc2.1_30s_srad_07.tif")
srad8<-raster("wc2.1_30s_srad_08.tif")
srad9<-raster("wc2.1_30s_srad_09.tif")
srad10<-raster("wc2.1_30s_srad_10.tif")
srad11<-raster("wc2.1_30s_srad_11.tif")
srad12<-raster("wc2.1_30s_srad_12.tif")
#Import ARCTOS/colorimetry owl csv
setwd("/Users/ibis/Documents/owls")
#read in ARCTOS spreadsheet of all GHOW specimens
ghow<-read.csv("ghow.csv")
#read in remeasurement data
remeasure<-read.csv("remeasure.csv")
#convert columns to numeric values
ghow$alt<-as.numeric(ghow$alt)
ghow$lon<-as.numeric(ghow$lon)
ghow$lat<-as.numeric(ghow$lat)
#assign species column to ghow dataset
ghow$spec<-""
#fill species column with criteria-specific species and subspecies names:
#"pin" = pinorumn, "pal" = pallescens, "hyb"= hybrib, "oth"=other.  
#The "spec" column is filled with logical (true/false) values by default to differentiate hybrids (which have an "x" in their sciname column)
ghow$spec<-grepl("x",ghow$sciname)	
"pinorum"->ghow$spec[which(ghow$sciname=="Bubo virginianus pinorum")]
"pallescens"->ghow$spec[which(ghow$sciname=="Bubo virginianus pallescens")]
"hybrid"->ghow$spec[which(ghow$spec=="TRUE")]
"other"->ghow$spec[which(ghow$spec=="FALSE")]

#General summary statistics
summary(ghow)
#number of specimens with location data:
length(ghow$lat[is.na(ghow$lat)==FALSE])

#create colorimeter variables
#calculate hue and chroma
#toe
ghow$toeh1<-""
ghow$toeh2<-""
ghow$toeh3<-""
ghow$toeh4<-""
ghow$toeh5<-""
ghow$toeh6<-""
ghow$toeh1<-atan(ghow$toeb1/ghow$toea1)
ghow$toeh2<-atan(ghow$toeb2/ghow$toea2)
ghow$toeh3<-atan(ghow$toeb3/ghow$toea3)
ghow$toeh4<-atan(ghow$toeb4/ghow$toea4)
ghow$toeh5<-atan(ghow$toeb5/ghow$toea5)
ghow$toeh6<-atan(ghow$toeb6/ghow$toea6)
ghow$toec1<-""
ghow$toec2<-""
ghow$toec3<-""
ghow$toec4<-""
ghow$toec5<-""
ghow$toec6<-""
ghow$toec1<-(ghow$toea1^2+ghow$toeb1^2)^.5
ghow$toec2<-(ghow$toea2^2+ghow$toeb2^2)^.5
ghow$toec3<-(ghow$toea3^2+ghow$toeb3^2)^.5
ghow$toec4<-(ghow$toea4^2+ghow$toeb4^2)^.5
ghow$toec5<-(ghow$toea5^2+ghow$toeb5^2)^.5
ghow$toec6<-(ghow$toea6^2+ghow$toeb6^2)^.5
#back
ghow$backh1<-""
ghow$backh2<-""
ghow$backh3<-""
ghow$backh4<-""
ghow$backh5<-""
ghow$backh6<-""
ghow$backh1<-atan(ghow$backb1/ghow$backa1)
ghow$backh2<-atan(ghow$backb2/ghow$backa2)
ghow$backh3<-atan(ghow$backb3/ghow$backa3)
ghow$backh4<-atan(ghow$backb4/ghow$backa4)
ghow$backh5<-atan(ghow$backb5/ghow$backa5)
ghow$backh6<-atan(ghow$backb6/ghow$backa6)
ghow$backc1<-""
ghow$backc2<-""
ghow$backc3<-""
ghow$backc4<-""
ghow$backc5<-""
ghow$backc6<-""
ghow$backc1<-(ghow$backa1^2+ghow$backb1^2)^.5
ghow$backc2<-(ghow$backa2^2+ghow$backb2^2)^.5
ghow$backc3<-(ghow$backa3^2+ghow$backb3^2)^.5
ghow$backc4<-(ghow$backa4^2+ghow$backb4^2)^.5
ghow$backc5<-(ghow$backa5^2+ghow$backb5^2)^.5
ghow$backc6<-(ghow$backa6^2+ghow$backb6^2)^.5
#light rectrix
ghow$rlighth1<-""
ghow$rlighth2<-""
ghow$rlighth3<-""
ghow$rlight4<-""
ghow$rlighth5<-""
ghow$rlighth6<-""
ghow$rlighth1<-atan(ghow$rlightb1/ghow$rlighta1)
ghow$rlighth2<-atan(ghow$rlightb2/ghow$rlighta2)
ghow$rlighth3<-atan(ghow$rlightb3/ghow$rlighta3)
ghow$rlighth4<-atan(ghow$rlightb4/ghow$rlighta4)
ghow$rlighth5<-atan(ghow$rlightb5/ghow$rlighta5)
ghow$rlighth6<-atan(ghow$rlightb6/ghow$rlighta6)
ghow$rlightc1<-""
ghow$rlightc2<-""
ghow$rlightc3<-""
ghow$rlightc4<-""
ghow$rlightc5<-""
ghow$rlightc6<-""
ghow$rlightc1<-(ghow$rlighta1^2+ghow$rlightb1^2)^.5
ghow$rlightc2<-(ghow$rlighta2^2+ghow$rlightb2^2)^.5
ghow$rlightc3<-(ghow$rlighta3^2+ghow$rlightb3^2)^.5
ghow$rlightc4<-(ghow$rlighta4^2+ghow$rlightb4^2)^.5
ghow$rlightc5<-(ghow$rlighta5^2+ghow$rlightb5^2)^.5
ghow$rlightc6<-(ghow$rlighta6^2+ghow$rlightb6^2)^.5
#dark rectrix
ghow$rdarkh1<-""
ghow$rdarkh2<-""
ghow$rdarkh3<-""
ghow$rdarkh4<-""
ghow$rdarkh5<-""
ghow$rdarkh6<-""
ghow$rdarkh1<-atan(ghow$rdarkb1/ghow$rdarka1)
ghow$rdarkh2<-atan(ghow$rdarkb2/ghow$rdarka2)
ghow$rdarkh3<-atan(ghow$rdarkb3/ghow$rdarka3)
ghow$rdarkh4<-atan(ghow$rdarkb4/ghow$rdarka4)
ghow$rdarkh5<-atan(ghow$rdarkb5/ghow$rdarka5)
ghow$rdarkh6<-atan(ghow$rdarkb6/ghow$rdarka6)
ghow$rdarkc1<-""
ghow$rdarkc2<-""
ghow$rdarkc3<-""
ghow$rdarkc4<-""
ghow$rdarkc5<-""
ghow$rdarkc6<-""
ghow$rdarkc1<-(ghow$rdarka1^2+ghow$rdarkb1^2)^.5
ghow$rdarkc2<-(ghow$rdarka2^2+ghow$rdarkb2^2)^.5
ghow$rdarkc3<-(ghow$rdarka3^2+ghow$rdarkb3^2)^.5
ghow$rdarkc4<-(ghow$rdarka4^2+ghow$rdarkb4^2)^.5
ghow$rdarkc5<-(ghow$rdarka5^2+ghow$rdarkb5^2)^.5
ghow$rdarkc6<-(ghow$rdarka6^2+ghow$rdarkb6^2)^.5
#create average columns
ghow$toelight<-""
ghow$toelight<-(ghow$toel1+ghow$toel2+ghow$toel3+ghow$toel4+ghow$toel5+ghow$toel6)/6
ghow$avgtoea<-""
ghow$avgtoea<-(ghow$toea1+ghow$toea2+ghow$toea3+ghow$toea4+ghow$toea5+ghow$toea6)/6
ghow$avgtoeb<-""
ghow$avgtoeb<-(ghow$toeb1+ghow$toeb2+ghow$toeb3+ghow$toeb4+ghow$toeb5+ghow$toeb6)/6
#create average toe hue column
ghow$toehue<-""
ghow$toehue<-atan(ghow$avgtoeb/ghow$avgtoea)
#create average toe chroma column
ghow$toechroma<-""
ghow$toechroma<-(ghow$avgtoea^2+ghow$avgtoeb^2)^.5
#create average toe melanin column
ghow$toemel<-""
ghow$toemel<-(ghow$toem1+ghow$toem2+ghow$toem3+ghow$toem4+ghow$toem5+ghow$toem6)/6
#create average back columns
ghow$backlight<-""
ghow$backlight<-(ghow$backl1+ghow$backl2+ghow$backl3+ghow$backl4+ghow$backl5+ghow$backl6)/6
ghow$avgbacka<-""
ghow$avgbacka<-(ghow$backa1+ghow$backa2+ghow$backa3+ghow$backa4+ghow$backa5+ghow$backa6)/6
ghow$avgbackb<-""
ghow$avgbackb<-(ghow$backb1+ghow$backb2+ghow$backb3+ghow$backb4+ghow$backb5+ghow$backb6)/6
ghow$backhue<-""
ghow$backhue<-atan(ghow$avgbackb/ghow$avgbacka)
ghow$backchroma<-""
ghow$backchroma<-(ghow$avgbacka^2+ghow$avgbackb^2)^.5
#create average back melanin column
ghow$backmel<-""
ghow$backmel<-(ghow$backm1+ghow$backm2+ghow$backm3+ghow$backm4+ghow$backm5+ghow$backm6)/6
#create average rectrix columns
#rectrix lightness
ghow$rdarklight<-""
ghow$rdarklight <-(ghow$rdarkm1+ghow$rdarkm2+ghow$rdarkm3+ghow$rdarkm4+ghow$rdarkm5+ghow$rdarkm6)/6
ghow$rlightlight<-""
ghow$rlightlight <-(ghow$rlightm1+ghow$rlightm2+ghow$rlightm3+ghow$rlightm4+ghow$rlightm5+ghow$rlightm6)/6
#rectrix hue
ghow$avgrlighta<-""
ghow$avgrlighta<-(ghow$rlighta1+ghow$rlighta2+ghow$rlighta3+ghow$rlighta4+ghow$rlighta5+ghow$rlighta6)/6
ghow$avgrlightb<-""
ghow$avgrlightb<-(ghow$rlightb1+ghow$rlightb2+ghow$rlightb3+ghow$rlightb4+ghow$rlightb5+ghow$rlightb6)/6
ghow$rlighthue<-""
ghow$rlighthue<-atan(ghow$avgrlightb/ghow$avgrlighta)
ghow$avgrdarka<-""
ghow$avgrdarka<-(ghow$rdarka1+ghow$rdarka2+ghow$rdarka3+ghow$rdarka4+ghow$rdarka5+ghow$rdarka6)/6
ghow$avgrdarkb<-""
ghow$avgrdarkb<-(ghow$rdarkb1+ghow$rdarkb2+ghow$rdarkb3+ghow$rdarkb4+ghow$rdarkb5+ghow$rdarkb6)/6
ghow$rdarkhue<-""
ghow$rdarkhue<-atan(ghow$avgrdarkb/ghow$avgrdarka)
#rectrix chroma
ghow$rlightchroma<-""
ghow$rlightchroma<-(ghow$avgrlighta^2+ghow$avgrlightb^2)^.5
ghow$rdarkchroma<-""
ghow$rdarkchroma<-(ghow$avgrdarka^2+ghow$avgrdarkb^2)^.5

#Create breeding season column based on dates from  Dickerman's Birds of New Mexico entry (exclude birds between Sep-December when dispersal is reported to occur)
ghow$month<-""
ghow$month<-months(as.Date(ghow$date,"%m/%d/%Y"))
ghow$breed<-0
ghow$breed[ghow$month %in% c("January","February","March","April","May","June","July","August")==TRUE]<-1
#define owls found in the Southwest region (between 26 and 38 and 96 and 115 W)
ghow$sw<-0
ghow$sw[ghow$lat>=26 & ghow$lat<=38 & ghow$lon<=-96 & ghow$lon>=-115]<-1

#assign binary sex column
ghow$fsex<-as.numeric("")
ghow$fsex[ghow$sex=="M"]<-0
ghow$fsex[ghow$sex=="F"]<-1
ghow$fsex[ghow$sex=="unk"]<-0.5

#subset data for only copmplete cases of lightness and color data
allowls<-ghow[complete.cases(ghow[,c(162:173)])==TRUE,]
#select only Southwestern owls
allowls<-allowls[which(allowls$sw==1),]

#select only adult birds
owlspca<-allowls[which(allowls$age%in%c("adult")),]

#standardize predictive variables for multiple regressions
owlspca$stlat<-(owlspca$lat-mean(owlspca$lat))/sd(owlspca$lat)
owlspca$stalt<-(owlspca$alt-mean(owlspca$alt))/sd(owlspca$alt)
owlspca$stfsex<-(owlspca$fsex-mean(owlspca$fsex))/sd(owlspca$fsex)

#pare down for breeding season/presumedly resident owls only
owlspcab<-owlspca[owlspca$breed==1,]
#standardize predictive variables for multiple regressions
owlspcab$stlat<-(owlspcab$lat-mean(owlspcab$lat))/sd(owlspcab$lat)
owlspcab$stalt<-(owlspcab$alt-mean(owlspcab$alt))/sd(owlspcab$alt)
owlspcab$stfsex<-(owlspcab$fsex-mean(owlspcab$fsex))/sd(owlspcab$fsex)

#Plot color space of breeding owl specimens
#create consolidated overall color adding toe and back values
ghowoLAB<-cbind(owlspcab$toelight+owlspcab$backlight,owlspcab$avgtoea+owlspcab$avgbacka,owlspcab$avgtoeb+owlspcab$avgbackb)
#Convert to LAB values
olab<-LAB(ghowoLAB)
plot(olab)
#Assign hex colors for easier R plotting
ghowcolors<-hex(olab,fixup=TRUE)
#For grayscale
ghowgrays<-desaturate(ghowcolors)
#Create data frame containing owl color values
ghowmapcolor<-as.data.frame(cbind(lat=owlspcab$lat,lon=owlspcab$lon,alt=owlspcab$alt,lightness=owlspcab$toelight+owlspcab$backlight,ghowcolors))
ghowmapcolor<-ghowmapcolor[is.na(ghowmapcolor$ghowcolors)=="FALSE",]
ghowmapcolor$lon<-as.numeric(paste(ghowmapcolor$lon))
ghowmapcolor$lat<-as.numeric(paste(ghowmapcolor$lat))
ghowmapcolor$alt<-as.numeric(paste(ghowmapcolor$alt))
ghowmapcolor$lightness<-as.numeric(paste(ghowmapcolor$lightness))
ghowmapcolor$ghowgrays<-""
ghowmapcolor$ghowgrays<-desaturate(ghowcolors)
plot(ghowmapcolor$lightness~ghowmapcolor$alt,type="n")
points(ghowmapcolor$lightness~ghowmapcolor$alt,col=ghowcolors,pch=20,cex=2)
colors<-paste(ghowmapcolor$ghowcolors)

#Added elevation column
plot(owlspca$lightw~owlspca$alt)
boxplot(owlspca$alt~owlspca$ssp)
barplot(sort(owlspca$alt[owlspca$ssp=="pal"]))
#linear regressions
altlm<-lm(owlspca$backlight+owlspca$toelight~owlspca$alt)
summary(altlm)
#ggplot of regression data
allGHOWlightness<-ggplot(data=owlspca,aes(x=alt,y=toelight+backlight))+
  geom_point(col="blue")+
  geom_smooth(method=lm,se=TRUE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Altitude(m)")+
  ylab("Foot+Back Lightness(L*)")+
  labs(title="All Adult GHOW Lightness by Altitude")+
  annotate("text",x=1600,y=6,label=paste("Rsq.=",round(R,2),", ","n=",length(owlspca$alt)," p=",pvalue,sep=""))

#regression in all owls controlling for latitude
altlatlm<-lm(owlspca$backlight+owlspca$toelight~owlspca$alt+owlspca$lat)
summary(altlatlm)
#make dummy variables for plot
aowlpred<-with(owlspca,data.frame(lat=mean(lat),alt=sort(owlspca$alt)))
aowlpred$altpred<-predict(altlatlm,newdata=aowlpred,type="response")
aowlse<-predict(altlatlm,newdata=aowlpred,type="response",se.fit=T,interval="prediction")
aowlpred$se<-aowlse$se.fit
plot(aowlpred$altpred~aowlpred$alt,type="n")
lines(lowess(aowlpred$altpred~aowlpred$alt))
PI<-predict(altlatlm,interval="prediction")
aowlpred<-cbind(aowlpred,PI)
alllightnesslatplot<-ggplot(data=aowlpred,aes(x=alt,y=altpred))+
  geom_point(col="blue")+
  geom_line(aes(y=lwr),col="red",linetype="dashed")+
  geom_line(aes(y=upr),col="red",linetype="dashed")+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Altitude(m)")+
  ylab("Foot+Back Lightness(L*)")+
  labs(title="All Adult GHOW Lightness by Altitude Controlling for Latitude")+
  annotate("text",x=1600,y=6,label=paste("Rsq.=",round(R,2),", ","n=",length(aowlpred$alt)," p=",pvalue,sep=""))

#look at breeding season owls only
lmbreedalt<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$alt)
summary(lmbreedalt)
breedGHOWlightness<-ggplot(data=owlspcab,aes(x=alt,y=toelight+backlight))+
  geom_point(col="blue")+
  geom_smooth(method=lm,se=TRUE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Altitude(m)")+
  ylab("Foot+Back Lightness (L*)")+
  labs(title="Breeding Season Adult GHOW Lightness by Altitude")+
  annotate("text",x=1600,y=6,label=paste("Rsq.=",round(R,2),", ","n=",length(owlspcab$alt)," p=",pvalue,sep=""))

#toe and foot lightness by altitude controlled by latitude (standardized as of 6/3/2019)
#unstandardized(raw)
lmbreedaltr<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$alt)
summary(lmbreedaltr)
lmbreedlaltr<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$alt+owlspcab$lat)
summary(lmbreedlaltr)
#standardized
lmbreedlalt<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$stalt)
summary(lmbreedalt)
lmbreedlalt<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$stalt+owlspcab$stlat)
summary(lmbreedlalt)
#add sex as control (standardized as of 6/3/2019) Include standardized and non-standardized sets of data in paper tables
#not standardized
lmbreedlaltsex<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$alt+owlspcab$lat+owlspcab$fsex)
summary(lmbreedlaltsex)
#standardized
stlmbreedlaltsex<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$stalt+owlspcab$stlat+owlspcab$stfsex)
summary(stlmbreedlaltsex)
plot(lmbreedlaltsex)
AIC(altlm)
AIC(lmbreedlalt)
AIC(lmbreedlaltsex)
#lower AIC when including sex
#make dummy variables for plot
owlpred<-with(owlspcab,data.frame(lat=mean(lat),fsex=mean(as.numeric(fsex)),alt=sort(owlspcab$alt)))
owlpred$altpred<-predict(lmbreedlaltsex,newdata=owlpred,type="response")
owlse<-predict(lmbreedlaltsex,newdata=owlpred,type="response",se.fit=T,interval="prediction")
owlpred$se<-owlse$se.fit
PI<-predict(lmbreedlaltsex,interval="prediction")
owlpred<-cbind(owlpred,PI)
lmowlpred<-lm(owlpred$fit~owlpred$alt)
summary(lmowlpred)
#Paper Figure: prediction plot on lightness by elevation controlling for latitude and sex
breedlightnesslatplot<-ggplot(data=owlpred,aes(x=alt,y=altpred))+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  geom_point(col=ghowcolors)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Elevation(m)")+
  ylab("Foot+Back Lightness(L*)")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
    )
#Grayscale Paper Figure 
breedlightnesslatplotG<-ggplot(data=owlpred,aes(x=alt,y=altpred))+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  geom_point(col=ghowgrays)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Elevation(m)")+
  ylab("Foot+Back Lightness(L*)")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )

#prediction plot for latitude
#make dummy variables for plot
owlpred<-with(owlspcab,data.frame(alt=mean(alt),fsex=mean(as.numeric(fsex)),lat=sort(owlspcab$lat)))
owlpred$latpred<-predict(lmbreedlaltsex,newdata=owlpred,type="response")
owlse<-predict(lmbreedlaltsex,newdata=owlpred,type="response",se.fit=T,interval="prediction")
owlpred$se<-owlse$se.fit
PI<-predict(lmbreedlaltsex,interval="prediction")
owlpred<-cbind(owlpred,PI)
#Figure for paper
breedlightnesslatelevplot<-ggplot(data=owlpred,aes(x=lat,y=latpred))+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  geom_point(col=ghowcolors)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Latitude(°)")+
  ylab("Foot+Back Lightness(L*)")+
  #removed title for paper publication
  #labs(title="Resident Adult GHOW Lightness by Altitude Controlling for Latitude and Sex")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
#Grayscale Figure for paper
breedlightnesslatelevplotG<-ggplot(data=owlpred,aes(x=lat,y=latpred))+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  geom_point(col=ghowgrays)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Latitude(°)")+
  ylab("Foot+Back Lightness(L*)")+
  #removed title for paper publication
  #labs(title="Resident Adult GHOW Lightness by Altitude Controlling for Latitude and Sex")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
#climate data 
#Create location data frame for breeding season owls
owllocs<-data.frame(GUID=owlspcab$GUID,lon=owlspcab$lon,lat=owlspcab$lat)
owllocs<-na.omit(owllocs)
owllocs<-owllocs[order(owllocs$GUID,owllocs$lon),]
#convert dataframe to raster object
coordinates(owllocs)<-c("lon","lat")
#extract temp and precipitation data from World Climate (worldclim.org) Data raster objects 
owltemp<-extract(x=climt,y=owllocs)
owlprecip<-extract(x=climp,y=owllocs)
#extract solar radiation data from World Climate (worldclim.org) Data raster objects, and average them across the year
owlsrad1<-extract(x=srad1,y=owllocs)
owlsrad2<-extract(x=srad2,y=owllocs)
owlsrad3<-extract(x=srad3,y=owllocs)
owlsrad4<-extract(x=srad4,y=owllocs)
owlsrad5<-extract(x=srad5,y=owllocs)
owlsrad6<-extract(x=srad6,y=owllocs)
owlsrad7<-extract(x=srad7,y=owllocs)
owlsrad8<-extract(x=srad8,y=owllocs)
owlsrad9<-extract(x=srad9,y=owllocs)
owlsrad10<-extract(x=srad10,y=owllocs)
owlsrad11<-extract(x=srad11,y=owllocs)
owlsrad12<-extract(x=srad12,y=owllocs)
mowlsrad<-(owlsrad1+owlsrad2+owlsrad3+owlsrad4+owlsrad5+owlsrad6+owlsrad7+owlsrad8+owlsrad9+owlsrad10+owlsrad11+owlsrad12)/12
  
  #convert owl raster object back to data frame
owllocs<-as.data.frame(owllocs)
#add climate data to location data
owlclim<-cbind(owllocs,temp=owltemp,precip=owlprecip,srad=mowlsrad)
#grab lightness, altitude, etc data from ghow
#owllight<-data.frame(GUID=ghow$GUID,alt=ghow$alt,toelight=ghow$toelight,backlight=ghow$backlight,toemel=ghow$toemel,backmel=ghow$backmel,toechroma=ghow$toechroma,backchroma=ghow$backchroma,avgtoea=ghow$avgtoea,avgbacka=ghow$avgbacka, breed=ghow$breed,age=ghow$age,fsex=ghow$fsex,ssp=ghow$ssp,sw=ghow$sw)
owllight<-data.frame(GUID=owlspcab$GUID,alt=owlspcab$alt,toelight=owlspcab$toelight,backlight=owlspcab$backlight,toemel=owlspcab$toemel,backmel=owlspcab$backmel,toechroma=owlspcab$toechroma,backchroma=owlspcab$backchroma,avgtoea=owlspcab$avgtoea,avgbacka=owlspcab$avgbacka, breed=owlspcab$breed,age=owlspcab$age,fsex=owlspcab$fsex,ssp=owlspcab$ssp,sw=owlspcab$sw)
#merge lightness data with climate data data
lightclim<-merge(owlclim,owllight,by="GUID",all.x=TRUE)
#subset for breeding adult owls
lightclimb<-lightclim[lightclim$breed==1 & lightclim$age=="adult" & lightclim$sw==1,]
#remove unknown sex
lightclimb<-lightclimb[lightclimb$fsex!=0.5,]

#Some climate regressions
#standardize variables
lightclimb$stalt<-(lightclimb$alt-mean(lightclimb$alt))/sd(lightclimb$alt)
lightclimb$stlat<-(lightclimb$lat-mean(lightclimb$lat))/sd(lightclimb$lat)
lightclimb$stfsex<-(lightclimb$fsex-mean(lightclimb$fsex))/sd(lightclimb$fsex)
lightclimb$stprecip<-(lightclimb$precip-mean(lightclimb$precip))/sd(lightclimb$precip)
lightclimb$sttemp<-(lightclimb$temp-mean(lightclimb$temp))/sd(lightclimb$temp)
#added solar radiation 11/10/2020
lightclimb$stsrad<-(lightclimb$srad-mean(lightclimb$srad))/sd(lightclimb$srad)
#added longitude 03/11/2021
lightclimb$stlon<-(lightclimb$lon-mean(lightclimb$lon))/sd(lightclimb$lon)
#run linear models
climalt<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$alt+lightclimb$lat+lightclimb$temp+lightclimb$precip)
summary(climalt)
climalt1<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$alt+lightclimb$lat+lightclimb$temp)
summary(climalt1)
climalt2<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$alt+lightclimb$lat)
summary(climalt2)
climalt3<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$alt)
summary(climalt3)
climalt4<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$alt+lightclimb$lat+lightclimb$precip)
summary(climalt4)
#Specific models to look at lightness relationship with precipitation and temperature.
#see if alt and precip are related (yes they are)
cor.test(lightclimb$alt,lightclimb$precip)
lmcorr<-lm(lightclimb$alt~lightclimb$precip)
summary(lmcorr)
#temperature and altitude correlation (yes)
cor.test(lightclimb$alt,lightclimb$temp)
lmcorrtemp<-lm(lightclimb$alt~lightclimb$temp)
summary(lmcorrtemp)
#precipitation and latitude correlation (no)
cor.test(lightclimb$lat,lightclimb$precip)
lmcorrpreciplat<-lm(lightclimb$lat~lightclimb$precip)
summary(lmcorrpreciplat)
#temperature and latitude correlation (yes)
cor.test(lightclimb$lat,lightclimb$temp)
lmcorrtemplat<-lm(lightclimb$lat~lightclimb$temp)
summary(lmcorrtemplat)
#solar radiation correlations to latitude, temperature, and elevation 11/10/2020
#srad and latitude (almost colinear 0.68)
cor.test(lightclimb$lat,lightclimb$srad)
#srad and elevation (.37)
cor.test(lightclimb$alt,lightclimb$srad)
#srad and temperature (0.57)
cor.test(lightclimb$temp,lightclimb$srad)
#srad and precipitation (0.32)
cor.test(lightclimb$precip,lightclimb$srad)
#elevation and latitude correlation (0.543)
cor.test(lightclimb$alt,lightclimb$lat)
#model lightness based on temperature, precipitation and (as of 11/10/2020) solar radiation
templight<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$temp)
summary(templight)
preciplight<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$precip)
summary(preciplight)
sradlight<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$srad)
summary(sradlight)
#plots of these relationships
lighttempplot<-ggplot(lightclimb,aes(x=temp,y=(backlight+toelight)))+
  geom_point()+
  geom_smooth(method="lm")+
scale_color_manual(values=c("grey","black"))+
  labs(title="GHOW Lightness by Average Temperature")+
  xlab("Temperature(C°)")+
  ylab("Lightness(L*)")+
  theme(panel.background = element_rect(fill = NA, color="black"),legend.key=element_blank())
lightprecipplot<-ggplot(lightclimb,aes(x=precip,y=(backlight+toelight)))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm",se=FALSE,color="red",linetype=2)+
  scale_color_manual(values=c("grey","black"))+
  labs(title="GHOW Lightness by Average Precipitation")+
  xlab("Avg. Precipitation(cm)")+
  ylab("Lightness(L*)")+
  theme(panel.background = element_rect(fill = NA, color="black"),legend.key=element_blank())
#Solar radiation and lightness
lightsradplot<-ggplot(lightclimb,aes(x=srad,y=(backlight+toelight)))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm",se=FALSE,color="red",linetype=2)+
  scale_color_manual(values=c("grey","black"))+
  labs(title="GHOW Lightness by Average Solar Radiation")+
  xlab("Avg. Solar Radiation(kjm-2d-1)")+
  ylab("Lightness(L*)")+
  theme(panel.background = element_rect(fill = NA, color="black"),legend.key=element_blank())
#Precipitation plotted by latitude
latprecipplot<-ggplot(lightclimb,aes(x=lat,y=precip))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm",se=FALSE,color="red",linetype=2)+
  labs(title="Average Precipitation by Latitude")+
  xlab("Latitude")+
  ylab("Avg. Precipitation (cm)")+
  theme(panel.background = element_rect(fill = NA, color="black"),legend.key=element_blank())
#temperature correlates with latitude
lattempplot<-ggplot(lightclimb,aes(x=lat,y=temp))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm",se=FALSE,color="red",linetype=2)+
  labs(title="Average Temperature by Latitude")+
  xlab("Latitude")+
  ylab("Avg.Temperature (C°")+
  theme(panel.background = element_rect(fill = NA, color="black"),legend.key=element_blank())
#Combined plot of temperature and precipitation by latitude:
lattempprecipplot<-ggplot(lightclimb,aes(x=lat,y=temp),shape=)+
  geom_point(color="grey60",shape=1)+
  geom_point(aes(y=precip/15),color="black", shape=2)+
    scale_y_continuous(sec.axis = sec_axis(~ . *15,name="Precipitation(mm)"))+
  geom_smooth(aes(y=temp),method="lm",se=FALSE,color="grey60",linetype=2)+
  geom_smooth(aes(x=lat,y=precip/15),method="lm",se=FALSE,color="black",linetype=2)+
  #labs(title="Average Temperature and Total Precipitation by Latitude")+
  xlab("Latitude")+
  ylab("Avg.Temperature (C°)= o")+
  theme(
    text=element_text(family="serif"),
    panel.background = element_rect(fill = NA, color="black"),
    #legend.key=element_blank(),
    axis.title.y.left = element_text(color="grey60"),
    #axis.title.y.right =element_text(color="black"))
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.title.y.right = element_blank()
  )
#Precipitation correlates with altitude
altprecipplot<-ggplot(lightclimb,aes(x=alt,y=precip))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm",se=FALSE,color="red",linetype=2)+
  labs(title="Average Precipitation by Altitude")+
  xlab("Altitude(m)")+
  ylab("Avg. Precipitation (cm)")+
  theme(panel.background = element_rect(fill = NA, color="black"),legend.key=element_blank())

#temperature correlates with altitude
alttempplot<-ggplot(lightclimb,aes(x=alt,y=temp))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm",se=FALSE,color="red",linetype=2)+
  labs(title="Average Temperature by Altitude")+
  xlab("Altitude(m)")+
  ylab("Avg.Temperature (C°")+
  theme(panel.background = element_rect(fill = NA, color="black"),legend.key=element_blank())
#Combined plot of temperature and precipitation by elevation/altitude:
alttempprecipplot<-ggplot(lightclimb,aes(x=alt,y=temp))+
  scale_y_continuous(sec.axis = sec_axis(~ . *15,name="Precipitation(mm)=∆"))+
  geom_point(color="grey60",shape=1)+
  geom_point(aes(x=alt,y=precip/15),color="black",shape=2)+
  geom_smooth(method="lm",se=FALSE,color="grey60",linetype=2)+
  geom_smooth(aes(x=alt,y=precip/15),method="lm",se=FALSE,color="black",linetype=2)+
  xlab("Elevation(m)")+
  #ylab("Avg.Temperature (C°)")+
  theme(
    text=element_text(family="serif"),
    panel.background = element_rect(fill = NA, color="black"),
    axis.title.y.right =element_text(color="black"),
    #axis.title.y = element_text(color="grey60"),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.title.y.left = element_blank()
    )
#altitude by latitude
lataltplot<-ggplot(lightclimb,aes(x=lat,y=alt))+
  geom_point(color="grey60",shape=1)+
  geom_smooth(method="lm",se=FALSE,color="grey60",linetype=2)+
  xlab("Latitude")+
  ylab("Elevation(m)")+
  theme(
    text=element_text(family="serif"),
    panel.background = element_rect(fill = NA, color="black"),
    legend.key=element_blank())
tiff("elvbylat.tiff",width=8,height=8, units="in",res=300)
lataltplot
dev.off()
pdf("elvbylat.pdf")
lataltplot
dev.off()
#standardized variables: build up climate regressions of combined lightness as with orignal altitude plots
climatelmtemp<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$sttemp)
climatelmprecip<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stprecip)
summary(climatelmtemp)
AIC(climatelmtemp)
summary(climatelmprecip)
AIC(climatelmprecip)
climatelm1<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$sttemp+lightclimb$stprecip)
summary(climatelm1)
AIC(climatelm1)
#model with standardized temperature and latitude
climatelm2<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$sttemp+lightclimb$stlat)
summary(climatelm2)
AIC(climatelm2)
#model with standardized temperature, latitude, and sex
climatelm3<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$sttemp+lightclimb$stlat+lightclimb$stfsex)
summary(climatelm3)
AIC(climatelm3)
#Using temperature models, excluding latitude due to colinearity
climatelm4st<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$sttemp+lightclimb$stfsex)
summary(climatelm4st)
AIC(climatelm4st)
#non-standardized form
climatelm4<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$temp+lightclimb$fsex)
summary(climatelm4)
AIC(climatelm4)

#use precipitation only
climatelp1<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stprecip)
summary(climatelp1)
AIC(climatelp1)
climatelp3<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stprecip+lightclimb$stlat)
summary(climatelp3)
AIC(climatelp3)
climatelp4<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stprecip+lightclimb$stlat+lightclimb$stfsex)
#plot(climatelp4)
summary(climatelp4)
AIC(climatelp4)
#6/26/2020: Added elevation to climate models
climatelp5<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stprecip+lightclimb$stalt+lightclimb$stlat+lightclimb$stfsex)
summary(climatelp5)
AIC(climatelp5)
#6/26/2020: added interaction terms for elevation and precipitation and latitude
climatelp6<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stprecip+lightclimb$stalt+lightclimb$stprecip:lightclimb$stalt+lightclimb$stlat+lightclimb$stfsex)
summary(climatelp6)
AIC(climatelp6)
#11/11/2020 Added standardized solar radiation for revision
rad1<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stsrad)
summary(rad1)
#srad and sex
rad2<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stsrad+lightclimb$stfsex)
summary(rad2)
#srad and sex and elevation
rad3<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stsrad+lightclimb$stfsex+lightclimb$stalt)
summary(rad3)
#srad and sex and elevation and precipitation
rad4<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stsrad+lightclimb$stalt+lightclimb$stfsex+lightclimb$stprecip)
summary(rad4)
#srad and elevation
rad5<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stsrad+lightclimb$stalt)
summary(rad6)
#srad and sex and precipitation and temperature
rad6<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stsrad+lightclimb$stprecip+lightclimb$stfsex+lightclimb$sttemp)
summary(rad7)
#7/14/2020: non-standardized regression for discussion
climatelp5raw<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$precip+lightclimb$lat+lightclimb$precip+lightclimb$fsex)
summary(climatelp5raw)
AIC(climatelp5raw)
climatelp6raw<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$precip+lightclimb$alt+lightclimb$lat+lightclimb$fsex)
summary(climatelp6raw)
AIC(climatelp6raw)

#lightness by latitude
latlight<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$lat)
summary(latlight)
AIC(latlight)
#latitude plot
latplot<-ggplot(lightclimb,aes(x=lat,y=backlight+toelight))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm",se=FALSE,color="red",linetype=2)+
  labs(title="GHOW Lightness by Latitude")+
  xlab("Latitude")+
  ylab("Lightness(L*)")+
  theme(panel.background = element_rect(fill = NA, color="black"),legend.key=element_blank())

#controlling for latitude and sex shows a significant effect of precipitation on lightness
summary(climatelp4)
#but precipitation is correlated with elevation too
cor.test(lightclimb$precip,lightclimb$alt)
#t=3.99, df=99, p-value=0.0001273, cor=0.372
#plot the controlled predictions
#make dummy variables for plot
climpred<-with(lightclimb,data.frame(lat=mean(lat),fsex=mean(fsex),precip=sort(lightclimb$precip)))
climpred$precippred<-predict(climatelp4,newdata=climpred,type="response")
climpredse<-predict(climatelp4,newdata=climpred,type="response",se.fit=T,interval="prediction")
climpred$se<-climpredse$se.fit
PI<-predict(climatelp4,interval="prediction")
climpred<-cbind(climpred,PI)
#One paper figure
preciplot<-ggplot(data=climpred,aes(x=precip,y=precippred))+
  geom_point(color=ghowcolors)+
  #removed SE lines per reviewer/editor request 03/12/2021
  #geom_smooth(method=loess,se=FALSE,aes(y=lwr),col="blue",linetype="dashed")+
  #geom_smooth(method=loess,se=FALSE, aes(y=upr),col="blue",linetype="dashed")+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Mean Precipitation(mm)")+
  ylab("Foot+Back Lightness(L*)")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
#Grayscale paper figure
preciplotG<-ggplot(data=climpred,aes(x=precip,y=precippred))+
  geom_point(color=ghowgrays)+
  #removed SE lines per reviewer/editor request 03/12/2021
  #geom_smooth(method=loess,se=FALSE,aes(y=lwr),col="blue",linetype="dashed")+
  #geom_smooth(method=loess,se=FALSE, aes(y=upr),col="blue",linetype="dashed")+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Mean Precipitation(mm)")+
  ylab("Foot+Back Lightness(L*)")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
#descriptives of this linear model
summary(lm(climpred$precippred~climpred$precip))
#Temperature prediction plot paper figure
#plot the controlled predictions
#make dummy variables for plot
temppred<-with(lightclimb,data.frame(fsex=mean(fsex),temp=sort(lightclimb$temp)))
temppred$temppred<-predict(climatelm4,newdata=temppred,type="response")
temppredse<-predict(climatelm4,newdata=temppred,type="response",se.fit=T,interval="prediction")
temppred$se<-temppredse$se.fit
PI<-predict(climatelm4,interval="prediction")
temppred<-cbind(temppred,PI)
temppplot<-ggplot(data=temppred,aes(x=temp,y=temppred))+
  geom_point(color=ghowcolors)+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Mean Temperature(°C)")+
  ylab("Foot+Back Lightness(L*)")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
#Grayscale
temppplotG<-ggplot(data=temppred,aes(x=temp,y=temppred))+
  geom_point(color=ghowgrays)+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Mean Temperature(°C)")+
  ylab("Foot+Back Lightness(L*)")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
#descriptives of this linear model
summary(lm(temppred$temppred~temppred$temp))
#Solar radiation prediction plot paper figure
#plot the controlled predictions
#make dummy variables for plot
sradpred<-with(lightclimb,data.frame(fsex=mean(fsex),alt=mean(alt),srad=sort(lightclimb$srad)))
sradpred$sradpred<-predict(rad3,newdata=sradpred,type="response")
sradpredse<-predict(rad3,newdata=sradpred,type="response",se.fit=T,interval="prediction")
sradpred$se<-sradpredse$se.fit
PI<-predict(rad3,interval="prediction")
sradpred<-cbind(sradpred,PI)
sradpplot<-ggplot(data=sradpred,aes(x=srad,y=sradpred))+
  geom_point(color=ghowcolors)+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  labs(x=expression (paste("Solar Radiation(kJ/",~m^2,"/d)")))+
  ylab("Foot+Back Lightness(L*)")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
#Grayscale
sradpplotG<-ggplot(data=sradpred,aes(x=srad,y=sradpred))+
  geom_point(color=ghowgrays)+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  labs(x=expression (paste("Solar Radiation(kJ/",~m^2,"/d)")))+
  ylab("Foot+Back Lightness(L*)")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
#descriptives of predicted solar radiation linear model
summary(lm(sradpred$sradpred~sradpred$srad))

#7/14/2020: made prediction plots for the full model including elevation and precipitation for paper figure
#Make elevation prediction plot
#make dummy variables for plot
elvpred<-with(lightclimb,data.frame(precip=mean(precip),lat=mean(lat),fsex=mean(fsex),alt=sort(lightclimb$alt)))
elvpred$elvpred<-predict(climatelp5raw,newdata=elvpred,type="response")
elvpredse<-predict(climatelp5raw,newdata=elvpred,type="response",se.fit=T,interval="prediction")
elvpred$se<-elvpredse$se.fit
PI<-predict(climatelp5raw,interval="prediction")
elvpred<-cbind(elvpred,PI)
elvplot<-ggplot(data=elvpred,aes(x=alt,y=elvpred))+
  geom_point(color=ghowcolors)+
  geom_smooth(method=loess,se=FALSE,aes(y=lwr),col="blue",linetype="dashed")+
  geom_smooth(method=loess,se=FALSE, aes(y=upr),col="blue",linetype="dashed")+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Elevation(m)")+
  ylab("Foot+Back Lightness(L*)")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
#Make precipitation prediction plot
#make dummy variables for plot
precippred<-with(lightclimb,data.frame(alt=mean(alt),lat=mean(lat),fsex=mean(fsex),precip=sort(lightclimb$precip)))
precippred$precippred<-predict(climatelp5raw,newdata=precippred,type="response")
precippredse<-predict(climatelp5raw,newdata=precippred,type="response",se.fit=T,interval="prediction")
precippred$se<-precippredse$se.fit
PI<-predict(climatelp5raw,interval="prediction")
precippred<-cbind(precippred,PI)
precipplot<-ggplot(data=precippred,aes(x=precip,y=precippred))+
  geom_point(color=ghowcolors)+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Average Annual Precipitation(mm)")+
  ylab("Foot+Back Lightness(L*)")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
#export tiffs for publication
tiff("AltFig.tiff",width=8,height=8,units="in",res=300)
breedlightnesslatplot
dev.off()
pdf("AltFig.pdf")
breedlightnesslatplot
dev.off()
tiff("Climate.tiff",width=10,height=10,units="in",res=300)
grid.arrange(lattempprecipplot,alttempprecipplot,ncol=2)
dev.off()
pdf("Climate.pdf")
grid.arrange(lattempprecipplot,alttempprecipplot,ncol=2)
dev.off()
tiff("PrecipPredict.tiff",width=8,height=8,units="in",res=300)
preciplot
dev.off()
pdf("PrecipPredict.pdf")
preciplot
dev.off()
#see if there are sex differences in lightness (based on other species,males are predicted to be lighter, hence one-sided t-test)
fowls<-owlspcab[owlspcab$fsex==1,]
mowls<-owlspcab[owlspcab$fsex==0,]
boxplot(owlspcab$toelight+owlspcab$backlight~owlspcab$fsex,notch=T)
mean(fowls$toelight+fowls$backlight)
sd(fowls$toelight+fowls$backlight)
range(fowls$toelight+fowls$backlight)
mean(fowls$toelight)
sd(fowls$toelight)
range(fowls$toelight)
mean(fowls$backlight)
sd(fowls$backlight)
range(fowls$backlight)
mean(mowls$toelight+mowls$backlight)
sd(mowls$toelight+mowls$backlight)
range(mowls$toelight+mowls$backlight)
mean(mowls$toelight)
sd(mowls$toelight)
range(mowls$toelight)
mean(mowls$backlight)
sd(mowls$backlight)
range(mowls$backlight)

t.test(fowls$toelight+fowls$backlight,mowls$toelight+mowls$backlight,alternative="less")#shows significant differences as of 5/2/2019

#now look at subspecies differences
pin<-owlspcab[owlspcab$ssp=="pin",]
pal<-owlspcab[owlspcab$ssp=="pal",]
t.test(pin$toelight+pin$backlight,pal$toelight+pal$backlight,alternative="less")#shows significant differences as of 5/2/2019
mean(pin$toelight)
mean(pin$backlight)
mean(pal$toelight)
mean(pal$backlight)
sd(pin$toelight)
sd(pin$backlight)
sd(pal$toelight)
sd(pal$backlight)
#combined averages
mean(pin$toelight+pin$backlight)
sd(pin$toelight+pin$backlight)
mean(pal$toelight+pal$backlight)
sd(pal$toelight+pal$backlight)
#Table 1: model comparisons, using standardized effects of altitude, latitude, and sex
#all adult owls
lmalt<-lm(owlspca$toelight+owlspca$backlight~owlspca$stalt)
summary(lmalt)
AIC(lmalt)
altlatlm<-lm(owlspca$backlight+owlspca$toelight~owlspca$stalt+owlspca$stlat)
summary(altlatlm)
AIC(altlatlm)
altlatsexlm<-lm(owlspca$backlight+owlspca$toelight~owlspca$stalt+owlspca$stlat+owlspca$stfsex)
summary(altlatsexlm)
AIC(altlatsexlm)
#breeding adult owls
#creating null model for AIC comparisons
lmbreednull<-lm(owlspcab$toelight+owlspcab$backlight~1)
summary(lmbreednull)
AIC(lmbreednull)
AICc(lmbreednull)
#Adding latitude only model in accordance with reviews 10/15/2020
lmbreedlat<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$stlat)
summary(lmbreedlat)
AIC(lmbreedlat)
AICc(lmbreedlat)
lmbreedalt<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$stalt)
summary(lmbreedalt)
AIC(lmbreedalt)
AICc(lmbreedalt)
#toe and foot lightness by altitude controlled by latitude
lmbreedlalt<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$stalt+owlspcab$stlat)
summary(lmbreedlalt)
AIC(lmbreedlalt)
AICc(lmbreedlalt)
#add sex as control (standardized as of 6/3/2019)
stlmbreedlaltsex<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$stalt+owlspcab$stlat+owlspcab$stfsex)
summary(stlmbreedlaltsex)
summary(lmbreedlaltsex)
AIC(stlmbreedlaltsex)
AICc(stlmbreedlaltsex)
#look at regressions using mean values for lightness
lmbreedalt2<-lm((owlspcab$toelight+owlspcab$backlight)/2~owlspcab$stalt)
summary(lmbreedalt2)
AIC(lmbreedalt2)
#toe and foot lightness by altitude controlled by latitude
lmbreedlalt2<-lm((owlspcab$toelight+owlspcab$backlight)/2~owlspcab$stalt+owlspcab$stlat)
summary(lmbreedlalt2)
AIC(lmbreedlalt2)
#toe and foot lightness by altitude controlled by latitude and sex
stlmbreedlaltsex1<-lm((owlspcab$toelight+owlspcab$backlight)/2~owlspcab$stalt+owlspcab$stlat+owlspcab$stfsex)
summary(stlmbreedlaltsex1)
AIC(stlmbreedlaltsex1)

#map of resident owl locations
#Paper figure
#Modified June 2021 acording to editor reviews (added compass, scale, place labels etc.)
#need to load this library temporarily to make scale bar
library(ggsn)
#Create map of measured specimens colored by with their CIELAB colors.
breedmap<-get_stamenmap(bbox = c(left=-113.004230375,bottom=31.13981803,right=-102.666133925,top=37.20758897),zoom=7,crop=TRUE,maptype="toner",source="stamen",xlab="Longitude",ylab="Latitude")
  #qmplot(lon, lat, data = ghowmapcolor,extent="panel",xlab="Longitude",ylab="Latitude",maptype="toner")+
breedbtmap<- ggmap(breedmap)+
geom_point(data=ghowmapcolor, aes(x=lon, y=lat,fill=ghowcolors,color=ghowcolors,alpha=0.1),
           show.legend=FALSE,shape=19,size=4.0)+
  scale_fill_manual(values=ghowcolors)+
  scale_color_manual(values=ghowcolors)+
  xlab("Longitude")+
  ylab("Latitude")+
  #add north arrow
  geom_segment(arrow=arrow(length=unit(5,"mm"), type="closed", angle=40), 
               aes(x=-103.5,xend=-103.5,y=36.5,yend=36.6), color="black") +
  geom_label(aes(x=-103.5, y=36.45, label="N"),
             size=3.5, label.padding=unit(0.75,"mm"), label.r=unit(0.4,"lines")) +
  #add scale bar (requires ggsn package)
  scalebar (y.min=min(ghowmapcolor$lat),y.max=max(ghowmapcolor$lat),x.min=min(ghowmapcolor$lon),x.max=max(ghowmapcolor$lon),dist = 100, dist_unit = "km",
           transform = TRUE, model = "WGS84", st.size=4) +
  theme(
    legend.position="none"
    )
graymap<- ggmap(breedmap)+
  geom_point(data=ghowmapcolor, aes(x=lon, y=lat,fill=ghowgrays,color=ghowgrays,alpha=0.1),
             show.legend=FALSE,shape=19,size=4.0)+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_fill_manual(values=ghowgrays)+
  scale_color_manual(values=ghowgrays)+
  #add north arrow
  geom_segment(arrow=arrow(length=unit(5,"mm"), type="closed", angle=40), 
               aes(x=-103.5,xend=-103.5,y=36.5,yend=36.6), color="black") +
  geom_label(aes(x=-103.5, y=36.45, label="N"),
             size=3.5, label.padding=unit(0.75,"mm"), label.r=unit(0.4,"lines")) +
  #add scale bar (requires ggsn package)
  scalebar (y.min=min(ghowmapcolor$lat),y.max=max(ghowmapcolor$lat),x.min=min(ghowmapcolor$lon),x.max=max(ghowmapcolor$lon),dist = 100, dist_unit = "km",
            transform = TRUE, model = "WGS84", st.size=4) +
  theme(
    legend.position="none"
  )
tiff("MeasuredSpecimenMap.tiff",width=10,height=10,units="in",res=300)
breedbtmap
dev.off()
pdf("MeasuredSpecimenMap.pdf",paper="USr")
breedbtmap
dev.off()
tiff("MeasuredSpecimenMapgray.tiff",width=10,height=10,units="in",res=300)
graymap
dev.off()
pdf("MeasuredSpecimenMapgray.pdf",paper="USr")
graymap
dev.off()
#detach ggsn package
detach(package:ggsn)
#summary stats
length(ghow$GUID[ghow$sw==1 &ghow$breed==1 &ghow$age%in%c("adult","")])

write.csv(ghow,file="ghowtreat.csv")

#elevation~latitude significance:
altlatlm<-lm(owlspcab$alt~owlspcab$lat)
summary(altlatlm)
#standard error for lightness
sd(owlspcab$toelight, na.rm=TRUE) /  
  sqrt(length(owlspcab$toelight[!is.na(owlspcab$toelight)]))
#Prediction Plots Paper Figure with latitude and altitude
tiff("PredictionPlots.tiff",width=8,height=8,units="in",res=300)
grid.arrange(breedlightnesslatelevplot,breedlightnesslatplot,nrow=2)
dev.off()
pdf("PredictionPlots.pdf")
grid.arrange(breedlightnesslatelevplot,breedlightnesslatplot,nrow=2)
dev.off()
#Grayscale Prediction Plots Paper Figure with latitude and altitude
tiff("PredictionPlotsG.tiff",width=10,height=10,units="in",res=300)
grid.arrange(breedlightnesslatelevplotG,breedlightnesslatplotG,nrow=2)
dev.off()
pdf("PredictionPlotsG.pdf")
grid.arrange(breedlightnesslatelevplotG,breedlightnesslatplotG,nrow=2)
dev.off()
tiff("PrecipTempRadPredictionPlot.tiff",width=8,height=8,units="in",res=300)
grid.arrange(preciplot,temppplot, sradpplot,nrow=3)
dev.off()
pdf("PrecipTempRadPredictionPlot.pdf")
grid.arrange(preciplot,temppplot, sradpplot,nrow=3)
dev.off()
#Grayscale
tiff("PrecipTempRadPredictionPlotG.tiff",width=10,height=10,units="in",res=300)
grid.arrange(preciplotG,temppplotG, sradpplotG,nrow=3)
dev.off()
pdf("PrecipTempRadPredictionPlotG.pdf")
grid.arrange(preciplotG,temppplotG,sradpplotG,nrow=3)
dev.off()
tiff("PrecipPredictionPlot.tiff",width=8,height=8,units="in",res=300)
preciplot
dev.off()
pdf("PrecipPredictionPlot.pdf")
preciplot
dev.off()
tiff("PrecipElevPredictionPlot.tiff",width=8,height=8,units="in",res=300)
grid.arrange(breedlightnesslatplot,preciplot,nrow=2)
dev.off()
pdf("PrecipElevPredictionPlot.pdf")
grid.arrange(breedlightnesslatplot,preciplot,nrow=2)
dev.off()
breedlightnesslatelevplot
breedlightnesslatplot
grid.arrange(breedlightnesslatelevplot,breedlightnesslatplot,nrow=2)
#make table of aggregate lightness by sex and subspecies
omean<-aggregate(owlspcab$toelight+owlspcab$backlight,list(SSP=owlspcab$sciname,Sex=owlspcab$fsex),FUN="mean")
osd<-aggregate(owlspcab$toelight+owlspcab$backlight,list(SSP=owlspcab$sciname,Sex=owlspcab$fsex),FUN="sd")
orange<-aggregate(owlspcab$toelight+owlspcab$backlight,list(SSP=owlspcab$sciname,Sex=owlspcab$fsex),FUN="range")
osum<-cbind(omean,osd$x,orange$x)
write.csv(osum,file="osum.csv")

#converting models to same named dataset for comparison (the climate and geographical datasets were identical, but separately named in prior analyses)
lmbreednull<-lm(lightclimb$toelight+lightclimb$backlight~1)
lmbreedlat<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stlat)
lmbreedalt<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stalt)
lmbreedlalt<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stalt+lightclimb$stlat)
stlmbreedlaltsex<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stalt+lightclimb$stlat+lightclimb$stfsex)
lmsex<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stfsex)
#11/11/2020 added solar radiation models to the mix, excluding latitude and radiation from same models
#03/02/2021 added L~lat+sex
lmbreedlatsex<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stlat+lightclimb$stfsex)
#Adding weights and ER in accordance with reviews 10/9/2020 using AICcmodavg package
#11/11/2020 added solar radiation models to the mix, excluding latitude and radiation from same models
#removed models with temperature and latitude in same models (climatelm2,climatelm3,"L~tem+lat","L~tem+lat+sex")
#03/02/2021 added latitude and sex model to mix per reviewer's request
candidatelms<-list(lmbreedlatsex,lmbreednull,lmbreedlat,lmbreedalt,lmbreedlalt,stlmbreedlaltsex,lmsex,climatelm1,climatelm4st,climatelp1,climatelp3,climatelp4,climatelp5,rad1,rad2,rad3,rad4,rad5,rad6,climatelmtemp)
candidatenames<-c("L~lat+sex","Null","L~lat","L~elv","L~elv+lat","L~elv+lat+sex","L~sex","L~tem+pre","L~tem+sex","L~pre","L~pre+lat","L~pre+lat+sex","L~pre+elv+lat+sex","L~rad","L~rad+sex","L~rad+sex+elv","L~rad+sex+elv+pre","L~rad+elv","L~rad+sex+pre+tem","L~tem")
#dump regression outputs to text file
sink("regressionoutputs.txt")
for (c in candidatelms) print(summary(c))
sink()
table2<-aictab(candidatelms,modnames=candidatenames)
#add R-squared values
candsums<-lapply(candidatelms,summary)
candrsq<-cbind(candidatenames,lapply(candsums,function(x) c(x$adj.r.squared)))
candrsq<-as.data.frame(candrsq)
colnames(candrsq)<-c("Modnames","adjRsq")
candrsq$adjRsq<-as.numeric(candrsq$adjRsq)
candrsq$Modnames<-paste(candrsq$Modnames)
#table2<-as.data.frame(table2)
table2<-merge(table2,candrsq,by="Modnames",sort=FALSE)
write.csv(table2,file="table2.csv")
#Model VIFs (run for all models with 2+ terms)
vifcandidates<-list(lmbreedlatsex,lmbreedlalt,stlmbreedlaltsex,climatelm1,climatelm3,climatelm4st,climatelp3,climatelp4,climatelp5,rad2,rad3,rad4,rad5,rad6)
vifcandidatenames<-c("L~lat+sex","L~elv+lat","L~elv+lat+sex","L~tem+pre","L~tem+lat+sex","L~tem+sex","L~pre+lat","L~pre+lat+sex","L~pre+elv+lat+sex","L~rad+sex","L~rad+sex+elv","L~rad+sex+elv+pre","L~rad+elv","L~rad+sex+pre+tem")
#make a table of VIF values for each model we used
i<-0
sink("viftable.txt")
for (c in vifcandidates){
  i<-i+1
  print(vifcandidatenames[i])
  print(vif(c))
}
sink()
#check VIFs for all inclusive model per review's suggestion 03/11/2021
lmksink<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stlon+lightclimb$stlat+lightclimb$sttemp+lightclimb$stprecip+lightclimb$stsrad+lightclimb$stalt+lightclimb$stfsex)
summary(lmksink)
vif(lmksink)
mean(vif(lmksink))
#remove highest VIF value (temp)
lmksink1<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stlon+lightclimb$stlat+lightclimb$stprecip+lightclimb$stsrad+lightclimb$stalt+lightclimb$stfsex)
summary(lmksink1)
vif(lmksink1)
#no VIF values over 5 anymore, but remove top (lat).
lmksink2<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stlon+lightclimb$stprecip+lightclimb$stsrad+lightclimb$stalt+lightclimb$stfsex)
summary(lmksink2)
vif(lmksink2)
#no VIF values over 2 anymore, but remove top (srad) anyway
lmksink3<-lm(lightclimb$toelight+lightclimb$backlight~lightclimb$stlon+lightclimb$stprecip+lightclimb$stalt+lightclimb$stfsex)
summary(lmksink3)
vif(lmksink3)
#make VIF tables for supplement.
#model summaries
sink(file="vifselectionmodels.txt")
summary(lmksink)
summary(lmksink1)
summary(lmksink2)
summary(lmksink3)
sink()
#VIF tables
viftab<-data.frame(vif(lmksink),row.names=names(vif(lmksink)))
names(viftab)<-"vif"
viftab1<-data.frame(vif(lmksink1),row.names=names(vif(lmksink1)))
names(viftab1)<-"vif"
viftab2<-data.frame(vif(lmksink2),row.names=names(vif(lmksink2)))
names(viftab2)<-"vif"
viftab3<-data.frame(vif(lmksink3),row.names=names(vif(lmksink3)))
names(viftab3)<-"vif"
vifcsv<-rbind("L~lon+lat+temp+pre+rad+elv+sex",viftab,"L~lon+lat+pre+rad+elv+sex",viftab1,"L~lon+pre+rad+elv+sex",viftab2,"L~lon+pre+elv+sex",viftab3)
write.csv(vifcsv,file="vif.csv")
#correlation matrix
#select numeric variables
cordata<-lightclimb[c("lon","lat","temp","precip","srad","alt")]
cortable<-cor(cordata)
#correlation matrix for supplement
write.csv(cortable,file="cormatrix.csv")
#Averaging model parameters
#Model averages for top three candidate models (where delta AIC <2)
topcandidatelms<-list(stlmbreedlaltsex,climatelp4,climatelp5)
topcandidatenames<-c("L~elv+lat+sex","L~pre+lat+sex","L~pre+elv+lat+sex")
#Averaged model using top 3 models
avglms<-model.avg(topcandidatelms)
table3<-summary(avglms)
table3sum<-table3$coefmat.sub
#table for paper
write.csv(table3sum,file="table3.csv")
#Model averages for top five candidate models (where delta AIC <5, and temperature variable is captured)
topcandidatelmsb<-list(stlmbreedlaltsex,climatelp4,climatelp5,lmbreedlatsex,climatelm4st)
topcandidatenamesb<-c("L~elv+lat+sex","L~pre+lat+sex","L~pre+elv+lat+sex","lat+sex","tem+sex")
#Averaged model using top 5 models
avglmsb<-model.avg(topcandidatelmsb)
table3b<-summary(avglmsb)
table3bsum<-table3b$coefmat.sub
#table for paper
write.csv(table3bsum,file="table3b.csv")
#make prediction interval for temperature using averaged model (using same code from previous draft Figure 2 above, but excluding errors)
#make dummy variables for plot
avgpred<-with(lightclimb,data.frame(lat=mean(lat),temp=mean(temp),precip=mean(precip),alt=sort(lightclimb$alt),fsex=mean(as.numeric(fsex))))
avgpred$altpred<-predict(avglms,newdata=avgpred,type="response")
plot(avgpred$altpred~avgpred$alt,type="n")
lines(lowess(avgpred$altpred~avgpred$alt))
avgaltpredplot<-ggplot(data=avgpred,aes(x=alt,y=altpred))+
  geom_point(color=ghowcolors)+
  geom_smooth(method=lm,se=FALSE,color="gray")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  xlab("Elevation(m)")+
  ylab("Foot+Back Lightness(L*)")+
  theme(
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
#Responses to reviewer comments 10/29/2020.
#Goodness of fit tests for lightness distributions testing whether the data are distributed bimodally or not
gof<-lightclimb$toelight+lightclimb$backlight
#FigureS3
hist(gof, breaks=10, xlab="Foot+Back Lightness",main="") #histogram looks pretty normal
#export for supplement
tiff("FigS4.tiff",width=8,height=8,units="in",res=150)
hist(gof, breaks=10, xlab="Foot+Back Lightness",main="",family="serif")
dev.off()
pdf("FigS4.pdf")
hist(gof, breaks=10, xlab="Foot+Back Lightness",main="",family="serif")
dev.off()
#test for multimodality
dip.test(gof)
#plot lightness by solar radiation
plot(lightclimb$toelight+lightclimb$backlight~lightclimb$srad)

#Reviewer responses February 2021
#boxplot looking at sex differences by subspecies: supplemental figure: cleaned up subspecies labels 2/26/2021
#create column for plotting
owlspcab$plotsciname<-""
owlspcab$plotsciname<-owlspcab$sciname
owlspcab$plotsciname<-gsub("Bubo","B.",owlspcab$plotsciname)
owlspcab$plotsciname<-gsub("virginianus","v.",owlspcab$plotsciname)
owlspcab$plotsciname<-gsub("v. v.","v. virginianus",owlspcab$plotsciname)
sexlight<-ggplot(data=owlspcab,aes(x=plotsciname,y=toelight+backlight,fill=sex))+
  geom_boxplot()+
  scale_fill_manual(values=c("grey65","white"),name="Sex")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))+
  ylab("Foot+Back Lightness(L*)")+
  xlab("")+
  theme(
    #axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,family="serif",face="italic",size=8),
    plot.title = element_text(family="serif", size=12, face="bold"),
    axis.title.x=element_text(family="serif", size=12),
    axis.title.y=element_text(family="serif", size=12)
  )
tiff("lightnessbysexbp.tiff",width=8,height=8,units="in",res=300)
sexlight
dev.off()
pdf("lightnessbysexbp.pdf",paper="USr")
sexlight
dev.off()

#remeasurement analysis
#split and aggregate data into long format
remeasure<-remeasure[1:48,]
#percent error
mean((remeasure$X1-remeasure$X2)/remeasure$X1)
#Response to editor comments June 2021
#Look at effects of specimen age on melanin levels (via lightness)
#June2021: assign year to look at whether specimen age is related to plumage lightness
owlspcab$year<-""
owlspcab$year<-strptime(owlspcab$date,"%m/%d/%Y")$year+1900
plot(owlspcab$toelight+owlspcab$backlight~owlspcab$year,xlab="Year Collected",ylab="Foot and Back Lightness (L*)")
pdf("ageplot.pdf",paper="US")
plot(owlspcab$toelight+owlspcab$backlight~owlspcab$year,xlab="Year Collected",ylab="Foot and Back Lightness (L*)",family="serif")
dev.off()
agemod<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$year)
summary(agemod)
#standardized version incorporating specimen age into regression model for lightness with other known important factors
owlspcab$styear<-(as.numeric(owlspcab$year)-mean(as.numeric(owlspcab$year)))/sd(as.numeric(owlspcab$year))
agemod1<-lm(owlspcab$toelight+owlspcab$backlight~owlspcab$styear+owlspcab$stlat+owlspcab$stalt+owlspcab$stfsex)
summary(agemod1)