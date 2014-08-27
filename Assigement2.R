


## 세팅 언어문제로 reading 오류가 발생했음. 영어로 세팅하니 문제없음.
library("plyr")

Sys.setlocale("LC_ALL", locale = "English_United States")
StormData<-read.csv("repdata-data-StormData.csv.bz2",header=TRUE,stringsAsFactors = TRUE)
CleanData <- StormData[,c("STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]

write.csv(CleanData,"CleanData.csv")

## data 정체
CleanData <- read.csv("CleanData.csv")
str(CleanData)
TinyData<-mutate(CleanData,Total_Fatalities=FATALITIES+INJURIES)

##fucntion for calculating damage using DMG and DMGEXP variable
## we will use PROPDMG and CROPDMG
damage_measure<-function(data_dmg,data_exp) {
  Total_value=data_dmg*ifelse(data_exp=="H"|data_exp=="h",100,0)
  Total_value=Total_value+data_dmg*ifelse(data_exp=="K"| data_exp=="k",1000,0)
  Total_value=Total_value+data_dmg*ifelse(data_exp=="M"| data_exp=="m",1000000,0)
  Total_value=Total_value+data_dmg*ifelse(data_exp=="B"| data_exp=="b",1000000000,0)
  
  Total_value=Total_value+ifelse(data_exp=="1",data_dmg*1000+100,0)
  Total_value=Total_value+ifelse(data_exp=="2",data_dmg*1000+200,0)
  Total_value=Total_value+ifelse(data_exp=="3",data_dmg*1000+300,0)
  Total_value=Total_value+ifelse(data_exp=="4",data_dmg*1000+400,0)
  Total_value=Total_value+ifelse(data_exp=="5",data_dmg*1000+500,0)
  Total_value=Total_value+ifelse(data_exp=="6",data_dmg*1000+600,0)
  Total_value=Total_value+ifelse(data_exp=="7",data_dmg*1000+700,0)
  Total_value=Total_value+ifelse(data_exp=="8",data_dmg*1000+800,0)
  Total_value=Total_value+ifelse(data_exp=="9",data_dmg*1000+900,0)
  Total_value=Total_value+ifelse(data_exp=="?" |data_exp=="+",data_dmg,0)
  Toal_value=Total_value+ifelse(data_exp=="" |data_exp=="-",data_dmg,0)  
  return(Total_value)
  
}

prop_damage<-damage_measure(TinyData$PROPDMG,TinyData$PROPDMGEXP)
crop_damage<-damage_measure(TinyData$CROPDMG,TinyData$CROPDMGEXP)

TinyData<-cbind(TinyData,as.data.frame(prop_damage))
TinyData<-cbind(TinyData,as.data.frame(crop_damage))

## 첫번째 문제


Population_health<-ddply(TinyData,.(EVTYPE),summarise, Total=sum(Total_Fatalities))
Max_Value<-Population_health[which.max(Population_health$Total),1]

refine_data<-arrange(Population_health,desc(Total),EVTYPE)
abstract_data<-head(refine_data,5)
kable(abstract_data)
barplot(abstract_data$Total,names.arg=as.character(abstract_data$EVTYPE))

## 두번째 문제
damages<-ddply(TinyData,.(EVTYPE),summarise, Total_damage=sum(prop_damage)+sum(crop_damage))
refine_data_second<-arrange(damages,desc(Total_damage),EVTYPE)
abstract_data_second<-head(refine_data_second,5)
Max_Value_damage<-damages[which.max(damages$Total_damage),1]
barplot(abstract_data_second$Total_damage,names.arg=as.character(abstract_data_second$EVTYPE))


## 참고

test<-CleanData[,c("PROPDMG","PROPDMGEXP")]

Total_value=test$PROPDMG*ifelse(test$PROPDMGEXP=="H"|test$PROPDMGEXP=="h",100,0)
Total_value=Total_value+test$PROPDMG*ifelse(test$PROPDMGEXP=="K"|test$PROPDMGEXP=="k",1000,0)
Total_value=Total_value+test$PROPDMG*ifelse(test$PROPDMGEXP=="M"|test$PROPDMGEXP=="m",1000000,0)
Total_value=Total_value+test$PROPDMG*ifelse(test$PROPDMGEXP=="B"|test$PROPDMGEXP=="b",1000000000,0)

Total_value=Total_value+ifelse(test$PROPDMGEXP=="1",test$PROPDMG*1000+100,0)
Total_value=Total_value+ifelse(test$PROPDMGEXP=="2",test$PROPDMG*1000+200,0)
Total_value=Total_value+ifelse(test$PROPDMGEXP=="3",test$PROPDMG*1000+300,0)
Total_value=Total_value+ifelse(test$PROPDMGEXP=="4",test$PROPDMG*1000+400,0)
Total_value=Total_value+ifelse(test$PROPDMGEXP=="5",test$PROPDMG*1000+500,0)
Total_value=Total_value+ifelse(test$PROPDMGEXP=="6",test$PROPDMG*1000+600,0)
Total_value=Total_value+ifelse(test$PROPDMGEXP=="7",test$PROPDMG*1000+700,0)
Total_value=Total_value+ifelse(test$PROPDMGEXP=="8",test$PROPDMG*1000+800,0)
Total_value=Total_value+ifelse(test$PROPDMGEXP=="9",test$PROPDMG*1000+900,0)


TinyData<-cbind(TinyData,as.data.frame(Total_value))

a<-as.factor(tolower(CleanData$EVTYPE))
b<-levels(a)
write.csv(b,file="category.csv")


memory.limit(size = 4080)


htotal<-CleanData
htotal$EVTYPE<-as.factor(tolower(CleanData$EVTYPE))

grepl("Hello", "Hello World")
grepl("^Hello$", "Hello World")
grepl("^Hello World$", "Hello World")

htotal$EVTYPE[grep("[Bb][Ll][Oo][Ww]", htotal$EVTYPE)] <- "WIND"
grep("flood", htotal$EVTYPE)
htotal$EVTYPE[grep("flood", htotal$EVTYPE)]



TinyData$EVTYPE<-as.factor(tolower(TinyData$EVTYPE))
TinyData<-mutate(TinyData,EventType="OTHERS",Used=FALSE)
Classfy_EVTYPE<-function(pattern,Event_Name, inputdata) {
  temp<-grepl(pattern, inputdata[,3]) & inputdata[,14]==FALSE
  inputdata[temp,13]<-Event_Name
  inputdata[temp,14]<-TRUE
  return(inputdata)
}

##copydata<-TinyData
TinyData<-copydata

TinyData<-Classfy_EVTYPE("astronomical","Astronomical Low Tide",TinyData)
TinyData<-Classfy_EVTYPE("avalance|avalanche","Avalanche",TinyData)
TinyData<-Classfy_EVTYPE("blizzard","Blizzard",TinyData)
TinyData<-Classfy_EVTYPE("coastal flood|beach|coastal|erosin","Coastal Flood",TinyData)
TinyData<-Classfy_EVTYPE("cold|wind chill|cool|windchill|low temperature","Cold/Wind Chill",TinyData)
TinyData<-Classfy_EVTYPE("debris","Debris Flow",TinyData)
TinyData<-Classfy_EVTYPE("dense fog","Dense Fog",TinyData)
TinyData<-Classfy_EVTYPE("dense smoke","Dense Smoke",TinyData)
TinyData<-Classfy_EVTYPE("drough|dries|dry","Drought",TinyData)
TinyData<-Classfy_EVTYPE("blowing dust|dust storm","Dust Storm",TinyData)
TinyData<-Classfy_EVTYPE("dust","Dust Devil",TinyData)
TinyData<-Classfy_EVTYPE("flash flood","Flash Flood",TinyData)
TinyData<-Classfy_EVTYPE("fog","Freezing Fog",TinyData)
TinyData<-Classfy_EVTYPE("freeze|frost","Frost/Freeze",TinyData)
TinyData<-Classfy_EVTYPE("funnel|cloud","Funnel Cloud  ",TinyData)
TinyData<-Classfy_EVTYPE("heat|hot|high temperature|hyperthermia","Heat",TinyData)
TinyData<-Classfy_EVTYPE("rain|wet","Heavy Rain",TinyData)
TinyData<-Classfy_EVTYPE("high surf|high waves|high tide|high swells|high water","High Surf",TinyData)
TinyData<-Classfy_EVTYPE("high winds|high wind","High Wind",TinyData)
TinyData<-Classfy_EVTYPE("hurricane|typhoon","Hurricane/Typhoon",TinyData)
TinyData<-Classfy_EVTYPE("ice|glaze|icy","Ice Storm",TinyData)
TinyData<-Classfy_EVTYPE("lake flood|lakeshore flood","Lakeshore Flood",TinyData)
TinyData<-Classfy_EVTYPE("lake-effect snow|lake effect snow","Lake-Effect Snow",TinyData)
TinyData<-Classfy_EVTYPE("lightning","Lightning",TinyData)
TinyData<-Classfy_EVTYPE("marine hail","Marine Hail",TinyData)
TinyData<-Classfy_EVTYPE("marine high wind|marine accident|marine mishap","Marine High Wind",TinyData)
TinyData<-Classfy_EVTYPE("marine strong wind","Marine Strong Wind",TinyData)
TinyData<-Classfy_EVTYPE("marine thunderstorm wind|marine tstm wind","Marine Thunderstorm Wind",TinyData)
TinyData<-Classfy_EVTYPE("rip current","Rip Current",TinyData)
TinyData<-Classfy_EVTYPE("seiche","Seiche",TinyData)
TinyData<-Classfy_EVTYPE("sleet","Sleet",TinyData)
TinyData<-Classfy_EVTYPE("storm surge|tide","Storm Tide",TinyData)
TinyData<-Classfy_EVTYPE("gusty|thuderstorm|thundeerstorm|tstm","Thunderstorm Wind",TinyData)
TinyData<-Classfy_EVTYPE("tornado","Tornado",TinyData)
TinyData<-Classfy_EVTYPE("tropical depression","Tropical Depression",TinyData)
TinyData<-Classfy_EVTYPE("tropical storm","Tropical Storm",TinyData)
TinyData<-Classfy_EVTYPE("tsunami","Tsunami",TinyData)
TinyData<-Classfy_EVTYPE("volcanic","Volcanic Ash",TinyData)
TinyData<-Classfy_EVTYPE("waterspout|gustnado|heavy mix|rising water","Waterspout",TinyData)
TinyData<-Classfy_EVTYPE("fire","Wildfire",TinyData)
TinyData<-Classfy_EVTYPE("winter storm","Winter Storm",TinyData)
TinyData<-Classfy_EVTYPE("winter weather|wintry mix","Winter Weather",TinyData)
TinyData<-Classfy_EVTYPE("microburst|strong wind|storm","Strong Wind",TinyData)
TinyData<-Classfy_EVTYPE("slide","Slide",TinyData)
TinyData<-Classfy_EVTYPE("wind","Wind",TinyData)
TinyData<-Classfy_EVTYPE("flood|precip|stream","Flood",TinyData)
TinyData<-Classfy_EVTYPE("hail","Hail",TinyData)
TinyData<-Classfy_EVTYPE("snow","Heavy Snow  ",TinyData)

TinyData$EventType<-as.factor(TinyData$EventType)

levels(TinyData$EventType)
summary(TinyData$EventType)


## 수정 첫번째 문제


Population_health<-ddply(TinyData,.(EventType),summarise, Total=sum(Total_Fatalities))
Max_Value<-Population_health[which.max(Population_health$Total),1]

refine_data<-arrange(Population_health,desc(Total),EventType)
abstract_data<-head(refine_data,5)
barplot(abstract_data$Total,names.arg=as.character(abstract_data$EventType))

## 수정 두번째 문제
damages<-ddply(TinyData,.(EventType),summarise, Total_damage=sum(prop_damage)+sum(crop_damage))
refine_data_second<-arrange(damages,desc(Total_damage),EventType)
abstract_data_second<-head(refine_data_second,5)
Max_Value_damage<-damages[which.max(damages$Total_damage),1]
barplot(abstract_data_second$Total_damage,names.arg=as.character(abstract_data_second$EventType))




Classfy_EVTYPE<-function(pattern,Event_Name, inputdata, Event,Used) {
  temp<-(grepl(pattern, Event)) & (Used==FALSE)
  inputdata[temp,12]<-Event_Name
  inputdata[temp,13]<-TRUE
  return(inputdata)
}

##copydata<-TinyData
TinyData<-copydata

TinyData<-Classfy_EVTYPE("astronomical","Astronomical Low Tide",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("avalance|avalanche","Avalanche",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("blizzard","Blizzard",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("coastal flood|beach|coastal|erosin","Coastal Flood",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("cold|wind chill|cool|windchill|low temperature","Cold/Wind Chill",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("debris","Debris Flow",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("dense fog","Dense Fog",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("dense smoke","Dense Smoke",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("drough|dries|dry","Drought",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("blowing dust|dust storm","Dust Storm",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("dust","Dust Devil",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("flash flood","Flash Flood",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("fog","Freezing Fog",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("freeze|frost","Frost/Freeze",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("funnel|cloud","Funnel Cloud  ",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("heat|hot|high temperature|hyperthermia","Heat",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("rain|wet","Heavy Rain",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("high surf|high waves|high tide|high swells|high water","High Surf",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("high winds|high wind","High Wind",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("hurricane|typhoon","Hurricane/Typhoon",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("ice|glaze|icy","Ice Storm",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("lake flood|lakeshore flood","Lakeshore Flood",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("lake-effect snow|lake effect snow","Lake-Effect Snow",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("lightning","Lightning",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("marine hail","Marine Hail",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("marine high wind|marine accident|marine mishap","Marine High Wind",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("marine strong wind","Marine Strong Wind",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("marine thunderstorm wind|marine tstm wind","Marine Thunderstorm Wind",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("rip current","Rip Current",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("seiche","Seiche",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("sleet","Sleet",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("storm surge|tide","Storm Tide",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("gusty|thuderstorm|thundeerstorm|tstm","Thunderstorm Wind",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("tornado","Tornado",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("tropical depression","Tropical Depression",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("tropical storm","Tropical Storm",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("tsunami","Tsunami",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("volcanic","Volcanic Ash",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("waterspout|gustnado|heavy mix|rising water","Waterspout",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("fire","Wildfire",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("winter storm","Winter Storm",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("winter weather|wintry mix","Winter Weather",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("microburst|strong wind|storm","Strong Wind",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("slide","Slide",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("wind","Wind",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("flood|precip|stream","Flood",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("hail","Hail",TinyData,TinyData$EVTYPE,TinyData$Used)
TinyData<-Classfy_EVTYPE("snow","Heavy Snow  ",TinyData,TinyData$EVTYPE,TinyData$Used)

TinyData$EventType<-as.factor(TinyData$EventType)



barplot(height = abstract_data$Total, 
        names.arg = abstract_data$EVTYPE, 
        las = 2, cex.axis = 0.8, cex.names = 0.6, col = "green", 
        main = "Top 10 Event Type - \n Causing Fatalities ", 
        ylab = "Number of of Fatalities")



library(ggplot2)
g1 <- ggplot(abstract_data)
g1 + geom_bar(aes(x=EVTYPE, y=Total), stat="identity") + 
  xlab("Event Type") +
  ylab("Total Casualties (Fatalities + Injuries)") +
  ggtitle("Top 10 events based on health impact") +
  coord_flip()

g2 <- ggplot(ecoimpactagg)
g2 + geom_bar(aes(x=STDTYPE, y=totdmgamt/1000000000), stat="identity") +
  xlab("Event Type") +
  ylab("Total Damages in billions (Property + Crop)") +
  ggtitle("Top 10 events based on economic impact") +
  coord_flip()

barplot(abstract_data$Total,col=rainbow(10),main="Top 10 Causes of Weather Event Deaths",ylab="Deaths",
        legend=abstract_data$EVTYPE)


http://rpubs.com/icshs1/25692

options(rpubs.upload.method = "internal"