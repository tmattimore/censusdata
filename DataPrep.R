######## Import Data######
  data = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", sep=",", header=F,col.names=c("age","employertype","fnlwgt","education","educationnum","marital","occupation","relationship","race","sex","capitalgain","capitalloss","hrperweek","country","income"), strip.white=T)
#Check its all there
  cat("# missing data:", sum(is.na(data)), "\n")
########Clean it up#######
  data$fnlwgt = NULL # ignore weighting
  data$education = NULL #ignore education... education num is easier
  
########CAPITAL GAIN/LOSS#########
    #make capital gain more compact
    data["capitalgain"] <- ordered(cut(data$capitalgain,c(-Inf,0,median(data["capitalgain"][data["capitalgain"]>0]),Inf)),labels = c("None","Low","High"))
    data["capitalloss"] <- ordered(cut(data$capitalloss,c(-Inf,0,median(data["capitalloss"][data["capitalloss"]>0]),Inf)),labels = c("None","Low","High"))
    
    #Check capital gain histograms
      p1<-ggplot(data, aes(x=capitalgain)) + ggtitle("( Capital Gain )") +
        geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="white") + ylab("Percentage")
      p2<-ggplot(data, aes(x=capitalloss)) + ggtitle("( Capital Loss )") +
        geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="white") + ylab("Percentage") 
      grid.arrange(p1,p2,ncol=2)
    #Remove them because >90 have none
      data$capitalgain = NULL
      data$capitalloss = NULL
  
####### COUNTRIES #######  
#make countries more compact ###-- EDIT: Remove country because >90 from US
 
  data$country = as.character(data$country)
  data$country[data$country=="Cambodia"] = "SE-Asia"
  data$country[data$country=="Canada"] = "British-Commonwealth"    
  data$country[data$country=="China"] = "China"       
  data$country[data$country=="Columbia"] = "South-America"    
  data$country[data$country=="Cuba"] = "Other"        
  data$country[data$country=="Dominican-Republic"] = "Latin-America"
  data$country[data$country=="Ecuador"] = "South-America"     
  data$country[data$country=="El-Salvador"] = "South-America" 
  data$country[data$country=="England"] = "British-Commonwealth"
  data$country[data$country=="France"] = "Euro_1"
  data$country[data$country=="Germany"] = "Euro_1"
  data$country[data$country=="Greece"] = "Euro_2"
  data$country[data$country=="Guatemala"] = "Latin-America"
  data$country[data$country=="Haiti"] = "Latin-America"
  data$country[data$country=="Holand-Netherlands"] = "Euro_1"
  data$country[data$country=="Honduras"] = "Latin-America"
  data$country[data$country=="Hong"] = "China"
  data$country[data$country=="Hungary"] = "Euro_2"
  data$country[data$country=="India"] = "British-Commonwealth"
  data$country[data$country=="Iran"] = "Other"
  data$country[data$country=="Ireland"] = "British-Commonwealth"
  data$country[data$country=="Italy"] = "Euro_1"
  data$country[data$country=="Jamaica"] = "Latin-America"
  data$country[data$country=="Japan"] = "Other"
  data$country[data$country=="Laos"] = "SE-Asia"
  data$country[data$country=="Mexico"] = "Latin-America"
  data$country[data$country=="Nicaragua"] = "Latin-America"
  data$country[data$country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
  data$country[data$country=="Peru"] = "South-America"
  data$country[data$country=="Philippines"] = "SE-Asia"
  data$country[data$country=="Poland"] = "Euro_2"
  data$country[data$country=="Portugal"] = "Euro_2"
  data$country[data$country=="Puerto-Rico"] = "Latin-America"
  data$country[data$country=="Scotland"] = "British-Commonwealth"
  data$country[data$country=="South"] = "Euro_2"
  data$country[data$country=="Taiwan"] = "China"
  data$country[data$country=="Thailand"] = "SE-Asia"
  data$country[data$country=="Trinadad&Tobago"] = "Latin-America"
  data$country[data$country=="United-States"] = "United-States"
  data$country[data$country=="Vietnam"] = "SE-Asia"
  data$country[data$country=="Yugoslavia"] = "Euro_2"
  data$country = as.factor(data$country) 
  data$country = NULL
  
######## REMOVE EMPTY ########
  #remove empty sets
  is.na(data) = data=='?'
  is.na(data) = data==' ?'
  data = na.omit(data)

#summary(data)
