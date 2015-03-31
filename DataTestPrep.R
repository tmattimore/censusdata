########### Import datatest#######
datatest = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test", sep=",", header=T,col.names=c("age","employertype","fnlwgt","education","educationnum","marital","occupation","relationship","race","sex","capitalgain","capitalloss","hrperweek","country","income"), strip.white=T)

#Check its all there
cat("# missing data:", sum(is.na(datatest)), "\n")
#######Clean it up######
  datatest$fnlwgt = NULL # ignore weighting
  datatest$education = NULL #ignore education because its redundant
########## CAPITALGAINS ######
    #make capital gain more compact
    datatest["capitalgain"] <- ordered(cut(datatest$capitalgain,c(-Inf,0,median(datatest["capitalgain"][datatest["capitalgain"]>0]),Inf)),labels = c("None","Low","High"))
    datatest["capitalloss"] <- ordered(cut(datatest$capitalloss,c(-Inf,0,median(datatest["capitalloss"][datatest["capitalloss"]>0]),Inf)),labels = c("None","Low","High"))
    
    #Remove them because >90 have none
      datatest$capitalgain = NULL
      datatest$capitalloss = NULL

  
    
########### COUNTRY #########
#make countries more compact ## --- EDIT: eliminate countries... >90% from US
  datatest$country = as.character(datatest$country)
  datatest$country[datatest$country=="Cambodia"] = "SE-Asia"
  datatest$country[datatest$country=="Canada"] = "British-Commonwealth"    
  datatest$country[datatest$country=="China"] = "China"       
  datatest$country[datatest$country=="Columbia"] = "South-America"    
  datatest$country[datatest$country=="Cuba"] = "Other"        
  datatest$country[datatest$country=="Dominican-Republic"] = "Latin-America"
  datatest$country[datatest$country=="Ecuador"] = "South-America"     
  datatest$country[datatest$country=="El-Salvador"] = "South-America" 
  datatest$country[datatest$country=="England"] = "British-Commonwealth"
  datatest$country[datatest$country=="France"] = "Euro_1"
  datatest$country[datatest$country=="Germany"] = "Euro_1"
  datatest$country[datatest$country=="Greece"] = "Euro_2"
  datatest$country[datatest$country=="Guatemala"] = "Latin-America"
  datatest$country[datatest$country=="Haiti"] = "Latin-America"
  datatest$country[datatest$country=="Holand-Netherlands"] = "Euro_1"
  datatest$country[datatest$country=="Honduras"] = "Latin-America"
  datatest$country[datatest$country=="Hong"] = "China"
  datatest$country[datatest$country=="Hungary"] = "Euro_2"
  datatest$country[datatest$country=="India"] = "British-Commonwealth"
  datatest$country[datatest$country=="Iran"] = "Other"
  datatest$country[datatest$country=="Ireland"] = "British-Commonwealth"
  datatest$country[datatest$country=="Italy"] = "Euro_1"
  datatest$country[datatest$country=="Jamaica"] = "Latin-America"
  datatest$country[datatest$country=="Japan"] = "Other"
  datatest$country[datatest$country=="Laos"] = "SE-Asia"
  datatest$country[datatest$country=="Mexico"] = "Latin-America"
  datatest$country[datatest$country=="Nicaragua"] = "Latin-America"
  datatest$country[datatest$country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
  datatest$country[datatest$country=="Peru"] = "South-America"
  datatest$country[datatest$country=="Philippines"] = "SE-Asia"
  datatest$country[datatest$country=="Poland"] = "Euro_2"
  datatest$country[datatest$country=="Portugal"] = "Euro_2"
  datatest$country[datatest$country=="Puerto-Rico"] = "Latin-America"
  datatest$country[datatest$country=="Scotland"] = "British-Commonwealth"
  datatest$country[datatest$country=="South"] = "Euro_2"
  datatest$country[datatest$country=="Taiwan"] = "China"
  datatest$country[datatest$country=="Thailand"] = "SE-Asia"
  datatest$country[datatest$country=="Trinadad&Tobago"] = "Latin-America"
  datatest$country[datatest$country=="United-States"] = "United-States"
  datatest$country[datatest$country=="Vietnam"] = "SE-Asia"
  datatest$country[datatest$country=="Yugoslavia"] = "Euro_2"
  datatest$country = as.factor(datatest$country)
  datatest$country = NULL
####### REMOVE EMPTY ########
  #remove empty sets
  is.na(datatest) = datatest=='?'
  is.na(datatest) = datatest==' ?'
  datatest = na.omit(datatest)

#summary(datatest)