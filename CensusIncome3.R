# Predict whether or not income is above 50k
# Census Income Data Set
# Thomas Mattimore

####### LIBARY #######
library('ggplot2')
library('XML')
library('car')
library('ROCR')
library('gplots')
library('gridExtra') 
library('nnet') 
library('rpart')
######## IMPORT DATA #####
source("DataPrep.R")
source("DataTestPrep.R")
######## FUNCTIONS######
predictivepowers <- function(fit2,datatest, predtyp){
  #Prediction of test data
  predi <- predict(fit2,datatest,type=predtyp)
  N.test <- length(predi) # how many values
  # threshold of .5 assigns predicted income
  y.hat<-rep(0,N.test) 
  y.hat[predi>=0.5] <- 1
  # Get outcome of the test data
  results <- levels(datatest$income)
  y.test <- rep(0,N.test)
  y.test[datatest$income==results[2]] <- 1
  # True vs Prediction
  conf.table <- table(y.hat,y.test)
  if(predtyp=="response"){
    colnames(conf.table) <- c(paste("Actual",results[1]), results[2])
    rownames(conf.table) <- c(paste("Predicted",results[1]), results[2])
    conf.table
  }
  #Accuracy
  accuracy <- sum(diag(conf.table))/N.test
  cat("accuracy =", accuracy*100, "miss-classified =", 100-accuracy*100)
  results <- list(accuracy, conf.table, predi)
  return(results)
}
predictivepowers2 <- function(predi,datatest){#, predtyp){
  #Prediction of test data
  #predi <- predict(fit2,datatest,type=predtyp)
  N.test <- length(predi) # how many values
  # threshold of .5 assigns predicted income
  y.hat<-rep(0,N.test) 
  y.hat[predi>=0.5] <- 1
  # Get outcome of the test data
  results <- levels(datatest$income)
  y.test <- rep(0,N.test)
  y.test[datatest$income==results[2]] <- 1
  # True vs Prediction
  conf.table <- table(y.hat,y.test)
  colnames(conf.table) <- c(paste("Actual",results[1]), results[2])
  rownames(conf.table) <- c(paste("Predicted",results[1]), results[2])
  conf.table
  #Accuracy
  accuracy <- sum(diag(conf.table))/N.test
  cat("accuracy =", accuracy*100, "miss-classified =", 100-accuracy*100)
  results <- list(accuracy, conf.table, predi)
  return(results)
}
####### Test Collinearity & Predict Fit on GLM ######
    #logistic regression
    fit1 = glm(income ~ ., family=binomial(logit), data = data)
    
    #test for collinear
    vif(fit1) #find that marital and relationship are highly collinear, remove relationship
    fit1accuracy <- predictivepowers(fit1, datatest, "response")
    data$relationship = NULL # relationship and marital are extremely collinear, remove relationship since its weird
    datatest$relationship = NULL
    #retest for collinear
    fit2 = glm(income ~ ., family=binomial(logit), data = data)
    vif(fit2) #nice! all set
    fit2accuracy <- predictivepowers(fit2,datatest, "response") # accuracy=82.87
    logit.pred = prediction(fit2accuracy[3],datatest$income) 
    logit.perf = performance(logit.pred,"tpr","fpr") 
    #DWStat 1.98=~2 low p-value, no correlation
    durbinWatsonTest(fit2)
    # Get GLM coefficients
    tab <- summary(fit2)$coefficients#get coefficients`
    sorter <- order(tab[,4])#sort by p value
    tab <- tab[sorter,]#sort by pvalue
    coeffs <- exp(coef(fit2)) 
    confints <- exp(confint.default(fit2))
####### CROSS VALIDATE ######
      # CART Decision Trees 
      mycontrol = rpart.control(cp = 0, xval = 10)
      tree.fit = rpart(income~., method = "class",data = data, control = mycontrol) 
      tree.fit$cptable 
      tree.cptarg = sqrt(tree.fit$cptable[8,1]*tree.fit$cptable[9,1]) 
      tree.prune = prune(tree.fit,cp=tree.cptarg) 
      tree.preds = predict(tree.prune,newdata=datatest,type="prob")[,2] 
      tree.pred = prediction(tree.preds,datatest$income)
      tree.perf = performance(tree.pred,"tpr","fpr") 
      treefitaccuracy <- predictivepowers2(tree.preds, datatest) # Accuracy: 83.21%
      # Neural Network 
      nnet.fit = nnet(income~., data=data,size=20,maxit=10000,decay=.001)
      nnet.preds = predict(nnet.fit,newdata=datatest,type="raw") 
      nnet.pred = prediction(nnet.preds,datatest$income) 
      nnet.perf = performance(nnet.pred,"tpr","fpr") 
      neuralfitaccuracy <-predictivepowers(nnet.fit, datatest, "raw") # Accuracy: 83.06%
      # Plot ROC Curves 
      plot(logit.perf,col=2,lwd=2,main="ROC Curve for Classifiers on Adult Dataset") 
      #plot(rf.perf,col=3,lwd=2,add=T) 
      plot(tree.perf,lwd=2,col=4,add=T) 
      plot(nnet.perf,lwd=2,col=5,add=T) 
      abline(a=0,b=1,lwd=2,lty=2,col="gray") 
      legend("bottomright", col=c(2:5),lwd=2,legend=c(paste("logit",as.character(round(fit2accuracy[[1]]*100,digits=2)),"%"),paste("CART",as.character(round(treefitaccuracy[[1]]*100,digits=2)),"%"),paste("logit",as.character(round(fit2accuracy[[1]]*100,digits=2)),"%")))
####### Male / Female (age 35), 45 hrs/week, Bachelors degree ########
      malefemale<-expand.grid(age=35,
                              employertype=unique(data[c("employertype")])$employertype,
                              educationnum=6:15,
                              marital=unique(data[c("marital")])$marital,
                              occupation=unique(data[c("occupation")])$occupation,
                              race=unique(data[c("race")])$race,
                              sex=c('Male','Female'),
                              hrperweek=45
                              )
        malefemale$pred<-predict(fit2,newdata=malefemale,type='response')
        ggplot(malefemale,aes(x=educationnum,y=pred, color=sex))+
          stat_smooth()+
          xlab("Education Level")+ylab("Prob. of income >= 50K")+
          ggtitle("Income vs. Education Level by Sex")
###### Occupation vs Age #########
    #(45 hrs/week, Highschool Grad, employertype=all, marital=all, sex=all, race=all)
        occupationage<-expand.grid(age=20:65,
                                employertype=unique(data[c("employertype")])$employertype,
                                educationnum=6:15,
                                marital=unique(data[c("marital")])$marital,
                                occupation=unique(data[c("occupation")])$occupation,
                                race=unique(data[c("race")])$race,
                                sex=c('Male','Female'),
                                hrperweek=45
        )
        occupationage$pred<-predict(fit2,newdata=occupationage,type='response')
        ggplot(occupationage,aes(x=age,y=pred, color=occupation))+
          stat_smooth()+
          xlab("Age")+ylab("Prob. of income >= 50K")+
          ggtitle("Income vs. Age by Occupation")
