################################################################
# Final Project - Quantitative Methods for Machine Learning    #
################################################################

setwd('Users\julia\Documents\College\Babson\quant_methods_for_ml_final_kaggle_startup')

library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(mltools)

# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('patchwork') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('vroom') # input/output
library('skimr') # overview
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('purrr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('fuzzyjoin') # data wrangling
library('data.table') # ingest

# specific visualisation
library('alluvial') # visualisation
library('ggrepel') # visualisation
library('ggforce') # visualisation
library('ggridges') # visualisation
library('gganimate') # animations
library('GGally') # visualisation
library('ggthemes') # visualisation
library('wesanderson') # visualisation
library('kableExtra') # display
library('ISLR')
# Date + forecast
library('lubridate') # date and time
library('forecast') # time series analysis
#library('prophet') # time series analysis
library('timetk') # time series analysis

# Interactivity
library('crosstalk')
library('plotly')

# parallel
library('foreach')
library('doParallel')

acquisitions <- read.csv("data\acquisitions.csv")
degrees <- read.csv("data\degrees.csv")
funding_rounds <- read.csv("data\funding_rounds.csv")
funds <- read.csv("data\funds.csv")
investments <- read.csv("data\investments.csv")
ipos <- read.csv("data\ipos.csv")
milestones <- read.csv("data\milestones.csv")
objects <- read.csv("data\objects.csv")
offices <- read.csv("data\offices.csv")
people <- read.csv("data\people.csv")
relationships <- read.csv("data\relationships.csv")

################## datachecking ###################

#################### ACQUISITIONS #################
str(acquisitions)
summary(acquisitions) 
sum(is.na(acquisitions))

acquisitions$term_code[is.na(acquisitions$term_code)] <- "N/A"
acquisitions$term_code <- as.factor(acquisitions$term_code)
acquisitions$price_currency_code <- as.factor(acquisitions$price_currency_code)

acquisitions$source_url[is.na(acquisitions$source_url)] <- "None"
acquisitions$source_description[is.na(acquisitions$source_description)] <- "None"

# one hot encoding funding rounds 
# 
testData <- mydata[c(5,6,7,21)]

############### DEGREES ##################
str(degrees)
summary(degrees)
sum(is.na(degrees))

########### Variable Transformations ##########

# Transform vars into factors for categorical

# Transform vars into dates (var type)

# Drop Vars - TEMPORARY; NEED TO REVISIT LATER

###############################################


############# FUNDING_ROUNDS #############
str(funding_rounds)
summary(funding_rounds)
sum(is.na(funding_rounds))

########### Variable Transformations ##########

# Transform vars into factors for categorical
funding_rounds$funding_round_type = as.factor(funding_rounds$funding_round_type)
funding_rounds$funding_round_code = as.factor(funding_rounds$funding_round_code)
funding_rounds$raised_currency_code = as.factor(funding_rounds$raised_currency_code)
funding_rounds$pre_money_currency_code = as.factor(funding_rounds$pre_money_currency_code)

# Transform vars into dates (var type)
funding_rounds$funded_at = as.Date(funding_rounds$funded_at)

# Transform vars into bools 
funding_rounds$is_first_round = as.boolean(funding_rounds$is_first_round)
funding_rounds$is_last_round = as.boolean(funding_rounds$is_last_round)

# Drop Vars - TEMPORARY; NEED TO REVISIT LATER
funding_rounds$created_at = NULL
funding_rounds$updated_at = NULL
funding_rounds$created_by = NULL

###############################################


############# FUNDS ######################
str(funds)
summary(funds)
sum(is.na(funds))

########### Variable Transformations ##########

# Transform vars into factors for categorical
funds$raised_currency_code = as.factor(funds$raised_currency_code)

# Transform vars into dates (var type)
funds$funded_at = as.Date(funds$funded_at)

# Drop Vars - TEMPORARY; NEED TO REVISIT LATER
funds$created_at = NULL
funds$updated_at = NULL

###############################################

################ INVESTMENTS #################

########### Variable Transformations ##########

# Transform vars into factors for categorical

# Transform vars into dates (var type)

# Drop Vars - TEMPORARY; NEED TO REVISIT LATER

###############################################


################# IPOS #######################
str(ipos)
summary(ipos)
sum(is.na(ipos))

########### Variable Transformations ##########

# Transform vars into factors for categorical
ipos$raised_currency_code = as.factor(ipos$raised_currency_code)
ipos$valuation_currency_code = as.factor(ipos$valuation_currency_code)

# Transform vars into dates (var type)
ipos$public_at = as.Date(ipos$public_at)

# Drop Vars - TEMPORARY; NEED TO REVISIT LATER
ipos$created_at = NULL
ipos$updated_at = NULL

###############################################


################### MILESTONES ################
str(milestones)
summary(milestones)
sum(is.na(milestones))

######### Variable Transformation #########

# Transform vars into factors for categorical
milestones$milestone_code = as.factor(milestones$milestone_code)

# Transform vars into dates (var type)
milestones$milestone_at = as.Date(milestones$milestone_at)

# Drop Vars - TEMPORARY; NEED TO REVISIT LATER
milestones$created_at = NULL
milestones$updated_at = NULL

################## OBJECTS ################### 
str(objects)
summary(objects)
sum(is.na(objects))

########### Variable Transformations ##########

# Transform vars into factors for categorical
objects$entity_type = as.factor(objects$entity_type)
objects$category_code = as.factor(objects$category_code)
objects$status = as.factor(objects$status)
objects$country_code = as.factor(objects$country_code)
objects$state_code = as.factor(objects$state_code)
objects$city = as.factor(objects$city)
objects$region = as.factor(objects$region)

# Transform Tag Col into vector
objects$tag_list = as.vector(objects$tag_list)

# Transform vars into dates (var type)
##### IF NA VALS THROW ERRORS, COMMENT THESE OUT ####
objects$first_funding_at = as.Date(objects$first_funding_at)
objects$first_investment_at = as.Date(objects$first_investment_at)
objects$first_milestone_at = as.Date(objects$first_milestone_at)

objects$last_funding_at = as.Date(objects$last_funding_at)
objects$last_milestone_at = as.Date(objects$last_milestone_at)
objects$last_investment_at = as.Date(objects$last_investment_at)

objects$founded_at = as.Date(objects$founded_at)
objects$closed_at = as.Date(objects$closed_at)

# Drop Vars - TEMPORARY; NEED TO REVISIT LATER
objects$created_at = NULL
objects$created_by = NULL

objects$logo_url = NULL
objects$logo_height = NULL
objects$logo_width = NULL

# Test subseting data to remove investment companies and funds 
testobj2 <- objects[objects$invested_companies == 0,]

###############################################

################### OFFICES ################### 
str(offices)
summary(offices)
sum(is.na(offices))

########### Variable Transformations ##########

# Transform vars into factors for categorical
offices$country_code = as.factor(offices$country_code)
offices$state_code = as.factor(offices$state_code)
offices$region = as.factor(offices$region)

# Transform vars into dates (var type)

# Drop Vars - TEMPORARY; NEED TO REVISIT LATER

###############################################

#################### PEOPLE ###################
str(people)
summary(people)
sum(is.na(people))

########### Variable Transformations ##########

# Transform vars into factors for categorical

# Transform vars into dates (var type)

# Drop Vars - TEMPORARY; NEED TO REVISIT LATER

###############################################

################### RELATIONSHIPS ##############
str(relationships)
summary(relationships)
sum(is.na(relationships))

########### Variable Transformations ##########

# Transform vars into factors for categorical

# Transform vars into dates (var type)

# Drop Vars - TEMPORARY; NEED TO REVISIT LATER

###############################################

###############################################
################## Data Frame Merging #########
###############################################

# objfunding = inner_join(funding_rounds, testobj2, by=c("object_id"="id"))
fundObj = inner_join(testobj2, funding_rounds, by=c('id'='object_id'))

acquisitionsobj = inner_join(acquisitions, objects, by=c("acquiring_object_id"="id"))

# acquisitionsobj = inner_join(acquisitionsobj, objects, by=c("acquired_object_id"="id"), suffix=c('_acquired','_acquired'), copy=TRUE)
acquisitionsobj = inner_join(objects, acquisitions, by=c("id"="acquiring_object_id", "id"="acquired_object_id"))

############################## Further Cleaning for M: ########################

fundObj$overview = NULL
fundObj$permalink = NULL
fundObj$name = NULL
fundObj$homepage_url = NULL
fundObj$domain = NULL
fundObj$twitter_username = NULL
fundObj$short_description = NULL
fundObj$description = NULL
fundObj$source_url = NULL
fundObj$source_description = NULL
fundObj$overview = NULL

# str_split_fixed(fundObj$tag_list, ", ", n=10)
fundObj$tag_list = NULL 

fundObj$city = NULL

onehotTest = one_hot(as.data.table(fundObj))

onehotTest$id = NULL
onehotTest$entity_id = NULL
onehotTest$parent_id = NULL
onehotTest$normalized_name = NULL

########### test ####

fundObj$pre_money_currency_code = as.numeric(levels(fundObj$pre_money_currency_code))
fundObj$raised_currency_code
fundObj$funding_round_code
fundObj$funding_round_type
fundObj$region
fundObj$country_code
fundObj$status
fundObj$category_code

###################################################

###############################################
################## Plotting ###################
###############################################
ggplot(data = objects[c()]) + geom_point(mapping = aes(x = funding_rounds, y = funding_total_usd, color = category_code))

ggplot(data=testobj2) + geom_bar(mapping=aes(x=funding_rounds))

numericVars <- which(sapply(fundObj, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(fundObj, is.factor)) #index vector factor variables

all_numVar <- fundObj[, numericVars]
nzv <- nearZeroVar(all_numVar, saveMetrics= TRUE)
cor_numVar <- cor(nzv, use="complete.obs") #correlations of all numeric variables


corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)


######################################################################################################
######################### Model Building #############################################################
######################################################################################################

############################### XGBoost ####################################################

## 1. Using XGBoost as a classifier -> the outcome variable is `status`

fundObj$myresponse = as.numeric(fundObj$status)
fundObj$status = NULL

# mydata = one_hot(as.data.table(fundObj))
mydata = as.data.table(fundObj)

mydata$id = NULL
mydata$entity_id = NULL
mydata$parent_id = NULL
mydata$normalized_name = NULL
mydata$id.y = NULL
mydata$updated_at = NULL
mydata$raised_amount = NULL
mydata$raised_currency_code = NULL
mydata$pre_money_currency_code = NULL
mydata$pre_money_valuation = NULL
mydata$post_money_currency_code = NULL
mydata$post_money_valuation = NULL
mydata$first_investment_at = NULL
mydata$last_investment_at = NULL

# fouded_at
# closed_at
# first_investment_at
# last_investment_at
# first_funding_at
# last_funding_at
# first_milestone_at
# last_milestone_at
# first_funding_at
# last_funding_at
# funded_at

mydata$founded_at_year = year(mydata$founded_at)
mydata$founded_at_month = month(mydata$founded_at)
mydata$founded_at_day = day(mydata$founded_at)

mydata$closed_at_year = year(mydata$closed_at)
mydata$first_funding_at_year = year(mydata$first_funding_at)
mydata$last_funding_year = year(mydata$last_funding_at)
mydata$firsT_milestone_year = year(mydata$first_milestone_at)
mydata$last_milestone_year = year(mydata$last_milestone_at)
mydata$funded_at_year = year(mydata$funded_at)


# mydata$_year = year(mydata$)
# mydata$_month = month(mydata$)
# mydata$_day = day(mydata$)


# mydata= onehotTest

model.type = "C"


# mydata$myresponse=mydata$status #Substitute "Status" with the name of your response variable
# mydata$status=NULL #Substitute "Status" with the name of your response variable

if (model.type=="C") (table(mydata$myresponse))

#The code below redefines the levels of categorical outcome to 0,1,...
if (model.type=="C"){
  
  if (is.character(mydata$myresponse)){
    mydata$myresponse=as.factor(mydata$myresponse)}
  
  mydata$myresponse=as.numeric(mydata$myresponse)
  
  if (min(mydata$myresponse)!=0){ 
    if (min(mydata$myresponse)>0)
      (mydata$myresponse=mydata$myresponse-min(mydata$myresponse)) else 
        (mydata$myresponse=mydata$myresponse+abs(min(mydata$myresponse)))}
  
  
  unique.vals=unique(mydata$myresponse)
  unique.vals=unique.vals[order(unique.vals)]
  
  if (sum(unique.vals==seq(0,length(unique.vals)-1))!=length(unique(unique.vals))){
    
    j=0
    
    for (i in unique.vals){
      mydata$myresponse[mydata$myresponse==i]=j
      j=j+1
    }
  }
}

if (model.type=="C") (table(mydata$myresponse))


#END OF RESPONSE REDEFINITION

#In the following statements substitute the names after "$" sign with the names of predictors
#in your data that are categorical but are read into R in a different format. If there are no such 
#variables in your data, then ignore.

#START OF PREDICTOR TRANSFORMATION

#mydata$xyz=as.factor(mydata$xyz)
#mydata$xyz=as.factor(mydata$xyz)
#mydata$xyz=as.factor(mydata$xyz)

#add statements similar to above as needed


#END OF PREDICTOR TRANSFORMATION

#The statements below remove all the variables that will not be passed to the tree algorithm
#as predictors. If no such redundant variables exist in your dataset, then the statements
#in the "REDUNDANT VARIABLE REMOVAL" section should be ignored.

#START OF REDUNDANT VARIABLE REMOVAL

#mydata$xyz=NULL    #Substitute "xyz" with the name of the variable in your data that 
#will not be passed to the tree algorithm. Add as many statements similar 
#to this as needed.

#END OF REDUNDANT VARIABLE REMOVAL

#############################################################################################
#####################################ATTENTION###############################################
#############################################################################################

#######################IF THE ABOVE MODIFICATIONS ARE MADE CORRECTLY,########################
####AT THIS POINT "MYDATA" DATA FRAME SHOULD CONTAIN ONLY THE PREDICTORS AND THE OUTCOME.#### 
####IN CASE IT CONTAINS ANYTHING MORE OR LESS, THE CODE BELOW WILL NOT FUNCTION PROPERLY.####
#############################################################################################

str(mydata) #make sure the structure of your data reflects all the modifications made above

#Start 80-20 partition
numpredictors=dim(mydata)[2]-1

numfac=0

for (i in 1:numpredictors) {
  if ((is.factor(mydata[,i]))){
    numfac=numfac+1} 
}

#End finding the number of categorical predictors 

nobs=dim(mydata)[1]


if (model.type=="R") {
  
  #Below is the setup for stratified 80-20 holdout sampling for a Regression Tree
  
  train_size=floor(0.8*nobs)
  test_size=nobs-train_size
  
} else {
  
  #Below is the setup for stratified 80-20 holdout sampling for a Classification Tree
  
  prop = prop.table(table(mydata$myresponse))
  length.vector = round(nobs*0.8*prop)
  train_size=sum(length.vector)
  test_size=nobs-train_size
  class.names = as.data.frame(prop)[,1]
  numb.class = length(class.names)}


resample=1
RNGkind(sample.kind = "Rejection")
set.seed(1) #sets the seed for random sampling

while (resample==1) {
  
  
  if (model.type=="C") {
    
    train_index = c()
    
    for(i in 1:numb.class){
      index_temp = which(mydata$myresponse==class.names[i])
      train_index_temp = sample(index_temp, length.vector[i], replace = F)
      train_index = c(train_index, train_index_temp)
    }} else {
      train_index=sample(nobs,train_size, replace=F)
    }
  
  mydata_train=mydata[train_index,] #randomly select the data for training set using the row numbers generated above
  mydata_test=mydata[-train_index,]#everything not in the training set should go into testing set
  
  right_fac=0 #denotes the number of factors with "right" distributions (i.e. - the unique levels match across mydata, test, and train data sets)
  
  
  for (i in 1:numpredictors) {
    if (is.factor(mydata_train[,i])) {
      if (sum(as.vector(unique(mydata_test[,i])) %in% as.vector(unique(mydata_train[,i])))==length(unique(mydata_test[,i])))
        right_fac=right_fac+1
    }
  }
  
  if (right_fac==numfac) (resample=0) else (resample=1)
  
}

dim(mydata_test) #confirms that testing data has only 20% of observations
dim(mydata_train) #confirms that training data has 80% of observations
#End 80-20 partition


#One-hot encoding of factor predictors for training and test sets separately

if (sum(sapply(mydata_train, is.factor))>0) {
  
  only.fac.train=mydata_train[,sapply(mydata_train, is.factor), drop=FALSE]
  only.fac.test=mydata_test[,sapply(mydata_test, is.factor), drop=FALSE]
  
  non.fac.train=mydata_train[,setdiff(colnames(mydata_train), colnames(only.fac.train))]
  non.fac.test=mydata_test[,setdiff(colnames(mydata_test), colnames(only.fac.test))]
  
  if (length(only.fac.train)>0) {
    dummy.train <- one_hot(as.data.table(only.fac.train))
    dummy.test  <- one_hot(as.data.table(only.fac.test))
    mydata_train=cbind(non.fac.train, dummy.train)
    mydata_test=cbind(non.fac.test, dummy.test)}
}


#Set the parameters for cross-validation and xgboost.
#You can try different values for nthread, max_depth, eta, gamma, etc., and see if you get lower prediction error.

#Start of Hyperparameter Setup

if ((model.type=="C") & (length(unique(mydata$myresponse))==2)) {
  cl.obj="binary:logistic"
  cl.eval.metric="error"} else {
    if ((model.type=="C") & (length(unique(mydata$myresponse))>2)) {
      
      cl.obj="multi:softmax"
      cl.eval.metric="merror"}}

if (model.type=="C") {
  
  if ((length(unique(mydata$myresponse))==2)) {
    
    param.cl       = list("objective" = cl.obj, 
                          #"num_class"= length(unique(mydata$myresponse)),
                          "eval_metric" = cl.eval.metric, 
                          "nthread" = 8,   			 # number of threads to be used 
                          "max_depth" = 1,    		 # maximum depth of tree 
                          "eta" = 0.3,    			 # step size shrinkage 
                          "gamma" = 0,    			 # minimum loss reduction 
                          "subsample" = 1,    		 # part of data instances to grow tree 
                          "colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
                          "min_child_weight" = 5,  		 # minimum sum of instance weight needed in a child 
                          "early_stopping_rounds"=10
    )
  } else {
    
    param.cl       = list("objective" = cl.obj, 
                          "num_class"= length(unique(mydata$myresponse)),
                          "eval_metric" = cl.eval.metric, 
                          "nthread" = 8,   			 # number of threads to be used 
                          "max_depth" = 1,    		 # maximum depth of tree 
                          "eta" = 0.3,    			 # step size shrinkage 
                          "gamma" = 0,    			 # minimum loss reduction 
                          "subsample" = 1,    		 # part of data instances to grow tree 
                          "colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
                          "min_child_weight" = 5,  		 # minimum sum of instance weight needed in a child 
                          "early_stopping_rounds"=10
    )
  }
} else {
  
  param.reg       = list("objective" = "reg:squarederror", 
                         "eval_metric" = "mape", 
                         "nthread" = 8,   			 # number of threads to be used 
                         "max_depth" = 1,    		 # maximum depth of tree 
                         "eta" = 0.3,    			 # step size shrinkage 
                         "gamma" = 0,    			 # minimum loss reduction 
                         "subsample" = 1,    		 # part of data instances to grow tree 
                         "colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
                         "min_child_weight" = 5,  		 # minimum sum of instance weight needed in a child 
                         "early_stopping_rounds"=10
  )
}

#End of Hyperparameter Setup


#Identify the Predictors and the dependent variable, aka label.
predictors = setdiff(colnames(mydata_train), "myresponse")

#Save the training output variable into "label"
label=mydata_train$myresponse

#########################################################################################################
# Step 1: Run a Cross-Validation to identify the round with the minimum loss or error.
#         Note: xgboost expects the data in the form of a numeric matrix.
##############################################################################################################################

set.seed(100)

if (model.type=="C"){
  bst.cv = xgb.cv(
    param=param.cl,
    data = as.matrix(mydata_train[,predictors]),
    label = label,
    nfold = 10,
    nrounds=cv.nround,
    prediction=T,
    verbose=F)
} else {
  
  bst.cv = xgb.cv(
    param=param.reg,
    data = as.matrix(mydata_train[,predictors]),
    label = label,
    nfold = 10,
    nrounds=cv.nround,
    prediction=T,
    verbose=F)}

#Find where the minimum loss occurred

if (model.type=="C"){
  if ((length(unique(mydata$myresponse))==2)) {
    
    min.error.idx = bst.cv$evaluation_log$iter[which.min(bst.cv$evaluation_log$test_error_mean)]} else {
      min.error.idx = bst.cv$evaluation_log$iter[which.min(bst.cv$evaluation_log$test_merror_mean)]}} else {
        
        min.error.idx = bst.cv$evaluation_log$iter[which.min(bst.cv$evaluation_log$test_mape_mean)]}

cat ("Minimum error occurred in round : ", min.error.idx, "\n")

# Minimum error
print(min(bst.cv$evaluation_log[,4]))

#Plot the CV error vs tree order

dd=as.data.frame(bst.cv$evaluation_log)
ylim.min=min(c(dd[,2], dd[,4]))
ylim.max=max(c(dd[,2], dd[,4]))

plot(dd[,2], col="blue", type="l", ylab="Error", xlab="Tree Number", main="Cross Validation Error vs Trees", ylim=c(ylim.min, ylim.max))
lines(dd[,4], col="red")
abline(v=min.error.idx, col="black", lwd=1, lty=2)


##############################################################################################################################
# Step 2: Train the xgboost model using min.error.idx found above.
#         Note, we have to stop at the round where we get the minimum error.

##############################################################################################################################

set.seed(100)

if (model.type=="C"){
  
  bst = xgboost(
    param=param.cl,
    data =as.matrix(mydata_train[,predictors]),
    label = label,
    #early_stopping_rounds=10,
    nrounds=min.error.idx,
    verbose=F)} else {
      
      bst = xgboost(
        param=param.reg,
        data =as.matrix(mydata_train[,predictors]),
        label = label,
        #early_stopping_rounds=10,
        nrounds=min.error.idx,
        verbose = F)} 


# Make prediction on the testing data.
mydata_test$prediction = predict(bst, as.matrix(mydata_test[,predictors]))

if ((model.type=="C") & length(unique(mydata$myresponse))==2) {
  mydata_test$prediction=0*(mydata_test$prediction<=0.5)+1*(mydata_test$prediction>0.5)
}


#LIME plots below
#For details see resources below for excellent explanations:
#https://uc-r.github.io/lime 
#https://christophm.github.io/interpretable-ml-book/lime.html 


#Below provide the row number of the testing set which you would like LIME to explain
row.to.explain=2

set.seed(100)
lime_to_explain <-mydata_test[row.to.explain,predictors] #Explaining the row stored in 'row.to.explain'
explainer <- suppressWarnings(lime(mydata_train, model = bst)) #specifying that the training model will be used for explanations


if (model.type=="R"){
  explanation <- explain(
    lime_to_explain, #the explanations will be given for these observations
    explainer, 
    n_features=5, #the top 5 features based on forward selection will be used in explanations
    feature_select = "forward_selection", #see above
    
    dist_fun = "euclidean",
    
    kernel_width = 0.5,
    
    
    n_permutations = 500 #for each obs in "lime_to_explain" there will be 5,00 permutations created
    #based on the data contained in "explainer", i.e. based on the variables
    #of training set. In other words, for each test set observation, 5K obs
    #are created using the distributions of the training data columns. Those then
    #are used to fit the local model in the vicinity of the test set obs in question.
  )
  
} else {
  
  explanation <- explain(
    lime_to_explain, #the explanations will be given for these observations
    explainer,
    n_labels = length(unique(mydata_test$myresponse)),
    n_features=5, #the top 5 features based on forward selection will be used in explanations
    feature_select = "forward_selection", #see above
    
    dist_fun = "euclidean",
    
    kernel_width = 0.5,
    
    
    n_permutations = 500 #for each obs in "lime_to_explain" there will be 5,00 permutations created
    #based on the data contained in "explainer", i.e. based on the variables
    #of training set. In other words, for each test set observation, 5K obs
    #are created using the distributions of the training data columns. Those then
    #are used to fit the local model in the vicinity of the test set obs in question.
  )
  
  
}  

plot_features(explanation)


#Summarize the predictive accuracy

if (model.type=="C"){
  #Compute the accuracy of predictions.
  CrossTable(mydata_test$myresponse,mydata_test$prediction,prop.chisq=F,prop.t=F)} else {
    
    rmse=sqrt(mean((mydata_test$myresponse-mydata_test$prediction)^2))
    mape=100*mean(abs(mydata_test$myresponse-mydata_test$prediction)/mydata_test$myresponse)
    
    print(paste("RMSE: ", round(rmse,2)))
    print(paste("MAPE: ", round(mape,2)))
  }



