#Clear memory and objects 
rm(list = ls(all= T))

#Libraries required
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest",
      "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE")
install.packages(x) #Batch installation
lapply(x,require, character.only = T)
rm(x)

#Set working directory
setwd('C:/Users/ankit/Desktop/Ankit_Project')
getwd()

#Load and understand data
marketing_train = read.csv("marketing_tr.csv",header = T,na.strings = c(" ", "", "NA"))

#Explore and understand data
str(marketing_train)

#Missing data
missing_val=data.frame(apply(marketing_train,2,function(x){sum(is.na(x))})) ##Made a list of count of missing values
missing_val$columns=row.names(missing_val)
names(missing_val)[1]="Missing_percent"
missing_val$Missing_percent=(missing_val$Missing_percent/nrow(marketing_train))*100
missing_val=missing_val[order(-missing_val$Missing_percent),]#Ordering list of missing values by percentage
row.names(missing_val)=NULL
missing_val=missing_val[,c(2,1)]
write.csv(missing_val,"Missing.csv", row.names = F)
#Visualization of missing data percentage
ggplot(data = missing_val[1:3,], aes(x=reorder(columns, -Missing_percent),y = Missing_percent))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()


#Univariate Analysis
marketing_train$schooling[marketing_train$schooling %in% "illiterate"]= "unknown"
marketing_train$schooling[marketing_train$schooling %in% c("basic.4y","basic.6y","basic.9y","high.school","professional.course")] = "high.school"
marketing_train$default[marketing_train$default %in% "yes"] = "unknown"
marketing_train$default = as.factor(as.character(marketing_train$default))
marketing_train$marital[marketing_train$marital %in% "unknown"] = "married"
marketing_train$marital = as.factor(as.character(marketing_train$marital))
marketing_train$month[marketing_train$month %in% c("sep","oct","mar","dec")] = "dec"
marketing_train$month[marketing_train$month %in% c("aug","jul","jun","may","nov")] = "jun"
marketing_train$month = as.factor(as.character(marketing_train$month))
marketing_train$loan[marketing_train$loan %in% "unknown"] = "no"
marketing_train$loan = as.factor(as.character(marketing_train$loan))
marketing_train$schooling = as.factor(as.character(marketing_train$schooling))
marketing_train$profession[marketing_train$profession %in% c("management","unknown","unemployed","admin.")] = "admin."
marketing_train$profession[marketing_train$profession %in% c("blue-collar","housemaid","services","self-employed","entrepreneur","technician")] = "blue-collar"
marketing_train$profession = as.factor(as.character(marketing_train$profession))

## Correlation Plot 
numeric_index = sapply(marketing_train,is.numeric) #selecting only numeric
corrgram(marketing_train[,numeric_index], order= F,
         upper.panel=panel.pie, text.panel=panel.txt, main="Correlation Plot")

## Chi-squared Test of Independence
factor_index = sapply(marketing_train,is.factor)
factor_data=marketing_train[,factor_index]
for (i in 1:10)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$responded,factor_data[,i])))
  print("next variable")
}

## Dimension Reduction
corr_var = c("pdays","emp.var.rate","day_of_week","loan","housing")
#Here we're considering only values with p values <0.05 therefore rejecting variables
#with more than 0.05
#thus accepting null hypothesis and rejecting the variable from the model 
marketing_train_deleted = subset(marketing_train,select = names(marketing_train)
                                 [!names(marketing_train) %in% corr_var])
#Rejecting highly correlated variables leaving us with only 17 variables in our train dataset

## BoxPlots - Distribution and Outlier Check
numeric_data = marketing_train[,numeric_index]
cnames = colnames(numeric_data)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i),ggplot(aes_string(y = (cnames[i]), x = "responded"), data = subset(marketing_train))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="responded")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}

## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=2)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn7,gn9,ncol=2)
gridExtra::grid.arrange(gn10,gn11,ncol=2)

#Remove outliers using boxplot method
# for(i in c(1,11,12,13,15:21)){
 #  val = marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
  # marketing_train = marketing_train[which(!marketing_train[,i] %in% val),]
   #print(i)
  #print(val)
#}


#KNN imputation
set.seed(1234)
marketing_train_deleted = knnImputation(marketing_train_deleted, k = 5)
sum(is.na(marketing_train_deleted))
str(marketing_train_deleted)

## Data Partition for internal evalution (Stratified Sampling)
set.seed(1234)
train.index = createDataPartition(marketing_train_deleted$responded, p = .80, list = FALSE)
train = marketing_train_deleted[ train.index,]
test  = marketing_train_deleted[-train.index,]

########### Handling Unbalanced data by TOMEK & SMOTE ####################

## Creating dummy variables
set.seed(1234)
train$responded = ifelse(train$responded == "no",0,1)
dummy = dummy.data.frame(train)
tomek = ubTomek(dummy[,-30], dummy[,30])
model_train_tomek = cbind(tomek$X,tomek$Y)
names(model_train_tomek)[30] = "responded"

## TOMEK Indices 
removed.index = tomek$id.rm

# Converting responded to factor variable
train$responded = ifelse(train$responded == 0,"no","yes")
train$responded = as.factor(train$responded)
train_tomek = train[-removed.index,]

## SMOTE
set.seed(1234)
train_tomek_smote = SMOTE(responded ~ ., train_tomek, perc.over = 400)
table(train_tomek_smote$responded)

############################### Random Forest ######################################
## Normal Data
set.seed(1234)
rf_model_complete = randomForest(responded~., train, ntree = 500)
rf_test_predictions_complete = predict(rf_model_complete, test, type = "class")
conf_matrix_complete_rf = table(test$responded,rf_test_predictions_complete)
confusionMatrix(conf_matrix_complete_rf)

## Smoted Data
set.seed(1234)
rf_model_tomek_smote = randomForest(responded~., train_tomek_smote,
                                    ntree=500,mtry=2,sampsize=c(50,250),
                                    nodesize=1, rules = TRUE)

rf_test_predictions = predict(rf_model_tomek_smote, test, type="class")
conf_matrix_rf = table(test$responded,rf_test_predictions)
confusionMatrix(conf_matrix_rf)


## Matthews correlation coefficient
tn=as.double(conf_matrix_rf[1,1]);
fp=as.double(conf_matrix_rf[1,2]);
fn=as.double(conf_matrix_rf[2,1]);
tp=as.double(conf_matrix_rf[2,2]);
MCC = (tp*tn - fp*fn) / sqrt( (tp + fp)*(tp + fn)*(tn + fp)*(tn + fn))

## Variable Importance List
rf_var_imp = data.frame(rf_model_tomek_smote$importance)
rf_var_imp$Variables = row.names(rf_var_imp)
rf_var_imp = rf_var_imp[order(-rf_var_imp$MeanDecreaseGini),]

########################## C50 ##################
## Normal Data
set.seed(1234)
C50_model_complete <- C5.0(responded ~.,train, trials = 100, rules = TRUE)
summary(C50_model_complete)
C50_test_predictions_complete = predict(C50_model_complete, test, type="class")
conf_matrix_complete_c50 = table(test$responded,C50_test_predictions_complete)
confusionMatrix(conf_matrix_complete_c50)

## Smoted Data
set.seed(1234)
C50_model <- C5.0(responded ~.,train_tomek_smote, trials = 100)
summary(C50_model)
C50_test_predictions = predict(C50_model, test, type="class")
conf_matrix_c50 = table(test$responded,C50_test_predictions)
confusionMatrix(conf_matrix_c50)

## Matthews correlation coefficient
tn=as.double(conf_matrix_c50[1,1]);
fp=as.double(conf_matrix_c50[1,2]);
fn=as.double(conf_matrix_c50[2,1]);
tp=as.double(conf_matrix_c50[2,2]);
MCC = (tp*tn - fp*fn) / sqrt( (tp + fp)*(tp + fn)*(tn + fp)*(tn + fn))
