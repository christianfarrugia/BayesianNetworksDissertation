library(bnclassify)
library(bnlearn)#for plot
library(Rcpp)
library(arules)
library(caret)#in addition to other things, this will be used for the confusion matrix
library(Rgraphviz)
library(adabag)#for adaboost
library(fastAdaboost)
library(PRROC)#for ROC curve
library(pROC)
library(randomForest)

help(bnc)

df<-read.csv("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Dataset\\Dataset_only_v2-csv.csv",header = TRUE)
length(unique(df$h_player_id))
#hist(df$log_avg_deposit_amount_per_active_day)
names(df)[names(df) == "lock_flag"] <- "problem_gambler"
names(df)[names(df) == "segment_value_on_exclusion_date"] <- "segment_value"

#removing all columns which are surely not useful. I also removed week_after_payday since it does not make sense in the network to include it.
#the estimated probability p(week after payday = true|problem gambler = true) is approximately 0.8 which reflects 1/4 weeks.
df_sub<-df[ , !names(df) %in% c("h_player_id","lock_removed_date","lock_started","lock_ended","lock_planned_duration","is_definite_self_exclusion","week_after_payday","payday_week","age")] 

#use the one below for the ROC
df_sub<-df[ , !names(df) %in% c("h_player_id","lock_removed_date","lock_started","lock_ended","lock_planned_duration","is_definite_self_exclusion","week_after_payday","payday_week")] 

#converting char columns to numeric
df_sub[, names(df_sub) %in% c("avg_stake","first_accepted_deposit_amount","log_total_deposit_count","cancelled_withdrawals")]<-lapply(df_sub[, names(df_sub) %in% c("avg_stake","first_accepted_deposit_amount","log_total_deposit_count","cancelled_withdrawals")],as.numeric)
help(discretizeDF)
#discretizing
disc_df<-discretizeDF(df_sub)

#removing cancelled_withdrawals because dicretizeDF only gave 1 bin
final_df<-disc_df[,!names(disc_df) %in% c("cancelled_withdrawals")]

#not for new models
final_df$segment_value[final_df$segment_value=="No Value Segment"]<-"NV"
final_df$segment_value[final_df$segment_value=="SVP Max"]<-"SVP MAX"

final_df[]<- lapply( final_df, factor)


str(final_df)

View(final_df)
#removing_nulls
length(final_df$problem_gambler)#131432
#final_df_wout_nulls<-final_df[complete.cases(final_df),]

#use the below command instead of the above
final_df_wout_nulls<-na.omit(final_df)


#write.csv(final_df_wout_nulls,"C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220228-final-df.csv", row.names = FALSE)
final_df_wout_nulls<-read.csv("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220228-final-df.csv",header = TRUE)

final_df_wout_nulls<-lapply(final_df_wout_nulls,factor)
final_df_wout_nulls<-as.data.frame(final_df_wout_nulls)
length(final_df_wout_nulls$problem_gambler)#130588

smp_size <- floor(0.75 * nrow(final_df_wout_nulls))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(final_df_wout_nulls)), size = smp_size)
#dividing into testing and training
train <- final_df_wout_nulls[train_ind, ]
test <- final_df_wout_nulls[-train_ind, ]

View(train)

#sum(final_df_wout_nulls$problem_gambler == "TRUE")

#sum(final_df_wout_nulls$problem_gambler == "FALSE")


length(final_df_wout_nulls$problem_gambler)

#write.csv(train,"C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220228-final-train-df.csv", row.names = FALSE)
#write.csv(test,"C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220228-final-test-df.csv", row.names = FALSE)

str(train)
levels(final_df_wout_nulls$category_segment)

test_accuracy<-function(actual,predicted){
  len<-length(actual)
  cc<-vector()
  for (l in 1:len){
    if(predicted[l] == actual[l]){
      cc[l]<-1
    }else{
      cc[l]<-0
    }
  }
  acc<-sum(cc)/length(cc)
  return(acc)
}
#function to convert conditional probabilities into predictions
convert_cp_to_factor<-function(conditional_prob,threshold = 0.5){
  predicted<-vector()
  for (j in 1:nrow(conditional_prob)) {
    if(conditional_prob[j,1]<1-threshold){
      predicted[j]<-"TRUE"
    }else{
      predicted[j]<-"FALSE"
    }
  }
  return(as.factor(predicted))
}


#######Non-modified CL-adapt
CL_adapt_orig<-bnc('tan_cl', 'problem_gambler',dataset =train , smooth = 0, dag_args = list(score = 'loglik'))
CL_adapt_orig<-bnc('tan_cl', 'problem_gambler',dataset =train , smooth = 0)


plot(CL_adapt_orig)
orig_best_preds<-predict(CL_adapt_orig,test,prob=TRUE)
orig_best_predicted<-convert_cp_to_factor(orig_best_preds)
orig_best_accuracy<-test_accuracy(test$problem_gambler,orig_best_predicted)

############################################################
#Where algorithm actually starts




names<-names(final_df_wout_nulls)
len<-length(names)
#the algorithm depends on which starting variable you choose
starting_variable<-attributes[1]
attributes<-vector()
for (i in 1:len){
  attributes[i]=names[i]
}
attributes<-attributes[attributes !="problem_gambler"]

models<-vector(mode = "list",length = length(attributes))#this will store the optimal model for each iteration. The difference between one iteration and 
#another will be the starting variable.
accuracy_vec<-vector()
print(attributes[1])

for (j in 1:length(attributes)) {
  starting_variable<-attributes[j]
  epsilon<-0.01
  diff<-1
  
  attributes_not_being_used<-sample(attributes,length(attributes),replace = FALSE)#sample to introduce randomness
  attributes_not_being_used<-attributes_not_being_used[attributes_not_being_used !=starting_variable]
  
  chosen_attributes<-vector()
  chosen_attributes[1]<-starting_variable
  
  current_best_attribute<-starting_variable
  
  #old_df<-final_df_wout_nulls[,names(final_df_wout_nulls) %in% c("problem_gambler",starting_variable,attributes_not_being_used[1])]
  old_df<-train[,names(train) %in% c("problem_gambler",starting_variable,attributes_not_being_used[1])]
  old_test_df<-test[,names(test) %in% c("problem_gambler",starting_variable,attributes_not_being_used[1])]
  old_bn<-bnc('tan_cl', 'problem_gambler',dataset =old_df , smooth = 0, dag_args = list(score = 'loglik'))
  #old_accuracy<-cv(old_bn,old_test_df,k=10)
  old_preds<-predict(old_bn,old_test_df,prob=TRUE)
  old_predicted<-convert_cp_to_factor(old_preds)
  old_accuracy<-test_accuracy(old_test_df$problem_gambler,old_predicted)
  current_accuracy<-old_accuracy
  
  
  while (diff>epsilon) {
    
    
    for (attribute in attributes_not_being_used) {
      #add attribute to list of currently chosen attributes
      new_df<-train[,names(train) %in% c("problem_gambler",chosen_attributes,attribute)]
      new_test_df<-test[,names(test) %in% c("problem_gambler",chosen_attributes,attribute)]
      #fit tan_cl model on the new dataset
      new_bn<-bnc('tan_cl', 'problem_gambler',dataset =new_df , smooth = 0, dag_args = list(score = 'loglik'))
      #new_accuracy<-cv(new_bn,new_test_df,k=10)
      new_preds<-predict(new_bn,new_test_df,prob=TRUE)
      new_predicted<-convert_cp_to_factor(new_preds)
      new_accuracy<-test_accuracy(new_test_df$problem_gambler,new_predicted)
      
      #decide whether BN with current attribute is better than BN with previous attribute
      if (new_accuracy>current_accuracy){
        current_accuracy<-new_accuracy
        new_model<-new_bn
        current_best_attribute<-attribute
      }
    } 
    #decide whether to add an attribute or not
    diff<-current_accuracy-old_accuracy
    if(diff>epsilon){
      old_bn<-new_model
      old_accuracy<-current_accuracy
      attributes_not_being_used<-attributes_not_being_used[attributes_not_being_used!=current_best_attribute]
      chosen_attributes<-append(chosen_attributes,current_best_attribute)
    }else{
      chosen_model<-old_bn
      models[[j]]<-chosen_model
      accuracy_vec[j]<-old_accuracy
      #store old accuracy
    }
    
  }#of the while
  
}#of the for

###### best cl adapt model####

#saveRDS(models,file = "C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220129epsilon_1_percent_models.rds")
#loaded_models<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220129epsilon_1_percent_models.rds")
#loaded_models<-load("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\models.rds")

#saveRDS(accuracy_vec,file="C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220129epsilon_1_percent_accuracy_vec.rds")
#loaded_accuracy<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220129epsilon_1_percent_accuracy_vec.rds")


#with new df


saveRDS(models,file = "C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220308epsilon_1_percent_models.rds")
loaded_models<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220308epsilon_1_percent_models.rds")
#loaded_models<-load("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\models.rds")

saveRDS(accuracy_vec,file="C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220308epsilon_1_percent_accuracy_vec.rds")
loaded_accuracy<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220308epsilon_1_percent_accuracy_vec.rds")



best_model<-loaded_models[[which.max(loaded_accuracy)]]
plot(best_model,fontsize=30,radius=700)

################################################################################################

attributes[1]
k<-1
for (model in loaded_models){
  print(k)
  print(model$.families)
  plot(model,fontsize=40)
  k<-k+1
}

max_index<-which.max(accuracy_vec)
accuracy_vec[max_index]
print(models[[1]]$.params)
#params_ij = P(row i | col J)

length(loaded_models)
length(loaded_accuracy)

top6_accuracy<-head(ord <- order(accuracy_vec, decreasing = TRUE))
top6_models<-vector(mode="list",length = length(top6_accuracy))
for (j in 1:length(top6_accuracy)){
  top6_models[[j]]<-models[[top6_accuracy[j]]]
}

for (topmodel in top6_models){
  plot(topmodel,fontsize=40)
  print(topmodel$.families)
}

#saving models

saveRDS(models,file = "C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\epsilon_1_percent_models.rds")
loaded_models<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\epsilon_1_percent_models.rds")
#loaded_models<-load("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\models.rds")

saveRDS(accuracy_vec,file="C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\epsilon_1_percent_accuracy_vec.rds")
loaded_accuracy<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\epsilon_1_percent_accuracy_vec.rds")


saveRDS(models,file = "C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\test_acc_models.rds")
loaded_models<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\test_acc_models.rds")
#loaded_models<-load("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\models.rds")

saveRDS(accuracy_vec,file="C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\test_acc_accuracy_vec.rds")
loaded_accuracy<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\test_acc_accuracy_vec.rds")



best_model<-loaded_models[[which.max(loaded_accuracy)]]
plot(best_model,fontsize=30,radius=700)

best_model$.params

c_week<-vector()
for (i in 1:nrow(best_train_df)) {
  if(best_train_df$week_after_payday[i]=="TRUE"){
    c_week[i]<-1
  }else{
    c_week[i]<-0
  }
}
sum(c_week)/nrow(best_train_df)

plot(best_model,fontsize=40)
plot(loaded_models[which.max(loaded_accuracy)],fontsize=40)
chosen_model<-loaded_models[[which.max(loaded_accuracy)]]
plot(chosen_model,fontsize=32,radius=250)
graphviz.plot(best_model)
#log_avg_stake_per_active_day,problem_gambler,season,country_code,brand,active_days,segment_value_on_exclusion_date,log_total_deposit_amount,log_avg_deposit_count_per_active_day,

#par(mar=c(1,1,1,1)) to solve error figure margin too large


best_test_df<-test[,names(test) %in% c("problem_gambler","country_code","active_days","segment_value","log_avg_stake_per_active_day","log_avg_deposit_count_per_active_day")]
best_train_df<-train[,names(test) %in% c("problem_gambler","country_code","active_days","segment_value","log_avg_stake_per_active_day","log_avg_deposit_count_per_active_day")]
cv(chosen_model,best_train_df,dag=FALSE,k=10)

chosen_model$.params

best_preds<-predict(best_model,best_test_df,prob=TRUE)
best_predicted<-convert_cp_to_factor(best_preds)
best_accuracy<-test_accuracy(best_test_df$problem_gambler,best_predicted)

best_training_preds<-predict(best_model,best_train_df,prob=TRUE)
best_training_predicted<-convert_cp_to_factor(best_training_preds)
best_training_accuracy<-test_accuracy(best_train_df$problem_gambler,best_training_predicted)

head(best_preds)

head(best_predicted)

#test confusion matrix
confusionMatrix(data=best_predicted,reference = best_test_df$problem_gambler,positive = "TRUE")

#train confusion matrix
confusionMatrix(data=best_training_predicted,reference = best_train_df$problem_gambler,positive = "TRUE")


View(train)
#roc curve

#using PRROC package
PRROC_obj <- roc.curve(scores.class0 = best_preds[,2], weights.class0=best_test_df$problem_gambler,
                       curve=TRUE)
plot(PRROC_obj)


predicted_prob<-vector()
for(j in 1:nrow(best_test_df) ){
  if(best_test_df$problem_gambler[j] =="TRUE"){
    predicted_prob[j]<-best_preds[j,2]#2nd col is true
  }else{
    predicted_prob[j]<-best_preds[j,1]
  }
}

options(scipen = 2)#to avoid exponentials

ROC_CL_adapt <- roc(best_test_df$problem_gambler,best_preds[,2],
                smoothed = FALSE,
                # arguments for ci
                ci=FALSE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE,legacy.axes = TRUE)




coords(ROC_CL_adapt, "best", ret = "threshold",best.method = "closest.topleft")
x_best<-coords(ROC_CL_adapt, "best", ret = "specificity",best.method = "closest.topleft")
y_best<-coords(ROC_CL_adapt, "best", ret = "sensitivity",best.method = "closest.topleft")
points(x_best, y_best, type = "p",pch=18,col=4,cex=2)


x_best
y_best
plot(ROC_CL_adapt)
plot(ROC_CL_adapt,col=3,add=TRUE)

ROC_CL_adapt$ci

head(predicted_prob)
head(best_preds)
head(best_test_df$problem_gambler)
head(best_preds)
head(best_predicted)
head(best_test_df$problem_gambler)
#need to output list of features used in each model
#don't always choose category segment first

for (model in loaded_models){
  plot(model,fontsize=40)
}


#########adaboost#################
#check this out for prediction probs
#https://stats.stackexchange.com/questions/188616/how-can-we-calculate-roc-auc-for-classification-algorithm-such-as-random-forest


adamodel_fast<-adaboost(problem_gambler~., data=train, 100)
saveRDS(adamodel_fast,file = "C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220202-AdaBoost.rds")

adamodel_fast_loaded<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220202-AdaBoost.rds")


  
ada_pred<-predict(adamodel_fast_loaded,test,prob=TRUE)

ada_pred_training<-predict(adamodel_fast_loaded,train)
head(ada_pred_training$class)
confusionMatrix(data=ada_pred_training$class,reference = train$problem_gambler,positive = "TRUE")


head(ada_pred)


###############Random Forest##############################

random_forest<-randomForest(problem_gambler~.,data=train,ntree=500)
saveRDS(random_forest,file = "C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220129-random-forest.rds")
random_forest<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220129-random-forest.rds")

rf_preds<-predict(random_forest,test,type="prob")
rf_predicted_values<-predict(random_forest,test)
head(rf_predicted_values)
head(rf_preds)

##for training data
training_rf_preds<-predict(random_forest,train,type="prob")
training_rf_predicted_values<-predict(random_forest,train)
#training confusion matrix

confusionMatrix(data=training_rf_predicted_values,reference = train$problem_gambler,positive = "TRUE")




#conf matrix for random forest


confusionMatrix(data=rf_predicted_values,reference = best_test_df$problem_gambler,positive = "TRUE")


roc_random_forest<-roc(test$problem_gambler,rf_preds[,2],
    smoothed = FALSE,
    # arguments for ci
    ci=FALSE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE,legacy.axes = TRUE)

plot(roc_random_forest,col=4,add=TRUE)

coords(roc_random_forest, "best", ret = "threshold",best.method = "closest.topleft")
coords(roc_random_forest, "best", ret = "specificity",best.method = "closest.topleft")
coords(roc_random_forest, "best", ret = "sensitivity",best.method = "closest.topleft")


legend("bottomright", legend=c("CL-adapt", "Random Forest"),
       col=c("red", "blue"),lty=1, cex=0.8)

head(random_forest$votes)


#####################################################
#naive bayes
nb_model<-bnc('nb', 'problem_gambler',dataset =train , smooth = 0)
cv(nb_model,test,dag=FALSE,k=10)
nb_pred<-predict(nb_model,test,prob=TRUE)
nb_preds<-convert_cp_to_factor(nb_pred)
test_accuracy(test$problem_gambler,nb_preds)
head(nb_pred)


############################
#logistic regression
contrasts(test$problem_gambler)
log_reg_model <- glm(problem_gambler ~.,family=binomial(link='logit'),data=train)

##using only variable which was returned after running multicollinearity diagnostics
log_reg_after_mc_diag<-glm(problem_gambler~log_avg_deposit_count_per_active_day,family=binomial(link='logit'),data=train)
summary(log_reg_after_mc_diag)
log_reg_preds_mc<-predict(log_reg_after_mc_diag,test,type="response")
log_reg_predicted_class_mc<-as.factor(ifelse(log_reg_preds_mc>0.5,"TRUE","FALSE"))
confusionMatrix(data=log_reg_predicted_class_mc,reference = test$problem_gambler,positive="TRUE")



saveRDS(log_reg_model,file = "C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220214-Log-Reg.rds")



log_reg_model<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220214-Log-Reg.rds")

summary(log_reg_model)

log_reg_preds<-predict(log_reg_model,test,type="response")
log_reg_predicted_class<-as.factor(ifelse(log_reg_preds>0.5,"TRUE","FALSE"))

log_reg_train_preds<-predict(log_reg_model,train,type="response")
log_reg_train_predicted_class<-as.factor(ifelse(log_reg_train_preds>0.5,"TRUE","FALSE"))
confusionMatrix(data=log_reg_train_predicted_class,reference = train$problem_gambler,positive="TRUE")

View(final_df_wout_nulls)
summary(log_reg_model)
log_reg_model$
test_accuracy(test$problem_gambler,log_reg_predicted_class)
head(log_reg_predicted_class)
confusionMatrix(data=log_reg_predicted_class,reference = test$problem_gambler,positive = "TRUE")
str(log_reg_preds)
View(log_reg_preds)



#####logistic regression predictions using spss####

log_reg_df<-read.csv("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\SPSS\\20220308-log-reg-test-preds.csv",header = TRUE)

log_reg_df$Predicted_class<-as.factor(log_reg_df$Predicted_class)
levels(log_reg_df$Predicted_class)

View(log_reg_df)

confusionMatrix(data=log_reg_df$Predicted_class,reference=test$problem_gambler,positive="TRUE")






