library(haven)
library(rcompanion)
library(dplyr)
Europe_Data_Waves_Only<-  read_sav("/Volumes/Maxtor/Europe Data Waves Only Binary 3.sav")
View(Europe_Data_Waves_Only)
YY=Europe_Data_Waves_Only$Vaccine_Accept_Binary
X=as.matrix(Europe_Data_Waves_Only)
XX=cbind(X[,seq(8,77,1)], X[,seq(81,103,1)], X[,seq(109,127,1)], X[,seq(130,131,1)],X[,133], X[,seq(183,185,1)],X[,189], YY)
XX[XX=="NA"]=NA
XX[XX==-1]=NA
XX[XX==-2]=NA
rrr=dim(XX)
Y=rep(0,rrr[2]-1)
Z=rep(0,rrr[2]-1)
for(i in seq(1,rrr[2]-1,1)){Y[i]= chisq.test(table(XX[,i],XX[,rrr[2]]))$p.value
Z[i] = as.numeric(cramerV(table(XX[,i],XX[,rrr[2]])))}
R=cbind(Y,Z)
View(R)


names<-names(final_df_wout_nulls)
len<-length(names)
attributes<-vector()
removed_attributes<-vector()


for (i in 1:len){
  attributes[i]=names[i]
}
attributes<-attributes[attributes !="problem_gambler"]




C<-final_df_wout_nulls$problem_gambler

#Step1: pairwise chisquare between respone and all predictors

chisq_matrix<-matrix(0,nrow = length(attributes),2)

for (i in 1:length(attributes)){
  var1<-final_df_wout_nulls[,names %in% c(attributes[i])]
  p_value_chisq<-chisq.test(var1,C)$p.value
  chisq_matrix[i,2]<-p_value_chisq
  chisq_matrix[i,1]<-attributes[i]
  
  }


chisq_df<-as.data.frame(chisq_matrix)

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(chisq_df)


#Step 2: CramerV with response and predictors

cramer_response_matrix<-matrix(0,nrow = length(attributes),2)


for (i in 1:length(attributes)){
  var1<-final_df_wout_nulls[,names %in% c(attributes[i])]
  cramer<-cramerV(table(var1,C))
  cramer_response_matrix[i,2]<-cramer
  cramer_response_matrix[i,1]<-attributes[i]
  
}

cramer_response_matrix
colnames(cramer_response_matrix)<-c("attribute","cramer")
cramer_response_df<-as.data.frame(cramer_response_matrix)


cramer_subset<-cramer_response_df[cramer_response_df$cramer >0.25]
  
cramer_subset<-cramer_response_df %>% 
  filter(cramer > 0.2)

write.excel(cramer_response_df)

attributes_cramer_response<-cramer_subset$attribute

#Step 3: Pairwise Chisq between variables in attributes_cramer_response


chisq_pairwise<-matrix(0,nrow = length(attributes_cramer_response),ncol=length(attributes_cramer_response))
rownames(chisq_pairwise)<-attributes_cramer_response


for (i in 1:length(attributes_cramer_response)){
  for (j in 1:length(attributes_cramer_response)) {
    
    if(i!=j){
      var1<-final_df_wout_nulls[,names %in% c(attributes_cramer_response[i])]
      var2<-final_df_wout_nulls[,names %in% c(attributes_cramer_response[j])]
      chisq_pairwise[i,j]<-chisq.test(var1,var2)$p.value
    }
  }
}



#Step 4: Pairwise CramerV between variables in attributes_cramer_response


cramer_pairwise<-matrix(0,nrow = length(attributes_cramer_response),ncol=length(attributes_cramer_response))
rownames(cramer_pairwise)<-attributes_cramer_response


for (i in 1:length(attributes_cramer_response)){
  for (j in 1:length(attributes_cramer_response)) {
    
    
      var1<-final_df_wout_nulls[,names %in% c(attributes_cramer_response[i])]
      var2<-final_df_wout_nulls[,names %in% c(attributes_cramer_response[j])]
      cramer_pairwise[i,j]<-cramerV(table(var1,var2))
    
  }
}

cramer_pairwise


cramer_pairwise_df<-as.data.frame(cramer_pairwise)
write.excel(cramer_pairwise_df)




#Step 5: Remove associated variables


removed_attributes<-vector()

for (i in 1:length(attributes_cramer_response)){
  for (j in 1:length(attributes_cramer_response)) {
    
    if(i!=j){
      var1<-final_df_wout_nulls[,names %in% c(attributes_cramer_response[i])]
      var2<-final_df_wout_nulls[,names %in% c(attributes_cramer_response[j])]
      cramer_pairwise[i,j]<-cramerV(table(var1,var2))
      
      if(cramer_pairwise[i,j] > 0.2){
        v1cv<-cramerV(table(var1,C))
        v2cv<-cramerV(table(var2,C))
        
        if (v1cv>v2cv){
          removed_attributes<-append(removed_attributes,attributes_cramer_response[j])#remove var2
        }else{
          removed_attributes<-append(removed_attributes,attributes_cramer_response[i])#remove var1
        }
      }
    }
  }
}

unique(removed_attributes)



chosen_attributes<-attributes_cramer_response[!(attributes_cramer_response %in% unique(removed_attributes))]

log_train<-train[,names(train) %in% append(chosen_attributes,"problem_gambler")]
log_test<-test[,names(test) %in% append(chosen_attributes,"problem_gambler")]

log_reg_model_w_mc_diag <- glm(problem_gambler ~.,family=binomial(link='logit'),data=log_train)

summary(log_reg_model_w_mc_diag)

#getting training confusion matrix
train_log_reg_preds<-predict(log_reg_model_w_mc_diag,log_train,type="response")
train_log_reg_predicted_class_mc<-as.factor(ifelse(train_log_reg_preds>0.5,"TRUE","FALSE"))
confusionMatrix(data=train_log_reg_predicted_class_mc,reference = log_train$problem_gambler,positive="TRUE")

str(train_log_reg_predicted_class_mc)


head(train_log_reg_predicted_class_mc)

##using only variable which was returned after running multicollinearity diagnostics

log_reg_preds_mc<-predict(log_reg_model_w_mc_diag,log_test,type="response")
log_reg_predicted_class_mc<-as.factor(ifelse(log_reg_preds_mc>0.5,"TRUE","FALSE"))
confusionMatrix(data=log_reg_predicted_class_mc,reference = test$problem_gambler,positive="TRUE")









############


for (i in 1:length(attributes)){
  for (j in 1:length(attributes)) {
    
    if(i!=j){
      var1<-final_df_wout_nulls[,names %in% c(attributes[i])]
      var2<-final_df_wout_nulls[,names %in% c(attributes[j])]
      pvalue<-chisq.test(var1,var2)$p.value
      
      if (pvalue<0.05){#null of chisq is that there is no association
        v1cv<-cramerV(table(var1,C))
        v2cv<-cramerV(var2,C)
        
        if (v1cv>v2cv){
          removed_attributes<-append(removed_attributes,attributes[j])#remove var2
          }else{
            removed_attributes<-append(removed_attributes,attributes[i])#remove var1
          }
        
        
      }

    }
  }
  
}

unique(removed_attributes)
attributes

setdiff(unique(removed_attributes),attributes)
length(unique(removed_attributes))
length(attributes)

#only log_avg_deposit_count_per_active_day not removed


chisq_matrix<-matrix(0,nrow = length(attributes),ncol=length(attributes))
cramer_matrix<-matrix(0,nrow = length(attributes),ncol=1 )
cramer_pairwise<-matrix(0,nrow = length(attributes),ncol=1 )


for (i in 1:length(attributes)){
  var1<-final_df_wout_nulls[,names %in% c(attributes[i])]
  length(var1)
  length(C)
  
  cramer_matrix[i,1]<-cramerV(table(var1,C))
  for (j in 1:length(attributes)) {
    
    
    var2<-final_df_wout_nulls[,names %in% c(attributes[j])]
    
    p_value<-chisq.test(var1,var2)$p.value
    independent<-ifelse(p_value<0.05,"TRUE","FALSE")
    chisq_matrix[i,j]<-independent
    cramer_pairwise[i,j]<-cramerV(table(var1,var2))
    
  }
  

}

chisq_matrix
cramer_matrix[which.max(cramer_matrix)]
attributes[18]
