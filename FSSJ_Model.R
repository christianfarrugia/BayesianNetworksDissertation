##FSSJ algorithm
library(bnclassify)
library(bnlearn)#for plot
library(Rcpp)
library(arules)
library(caret)#in addition to other things, this will be used for the confusion matrix
library(Rgraphviz)
df<-read.csv("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Dataset\\Dataset_only_v2-csv.csv",header = TRUE)
names(df)[names(df) == "lock_flag"] <- "problem_gambler"
View(df)
names(df)
#removing all columns which are surely not useful
df_sub<-df[ , !names(df) %in% c("h_player_id","lock_removed_date","lock_started","lock_ended","lock_planned_duration","is_definite_self_exclusion")] 
#converting char columns to numeric
df_sub[, names(df_sub) %in% c("avg_stake","first_accepted_deposit_amount","log_total_deposit_count","cancelled_withdrawals")]<-lapply(df_sub[, names(df_sub) %in% c("avg_stake","first_accepted_deposit_amount","log_total_deposit_count","cancelled_withdrawals")],as.numeric)

#discretizing
disc_df<-discretizeDF(df_sub)
#removing cancelled_withdrawals because dicretizeDF only gave 1 bin
final_df<-disc_df[,!names(disc_df) %in% c("cancelled_withdrawals")]
final_df[] <- lapply( final_df, factor)

#removing_nulls
length(final_df$problem_gambler)#131432
final_df_wout_nulls<-final_df[complete.cases(final_df),]
length(final_df_wout_nulls$problem_gambler)#130588

smp_size <- floor(0.75 * nrow(final_df_wout_nulls))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(final_df_wout_nulls)), size = smp_size)
#dividing into testing and training
train <- final_df_wout_nulls[train_ind, ]
test <- final_df_wout_nulls[-train_ind, ]


fssj_model<-bnc("fssj","problem_gambler",train,smooth=0,dag_args =list(k=5,epsilon=0.01))
fssj_pred_probs<-predict(fssj_model,test,prob=TRUE)
fssj_preds<-predict(fssj_model,test,prob=FALSE)
head(fssj_preds)

fssj_model$.params

test_accuracy(test$problem_gambler,fssj_preds)

plot(fssj_model,fontsize=40)

best_preds<-predict(best_model,best_test_df,prob=TRUE)
best_predicted<-convert_cp_to_factor(best_preds)
best_accuracy<-test_accuracy(best_test_df$problem_gambler,best_predicted)

training_fssj_preds<-predict(fssj_model,train,prob=TRUE)
training_fssj_predicted<-convert_cp_to_factor(training_fssj_preds)
training_fssj_accuracy<-test_accuracy(train$problem_gambler,training_fssj_predicted)

#test confusion matrix
confusionMatrix(data=fssj_preds,reference = test$problem_gambler,positive = "TRUE")

#training confusion matrix
confusionMatrix(data=training_fssj_predicted,reference = train$problem_gambler,positive = "TRUE")


saveRDS(fssj_model,file="C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220130-fssj_model.rds")
fssj_model<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220130-fssj_model.rds")




ROC_FSSJ <- roc(test$problem_gambler,fssj_pred_probs[,2],
                    smoothed = FALSE,
                    # arguments for ci
                    ci=FALSE, ci.alpha=0.9, stratified=FALSE,
                    # arguments for plot
                    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE,legacy.axes = TRUE)




coords(ROC_FSSJ, "best", ret = "threshold",best.method = "closest.topleft")
x_best_fssj<-coords(ROC_FSSJ, "best", ret = "specificity",best.method = "closest.topleft")
y_best_fssj<-coords(ROC_FSSJ, "best", ret = "sensitivity",best.method = "closest.topleft")
points(x_best_fssj, y_best_fssj, type = "p",pch=18,col=4,cex=2)
x_best_fssj
y_best_fssj




