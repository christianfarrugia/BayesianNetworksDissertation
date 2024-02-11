#Comparing ROC curves

#cl_adapt predictions
loaded_models<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220129epsilon_1_percent_models.rds")
loaded_accuracy<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220129epsilon_1_percent_accuracy_vec.rds")
best_model<-loaded_models[[which.max(loaded_accuracy)]]
plot(best_model)

best_test_df<-test[,names(test) %in% c("problem_gambler","country_code","active_days","segment_value","log_avg_stake_per_active_day","log_avg_deposit_count_per_active_day")]
best_train_df<-train[,names(test) %in% c("problem_gambler","country_code","active_days","segment_value","log_avg_stake_per_active_day","log_avg_deposit_count_per_active_day")]
best_preds<-predict(best_model,best_test_df,prob=TRUE)

ROC_CL_adapt <- roc(best_test_df$problem_gambler,best_preds[,2],
                    smoothed = FALSE,
                    # arguments for ci
                    ci=FALSE, ci.alpha=0.9, stratified=FALSE,
                    # arguments for plot
                    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE,legacy.axes = TRUE)

##fssj
fssj_model<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220130-fssj_model.rds")

fssj_pred_probs<-predict(fssj_model,test,prob=TRUE)


ROC_FSSJ <- roc(test$problem_gambler,fssj_pred_probs[,2],
                smoothed = FALSE,
                # arguments for ci
                ci=FALSE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE,legacy.axes = TRUE)


##random forest
random_forest<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220129-random-forest.rds")

rf_preds<-predict(random_forest,test,type="prob")
head(rf_preds)
roc_random_forest<-roc(test$problem_gambler,rf_preds[,2],
                       smoothed = FALSE,
                       # arguments for ci
                       ci=FALSE, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE, show.thres=TRUE,legacy.axes = TRUE)

##adaboost

adamodel_fast_loaded<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220202-AdaBoost.rds")
ada_pred<-predict(adamodel_fast_loaded,test,type="prob")
head(ada_pred)

roc_ada_boost<-roc(test$problem_gambler,ada_pred$prob[,2],
                       smoothed = FALSE,
                       # arguments for ci
                       ci=FALSE, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE, show.thres=TRUE,legacy.axes = TRUE)

##Logistic regression
#run code in Log-reg-testing-assumptions

log_reg_df<-read.csv("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\SPSS\\20220323-log-reg-test-preds.csv",header = TRUE)
head(log_reg_df)


#log_reg_model<-readRDS("C:\\Users\\chris\\OneDrive\\Desktop\\Thesis\\Thesis-RCode\\20220214-Log-Reg.rds")
#log_reg_preds<-predict(log_reg_model,test,type="response")
#log_reg_predicted_class<-as.factor(ifelse(log_reg_preds>0.5,"TRUE","FALSE"))


log_reg_preds_mc<-predict(log_reg_model_w_mc_diag,log_test,type="response")
log_reg_predicted_class_mc<-as.factor(ifelse(log_reg_preds_mc>0.5,"TRUE","FALSE"))


#the below has been updated to use predictions obtained using SPSS. These are found in the data frame log_reg_df
View(log_reg_predicted_class)

roc_log_reg<-roc(test$problem_gambler,log_reg_df$Prob_true,
                   smoothed = FALSE,
                   # arguments for ci
                   ci=FALSE, ci.alpha=0.9, stratified=FALSE,
                   # arguments for plot
                   plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                   print.auc=TRUE, show.thres=TRUE,legacy.axes = TRUE)


log_opt_threshold<-coords(roc_log_reg, "best", ret = "threshold",best.method = "closest.topleft")#0.4942058
log_x_best<-coords(roc_log_reg, "best", ret = "specificity",best.method = "closest.topleft")
log_y_best<-coords(roc_log_reg, "best", ret = "sensitivity",best.method = "closest.topleft")
points(log_x_best, log_y_best, type = "p",pch=18,col=4,cex=2)

log_reg_predicted_class_opt<-as.factor(ifelse(log_reg_preds_mc>log_opt_threshold$threshold,"TRUE","FALSE"))

confusionMatrix(data=log_reg_predicted_class_opt,reference = test$problem_gambler,positive = "TRUE")

####all ROC curves

plot(ROC_CL_adapt,col="red")
plot(ROC_FSSJ,col="blue",add=TRUE)
plot(roc_random_forest,col="green",add=TRUE)
plot(roc_ada_boost,col="purple",add=TRUE)
plot(roc_log_reg,col="orange",add=TRUE)

legend("bottomright", legend=c("CL-adapt", "FSSJ","BRF","AdaBoost","LogReg"),
       col=c("red", "blue","green","purple","orange"),lty=1, cex=0.8)



