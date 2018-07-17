Final_Set = train_after_smote

library(caret)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

train_after_smote$Churn = as.factor(train_after_smote$Churn)
test_preproc$Churn = as.factor(test_preproc$Churn)

svm_Linear <- train(Churn ~., data = train_after_smote, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

test_pred <- predict(svm_Linear, newdata = test_preproc)
confusionMatrix(test_pred, test_preproc$Churn )

ROCRpred = prediction(as.numeric(test_pred), as.numeric(test_preproc$Churn))
ROCRperf = performance(ROCRpred, 'tpr','fpr')
gg12 = plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc(gg12)

gg11 = roc(Churn ~ as.numeric(test_pred), data = test_preproc)
plot(gg11) 
auc(gg11)
