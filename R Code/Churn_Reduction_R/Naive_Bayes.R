#*****Lets apply naive bayes*****

nb_data = train_after_smote
NB_model = naiveBayes(Churn ~ ., data = nb_data)

#predict on test cases #raw
NB_Predictions = predict(NB_model, test_preproc[,1:11], type = 'class')

#Look at confusion matrix
Conf_matrix = table(observed = test_preproc[,12], predicted = NB_Predictions)

confusionMatrix(Conf_matrix)

# Conf_matrix
#predicted
#observed    1    2
#       1 1116  327
#        2   66  158