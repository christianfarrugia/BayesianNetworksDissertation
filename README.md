# Predicting Problem Gambling: A Bayesian Networks and Random Forests Study

## Introduction

This repository contains code related to an undergraduate dissertation focusing on the application of Bayesian Networks (BN) and Random Forests (RF) to predict the likelihood of someone being a problem gambler. The study explored the efficacy of these methods using self-exclusion as a proxy for problem gambling prediction.

## Repository Structure

- **code:** This folder contains all the R code that was used throughout the thesis.
  - *TAN_CL-Forward_Selection_Algorithm_UPD*: This file contains the code used to apply an adaptation of the Chow-Liu algorithm which was devised during the study.
  - *FFSJ_Model*: This file contains the code used to apply the FSSJ Bayesian Network to the data.
  - *ComparingModels-BNsRandomForestsLogReg*: This file contains the code related to the application of Random Forests and Logistic Regression to the data and comparing these models against the Bayesian Networks through the use of ROC curves.

## Bayesian Networks

Bayesian Networks were introduced as Directed Acyclic Graphs (DAGs), where directed edges indicated variable influences. Bayesian Network classifiers were utilized to predict the class label of a response variable. The structure of a BN classifier can be determined from a given dataset using algorithms like CL-adapt or FSSJ. The CL-adapt algorithm maximizes the likelihood of the data, albeit with strong assumptions on the graph structure. Conversely, the FSSJ algorithm is a heuristic that relaxes these assumptions.

## Conditional Probabilities and Parameters

The conditional probabilities, or parameters, of the BN were introduced, with asymptotic results presented. The Maximum Likelihood Estimator (MLE) was proven to be a consistent estimator.

## Tree-Based Classifiers

Tree-based classifiers, including AdaBoost and Breiman’s random forest algorithm, were introduced as another type of classifier. These algorithms were employed to build the structure of the classification models.

## Model Application and Comparison

The aforementioned methods, along with Logistic Regression, were applied to a dataset provided by a local gaming company. Additionally, a modified version of the CL-adapt algorithm was presented, returning a more parsimonious model without significant loss in accuracy. 

In terms of model performance, the Random Forest classifiers outperformed the Bayesian techniques and the Logistic Regression model. All models, except for AdaBoost, demonstrated superiority in correctly identifying non-problem gamblers compared to problem gamblers.

## Conclusion

This study provides insights into the application of Bayesian Networks and Random Forests in predicting problem gambling. The findings suggest that Random Forest classifiers offer better predictive performance in this context.
