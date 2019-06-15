library(lime)

set.seed(304)

# Seperate features and response
ind <- sample(1:150, 5)

iris_explain <- iris[ind, 1:4] # pick 5 rand samples for explanation
iris_train <- iris[-ind, 1:4]
iris_lab <- iris$Species[-ind]

library(caret)
model <- train(iris_train, iris_lab, method = "rf") # random forest

explainer <- lime(iris_train,
                     model,
                     bin_continuous = TRUE,
                     n_bins = 4,
                     quantile_bins = TRUE)

explanation <- lime::explain(iris_explain,
                             explainer,
                             n_labels = 1,
                             n_features = 4,
                             n_permutations = 5000)

# Answers: Why did the model make the prediction?
plot_features(explanation)






# Excercise 3:
library(arules)
library(tidyverse)

data("AdultUCI")

AdultUCI %>% nrow
dataset <- AdultUCI %>% drop_na
dataset$education <- factor(dataset$education, ordered = FALSE)
dataset$income    <- factor(dataset$income, ordered = FALSE)
picked <- dataset %>% sample_n(1000)

ind <- sample(1:nrow(picked), 5)
adult_explain <- picked[ind,] # pick 5 rand samples for explanation
adult_train   <- picked[-ind,]
adult_lab     <- picked$income[-ind]

adult_model <- train(adult_train, adult_lab, method = "rf") # random forest

adult_explainer <- lime(adult_train, adult_model)
adult_explanation <- lime::explain(adult_explain,
                                   adult_explainer,
                                   n_labels = 1,
                                   n_features = 4,
                                   n_permutations = 5000)

# Answers: Why did the model make the prediction?
plot_features(adult_explanation)


