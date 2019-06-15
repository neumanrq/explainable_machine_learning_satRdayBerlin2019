library(DALEX)
library(randomForest)
library(tidyverse)
library(pdp)
library(factorMerger)

set.seed(519)
# Dataset about Warszaw house prices
apartments %>% head
apartments_test %>% head

apartments_rf_model <- randomForest(m2.price ~ ., data = apartments)
predicted_rf <- predict(apartments_rf_model, apartments_test)

apartments_lm_model <- lm(m2.price ~ ., data = apartments)
predicted_lm <- predict(apartments_lm_model, apartments_test)

# RMSE
sqrt(mean(predicted_rf - apartments_test$m2.price)^2)
sqrt(mean(predicted_lm - apartments_test$m2.price)^2)

# Explainer object:
explainer_rf <- explain(model = apartments_rf_model,
                        data  = apartments_test[, 2:6], # "remove response"
                        y = apartments_test$m2.price)

explainer_lm <- explain(model = apartments_lm_model,
                        data  = apartments_test[, 2:6], # "remove response"
                        y = apartments_test$m2.price)


# What does the percentages mean?

mp_rf <- explainer_rf %>% model_performance
mp_lm <- explainer_lm %>% model_performance

# DIFF means residuals!
str(mp_rf)
mp_rf %>% ggplot(aes(observed, diff)) + geom_point()
mp_lm %>% ggplot(aes(observed, diff)) + geom_point()

# You can also use DALEX:

mp_rf %>% ggplot(aes(observed, diff)) + geom_point()

plot(mp_rf, mp_lm)

# Now: Measuring Variable importance

vi_rf <- explainer_rf %>% variable_importance
vi_lm <- explainer_lm %>% variable_importance

plot(vi_rf, vi_lm)

sv_rf <- explainer_rf %>% single_variable(variable = "construction.year", type = "pdp")
sv_lm <- explainer_lm %>% single_variable(variable = "construction.year", type = "pdp")

# Low years => attractive, recent years=> attractive, in between: not
# linear model cannot capture this!
plot(sv_rf, sv_lm)


# Now: A categorical variable (district)

svd_rf <- explainer_rf %>% single_variable(variable = 'district', type = 'factor')
svd_lm <- explainer_rf %>% single_variable(variable = 'district', type = 'factor')
plot(svd_rf, svd_lm)

# install.packages('units')
