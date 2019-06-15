library(DALEX)

dragons %>% head
dragons_test %>% head


# Lifel length is to predict
dragons_rf_model <- randomForest(life_length ~ ., data = dragons)
predicted_rf <- predict(dragons_rf_model, dragons_test)

dragons_lm_model <- lm(life_length ~ ., data = dragons)
predicted_lm <- predict(dragons_lm_model, dragons_test)

# RMSE
sqrt(mean(predicted_rf - dragons_test$life_length)^2)
sqrt(mean(predicted_lm - dragons_test$life_length)^2)

# Explainer object:
explainer_rf <- explain(model = dragons_rf_model,
                        data  = dragons_test,
                        y     = dragons_test$life_length)

explainer_lm <- explain(model = dragons_lm_model,
                        data  = dragons_test,
                        y     = dragons_test$life_length)


mp_rf <- explainer_rf %>% model_performance
mp_lm <- explainer_lm %>% model_performance

explainer_rf
plot(mp_rf, mp_lm)
plot(mp_rf, mp_lm, geom = 'boxplot')


# Excercise 2: Variable importance

vi_rf <- explainer_rf %>% variable_importance
vi_lm <- explainer_lm %>% variable_importance

# note height is negative for lm and positive
plot(vi_rf, vi_lm)

sv_rf <- explainer_rf %>% single_variable(variable = "height", type = "pdp")
sv_lm <- explainer_lm %>% single_variable(variable = "height", type = "pdp")

# Low years => attractive, recent years=> attractive, in between: not
# linear model cannot capture this!
plot(sv_rf, sv_lm)

