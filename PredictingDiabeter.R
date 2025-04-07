set.seed(26)
sample <- sample(c(TRUE,FALSE), nrow(health), replace = TRUE, prob=c(.8,.2))
training_data <- health[sample,]
test_data <- health[!sample,]
multregr_health <- lm(formula = cost_t ~ gagne_sum_tm1 + dem_female + alcohol_elixhauser_tm1 + bps_mean_t + cre_mean_t + drugabuse_elixhauser_tm1 + obesity_elixhauser_tm1 + ldl_mean_t + lasix_dose_count_tm1 + hct_mean_t, data = training_data)
summary(multregr_health)
test_data[c('predicted_cost')] <- predict(object = multregr_health, newdata = test_data)
test_drop_na <- test_data %>% drop_na(predicted_cost) %>% drop_na(cost_t)
rmse(actual = test_drop_na$cost_t, predicted = test_drop_na$predicted_cost)
# create decision tree
tree_model <- rpart(cost_t ~ gagne_sum_tm1 + dem_female + alcohol_elixhauser_tm1 + bps_mean_t + cre_mean_t + drugabuse_elixhauser_tm1 + obesity_elixhauser_tm1 + ldl_mean_t + lasix_dose_count_tm1 + hct_mean_t, data = training_data,  control = rpart.control(cp = 0.003))
rpart.plot(tree_model, main = "Tree Model of Predicted Medical Cost", type = 5, digits = 4)
test_data[c('predicted_cost')] <- predict(object = tree_model, newdata = test_data)
test_drop_na <- test_data %>% drop_na(predicted_cost) %>% drop_na(cost_t)
rmse(actual = test_drop_na$cost_t, predicted = test_drop_na$predicted_cost)
# create decision tree
tree_model <- rpart(cost_t ~ gagne_sum_tm1 + dem_female + alcohol_elixhauser_tm1 + bps_mean_t + cre_mean_t + drugabuse_elixhauser_tm1 + obesity_elixhauser_tm1 + ldl_mean_t + lasix_dose_count_tm1 + hct_mean_t + risk_score_t, data = training_data,  control = rpart.control(cp = 0.003))
rpart.plot(tree_model, main = "Tree Model of Predicted Medical Score considering Risk Score", type = 5, digits = 4)
test_data[c('predicted_cost')] <- predict(object = tree_model, newdata = test_data)
test_drop_na <- test_data %>% drop_na(predicted_cost) %>% drop_na(cost_t)
rmse(actual = test_drop_na$cost_t, predicted = test_drop_na$predicted_cost)
white <- health %>% filter(race == 'white')
black <- health %>% filter(race == 'black')
white %>%
  ggplot(aes(risk_score_t, cost_t, color = "white")) +
  geom_smooth(method = "lm", se = FALSE, formula = "y ~ x", na.rm = TRUE) +
  xlim (0,40) +
  stat_summary_bin(fun='mean', bins=20, size=2, geom='point', na.rm = TRUE) +
  geom_smooth(data = black, aes(risk_score_t, cost_t, color = "Black"), method = "lm", se = FALSE, formula = "y ~ x", na.rm = TRUE) +
  stat_summary_bin(data = black, aes(risk_score_t, cost_t, color = "Black"), fun='mean', bins=20, size=2, geom='point', na.rm = TRUE) +
  labs(x = "Risk Score", 
       y = "Total Cost for Healthcare",
       title = "Risk score vs. Total Cost for Healthcare among Black and White patients in the bottom 40th percentile",
       color = "Legend")
white <- health %>% filter(race == 'white')
black <- health %>% filter(race == 'black')
white %>%
  ggplot(aes(risk_score_t, gagne_sum_t, color = "white")) +
  geom_smooth(method = "lm", se = FALSE, formula = "y ~ x", na.rm = TRUE) +
  xlim (0,40) +
  stat_summary_bin(fun='mean', bins=20, size=2, geom='point', na.rm = TRUE) +
  geom_smooth(data = black, aes(risk_score_t, gagne_sum_t, color = "Black"), method = "lm", se = FALSE, formula = "y ~ x", na.rm = TRUE) +
  stat_summary_bin(data = black, aes(risk_score_t, gagne_sum_t, color = "Black"), fun='mean', bins=20, size=2, geom='point', na.rm = TRUE) +
  labs(x = "Risk Score", 
       y = "Total number active of chronic illnesses",
       title = "Risk score vs. Total number active of chronic illnesses among Black and White patients in the bottom 40th percentile",
       color = "Legend")
# create decision tree
tree_model <- rpart(cost_t ~ gagne_sum_tm1 + alcohol_elixhauser_tm1 + bps_mean_t + cre_mean_t + drugabuse_elixhauser_tm1 + obesity_elixhauser_tm1 + ldl_mean_t + lasix_dose_count_tm1 + hct_mean_t + risk_score_t + compdiabetes_elixhauser_tm1 + race, data = training_data,  control = rpart.control(cp = 0.003))
rpart.plot(tree_model, main = "Tree Model Predicting Cost with New Variables", type = 5, digits = 4)
test_data[c('predicted_cost')] <- predict(object = tree_model, newdata = test_data)
test_drop_na <- test_data %>% drop_na(predicted_cost) %>% drop_na(cost_t)
rmse(actual = test_drop_na$cost_t, predicted = test_drop_na$predicted_cost)

