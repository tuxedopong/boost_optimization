## HSD HSD Boost Optimization Challenge - Toy Sandbox Model
## Last updated on: 3/16/25
## R version 4.4.2 (2024-10-31)

setwd('~/Desktop/code/boost_optimization/r/')
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2025-03-15"), scipen = 5)
library(tidymodels)

lm1 = linear_reg() |> set_engine("glm", family = gaussian(link = "log")) ## log-link as a starting point since we have highly right-skewed outcome data

fit0 = lm1 |> fit(time_between_claimed_scheduled_h ~ dollars_paid_to_driver + trip_starts_during_peak_hours + cumulative_boost_as_fare_pct, data = df_clean)
# print(fit0)

## Let's improve on fit0 by adding more relevant input features
fit1 = lm1 |> fit(time_between_claimed_scheduled_h ~
                    log(dollars_paid_to_driver) + 
                    log(total_predicted_duration_mins) +
                    trip_starts_during_peak_hours +
                    cumulative_boost_as_fare_pct +
                    count_monthly_rides_by_metro_00s +
                    avg_commute_mins_by_metro
                  , data = df_clean)

print(fit1)
glance(fit1)

## Calculate the model residuals and check if they're centered around zero + std. dev.
df_clean = df_clean |> mutate(predicted = predict(fit1, df_clean)$.pred,
                               residuals = time_between_claimed_scheduled_h - predicted)
df_clean |> summarise(mean_resid = mean(residuals), sd_resid = sd(residuals)) |> print()

# Residuals plots, check for asymmetry and heteroskedasticity
# ggplot(df_clean, aes(x = residuals)) + geom_histogram() + xlab("Residuals") + ylab("")
# ggplot(df_clean, aes(x = predicted, y = residuals)) + 
#   geom_point() + 
#   geom_hline(yintercept = 0, linetype = "dashed") + 
#   ggtitle("Residuals vs. Fitted Values") +
#   stat_smooth()

