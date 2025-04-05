## HSD HSD Boost Optimization Challenge - EDA
## Last updated on: 3/16/25
## R version 4.4.2 (2024-10-31)

setwd('~/Desktop/code/boost_optimization/r/')
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2025-03-15"), scipen = 5)
library(ggplot2)

summary(df_clean$trip_completed_at) ## data range from 2024-05-01 to 2024-08-30
summary(df_clean$dollars_paid_to_driver) ## important to consider $$ paid out
unique(df_clean$trip_state) ## all trips here were completed
summary(df_clean$single_boost_amount_cents) ## negative boosts?
summary(df_clean$commute_minutes) ## negative commute time doesn't make sense
summary(df_clean$total_predicted_duration_mins)
summary(df_clean$seq_boost_count > 0)
df_clean |> filter(cumulative_boost_amount_cents >= 2000) |> nrow() # 1,559 trips (9%) received $20+ in cum boosts 

df_clean |> select(origin_metro_area_name) |> table() |> sort() # Orlando, DFW, Sac, Greensboro, Worcester, Hampton rds top 6 metros by ride volume
metros_top6 = c("Orlando Metro", "DFW", "Sacramento Metro", "Greensboro Metro", "Worcester Metro", "Hampton Roads")
# z = df_clean |> filter(commute_minutes > 180 | commute_minutes < 0) # unlikely to drive 3+ hours or back in time?
# f = df_clean |> filter(boost_ind != manual_boost) # a true zero $ boost seems odd

summary(df_clean) |> print()
# Among 17.5k unique trips (originally 21,861 rows)...
# All scheduled between 2024-05-01 and 2024-08-30
# 43% of trips required a manual boost -- can we do better?
# 47% of trips were unclaimed at some point
# 24.7% of trips start during peak hours
# 2.8% of trips are same-day rides -- ops team could focus on these in the future
# 5.9 miles - average commute distance from driver to pickup
# 16 mins - average commute time (remove anything over 3 hours)
# 8.1 miles - average trip distance (predicted)
# 21.4 mins - average trip duration (predicted)
# $4.22 avg individual boost amount ($1.40 median)
# $5.27 avg cum boost ($0 median)
# $24.52 - avg. fare paid to drivers

## EDA -- simple plots on key features and variables of interest
ggplot(df_clean, aes(x=seq_boost_count)) + geom_histogram()
ggplot(df_clean, aes(x=commute_minutes)) + geom_histogram() + facet_wrap(~origin_metro_area_name) + xlim(c(0,120))

## Understanding the existing boosting mechanism -- proportion, timing, & amount
summary(df_clean$manual_boost) ## 42.7% of trips received one or more manual boosts!**
df_clean |> select(dollars_paid_to_driver) |> sum()/1000 ## $428k paid out to drivers over 4 months in 2024

summary(df_boosted$single_boost_amount_cents/100) ## Among boosted trips, avg. single boost: $7.81, median: $4.90
df_boosted |> select(single_boost_amount_cents) |> sum()/1e5 ## This is the million dollar question (~25k/month)**
## Of $42.88MM paid to drivers in this time period, over a fifth (21.5%) came from boosts (also equivalent: $5.27 avg. boost / $24.52 avg. fare)**
## 7.1k boosts in may, 1.5k in june, 563 in july, 2.6k in august
df_boosted |> mutate(boost_month = month(boost_timestamp)) |> filter(boost_month == 5) |> nrow()

## Boosts only ever occur within 7 hours of ride time (0-7.1 hrs)
summary(df_boosted$cumulative_boost_amount_cents/100) ## Among boosted, cumulative amount's IQR was $5.69-$19.60
summary(df_boosted$seq_boost_count) ## Among boosted, an average of 1.54 boosts were applied (up to 11!)
ggplot(df_boosted, aes(x=seq_boost_count)) + geom_bar() + xlab("Number of incremental boosts among boosted rides")

summary(df_boosted$time_between_boost_scheduled_h)
## It appears the boosts team typically starts its interventions about 2-3 hours out**
ggplot(df_boosted, aes(x=time_between_boost_scheduled_h*-1, y = single_boost_amount_cents/100)) + 
  geom_point(alpha=0.5) + 
  ylim(c(-6,40)) + 
  stat_smooth(se=T, col="red", lwd=1.5) +
  xlab("Time until scheduled pick-up (hours)") +
  ylab("Single Boost Amount ($)")
## For this plot, I asked GPT-4o to remind me about the smoothing splines fitted by ggplot's default generative additive model:
## i.e. "tell me about the properties of the gam used for plotting in ggplot2 (e.g. stat_smoth())"
# In ggplot2, stat_smooth() is often used to fit and plot smooth curves through data points. 
# When method = "gam" is used, it fits a Generalized Additive Model (GAM) using thin-plate regression splines as the smoothing method.
# Confidence Intervals: stat_smooth() by default plots a 95% confidence interval around the smooth line.
# It is based on the standard errors of the predicted values. Can be disabled with se = FALSE.
# Automatic Smoothing Parameter Selection (lambda): The smoothness of the curve is automatically determined by the model.
# The penalized regression approach prevents overfitting.
# Flexibility vs. Overfitting: GAMs allow for nonlinear relationships without assuming a parametric form.
# However, overfitting can occur if the smoothing parameter is too low. [We'll take it with a grain of salt. -F.O.]


# Output/independent variable is the gap between a driver claiming a ride and its scheduled start time
summary(df_clean$time_between_claimed_scheduled_h)
summary(df_clean$time_between_claimed_scheduled_m <= 60) ## 33.2% of rides remain unclaimed just an hour ahead of scheduled pick-up**
ggplot(df_clean, aes(x=time_between_claimed_scheduled_h*-1)) + geom_histogram() + xlab("Time between a claimed trip and its scheduled start, in hours") ## bimodal distribution makes me want to focus on boosted rides**
ggplot(df_clean, aes(x=log1p(time_between_claimed_scheduled_h))) + geom_histogram() + xlab("Time between a claimed trip and its scheduled start, log hours") ## bimodal distribution makes me want to focus on boosted rides

## Total fare paid ($), total predicted distance in miles and minutes
ggplot(df_clean, aes(x=dollars_paid_to_driver)) + geom_histogram() + xlab("Fare paid to driver ($)") ## total fare**
ggplot(df_clean, aes(x=dollars_paid_to_driver)) + geom_histogram() + xlab("Fare paid to driver ($)") + facet_wrap(~origin_metro_area_name)
ggplot(df_clean, aes(x=total_predicted_distance_miles)) + geom_histogram() + xlab("Predicted Trip Duration (miles)") ## total predicted miles
ggplot(df_clean, aes(x=total_predicted_duration_mins)) + geom_histogram() + xlab("Predicted Trip Duration (mins)") ## total predicted minutes
ggplot(df_clean, aes(x=total_predicted_duration_mins)) + geom_density() + 
  facet_wrap(~origin_metro_area_name) + xlim(c(0,90)) + ylim(c(0,0.2)) ## density plot of trip duration by metro, Columbia n < 5

## Distribution of single and cumulative boosts, and boost as a percentage of the total fare
ggplot(df |> filter(single_boost_amount_cents>0), aes(x=single_boost_amount_cents/100)) + geom_histogram() + xlab("Single boosts among boosted rides ($)")
ggplot(df_clean, aes(x=cumulative_boost_amount_cents/100)) + geom_histogram() + xlab("Cumulative boost histogram ($)")
ggplot(df_clean |> filter(cumulative_boost_amount_cents>0), aes(x=cumulative_boost_as_fare_pct*100)) + geom_histogram() + xlab("Cumulative Boost as a Percentage of Paid Fare (%)")
## Cumulative boost as % of fare paid to driver**
ggplot(df_clean |> filter(cumulative_boost_amount_cents>0, origin_metro_area_name %in% metros_top6), aes(x=cumulative_boost_as_fare_pct*100)) + 
  geom_histogram() + facet_wrap(~origin_metro_area_name) + #fill=factor(time_between_claimed_scheduled_h)
  xlab("Cumulative Boost as Driver's Fare Percentage (%), selected metro areas")
ggplot(df_clean |> filter(cumulative_boost_amount_cents>0, origin_metro_area_name %in% metros_top6), aes(x=cumulative_boost_as_fare_pct*100)) + 
  geom_histogram(aes(fill=factor(ever_unclaimed))) + facet_wrap(~origin_metro_area_name) +
  xlab("Cumulative Boost as Driver's Fare Percentage (%), selected metro areas")

## Boolean vars: trip during peak hours, received more than one boost, or was ever unclaimed (before being re-claimed later on)
ggplot(df_clean, aes(x=trip_starts_during_peak_hours)) + geom_bar()
ggplot(df_clean, aes(x=multiple_boosts_applied)) + geom_bar()
ggplot(df_clean, aes(x=ever_unclaimed)) + geom_bar()

## Geographic-level, time-variant features capture some seasonality: count of monthly rides & avg. commute mins by metro
ggplot(df_clean, aes(x=count_monthly_rides_by_metro)) + geom_histogram()
ggplot(df_clean, aes(x=avg_commute_mins_by_metro)) + geom_histogram()

