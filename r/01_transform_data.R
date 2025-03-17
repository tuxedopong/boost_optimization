## HSD Boost Optimization Challenge - Transform Data
## Last updated on: 3/17/25
## R version 4.4.2 (2024-10-31)

setwd('~/Desktop/code/boost_optimization/r/')
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2025-03-15"), scipen = 5)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

## Load the data and clean it up
df = read_csv('../data/boost_df.csv')
glimpse(df)
df |> select(trip_id) |> unique() |> tally() # 17,500 unique trips

## Reduce our trips dataset to a single row per trip
df_clean = df |> 
  group_by(trip_id) |>
  mutate(max_boost_seq = max(seq_boost_count)) |>
  filter(seq_boost_count == max_boost_seq) |>
  ungroup() |>
  filter(created_at <= scheduled_starts_at
         & commute_minutes >= 0 
         & commute_distance >= 0) 
## Removing NAs and unlikely/nonsensical trips (e.g. 8 rides claimed after ride start time)

## Keep a copy of the boosts-only data
df_boosted = df |> filter(manual_boost==1) |> 
  mutate(time_between_boost_scheduled_s = scheduled_starts_at - boost_timestamp
         , time_between_boost_scheduled_m = as.numeric(round(time_between_boost_scheduled_s/60,1))
         , time_between_boost_scheduled_h = as.numeric(round(time_between_boost_scheduled_m/60,2))
  )

## Feature engineering & operationalizing the outcome ("time between rides claimed and scheduled")
## Our goal is to eventually reduce the proportion of trips claimed near the scheduled pickup time
df_clean = df_clean |>
  mutate(cumulative_boost_as_fare_pct =  round(cumulative_boost_amount_cents/(dollars_paid_to_driver*100),3)
         , multiple_boosts_applied = max_boost_seq > 1
         , trip_year = year(scheduled_starts_at)
         , trip_month = month(scheduled_starts_at)
         , time_between_claimed_scheduled_s = scheduled_starts_at - claimed_at
         , time_between_claimed_scheduled_m = as.numeric(round(time_between_claimed_scheduled_s/60,1))
         , time_between_claimed_scheduled_h = as.numeric(round(time_between_claimed_scheduled_m/60,2)) #time delta
  ) |>
  group_by(origin_metro_area_name, trip_month) |>
  mutate(count_monthly_rides_by_metro_00s = n()/100
         , avg_commute_mins_by_metro = round(mean(commute_minutes,na.rm=T),1)) |>
  group_by(origin_metro_area_name) |>
  mutate(avg_boost_as_fare_pct = round(mean(cumulative_boost_as_fare_pct),2)) |>
  ungroup() |>
  filter(time_between_claimed_scheduled_s > 0)

stopifnot(nrow(df_clean) > 1)
