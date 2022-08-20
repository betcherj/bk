import pandas as pd
import numpy as np

pbp = pd.read_csv('static_data/pbp21.csv')

pbp = pbp[(pbp['play_type'] == "pass") | (pbp['play_type'] == "run")]
pbp['pass_count'] = 1

# We should make our own model and see if we can improve on the canned approach
# Can we include weather data and estimates on the opponent relative pass vs rush ability to improve model
# The final "above expected passing" should correlate better to future pass rate (percentage)

# Features to use:

# x_feature = pbp[['yardline_100','ydstogo','rain','snow','surface','wind', 'winddirection',
#                        'half_seconds_remaining','score_differential','wp']]
# y_feature = pbp['went_for_it']

# Fit Models

# Calibration plot of expected vs actual

# Success tables

pass_rates = pbp.groupby(['game_id','week','season',"posteam", "defteam"], as_index=False).agg(
  {'xpass': 'sum', 'pass_oe': 'sum', 'pass_count': 'sum'})

pass_rates['pass_oe'] = pass_rates['pass_oe']/100

# Some function that does correlation between weeks 1:n to n+i

# Save most predictive rolling features to csv

pass_rates.to_csv("processed_data/pass_rates.csv")