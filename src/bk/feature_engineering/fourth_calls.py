import pandas as pd
import numpy as np

pbp = pd.read_csv('static_data/pbp21.csv')

pbp = pbp[(pbp['down'] == 4)]
pbp['went_for_it'] = np.where(((pbp['play_type'] == "pass") | (pbp['play_type'] == "run")),1,0)
pbp['fourth_count'] = 1

# Check for missing down or distance or NAs
# Should we subset between certain win probability?
# How to clean up half seconds remaining and win probability/score differential?
# Can we include estimates on the team and opponent relative offense vs defense ability to improve model
# The final "above expected features" should correlate better to future offensive points/ total game points

# Features to use:

# x_feature = pbp[['yardline_100','ydstogo','rain','snow','surface','wind', 'winddirection',
#                        'half_seconds_remaining','score_differential','wp']]
# y_feature = pbp['went_for_it']

# Fit Models

# Calibration plot of expected vs actual

# Success tables

fourth_rates = pbp.groupby(['game_id','week','season',"posteam", "defteam"], as_index=False).agg(
  {'went_for_it': 'sum', 'fourth_count': 'sum'})

# Some function that does correlation between weeks 1:n to n+i

# Save most predictive rolling features to csv

fourth_rates.to_csv("processed_data/fourth_rates.csv")