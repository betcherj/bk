import pandas as pd
import numpy as np

pbp = pd.read_csv('static_data/pbp21.csv')
run_data = pbp[pbp['play_type'] =="run"]
run_data['run_count'] = 1

# Check for missing down or distance or NAs
# Should we subset between certain win probability?
# The final "above expected features" should correlate better to future run success/ above expected success

# Features to use:

# x_feature = run_data[['yardline_100','ydstogo','rain','snow','surface','wind']]
# y_feature = run_data['success']

# Down Subsets

run_data1 = run_data[run_data['down'] == 1]
run_data2 = run_data[run_data['down'] == 2]
run_data3 = run_data[(run_data['down'] == 3) | (run_data['down'] == 4)]

# Fit Models

# Calibration plot of expected vs actual

# Success tables

run_success = run_data.groupby(['game_id','week','season',"posteam", "defteam"], as_index=False).agg(
  {'success': 'sum', 'run_count': 'sum'})

run_success.rename(columns={"success": "run_success"}, inplace=True)

# Some function that does correlation between weeks 1:n to n+i

# Save most predictive rolling features to csv

run_success.to_csv("processed_data/run_success.csv")