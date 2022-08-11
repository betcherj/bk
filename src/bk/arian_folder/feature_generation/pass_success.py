import pandas as pd
import numpy as np

pbp = pd.read_csv('static_data/pbp21.csv')
pass_data = pbp[pbp['play_type'] == "pass"]
pass_data['pass_count'] = 1

# Check for missing down or distance or NAs
# Should we subset between certain win probability?
# The final "above expected features" should correlate better to future pass success/ above expected success
# Can we determine "into the wind" based on the direction of the field they are throwing into?

# Features to use:

# x_feature = pass_data[['yardline_100','ydstogo','rain','snow','surface','wind', 'winddirection']]
# y_feature = pass_data['success']

# Down Subsets
pass_data1 = pass_data[pass_data['down'] == 1]
pass_data2 = pass_data[pass_data['down'] == 2]
pass_data3 = pass_data[(pass_data['down'] == 3) | (pass_data['down'] == 4)]

# Fit Models

# Calibration plot of expected vs actual

# Success tables

pass_success = pass_data.groupby(['game_id','week','season',"posteam", "defteam"], as_index=False).agg(
  {'success': 'sum', 'pass_count': 'sum'})

pass_success.rename(columns={"success": "pass_success"}, inplace=True)
# Some function that does correlation between weeks 1:n to n+i

# Save most predictive rolling features to csv

pass_success.to_csv("processed_data/pass_success.csv")