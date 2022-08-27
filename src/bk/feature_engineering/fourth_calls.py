import pandas as pd
import os
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split


def fourth_down_train_and_test(pbp):
  #Filter for fourth down calls only
  pbp = pbp[(pbp['down'] == 4)]

  #Did they go for it

  X = pbp[['yardline_100','ydstogo','rain','snow','surface','wind', 'winddirection',
                        'half_seconds_remaining','qtr','score_differential','wp']]

  y = np.where(((pbp['play_type'] == "pass") | (pbp['play_type'] == "run")), 1, 0)

  X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.33, random_state = 42)

  model = LogisticRegression()

  model = model.fit(X_train, y_train)

  score = model.score(X_test, y_test)

  print(f"Score of the model {score}.")
  return model

if __name__ == "__main__":
  pbp = pd.read_csv(os.path.dirname(os.path.abspath(os.curdir)) + 'data/pbp_full.csv')

  model = fourth_down_train_and_test(pbp)



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

# fourth_rates = pbp.groupby(['game_id','week','season',"posteam", "defteam"], as_index=False).agg(
#   {'went_for_it': 'sum', 'fourth_count': 'sum'})

# Some function that does correlation between weeks 1:n to n+i

# Save most predictive rolling features to csv

# fourth_rates.to_csv("processed_data/fourth_rates.csv")

