import pandas as pd
import numpy as np
import os
import pickle
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split

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

def pass_rates_train_and_test(pbp, team):
  #Filter for fourth down calls only

  #TODO should we filter more things out of this
  pbp = pbp[(pbp['posteam'] == team)]

  X = pbp[['yardline_100','ydstogo','rain','snow','surface','wind', 'winddirection',
                          'half_seconds_remaining','score_differential','wp']]

  y = np.where(((pbp['attempted_pass'] == "pass") | (pbp['play_type'] == "pass")), 1, 0)

  X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.33, random_state = 42)

  model = LogisticRegression()

  model = model.fit(X_train, y_train)

  score = model.score(X_test, y_test)

  print(f"Score of the model {score}.")

  out_path = pd.read_csv(os.path.dirname(os.path.abspath(os.curdir)) + '/models/fourth_down.sav')
  print(f"Savining model to {out_path}")
  pickle.dump(model, open(out_path, 'wb'))


if __name__ == "__main__":
  pbp = pd.read_csv(os.path.dirname(os.path.abspath(os.curdir)) + '/data/pbp_full.csv')

  weather = pd.read_csv(os.path.dirname(os.path.abspath(os.curdir)) + '/processed_data/clean_weather.csv')

  pbp = pd.merge(pbp, weather, on='game_id', how='left')

  model = pass_rates_train_and_test(pbp)
