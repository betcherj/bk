import pandas as pd
import numpy as np

pbp = pd.read_csv('static_data/pbp21.csv')

# Cleaning - decisions to be made here about hail mary type plays or free prevent defense EPA
pbp['count'] = 1

# Some serious philosophical conversations need to occur here.
# What does the current model use?
# No need to necessarily recreate the wheel here- EPA models are difficult to create and would take a long time.
# However: High leverage plays - turnovers and 3rd / fourth down. How can we combat this for predictability?
# Higher variability will also exist inside the 20
# Can we ID if red zone conversion is something repeatable beyond just a function of a healthy offense outside of the
# red zone: IE QB ability, coaching ability, anything else you can think of
# Can we optimally weight the EPA to better predict future EPA using win probability

# Success tables

epa = pbp.groupby(['game_id','week','season',"posteam", "defteam","play_type"], as_index=False).agg(
  {'epa': 'sum', 'count': 'sum'})

pass_epa = epa[epa['play_type'] == "pass"]
pass_epa.rename(columns={"epa": "pass_epa","count": "pass_count"}, inplace=True)
pass_epa.drop(columns=['play_type'], inplace=True)

run_epa = epa[epa['play_type'] == "run"]
run_epa.rename(columns={"epa": "run_epa", "count": "run_count"}, inplace=True)
run_epa.drop(columns=['play_type'], inplace=True)

# Some function that does correlation between weeks 1:n to n+i

# Save most predictive rolling features to csv

pass_epa.to_csv("processed_data/pass_epa.csv")
run_epa.to_csv("processed_data/run_epa.csv")