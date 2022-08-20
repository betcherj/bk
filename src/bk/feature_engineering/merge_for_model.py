import pandas as pd
import functools as ft
import numpy as np

schedule = pd.read_csv('static_data/Schedule.csv')
schedule['posteam'] = schedule['home_team']
schedule['defteam'] = schedule['away_team']

schedule = schedule[['game_id','week','season',"posteam", "defteam","weekday","gametime","away_score",
                     "home_score","location","roof","surface"]]

fourth_rates = pd.read_csv("processed_data/fourth_rates.csv", index_col=0)
pass_epa = pd.read_csv("processed_data/pass_epa.csv", index_col=0)
run_epa = pd.read_csv("processed_data/run_epa.csv", index_col=0)
pass_rates = pd.read_csv("processed_data/pass_rates.csv", index_col=0)
run_success = pd.read_csv("processed_data/run_success.csv", index_col=0)
pass_success = pd.read_csv("processed_data/pass_success.csv", index_col=0)


merged_data = ft.reduce(lambda left, right: pd.merge(left, right, on=['game_id','week','season',"posteam", "defteam"]),
                        [schedule, fourth_rates, pass_epa, run_epa, pass_rates, run_success, pass_success])

merged_data.to_csv("processed_data/merged_data.csv")