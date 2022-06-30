import pandas as pd
import os

teams = ['SF', 'DET', 'BAL', 'LA', 'MIN' ,'KC', 'JAX', 'CIN', 'CAR', 'NE', 'BUF', 'WAS', 'NYG',
 'ATL', 'NO', 'DEN', 'DAL', 'HOU', 'MIA', 'TB', 'CLE', 'PIT', 'GB', 'TEN', 'LAC',
 'PHI', 'IND', 'SEA', 'LV', 'CHI', 'NYJ', 'ARI']

def get_play_by_play(year, url='https://github.com/guga31bb/nflfastR-data/blob/master/data/'):
     data = pd.read_csv(url + 'play_by_play_' + str(year) + '.csv.gz?raw=True',
               compression='gzip', low_memory=False)
     return data



def write_year_to_csv(year):
    data = get_play_by_play(year)
    team_data = pd.DataFrame()

    for team in teams:
        print("Writing Data for " + team)
        path = os.path.dirname(os.path.abspath(os.curdir)) + "/data/" + str(year) + '/' + team
        if not os.path.exists(path):
            os.makedirs(path)

        team_home_games = data.loc[data["home_team"] == team]
        team_home_games.to_csv(path + "/home_pbp")

        team_away_games = data.loc[data["away_team"] == team]
        team_away_games.to_csv(path + "/away_pbp")

if __name__ == "__main__":
    write_year_to_csv(2020)
