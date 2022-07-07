import pandas as pd
import requests
import json
import os

from bk.utils.config import read_yaml

config = read_yaml(os.path.abspath(os.pardir + "/configs/data_config.yaml"))
api_key = config['api_keys']['open_weather']


def get_play_by_play(year, url='https://github.com/guga31bb/nflfastR-data/blob/master/data/'):
     print("Reading play by play data for year " + year)
     data = pd.read_csv(url + 'play_by_play_' + str(year) + '.csv.gz?raw=True',
               compression='gzip', low_memory=False)
     return data


def get_current_weather_city(city_name, base_url= 'http://api.openweathermap.org/data/2.5/weather?'):
    '''
    Free plan	Hourly forecast: unavailable
    Daily forecast: unavailable
    Calls per minute: 60
    3 hour forecast: 5 days
    ----
    :param city_name:
    :param base_url:
    :return:
    '''
    url = base_url + "appid=" + api_key + "&q=" + city_name
    response = requests.get(url)
    return response.json()


def get_5_day_forecast_lat_lon(lat, lon, base_url='http://api.openweathermap.org/data/2.5/forecast?'):
    url = base_url + "lat=" + lat + "&lon=" + lon + "&appid=" + api_key
    response = requests.get(url)
    return response.json()


def write_year_to_csv(year):
    data = get_play_by_play(year)
    # team_data = pd.DataFrame()
    teams = config['teams']
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
    #TODO add coordinate change api
    write_year_to_csv('2021')
    print(get_5_day_forecast_lat_lon('47.5952', '122.3316'))
