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


def get_geo_coordinates(city_name, base_url='http://api.openweathermap.org/geo/1.0/direct?q='):
    '''
    http://api.openweathermap.org/geo/1.0/direct?q={city name},{state code},{country code}&limit={limit}&appid={API key}
    :param city_name:
    :param url:
    :return:
    '''
    url = base_url + city_name + '&limit=1&appid=' + api_key
    json = requests.get(url).json()[0]
    return str(json['lat']), str(json['lon'])

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
    json = requests.get(url).json()
    return json


def get_5_day_forecast_city(city_name, base_url='http://api.openweathermap.org/data/2.5/forecast?'):
    lat, lon = get_geo_coordinates(city_name)
    url = base_url + "lat=" + lat + "&lon=" + lon + "&appid=" + api_key
    response = requests.get(url)
    return response.json()


def write_year_to_csv(year):
    data = get_play_by_play(year)
    teams = config['teams']
    base_path = os.path.dirname(os.path.abspath(os.curdir)) + "/data/" + str(year)
    if not os.path.exists(base_path):
        os.makedirs(base_path)
    for team in teams:
        print("Writing Data for " + team)
        path = base_path + '/' + team
        team_games = data.loc[(data["home_team"] == team) | (data["away_team"] == team)]
        team_games.to_csv(path)

    print("Completed writing data for " + str(year))

def get_csv(year, team, datetime_col = None, idx_col= None):
    path = os.path.dirname(os.path.abspath(os.curdir)) + "/data/" + str(year) + '/' + str(team)
    if not os.path.exists(path):
        print("CSV does not exist")
        return None
    return pd.read_csv(path)


def create_rolling_features(df,
                            feature,
                            agg_method,
                            window_size):
    if agg_method == 'sum':
        df['sum_' + feature + '_' + str(window_size)] = df[feature].rolling(window=window_size).sum()
    elif agg_method == 'mean':
        df['mean_' + feature + '_' + str(window_size)] = df[feature].rolling(window=window_size).mean()
    return df


if __name__ == "__main__":
    # df = get_csv(2021, 'ATL.csv')
    # df = create_rolling_features(df, 'yards_gained', 'mean', 20)
    # print(df['mean_yards_gained_' + '20'][40:80])
