import pandas as pd
import numpy as np

historic_odds = pd.read_csv('static_data/HistoricNFLOdds.csv')
Odds22 = pd.read_csv('static_data/NFLOdds22.csv')
Odds22['Year'] = 2021
Odds21 = pd.read_csv('static_data/NFLOdds21.csv')
Odds21['Year'] = 2020
Odds = pd.concat([Odds21, Odds22])

Odds.dropna(axis=0, how='any', inplace=True)
Odds['Rot'] = np.where(Odds['Rot'] % 2 == 0, Odds['Rot']-1, Odds['Rot'])

Odds['Open'] = np.where(Odds['Open'] =="pk",0, Odds['Open'])
Odds['Close'] = np.where(Odds['Close'] =="pk",0, Odds['Close'])
Odds['ML'] = np.where(Odds['ML'] =="pk",0, Odds['ML'])
Odds['ML'] = np.where(Odds['ML'] =="NL",0, Odds['ML'])

Odds['Open'] = Odds['Open'].apply(float)
Odds['Close'] = Odds['Close'].apply(float)
Odds['ML'] = Odds['ML'].apply(float)

Odds = Odds.assign(
    SpreadClose =
    Odds
    .groupby(["Year", "Rot",  "Date"])
    .Close
    .transform('min')
)

Odds = Odds.assign(
    Spread=
    Odds
    .groupby(["Year", "Rot",  "Date"])
    .Open
    .transform('min')
)

Odds = Odds.assign(
    Total=
    Odds
    .groupby(["Year", "Rot",  "Date"])
    .Open
    .transform('max')
)

Odds = Odds.assign(
    TotalClose=
    Odds
    .groupby(["Year", "Rot",  "Date"])
    .Close
    .transform('max')
)

Odds = Odds.assign(
    Underdog=
    Odds
    .groupby(["Year", "Rot",  "Date"])
    .ML
    .transform('max')
)

Odds['Spread'] = np.where(Odds['Spread'] == Odds['Open'], Odds['Spread']*-1, Odds['Spread'])
Odds['SpreadClose'] = np.where(Odds['SpreadClose'] == Odds['Close'], Odds['SpreadClose']*-1, Odds['SpreadClose'])


keeps = ["Date", "Rot", "Year", "VH", "Team" ,"ML" , "Spread", "Total", "SpreadClose", "TotalClose"]
Odds = Odds[keeps]

team_dict = {"Arizona" : 'ARI', "Atlanta" : 'ATL', "Baltimore": 'BAL', "Buffalo" : 'BUF', "BuffaloBills": 'BUF',
        "Carolina" : 'CAR', "Chicago" : 'CHI', "Cincinnati": 'CIN', "Cleveland" : 'CLE', "Dallas": 'DAL',
        "Denver" : 'DEN', "Detroit" : 'DET', "GreenBay": 'GB', "Houston" : 'HOU', "HoustonTexans": 'HOU',
        "Indianapolis" : 'IND', "Jacksonville" : 'JAC', "KansasCity": 'KC', "LAChargers" : 'LAC', "LARams": 'LA',
        "LosAngeles" : 'LA', "Miami" : 'MIA', "NewEngland": 'NE', "NewOrleans" : 'NO', "NewO rleans": 'NO',
        "LA Charger" : 'LAC', "NY Giants" : 'NYG', "San Francisco": 'SF', "Green Bay" : 'GB', "Las Las": 'LV',
        "NewYork": 'NYG', "NYGiants": 'NYG', "NYJets": 'NYJ', "Oakland": 'OAK', "LasVegas": 'LV',
        "Philadelphia": 'PHI', "Pittsburgh": 'PIT', "SanDiego": 'SD', "SanFrancisco": 'SF', "Seattle": 'SEA',
        "TampaBay": 'TB', "Tennessee": 'TEN', "Washington": 'WAS', "Washingtom": 'WAS', "St.Louis": 'STL',
        "Minnesota": 'MIN', "LVRaiders": 'LV', "New KCChiefs": 'KC', "Tampa": 'TB', "Kansas": 'KC'}

Odds["Team"] = Odds['Team'].map(team_dict)

Odds = Odds.assign(
    Opp=
    Odds
    .groupby(["Year", "Rot",  "Date"])
    .Team
    .shift(1)
)

Odds['Opp'] = np.where(Odds['Opp'].isnull(), Odds['Team'].shift(-1), Odds['Opp'])

Odds['Year'] = np.where(Odds['Date'] < 900, Odds['Year']+1, Odds['Year'])
Odds['Date'] = Odds['Date'].apply(str)
Odds['Date'] = Odds['Date'].str.zfill(4)
Odds['Date'].add_suffix(Odds['Year'])
Odds['Date'] = Odds['Date'].astype(str) + Odds['Year'].astype(str)
Odds['Date'] = pd.to_datetime(Odds['Date'], format="%m%d%Y")

keeps = ["Date", "Team", "Opp", "ML", "Spread", "Total", "SpreadClose", "TotalClose"]
Odds = Odds[keeps]

#

Odds = Odds.sort_values(['Total'], ascending=[True])
Odds['Total'] = np.where(Odds['TotalClose']-Odds['Total'] > 10, Odds['TotalClose'], Odds['Total'])

historic_odds = historic_odds[historic_odds['Date'] < '2020-02-20']
historic_odds = historic_odds.iloc[: , 1:]
Odds = pd.concat([Odds, historic_odds])
Odds.to_csv('static_data/HistoricNFLOdds.csv')