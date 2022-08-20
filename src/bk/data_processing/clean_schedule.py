import pandas as pd

schedule = pd.read_csv('static_data/Schedule.csv')

surface_dict = {'ARI':'grass','ATL':'fieldturf', 'ATL':'fieldturf', 'BAL':'grass', 'BUF':'astroturf', 'CAR':'grass',
'CHI':'grass', 'CIN':'grass', 'CLE':'grass', 'DAL':'fieldturf', 'DEN':'grass', 'DET':'fieldturf',
'GB':'grass', 'HOU':'grass', 'IND':'fieldturf', 'JAX':'grass', 'KC':'astroturf', 'LA':'grass',
'LAC':'grass', 'MIA':'grass', 'MIN':'sportturf', 'NE':'grass', 'NO':'astroturf', 'NYG':'fieldturf',
 'NYJ':'fieldturf', 'OAK':'grass', 'LV':'fieldturf', 'PHI':'grass', 'PIT':'grass',
'SEA':'grass', 'SF':'grass', 'TB':'grass', 'TEN':'grass', 'WAS':'grass'}

schedule['surface'] = schedule.home_team.map(surface_dict)

roof_dict = {'ARI':'closed','ATL':'closed', 'ATL':'outdoors', 'BAL':'outdoors', 'BUF':'outdoors', 'CAR':'outdoors',
'CHI':'outdoors', 'CIN':'outdoors', 'CLE':'outdoors', 'DAL':'closed', 'DEN':'outdoors', 'DET':'closed',
'GB':'outdoors', 'HOU':'closed', 'IND':'closed', 'JAX':'outdoors', 'KC':'outdoors', 'LA':'outdoors',
'LAC':'outdoors', 'MIA':'outdoors', 'MIN':'closed', 'NE':'outdoors', 'NO':'closed', 'NYG':'outdoors',
 'NYJ':'outdoors', 'OAK':'outdoors', 'LV':'closed', 'PHI':'outdoors', 'PIT':'outdoors',
'SEA':'outdoors', 'SF':'outdoors', 'TB':'outdoors', 'TEN':'outdoors', 'WAS':'outdoors'}

schedule['roof'] = schedule.home_team.map(roof_dict)

team_dict = {'STL':'LA','OAK':'LV', 'SD':'LAC'}

schedule['home_team'] = schedule['home_team'].replace(team_dict)
schedule['away_team'] = schedule['away_team'].replace(team_dict)

schedule.to_csv("static_data/Schedule.csv")

