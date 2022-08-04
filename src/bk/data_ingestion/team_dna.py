from bk.utils import data

def week_features(team_df, year, week, team):
    game_df = team_df[team_df.game_id.startswith(str(year) + "_" + str(week))]


def create_team_df(team_name, city):
    # (1)
    pass


if __name__ == "__main__":
    pass