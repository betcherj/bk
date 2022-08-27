import pandas as pd
import numpy as np
import os
import re


def get_wind_speed(weather_info):
    if not weather_info or weather_info == "null" or 'wind: ' not in str(weather_info).lower():
        return None
    speed = re.findall("\d+", str(weather_info).lower().split("wind: ")[1])
    if not speed:
        return None
    return speed[0]

def get_wind_direction(weather_info):
    if not weather_info or weather_info == "null" or 'wind: ' not in str(weather_info).lower():
        return None
    return re.sub('[^a-zA-Z]+',"", str(weather_info).lower().split("wind: ")[1].split("mph")[0]).replace("south", "s").replace("north", "n").replace("west", "w").replace("east", "e")

def get_rain(weather_info):
    return 'rain' in str(weather_info).lower() and not 'light' in str(weather_info).lower()

def get_snow(weather_info):
    return 'snow' in str(weather_info).lower() and not 'light' in str(weather_info).lower()

def clean_weather_data(pbp):
    weather_df = pbp.groupby(['game_id'])['weather'].apply(np.random.choice).reset_index()

    weather_df['temperature'] = weather_df['weather'].str.split('°').str[0].str[-3:]

    weather_df['rain'] = weather_df['weather'].apply(get_rain)

    weather_df['wind_speed'] = weather_df['weather'].apply(get_wind_speed)

    weather_df['wind_direction'] = weather_df['weather'].apply(get_wind_direction)

    weather_df['snow'] = weather_df['weather'].apply(get_snow)

    weather_df = weather_df.drop(columns='weather')

    return weather_df

if __name__ == "__main__":
    print(os.path.dirname(os.path.abspath(os.curdir)) + '/data/Schedule.csv')
    schedule = pd.read_csv(os.path.dirname(os.path.abspath(os.curdir)) + '/data/Schedule.csv')
    pbp = pd.read_csv(os.path.dirname(os.path.abspath(os.curdir)) + '/data/pbp_full.csv')

    # Add roof and surface when NA

    schedule = schedule[['game_id', 'roof', 'surface']]
    pbp = pbp.merge(schedule, on='game_id')
    pbp.roof_x.fillna(pbp.roof_y, inplace=True)
    del pbp['roof_y']
    pbp.surface_x.fillna(pbp.surface_y, inplace=True)
    del pbp['surface_y']
    pbp = pbp.rename(columns={'roof_x': 'roof', 'surface_x': 'surface'})

    ### Remove Clock Plays###
    pbp = pbp[pbp['qb_spike'] == 0]
    pbp = pbp[pbp['qb_kneel'] == 0]

    ### Clean weather columns
    weather_df = pbp.groupby(['game_id'])['weather'].apply(lambda x: x.notnull()).reset_index()

    # out_path = os.path.dirname(os.path.abspath(os.curdir)) + '/data/just_weather.csv'
    # weather_df.to_csv(out_path, na_rep='null')

    clean_weather_df = clean_weather_data(pbp)

    out_path = os.path.dirname(os.path.abspath(os.curdir)) + '/processed_data/clean_weather.csv'
    clean_weather_df.to_csv(out_path, na_rep='null')



#
# Kneel <- "Rain"
# Kneel_match <- gregexpr(pattern=Kneel, ignore.case = TRUE, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$Rain <- ifelse(Kneel_list == "Rain" , 1, 0)
# ftable(Weather_df$Rain)
#
# Kneel <- "rain"
# Kneel_match <- gregexpr(pattern=Kneel, ignore.case = TRUE, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$Rain <- ifelse(Kneel_list == "rain" , 1, Weather_df$Rain)
# ftable(Weather_df$Rain)
#
# Kneel <- "Snow"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$Snow <- ifelse(Kneel_list == "Snow" , 1, 0)
# ftable(Weather_df$Snow)
#
# Kneel <- "snow"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$Snow <- ifelse(Kneel_list == "snow" , 1, Weather_df$Snow)
# ftable(Weather_df$Snow)
#
# Kneel <- "Light Snow"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$Snow <- ifelse(Kneel_list == "Light Snow" , 0, Weather_df$Snow)
# Weather_df$LightRain <- ifelse(Kneel_list == "Light Snow" , 1, 0)
# ftable(Weather_df$Snow)
#
# Kneel <- "Light rain"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain <- ifelse(Kneel_list == "Light rain" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "Light Rain"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain <- ifelse(Kneel_list == "Light Rain" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "showers"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain <- ifelse(Kneel_list == "showers" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "Showers"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain <- ifelse(Kneel_list == "Showers" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "Chance of Rain"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain <- ifelse(Kneel_list == "Chance of Rain" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "Rain Chance"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain<- ifelse(Kneel_list == "Rain Chance" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "Freezing Rain"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain<- ifelse(Kneel_list == "Freezing Rain" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Weather_df$Snow<-ifelse(Weather_df$LightRain==1,0,Weather_df$Snow)
# Weather_df$Rain<-ifelse(Weather_df$LightRain==1,0,Weather_df$Rain)
# Weather_df$Rain<-ifelse(Weather_df$roof!="outdoors",0,Weather_df$Rain)
# Weather_df$LightRain<-ifelse(Weather_df$roof!="outdoors",0,Weather_df$LightRain)
# Weather_df$Snow<-ifelse(Weather_df$roof!="outdoors",0,Weather_df$Snow)
#
# Weather_df<-Weather_df[,c(1,373,374,375)]
#
# pbp21<-merge(pbp21,Weather_df,by="game_id")
# pbp21$temp<-ifelse(pbp21$roof!="outdoors",68,pbp21$temp)
# pbp21$wind<-ifelse(pbp21$roof!="outdoors",0,pbp21$wind)
#
# ### ONLY FOR New DATA?
#
# library(dplyr)
# Weather_df <- pbp21 %>% group_by(game_id) %>% sample_n(1)
#
# WeatherData<-as.data.frame(Filter(function(x) any(!is.na(x)),
#        read.csv(text = gsub("\\D+", ",", Weather_df$weather),
#                 fill = TRUE, header = FALSE)))
#
# WeatherData<-subset(WeatherData,is.na(WeatherData$V1))
#
# Weather_df$temp<-WeatherData$V2
# Weather_df$wind<-WeatherData$V4
# #
# Weather_df<-Weather_df[,c(1,334,335)]
#
# pbp21<-merge(pbp21,Weather_df,by="game_id")
# pbp21$temp.x<-pbp21$temp.y
# pbp21$wind.x<-pbp21$wind.y
# pbp21<-pbp21[,-c(376,377)]
# names(pbp21)[names(pbp21) == "temp.x"] <- "temp"
# names(pbp21)[names(pbp21) == "wind.x"] <- "wind"
#
# ### Deal w windspeed
#
# pbp21$wind <- ifelse(pbp21$roof=="dome"|pbp21$roof=="closed" , 0, pbp21$wind)
# pbp21$wind<-ifelse(is.na(pbp21$wind),0,pbp21$wind)
#
# tapply(pbp21$temp, pbp21$roof, summary, na.action = na.pass)
#
# pbp21$temp <- ifelse(pbp21$roof=="dome"|pbp21$roof=="closed" , 69, pbp21$temp)
# pbp21$temp<-ifelse(is.na(pbp21$temp),60,pbp21$temp)
#
# pbp21$success<-ifelse(is.na(pbp21$success) & pbp21$series_success==1,1,pbp21$success)
# pbp21$success<-ifelse(is.na(pbp21$success) & pbp21$series_success==0,0,pbp21$success)
#
# pbp21$i_wind<-ifelse(pbp21$wind>0,1,0)
# pbp21$i_surface<-ifelse(pbp21$surface=="grass",1,0)
# pbp21$i_roof<-ifelse(pbp21$roof=="outdoors",0,1)
#
# summary(pbp21$i_roof)
# summary(pbp21$i_surface)
# summary(pbp21$i_wind)
# summary(pbp21$wind)
# summary(pbp21$Snow)
# summary(pbp21$LightRain)
# summary(pbp21$Rain)
# summary(pbp21$temp)
#
# ### Clean other variables
#
#
# pbp21$yardline_100<-ifelse(is.na(pbp21$yardline_100),20,pbp21$yardline_100)
# pbp21$wind<-ifelse(pbp21$wind>21,21,pbp21$wind)
# pbp21$ydstogo<-ifelse(pbp21$ydstogo>25,26,pbp21$ydstogo)
# pbp21$score_differential<-ifelse(pbp21$score_differential>28,29,pbp21$score_differential)
# pbp21$score_differential<-ifelse(pbp21$score_differential< -28,-29,pbp21$score_differential)
#
# pbp21$qtr<-ifelse(pbp21$qtr>4,5,pbp21$qtr)
# pbp21$qtr<-as.factor(pbp21$qtr)
# pbp21$down<-as.factor(pbp21$down)
# pbp21$Rain<-as.factor(pbp21$Rain)
# pbp21$Snow<-as.factor(pbp21$Snow)
# pbp21$LightRain<-as.factor(pbp21$LightRain)
# pbp21$i_surface<-as.factor(pbp21$i_surface)
# pbp21$i_roof<-as.factor(pbp21$i_roof)
# pbp21$i_wind<-as.factor(pbp21$i_wind)
#
#
# #
#
# write.csv(pbp21,'/Users/am/Desktop/NFLSets/PBP/NFLPBPClean21.csv')


#
# ## Old Data
# #
#
# # Clean PBP Data
# pbpAll<- fread('/Users/am/Desktop/NFLSets/PBP/NFLPBPNEW.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
# Schedule<- read.csv('/Users/am/Desktop/NFLSets/PBP/Schedule.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
# Schedule<-Schedule[,c(1,15,16)]
# pbpAll<-merge(pbpAll,Schedule,by=c("game_id"),all.x=TRUE)
# pbpAll$roof.x<-pbpAll$roof.y
# pbpAll$surface.x<-pbpAll$surface.y
# pbpAll<-pbpAll[,-c(373,374)]
# names(pbpAll)[names(pbpAll) == "roof.x"] <- "roof"
# names(pbpAll)[names(pbpAll) == "surface.x"] <- "surface"
#
# ### Remove Clock Plays###
# pbpAll<-subset(pbpAll,pbpAll$qb_spike==0)
# pbpAll<-subset(pbpAll,pbpAll$qb_kneel==0)
#
# ### weather
# Weather_df <- pbpAll %>% group_by(game_id) %>% sample_n(1)
#
# Kneel <- "Rain"
# Kneel_match <- gregexpr(pattern=Kneel, ignore.case = TRUE, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$Rain <- ifelse(Kneel_list == "Rain" , 1, 0)
# ftable(Weather_df$Rain)
#
# Kneel <- "rain"
# Kneel_match <- gregexpr(pattern=Kneel, ignore.case = TRUE, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$Rain <- ifelse(Kneel_list == "rain" , 1, Weather_df$Rain)
# ftable(Weather_df$Rain)
#
# Kneel <- "Snow"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$Snow <- ifelse(Kneel_list == "Snow" , 1, 0)
# ftable(Weather_df$Snow)
#
# Kneel <- "snow"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$Snow <- ifelse(Kneel_list == "snow" , 1, Weather_df$Snow)
# ftable(Weather_df$Snow)
#
# Kneel <- "Light Snow"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$Snow <- ifelse(Kneel_list == "Light Snow" , 0, Weather_df$Snow)
# Weather_df$LightRain <- ifelse(Kneel_list == "Light Snow" , 1, 0)
# ftable(Weather_df$Snow)
#
# Kneel <- "Light rain"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain <- ifelse(Kneel_list == "Light rain" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "Light Rain"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain <- ifelse(Kneel_list == "Light Rain" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "showers"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain <- ifelse(Kneel_list == "showers" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "Showers"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain <- ifelse(Kneel_list == "Showers" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "Chance of Rain"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain <- ifelse(Kneel_list == "Chance of Rain" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "Rain Chance"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain<- ifelse(Kneel_list == "Rain Chance" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Kneel <- "Freezing Rain"
# Kneel_match <- gregexpr(pattern=Kneel, Weather_df$weather)
# Kneel_list <- regmatches(Weather_df$weather, Kneel_match)
# Weather_df$LightRain<- ifelse(Kneel_list == "Freezing Rain" , 1, Weather_df$LightRain)
# ftable(Weather_df$LightRain)
#
# Weather_df$Snow<-ifelse(Weather_df$LightRain==1,0,Weather_df$Snow)
# Weather_df$Rain<-ifelse(Weather_df$LightRain==1,0,Weather_df$Rain)
# Weather_df$Rain<-ifelse(Weather_df$roof!="outdoors",0,Weather_df$Rain)
# Weather_df$LightRain<-ifelse(Weather_df$roof!="outdoors",0,Weather_df$LightRain)
# Weather_df$Snow<-ifelse(Weather_df$roof!="outdoors",0,Weather_df$Snow)
#
# Weather_df<-Weather_df[,c(1,373,374,375)]
#
# pbpAll<-merge(pbpAll,Weather_df,by="game_id")
# pbpAll$temp<-ifelse(pbpAll$roof!="outdoors",68,pbpAll$temp)
# pbpAll$wind<-ifelse(pbpAll$roof!="outdoors",0,pbpAll$wind)
#
# # ### ONLY FOR New DATA?
# # library(dplyr)
# # Weather_df <- pbpAll %>% group_by(game_id) %>% sample_n(1)
# # Weather_df$weather<-ifelse(is.na(Weather_df$weather),"n 0 0 0 0",Weather_df$weather)
# # WeatherData<-as.data.frame(Filter(function(x) any(!is.na(x)),
# #                                   read.csv(text = gsub("\\D+", ",", Weather_df$weather),
# #                                            fill = TRUE, header = FALSE)))
# #
# # WeatherData<-subset(WeatherData,(WeatherData$V1!=0))
# # WeatherData<-subset(WeatherData,is.na(WeatherData$V1))
# #
# #
# # Weather_df$temp<-WeatherData$V2
# # Weather_df$humd<-WeatherData$V3
# # Weather_df$wind<-WeatherData$V4
# # #
# # which( colnames(Weather_df)=="season" )
# #
# # Weather_df<-Weather_df[,c(1,286,334,335,376)]
# # ftable(is.na(pbpAll$wind),pbpAll$season)
# # ftable(is.na(Weather_df$wind),Weather_df$season)
# #
# # pbpAll<-merge(pbpAll,Weather_df,by="game_id")
# # pbpAll$temp.x<-pbpAll$temp.y
# # pbpAll$wind.x<-pbpAll$wind.y
# # pbpAll<-pbpAll[,-c(376,377)]
# # names(pbpAll)[names(pbpAll) == "temp.x"] <- "temp"
# # names(pbpAll)[names(pbpAll) == "wind.x"] <- "wind"
#
# ### Deal w windspeed
#
# pbpAll$wind <- ifelse(pbpAll$roof=="dome"|pbpAll$roof=="closed" , 0, pbpAll$wind)
# pbpAll$wind<-ifelse(is.na(pbpAll$wind),0,pbpAll$wind)
#
# pbpAll$temp <- ifelse(pbpAll$roof=="dome"|pbpAll$roof=="closed" , 69, pbpAll$temp)
# pbpAll$temp<-ifelse(is.na(pbpAll$temp),60,pbpAll$temp)
#
# pbpAll$success<-ifelse(is.na(pbpAll$success) & pbpAll$series_success==1,1,pbpAll$success)
# pbpAll$success<-ifelse(is.na(pbpAll$success) & pbpAll$series_success==0,0,pbpAll$success)
#
# pbpAll$i_wind<-ifelse(pbpAll$wind>0,1,0)
# pbpAll$i_surface<-ifelse(pbpAll$surface=="grass",1,0)
# pbpAll$i_roof<-ifelse(pbpAll$roof=="outdoors",0,1)
#
# summary(pbpAll$i_roof)
# summary(pbpAll$i_surface)
# summary(pbpAll$i_wind)
# summary(pbpAll$wind)
# summary(pbpAll$Snow)
# summary(pbpAll$LightRain)
# summary(pbpAll$Rain)
# summary(pbpAll$temp)
#
# pbpAll$yardline_100<-ifelse(is.na(pbpAll$yardline_100),20,pbpAll$yardline_100)
# pbpAll$wind<-ifelse(pbpAll$wind>21,21,pbpAll$wind)
# pbpAll$ydstogo<-ifelse(pbpAll$ydstogo>25,26,pbpAll$ydstogo)
# pbpAll$score_differential<-ifelse(pbpAll$score_differential>28,29,pbpAll$score_differential)
# pbpAll$score_differential<-ifelse(pbpAll$score_differential< -28,-29,pbpAll$score_differential)
#
# pbpAll$qtr<-ifelse(pbpAll$qtr>4,5,pbpAll$qtr)
# pbpAll$qtr<-as.factor(pbpAll$qtr)
# pbpAll$down<-as.factor(pbpAll$down)
# pbpAll$Rain<-as.factor(pbpAll$Rain)
# pbpAll$Snow<-as.factor(pbpAll$Snow)
# pbpAll$LightRain<-as.factor(pbpAll$LightRain)
# pbpAll$i_surface<-as.factor(pbpAll$i_surface)
# pbpAll$i_roof<-as.factor(pbpAll$i_roof)
# pbpAll$i_wind<-as.factor(pbpAll$i_wind)
#
#
# write.csv(pbpAll,'/Users/am/Desktop/NFLSets/PBP/NFLPBPClean.csv')
#
