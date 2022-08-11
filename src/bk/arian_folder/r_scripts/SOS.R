library(tools)
library(utils)
library(stargazer)
library(ggpubr)
library(nnet)
library(MASS)
library(mlogit)
library(foreign)
library(ggplot2)
library(MLmetrics)
library(caret)
library(leaps)
library(MPV)
library(zoo)
library(tidyverse)
library(data.table)


#rm(list = ls())

#pbp21<- fread('/Users/am/Desktop/NFLSets/PBP/NFLPBPClean.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)
#pbpAll<- fread('/Users/am/Desktop/NFLSets/PBP/NFLPBPClean21.csv', sep = ',', header = T, quote="\"", stringsAsFactors = F)

#pbp<-rbind(pbp21,pbpAll)
pbp_game<-pbp
### Final Game Info

Kneel <- "END GAME"
Kneel_match <- gregexpr(pattern=Kneel, pbp_game$desc)
Kneel_list <- regmatches(pbp_game$desc, Kneel_match)
pbp_game$lastPlay <- ifelse(Kneel_list == "END GAME" , 1, 0)
ftable(pbp_game$lastPlay)
length(unique(pbp_game$game_id))

pbp_game$keeps<-ifelse(lead(pbp_game$lastPlay)==1,1,0)
ftable(pbp_game$keeps)
pbpGame<-subset(pbp_game,pbp_game$keeps==1)
pbpGame<-as.data.frame(pbpGame)

detach("package:plyr", unload = TRUE)
library(dplyr)

Bins<- pbpGame %>%
  group_by(game_id,home_team,away_team) %>%
  summarize(PF=(home_score),PA=(away_score),spread_line=(spread_line),
            home_team=home_team,away_team=away_team,game_id=game_id)
Bins$W<-ifelse(Bins$PF>Bins$PA,1,0)
Bins$W<-ifelse(Bins$PF==Bins$PA,0.5,0)

keeps <- c("game_id","home_team","away_team","season","week","game_date")
Games<-pbpGame[ , (names(pbpGame) %in% keeps)]

AllGames<-merge(Games,Bins,by=c("game_id","home_team","away_team"))


Game2<-AllGames
Game2$away_team2<-Game2$home_team
Game2$home_team<-Game2$away_team
Game2$away_team<-Game2$away_team2
Game2$spread_line<-Game2$spread_line*-1
Game2$PA2<-Game2$PF
Game2$PF<-Game2$PA
Game2$PA<-Game2$PA2
Game2$W<-abs(Game2$W-1)

keeps <- c("game_id","home_team","away_team","season","week","game_date","PF","PA","spread_line","W")
Game2<-Game2[ , (names(Game2) %in% keeps)]

Game<-rbind(AllGames,Game2)

### Run GameData

Game<-merge(Game,GameData,by=c("game_id","home_team","away_team","season","week"),all.x=TRUE)

#Game<-subset(Game,Game$week<18)
library(tidyverse)
Game<-Game %>% complete(nesting(home_team,season), week = seq(min(week), max(week), 1L))
Game<-Game %>% complete(nesting(away_team,season), week = seq(min(week), max(week), 1L))

Game$PF<-ifelse(is.na(Game$PF),0.01,Game$PF)
Game$PA<-ifelse(is.na(Game$PA),0.01,Game$PA)
Game$spread_line<-ifelse(is.na(Game$spread_line),0,Game$spread_line)
Game$W<-ifelse(Game$PF>Game$PA,1,0)
Game$W<-ifelse(Game$PF==Game$PA,0,Game$W)
Game$W<-ifelse(is.na(Game$W),0,Game$W)

Game[,(7:67)]<-ifelse(is.na((Game[,(7:67)])),0.01,unlist(Game[,(7:67)]))

Game$season<-ifelse(is.na(Game$season),lag((Game$season)),(Game$season))
Game$Count<-ifelse(is.na(Game$game_date),0,1)
#Game$game_id<-ifelse(is.na(Game$game_id),lag((Game$game_id)+0.5),(Game$game_id))

GamesNew<-arrange(Game,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumPtsA= rollapply(PF, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumPts= rollapply(PA, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumSprd= rollapply(spread_line, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumW= rollapply(W, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumCount= rollapply(Count, list(-seq(22)), sum, partial = TRUE, fill = NA))

GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumPass_epa= rollapply(pass_epa, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumRun_epa= rollapply(run_epa, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumDPass_epa= rollapply(Dpass_epa, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumDRun_epa= rollapply(Drun_epa, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumpassCount= rollapply(passCount, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumRunCount= rollapply(RunCount, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumDpassCount= rollapply(DpassCount, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumDRunCount= rollapply(DRunCount, list(-seq(22)), sum, partial = TRUE, fill = NA))

GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumPass_success= rollapply(Pass_success, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumRun_success= rollapply(Run_success, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumDPass_success= rollapply(DPass_success, list(-seq(22)), sum, partial = TRUE, fill = NA))
GamesNew<-arrange(GamesNew,away_team,season,week) %>%
  group_by(season,away_team)%>%
  mutate(CumDRun_success= rollapply(DRun_success, list(-seq(22)), sum, partial = TRUE, fill = NA))


NewData <- Game[rep(rownames(Game), 21-Game$week), ]
NewData$week <- NewData$week + (sequence(21-Game$week))
NewData<-merge(NewData,GamesNew,by=c("away_team","week","season"))
#NewData$count<-1
#NewData$count<-ifelse(is.na(NewData$game_id.x),0,NewData$count)


AllGames2<-rbind(AllGames,Game2)
AllGames2<-merge(AllGames2,GameData,by=c("game_id","home_team","away_team","season","week"))
#AllGames2<-subset(AllGames2,AllGames2$week<18)
AllGames2<-AllGames2 %>% complete(nesting(home_team,season), week = seq(min(week), max(week), 1L))
AllGames2<-AllGames2 %>% complete(nesting(away_team,season), week = seq(min(week), max(week), 1L))

AllGames2$PF<-ifelse(is.na(AllGames2$PF),0.01,AllGames2$PF)
AllGames2$PA<-ifelse(is.na(AllGames2$PA),0.01,AllGames2$PA)
AllGames2$spread_line<-ifelse(is.na(AllGames2$spread_line),0,AllGames2$spread_line)
AllGames2$W<-ifelse(AllGames2$PF>AllGames2$PA,1,0)
AllGames2$W<-ifelse(AllGames2$PF==AllGames2$PA,0,AllGames2$W)
AllGames2$W<-ifelse(is.na(AllGames2$W),0,AllGames2$W)
AllGames2$Count<-ifelse(is.na(AllGames2$game_date),0,1)
AllGames2[,(7:67)]<-ifelse(is.na((AllGames2[,(7:67)])),0.01,unlist(AllGames2[,(7:67)]))

OwnPoints<-arrange(AllGames2,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumPtsF= rollapply(PF, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumPtsA= rollapply(PA, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumSprd= rollapply(spread_line, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumW= rollapply(W, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumCount= rollapply(Count, list(-seq(22)), sum, partial = TRUE, fill = NA))

OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumPass_epa= rollapply(pass_epa, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumRun_epa= rollapply(run_epa, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumDPass_epa= rollapply(Dpass_epa, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumDRun_epa= rollapply(Drun_epa, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumpassCount= rollapply(passCount, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumRunCount= rollapply(RunCount, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumDpassCount= rollapply(DpassCount, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumDRunCount= rollapply(DRunCount, list(-seq(22)), sum, partial = TRUE, fill = NA))

OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumPass_success= rollapply(Pass_success, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumRun_success= rollapply(Run_success, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumDPass_success= rollapply(DPass_success, list(-seq(22)), sum, partial = TRUE, fill = NA))
OwnPoints<-arrange(OwnPoints,home_team,season,week) %>%
  group_by(season,home_team)%>%
  mutate(CumDRun_success= rollapply(DRun_success, list(-seq(22)), sum, partial = TRUE, fill = NA))

Bins<- NewData %>%
  group_by(season,home_team.x,week) %>%
  summarize(CumPts=sum(CumPts),CumPtsA=sum(CumPtsA)
            ,CumSprd=sum(CumSprd),CumW=sum(CumW),count=sum(CumCount),
            CumPass_epa=sum(CumPass_epa),CumRun_epa=sum(CumRun_epa),
            CumDPass_epa=sum(CumDPass_epa),CumDRun_epa=sum(CumDRun_epa),
            CumpassCount=sum(CumpassCount),CumRunCount=sum(CumRunCount),
            CumDpassCount=sum(CumDpassCount),CumDRunCount=sum(CumDRunCount),
            CumPass_success=sum(CumPass_success),CumRun_success=sum(CumRun_success),
            CumDPass_success=sum(CumDPass_success),CumDRun_success=sum(CumDRun_success),)

Bins<-subset(Bins,!is.na(Bins$home_team.x))
Bins$Team<-Bins$home_team.x


OwnPointsBins<- OwnPoints %>%
  group_by(season,home_team,week) %>%
  summarize(HCumPts=sum(CumPtsF),HCumPtsA=sum(CumPtsA)
            ,HCumSprd=sum(CumSprd),HCumW=sum(CumW),Hcount=sum(CumCount),
            HCumPass_epa=sum(CumPass_epa),HCumRun_epa=sum(CumRun_epa),
            HCumDPass_epa=sum(CumDPass_epa),HCumDRun_epa=sum(CumDRun_epa),
            HCumpassCount=sum(CumpassCount),HCumRunCount=sum(CumRunCount),
            HCumDpassCount=sum(CumDpassCount),HCumDRunCount=sum(CumDRunCount),
            HCumPass_success=sum(CumPass_success),HCumRun_success=sum(CumRun_success),
            HCumDPass_success=sum(CumDPass_success),HCumDRun_success=sum(CumDRun_success))

OwnPointsBins<-subset(OwnPointsBins,!is.na(OwnPointsBins$home_team))
#OwnPointsBins<-subset(OwnPointsBins,OwnPointsBins$week<18)
OwnPointsBins$Team<-OwnPointsBins$home_team

FinalBins<-merge(Bins,OwnPointsBins,by=c("Team","season","week"),all.x=T)
FinalBins<-arrange(FinalBins,Team,season,week)

FinalBins$count<-FinalBins$count-FinalBins$Hcount
FinalBins$CumW<-FinalBins$CumW-FinalBins$HCumW
FinalBins$CumPtsA<-FinalBins$CumPtsA-FinalBins$HCumPts
FinalBins$CumPts<-FinalBins$CumPts-FinalBins$HCumPtsA
FinalBins$CumSprd<-FinalBins$CumSprd-FinalBins$HCumSprd
FinalBins[,c(5:8)]<-(FinalBins[,c(5:8)])/FinalBins$count

FinalBins$CumpassCount<-FinalBins$CumpassCount-FinalBins$HCumpassCount
FinalBins$CumDpassCount<-FinalBins$CumDpassCount-FinalBins$HCumDpassCount
FinalBins$CumRunCount<-FinalBins$CumRunCount-FinalBins$HCumRunCount
FinalBins$CumDRunCount<-FinalBins$CumDRunCount-FinalBins$HCumDRunCount

FinalBins$CumPass_epa<-FinalBins$CumPass_epa-FinalBins$HCumPass_epa
FinalBins$CumRun_epa<-FinalBins$CumRun_epa-FinalBins$HCumRun_epa
FinalBins$CumDPass_epa<-FinalBins$CumDPass_epa-FinalBins$HCumDPass_epa
FinalBins$CumDRun_epa<-FinalBins$CumDRun_epa-FinalBins$HCumDRun_epa

FinalBins$CumOEpa<-round((FinalBins$CumPass_epa+FinalBins$CumRun_epa)/(FinalBins$CumpassCount+FinalBins$CumRunCount),5)
FinalBins$CumDEpa<-round((FinalBins$CumDPass_epa+FinalBins$CumDRun_epa)/(FinalBins$CumDpassCount+FinalBins$CumDRunCount),5)

FinalBins$CumPass_epa<-FinalBins$CumPass_epa/FinalBins$CumpassCount
FinalBins$CumRun_epa<-FinalBins$CumRun_epa/FinalBins$CumRunCount
FinalBins$CumDPass_epa<-FinalBins$CumDPass_epa/FinalBins$CumDpassCount
FinalBins$CumDRun_epa<-FinalBins$CumDRun_epa/FinalBins$CumDRunCount

FinalBins$CumPass_success<-FinalBins$CumPass_success-FinalBins$HCumPass_success
FinalBins$CumRun_success<-FinalBins$CumRun_success-FinalBins$HCumRun_success
FinalBins$CumDPass_success<-FinalBins$CumDPass_success-FinalBins$HCumDPass_success
FinalBins$CumDRun_success<-FinalBins$CumDRun_success-FinalBins$HCumDRun_success

FinalBins$CumOsuccess<-round((FinalBins$CumPass_success+FinalBins$CumRun_success)/(FinalBins$CumpassCount+FinalBins$CumRunCount),5)
FinalBins$CumDsuccess<-round((FinalBins$CumDPass_success+FinalBins$CumDRun_success)/(FinalBins$CumDpassCount+FinalBins$CumDRunCount),5)

FinalBins$CumPass_success<-FinalBins$CumPass_success/FinalBins$CumpassCount
FinalBins$CumRun_success<-FinalBins$CumRun_success/FinalBins$CumRunCount
FinalBins$CumDPass_success<-FinalBins$CumDPass_success/FinalBins$CumDpassCount
FinalBins$CumDRun_success<-FinalBins$CumDRun_success/FinalBins$CumDRunCount

FinalBins2<-FinalBins[,c(1,2,3,5:8,10:13,18:21)]
write.csv(FinalBins2,"/Users/am/Desktop/NFLSets/GBGData/SRS.csv")

library(nflfastR)
logos <- teams_colors_logos %>% select(team_abbr, team_logo_espn)
logos$Team<-logos$team_abbr
FinalBins<-merge(FinalBins,logos, by="Team")
library(ggimage)

