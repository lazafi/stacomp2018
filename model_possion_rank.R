teams.all <- c("Russia", "Portugal", "France", "Argentina", "Saudi Arabia", "Spain", "Australia", "Egypt", "Morocco", "Peru", "Croatia", "Uruguay", "Iran", "Denmark", "Nigeria", "Brazil", "Germany", "Belgium", "Poland", "Switzerland", "Sweden",  "Senegal", "Costa Rica", "Mexico", "Tunisia", "Colombia", "Serbia", "Korea Republic", "England", "Japan")
# "Iceland","Panama",

matches <- read.csv("data/WorldCupMatches.csv") %>%
  dplyr::filter(
    Away.Team.Name %in% teams.all | Home.Team.Name %in% teams.all,
    Year >= 1970
  )
ranks <- med_rank_by_country$last_rank
names(ranks) <- med_rank_by_country$country_full
matches.a <- matches %>%
  transmute(
    team = Home.Team.Name,
    goals = Home.Team.Goals,
    opponent = Away.Team.Name,
  )
matches.b <- matches %>%
  transmute(
    team = Away.Team.Name,
    goals = Away.Team.Goals,
    opponent = Home.Team.Name,
  )
rest <- expand.grid(team = c("Panama", "Peru", "Iceland"),
                    opponent = c("Panama", "Peru", "Iceland"),
                    goals = 0
                )                  

matches.a <- matches.a %>%
  mutate(
    opponent_rank = ranks[opponent],
    team_rank = ranks[team]
  )

matches.b <- matches.b %>%
  mutate(
    opponent_rank = ranks[opponent],
    team_rank = ranks[team]
  )

rest <- rest %>%
  mutate(
    opponent_rank = 1000,
    team_rank = 1000
  )



model.poisson.rank <- rbind(matches.a, matches.b, rest) %>%
  glm(data=., goals ~ team + opponent + team_rank + opponent_rank, family="poisson")

#summary(model.poisson.rank)

predict.wm.rank <- function(home.team, away.team, model) {
  #model=model.poisson
  #home.team="Russia"
  #away.team="Italy"
  hrank <- ifelse (is.na(ranks[home.team]), 0, ranks[home.team])
  names(hrank) <- NULL
  orank <- ifelse (is.na(ranks[away.team]), 0, ranks[away.team])
  names(orank) <- NULL
  
  #print(home.team)
  #print(hrank)
  #print(away.team)
  #print(orank)
  input <- data.frame(team=c(home.team, away.team),opponent=c(away.team, home.team), team_rank=c(hrank, orank), opponent_rank=c(orank, hrank))
  input$prediction <- predict(model, newdata=input, type="response")
  
  m <- dpois(0:10, input[1,]$prediction) %o%  dpois(0:10, input[2,]$prediction)
  
  r <- c(
    'W' = sum(m[lower.tri(m)]),  
    'D' = sum(diag(m)),
    'L' = sum(m[upper.tri(m)])
  )
  #max(home.prob, draw.prob, away.prob)
  #colname(max(r))
  names(which.max(r))
}

predict.matrix <- function(teams, predict_function, predict_args = NULL) {
  #teams <- teams.all
  #predict_function <- predict.wm.rank
  #predict_args <- model.poisson
  
  m <- matrix(0, nrow = length(teams), ncol = length(teams), dimnames = list(teams, teams))
  diag(m) <- NA
  
  for(j in 1:ncol(m)){
    for(i in 1:nrow(m)){
      if (i != j) {
        hometeam <- colnames(m)[i]
        oppteam <- colnames(m)[j]
        args <- list(hometeam, oppteam)
        m[i,j] = do.call(predict_function, c(args,list(predict_args)))
      }
    }
  } 
  m
}


prediction <- predict.matrix(teams.all, predict.wm.rank, predict_args=model.poisson.rank)
peek(prediction, 5)
evaluate.wm(prediction, fifa.results)
