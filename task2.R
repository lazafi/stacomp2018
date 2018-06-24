#task 2
library(magrittr)
library(dplyr)
library(ggplot2)

ranking <- read.csv("data/fifa_ranking.csv") 
#convert to date
ranking$rank_date <- as.Date(ranking$rank_date)
head(ranking)
str(ranking)
summary(ranking)
#table(ranking)

plot(ranking$rank)
ranking$rank
med_rank_by_country <- aggregate(ranking$rank, by=list(ranking$country_abrv), FUN=median)

med_rank_by_country <- ranking %>%
group_by(country_abrv) %>%
  summarise(
    med_rank = median(rank)
    ) %>%
  arrange(med_rank)  %>%
 transmute(country_abrv = as.character(country_abrv),
           med_rank = med_rank)

first20 <- med_rank_by_country[1:20,] %>%
  arrange(med_rank)  %>%
  transmute(country_abrv = as.factor(country_abrv, levels=country_abrv),
            med_rank = med_rank)
head(first20)

# TODO: order by rank
ggplot(first20, aes(x=country_abrv, y=med_rank)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


## plot time by country

bra_ts <- ranking %>%
  filter(country_abrv == "BRA") %>%
  transmute(
    date = rank_date,
    rank = rank
  )


ger_ts <- ranking %>%
  filter(country_abrv == "GER") %>%
  transmute(
            date = rank_date,
            rank = rank
            )
#plot(ger_ts)

ggplot(ger_ts, aes(x=date)) + 
  geom_line(aes(y=rank)) + 
  labs(title="Time Series Chart", 
       subtitle="Returns Percentage from 'Economics' Dataset", 
       caption="Source: Economics", 
       y="Returns %")

