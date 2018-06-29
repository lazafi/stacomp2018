#task 2
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)

ranking <- read.csv("data/fifa_ranking.csv")  %>%
  mutate (
    rank_date = as.Date(rank_date)
  )
#convert to date
#ranking$rank_date <- as.Date(ranking$rank_date)
head(ranking)
str(ranking)
summary(ranking)
#table(ranking)

ranking_de <- ranking  %>%
  filter(
    country_abrv == "GER"
  )  %>%
  select(
    rank_date, 
#    cur_year_avg,
#    cur_year_avg_weighted
 matches("*avg*"),
 - country_full,
 - country_abrv,
 - confederation
         ) %>%
  gather(key = "variable", value = "value", -rank_date)

ggplot(ranking_de, aes(x = rank_date, y = value)) + 
  geom_line(aes(color = variable)) +
#  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()
  



# ranking_de_num <- ranking_de[,sapply(ranking_de, is.numeric)]
# plot <- ggplot(ranking_de, aes(rank_date))
# #plot <- baseplot + geom_line(aes_string(y=col)) 
# i<-0
# for (col in names(ranking_de_num)) {
#   i<-i+1
#   print(i)
#   print(col)
#   plot <- plot + geom_line(aes_string(y=col, col=i))
# }
# print(plot)
# 
# col="three_year_ago_weighted"
# col2 = "cur_year_avg_weighted"
# plots <- ggplot(ranking_de, aes(rank_date)) + geom_line(aes_string(y=col)) + geom_line(aes_string(y=col2))
# print(plots)
# 
# baseplot <- ggplot(ranking_de, aes(rank_date))
# plot <- baseplot + geom_line(aes_string(y=col)) 
# plot <- plot + geom_line(aes_string(y=col2))
# 
# print(plot)
# #plot(ranking$rank)
# #ranking$rank
# #med_rank_by_country <- aggregate(ranking$rank, by=list(ranking$country_abrv), FUN=median)

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


# bra_ts <- ranking %>%
#   filter(country_abrv == "BRA") %>%
#   transmute(
#     abrv = factor("BRA"),
#     date = rank_date,
#     rank = rank
#   )
# 
# 
# ger_ts <- ranking %>%
#   filter(country_abrv == "GER") %>%
#   transmute(
#     abrv = factor("GER"),
#     date = rank_date,
#             rank = rank
#             )
# #plot(ger_ts)
#gb_ts <- rbind(bra_ts, ger_ts)

favorites <- c("Germany", "Brazil", "Belgium", "France", "Argentina")

favs_ts <- ranking %>%
  filter(country_full %in% favorites)


ggplot(favs_ts, aes(x=rank_date)) + 
  geom_line(aes(y=rank, col=country_abrv)) + 
  scale_y_reverse() +
  labs(title="WC Ranking", 
       subtitle="Wold Cup Ranking from 1992",
       caption="Source: FIFA",
       y="FIFA Ranking",
       x="Date")

