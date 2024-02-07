# Jakub Pączek Nr.Albumu 414592

# Dane, które będę rozpatrywał, dotyczą PKB per capita dla całego świata

# Wczytanie danych

#install.packages("rvest")
library(rvest)
page <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita")
dane <- html_node(page, ".wikitable")
dane <- html_table(dane, fill = TRUE, dec=",")

# Obróbka danych

dane <- dane[3:nrow(dane),] #dwa pierwsze wiersze są zbędne
fun1 <- function(x){gsub("\\[.*?\\]","",x)}
dane <- data.frame(lapply(dane, fun1)) # usuwam przypisy
fun2 <- function(x){ifelse(x =="—", NA, x)}
dane <- data.frame(lapply(dane, fun2)) # braki w danych
dane[,2] <- as.factor(dane[,2])
fun3 <- function(x){as.numeric(gsub(",", "", x))} # konwertuję na typ numeryczny
dane[,3:8] <- lapply(dane[,3:8], fun3)

# Analiza danych

cat("Dane dotyczą w sumie", nrow(dane), "państw świata i terytoriów zależnych")
cat("Tak wyglądają ich liczby względem kontynentów:")
table(dane[,2])

# Dane zwierają wielkości PKB per capita według trzech osobnych rankingów

cat("Braków w danych w sumie jest", sum(is.na(dane)))

# Zaprezentuję jak wyglądają średnie i odchylenia standardowe z podziałem na kontynenty

library(dplyr)
dane %>%
  group_by(UN.Region) %>%
  summarize(Średnia1 = mean(IMF.4..5., na.rm = T),
            Odch1 = sd(IMF.4..5., na.rm = T),
            Srednia2 = mean(World.Bank.6., na.rm = T),
            Odch2 = sd(World.Bank.6., na.rm = T),
            Srednia3 = mean(United.Nations.7., na.rm = T),
            Odch3 = sd(United.Nations.7., na.rm = T))

# Wyznaczę kraje o najwyższym PKB per capita (dla pierwszego z rankingów)
dane %>%
  top_n(10, IMF.4..5.) %>%
  select(1:4)

# I o najniższym
dane %>%
  top_n(-10, IMF.4..5.) %>%
  select(1:4)

# Sporządzę teraz histogramy dla każdego rankingu

library(ggplot2)

dane %>%
  ggplot(aes(x = IMF.4..5.)) +
  geom_histogram(na.rm = T) +
  labs(title = "Histogram PKB per capita IMF",
       x = "PKB per capita")

dane %>%
  ggplot(aes(x = World.Bank.6.)) +
  geom_histogram(na.rm = T) +
  labs(title = "Histogram PKB per capita Bank Światowy",
       x = "PKB per capita")

dane %>%
  ggplot(aes(x = United.Nations.7.)) +
  geom_histogram(na.rm = T) +
  labs(title = "Histogram PKB per capita ONZ",
       x = "PKB per capita")

# Sporządze dla pierwszego rankingu wykresy pudełkowe z wąsem w zależności od kontynentu

dane %>%
  ggplot(aes(x = IMF.4..5., y = UN.Region)) +
  geom_boxplot(na.rm = T) +
  labs(title = "PKB per capita wg. kontynentu",
       x = "PKB per capita",
       y = "Kontynent")
