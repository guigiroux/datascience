library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(rio)

setwd("C:/Users/Paul/Desktop/Data")
load("flights.rda")
load("airports.rda")
load("airlines.rda")
load("weather.rda")
load("planes.rda")





#Combien y-a-t-il d'aéroports, de compagnies, 
#de destinations, d'avions, de fuseaux horaires 
#et de zones aux Etats-Unis où on ne passe pas à l'heure d'été
airports %>% filter(dst == "N") %>% count()

#Quel aéroport de départ est le plus prisé ?
Orig_Airport <- flights %>% count(origin, sort = TRUE)
Depart <- ggplot(Orig_Airport, aes(origin, n))
Depart + geom_col(colour = "Blue") + labs(x = "Airport of origin", y = "Number of flights")

# -> Quelles sont les 10 destinations les plus (moins) prisées ?
Dest_Airport <- flights %>% count(dest, sort = TRUE)
View(Dest_Airport)

# Quels sont les 10 avions qui ont le plus (moins) décollé ?
View(flights %>% count(tailnum, sort = TRUE))

#Trouver combien chaque compagnie a desservi de destination ; 
#combien chaque compagnie a desservie de destination par aéroport d'origine.
#Réaliser les graphiques adéquats qui synthétisent ces informations ? 
Comp_Dest <- flights %>% count(carrier)
GComp_Dest <- ggplot(Comp_Dest, aes(carrier, n))
GComp_Dest + geom_col()  + labs(x = "Company", y = "Number of flights")

EWR <- flights %>% filter(flights$origin == "EWR") %>% count(carrier)
gEWR <- ggplot(EWR, aes(carrier, n))
gEWR + geom_col() + labs(x = "Company", y = "Number of flights from EWR")

JFK <- flights %>% filter(flights$origin == "JFK") %>% count(carrier)
gJFK <- ggplot(JFK, aes(carrier, n))
gJFK + geom_col() + labs(x = "Company", y = "Number of flights from JFK")

LGA <- flights %>% filter(flights$origin == "LGA") %>% count(carrier)
gLGA <- ggplot(LGA, aes(carrier, n))
gLGA + geom_col() + labs(x = "Company", y = "Number of flights from LGA")


#Trouver tous les vols ayant atterri à Houston (IAH ou HOU).
View(flights %>% filter(dest == "HOU" | dest == "IAH") %>% count())

#Combien de vols partent de NYC airports vers Seattle ?
flights %>% filter(dest == "SEA") %>% count()

#Combien de compagnies desservent cette destination ?
flights %>% filter(dest == "SEA") %>% count(carrier)

#combien d'avions "uniques" (indice : 936 avions) ?
q5 <- flights %>% select(tailnum, dest) %>% 
  filter(dest == "SEA") %>% 
  distinct(tailnum, dest) %>% 
  count(dest)

# q6--------------
#Trier les vols suivant l'aéroport d'origine, 
#la compagnie et la destination dans un ordre alphabétique croissant 
#(en réalisant les jointures nécessaires pour obtenir les noms des explicites des aéroports) ? 
q6 <- flights %>% select(flight, origin, dest, carrier) %>%
  inner_join(airlines) %>%
  inner_join(airports, by=c("origin" = "faa")) %>%
  inner_join(airports, by=c("dest"="faa")) %>% 
  select(name, name.y, name.x, origin, dest, carrier, flight) %>% 
  rename(airport_dest = name, name_carrier = name.x, airport_orig = name.y) %>% 
  distinct(flight, origin, dest, carrier, name_carrier,  airport_orig, airport_dest) %>% 
  select(airport_dest, airport_orig, name_carrier, origin, dest, carrier, flight) %>% 
  arrange(airport_dest, airport_orig, carrier, flight)


q6_nb <- q6 %>% group_by(airport_dest) %>% summarise(nb_vol_unique = n()) %>% 
  arrange(desc(nb_vol_unique))
View(q6_nb)

#Quelles sont les compagnies qui n'opèrent pas sur tous les aéroports d'origine ?
#Quelles sont les compagnies qui desservent l'ensemble de destinations ?
#Faire un tableau où l'on récupère l'ensemble des origines et des destinations 
#pour l'ensemble des compagnies

# q7------------------
q7_1 <- flights %>% select(origin, carrier, dest) %>% 
  inner_join(airlines) %>% 
  select(carrier, dest, name) %>%
  group_by(carrier) %>% 
  distinct(dest, carrier, name) %>% 
  arrange(dest, carrier, name) %>% 
  count(name) %>% 
  rename(nb_dest = n)

q7_2 <- flights %>% select(carrier, origin) %>% 
  inner_join(airlines) %>% 
  distinct(carrier, origin, name) %>% 
  count(carrier, name) %>% 
  rename(nb_origin = n)

q7 <- q7_1 %>% select(carrier, name, nb_dest) %>% 
  inner_join(q7_2) %>% 
  select(nb_origin, carrier, name, nb_dest)
  

#Quelles sont les destinations qui sont exclusives à certaines compagnies
#q8-----------------
q8_1 <- flights %>% select(carrier, dest) %>% 
  inner_join(airlines) %>% 
  inner_join(airports, by = c("dest" = "faa")) %>% 
  distinct(dest, carrier, name.x, name.y) %>% 
  rename("Compagnie" = name.x, Aeroport = name.y) %>% 
  arrange(dest, Aeroport, Compagnie, carrier) %>% 
  count(dest) %>% 
  filter(n == 1) %>% 
  rename(nb_dest = n)
  
  
q8_2 <- flights %>% select(carrier, dest) %>% 
  inner_join(airlines) %>% 
  inner_join(airports, by = c("dest" = "faa")) %>% 
  distinct(dest, carrier, name.x, name.y) %>% 
  rename(Compagnie = name.x, Aeroport = name.y) %>% 
  arrange(dest, Aeroport, Compagnie, carrier)
  

q8 <- q8_1 %>% select(dest, nb_dest) %>% 
  inner_join(q8_2) %>% 
  select(dest, Aeroport, nb_dest, Compagnie)

  
##Filtrer le vol pour trouver ceux exploités par United, American ou Delta
#q9-----------------
q9 <- flights %>%  select(flight, carrier, year, month, day, hour) %>% 
  inner_join(airlines) %>% 
  select(flight, carrier, name, year, month, day, hour) %>%
  filter(name == "United Air Lines Inc." | name == "Delta Air Lines Inc." | name == "American Airlines Inc.") %>% 
  arrange(name, flight)

#reporting
#rq1--------
rq1 <- flights %>%  select(flight, origin, dest, year, month, day, hour, minute) %>% 
  unite(col = "Schedulate Deptime", year, month, day, hour, minute)
  
  
#rq2-------


#QGraphe

#Qui veut bien rendre ce graphique plus attractif ? 
#  Produire les éventuelles autres graphiques ?

Data <- flights %>% count(month)

g <- ggplot(Data, aes(month, n))
g + geom_smooth(colour = 'red') + labs(y = "flights per month") + scale_x_continuous(expand = c(0, 0))
