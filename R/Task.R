####Task 2#### 0.25 Punkte
i <- 1
while (i > 0) {
  print(i)
  i <- i + 1
}

#Endless/infinitine loop because i is always bigger than 0. Programm will continue unless an external intervention occurs. To stop the loop we need a termination condition like 


####Task 3####

library(tidyverse)

set.seed(set.seed(20210614))
outcomes <- c("KOPF", "ZAHL")

sample(outcomes, 1, prob = c(.5, .5))

sample(outcomes, 1, prob = c(.5, .5))

sample(outcomes, 1, prob = c(.5, .5))

sample(outcomes, 1, prob = c(.5, .5))

#a) 4 Mal Münze werfen. In wie vielen Versuchen wird Kopf geworfen. 

set.seed(set.seed(20210614))
coin <- c("KOPF", "ZAHL")
out <- sample(coin, 4, replace=TRUE) 

for (i in 1:4) {
  out <- as.vector(table(sample(coin, 4, replace=TRUE)))
}
out
out[[1]] #Nur Kopf wird ausgegeben

###

set.seed(set.seed(20210614))
coin <- c("KOPF", "ZAHL")
out <- sample(coin, 4, replace=TRUE) 

for (i in 1:4) {
  out <- (table(sample(coin, 4, replace=TRUE)))
}
out
out[[1]] #Nur Kopf wird ausgegeben


#b) Unter Verwendung der Funkton aus a), schreibe einen Funktion, die n(Input) solcher Experimente durchführt (also n mal 4 mal Münze werfen). 
#Output soll ein Vektor der Länge n sein, der pro Eintrag die Anzahl der Versuche mit Ausgang "Kopf" angbit
#Verwende Funktion mit n=10000 und speichere das Ergebnis im Objekt outcome. 
#Berechne mit Hilfe von outcome die Wahreichnlichkeit für "4 mal Kopf bei 4 Münzwürfen"

flip_kopf <- function(n){
  out <- vector(n)
  coin <- c("KOPF", "ZAHL")
  biased <- c(.5,.5)
  for (i in 1:4) {
    out[1] <- replicate(n, sample(coin, 4, replace=TRUE, prob=biased))
  }
  out[1]
}

outcome <- flip_kopf(10000)

prop.table(table(sample(outcome, 4, replace=TRUE)))


#c) Welchen Gewinn/Verlust erwatet man, wenn man die Wette eingeht
#Wahrscheinlichkeit, dass Münze auf Kopf landet: 0,5
#Wahrscheinlichkeit, dass Münze zwei Mal auf Kopf landet: 0,5 x 0,5
#Wahrscheinlichkeit, dass Münze drei Mal auf Kopf landet: 0,5 x 0,5 x 0,5
#Wahrscheinlichkeit, dass Münze vier Mal auf Kopf landet: 0,5 x 0,5 x 0,5 x 0,5
#Verlustwahrscheinlichkeit: 0,0625 - > 6,25% (W'keit dass man 50EUR verliert)
#-> Erwartungswert: 

(0.0625 * -50) + (0.9375 * 1)
#-2.1875 ->-2,19 EUR Verlust wird erwartet




#d)

out <- as.vector(table(sample(coin, 4, replace=TRUE)))
out_head <- out[[1]]

coin_flip <- function(out_head) {                           
  nflips <- 0                                           
  nheads <- 0
  ntails <- 0
  while (nheads != 4 ) {                                 
    nflips <- nflips + 1
    flip <- sample(c(1,0),1,prob=c(out_head,1-out_head))
    if (flip == 1) {
      nheads <- nheads + 1
      ntails <- 0
    } else {
      ntails <- ntails + 1
      nheads <- 0
    }
  } 
  return(nflips)
}

##Source: https://stackoverflow.com/questions/59959209/coin-flip-simulation-using-r


#e)

x <- c(rep(nflips,100))
x


#f)
#diskrete Verteilung
plot(density(x))


###Task4###

#a) Plotten Sie die gesamte Zeitreihe fur das Bundesland Bayern (region == "DE-BY") als
#Liniendiagramm. Jeder einzelne Impfsto sollte als eigene Linie mit einer individuellen
#Farbe dargestellt sein.

vaccs <- readRDS("Downloads/vaccs.Rds")
View(vaccs)

library(tidyverse)
library(kableExtra)
library(streamgraph)
library(viridis)
library(DT)
library(plotly)

vaccs_1 <- subset(vaccs, region=="DE-BY")
View(vaccs_1)  

ggplot(data=vaccs_1, aes(x=date, y=dosen, group=factor(impfstoff), color=impfstoff))+
  geom_line()

##############################################
vaccs_x <- as.data.frame(vaccs_1)
str(vaccs_x)
vaccs_x$date <- as.numeric(vaccs_x$date)
vaccs_x$impfstoff <- as.numeric(vaccs_x$impfstoff)
str(vaccs_x)

library(ggthemes)

color_code <- c("astra" = "black",
                "comirnaty"="red",
                "johnson"="green",
                "moderna"="blue")

astra <- vaccs_x$impfstoff=="astra"
comirnaty <- vaccs_x$impfstoff=="comirnaty"
johnson <- vaccs_x$impfstoff=="johnson"
moderna <- vaccs_x$impfstoff=="moderna"


impfstoff_plot <- ggplot(data = vaccs_1, aes(x=date)) + 
  geom_line(aes(y=astra, col="astra"))+
  geom_line(aes(y=comirnaty, col="comirnaty"))+
  geom_line(aes(y=johnson, col="johnson"))+
  geom_line(aes(y=moderna, col="moderna"))+
  labs(x="Date", y="Index Value")+
  scale_color_manual("Index", values = color_code)+
  theme_wsj()


impfstoff_plot

#b) Erzeugen Sie einen gemeinsamen Plot, in dem das in a) erzeugte Diagramm fur die
#Bundeslander Saarland, Bremen, Hessen und Berlin ("DE-SL", "DE-HB", "DE-HE",
#"DE-BE") dargestellt wird. Die 4 Diagramme sollen in 2 Zeilen und 2 Spalten dargstellt werden.

vaccs_2 <- subset(vaccs, region=="DE-SL")
summary(vaccs_2)
vaccs_3 <- subset(vaccs, region=="DE-HB")
summary(vaccs_3)
vaccs_4 <- subset(vaccs, region=="DE-HE")
summary(vaccs_4)

vaccs_5 <- subset(vaccs, region=="DE-BE")
summary(vaccs_5)




ggplot_1 <- ggplot(data=vaccs_2, aes(x=date, y=dosen, group=factor(impfstoff), color=impfstoff))+geom_line() + labs(x="DE-SL")
ggplot_2 <- ggplot(data=vaccs_3, aes(x=date, y=dosen, group=factor(impfstoff), color=impfstoff))+geom_line() + labs(x="DE-HB")
ggplot_3 <- ggplot(data=vaccs_4, aes(x=date, y=dosen, group=factor(impfstoff), color=impfstoff))+geom_line() + labs(x="DE-HE")
ggplot_4 <- ggplot(data=vaccs_5, aes(x=date, y=dosen, group=factor(impfstoff), color=impfstoff))+geom_line() + labs(x="DE-BE")

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

figure <- ggarrange(ggplot_1, ggplot_2, ggplot_3, ggplot_4, ncol = 2, nrow = 2)
figure

#source: http://www.sthda.com/english/articles/32-r-graphics-essentials/126-combine-multiple-ggplots-in-one-graph/



#(c)  Wiederholen Sie die vorherige Teilaufgabe mit der relativen Anzahl geimpfter
#(= Anzahl geimpfter/Bevolkerung Bundesland).

#Saarland: 986.887 dosen:615.049
#Bremen: 681.202 dosen:342.651
#Hessen: 6.288.080 dosen: 3.153.144
#Berlin: 3.669.491 dosen: 1.875.955 
#rel.Häufigkeit: dosen/Einwohner

ggplot_5 <- ggplot(data=vaccs_2, aes(x=date, y=(dosen/986887), group=factor(impfstoff), color=impfstoff))+geom_line() + labs(x="DE-SL")
ggplot_6 <- ggplot(data=vaccs_3, aes(x=date, y=(dosen/681202), group=factor(impfstoff), color=impfstoff))+geom_line() + labs(x="DE-HB")
ggplot_7 <- ggplot(data=vaccs_4, aes(x=date, y=(dosen/6288080), group=factor(impfstoff), color=impfstoff))+geom_line() + labs(x="DE-HE")
ggplot_8 <- ggplot(data=vaccs_5, aes(x=date, y=(dosen/3669491), group=factor(impfstoff), color=impfstoff))+geom_line() + labs(x="DE-BE")


figure_2 <- ggarrange(ggplot_5, ggplot_6, ggplot_7, ggplot_8, ncol = 2, nrow = 2)
figure_2
