# Installation packages necessaires
install.packages("ggplot2")
library(ggplot2)

# Importation data frame
top14_class2015 <- read_csv("data/top14_class2015.csv")
top14_def2015 <- read_csv("data/top14_def2015.csv")

top14_class2016 <- read_csv("data/top14_class2016.csv")
top14_def2016 <- read_csv("data/top14_def2016.csv")

top14_class2017 <- read_csv("data/top14_class2017.csv")
top14_def2017 <- read_csv("data/top14_def2017.csv")

top14_class2019 <- read_csv("data/top14_class2019.csv")
top14_def2019 <- read_csv("data/top14_def2019.csv")

top14_class2020 <- read_csv("data/top14_class2020.csv")
top14_def2020 <- read_csv("data/top14_def2020.csv")

# Création de data frame pour chaque saison

## Renommage pour fusion
names(top14_class2015)[2]<-"Clubs"
names(top14_class2016)[2]<-"Clubs"
names(top14_class2017)[2]<-"Clubs"
names(top14_class2019)[2]<-"Clubs"
names(top14_class2020)[2]<-"Clubs"

## Regroupement des 2 fichiers pour chaque saisons

bilan2015<-merge(top14_class2015,top14_def2015, by = "Clubs")
bilan2016<-merge(top14_class2016,top14_def2016, by = "Clubs")
bilan2017<-merge(top14_class2017,top14_def2017, by = "Clubs")
bilan2019<-merge(top14_class2019,top14_def2019, by = "Clubs")
bilan2020<-merge(top14_class2020,top14_def2020, by = "Clubs")

## Création variable saison pour chaque table 
bilan2015$saison<-c("2015")
bilan2016$saison<-c("2016")
bilan2017$saison<-c("2017")
bilan2019$saison<-c("2019")
bilan2020$saison<-c("2020")

# Regroupement des toutes les saisons dans une table unique

bilan15_20<-do.call("rbind",list(bilan2015,bilan2016, bilan2017, bilan2019, bilan2020))


# Création variable plaq_p_m et pts_e_p_m

bilan2020$plaq_p_m<-bilan2020$`Plaquages offensifs réussis`/bilan2020$Matchs
bilan2020$pts_e_p_m<-bilan2020$Pts.E/bilan2020$J.

# Graphiques plaquages / pts encaissés
ggplot(bilan2020, aes(x=plaq_p_m, y=pts_e_p_m)) +
  geom_point() + # Show dots
  geom_label(label= bilan2020$Clubs,nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T )+
  ggtitle("Saison 2019-2020 : plaquages offensifs et points encaissés (après 12 journées)")+
  xlab ("Nombre de plaquage offensif par rencontre")+
  ylab("Nombre de points encaissés par rencontre")

# Graphiques plaquages / Classement
ggplot(bilan2020, aes(x=plaq_p_m, y=Rang)) +
  geom_point() + # Show dots
  geom_label(label= bilan2020$Clubs,nudge_x = 0.25, nudge_y = 0.25, 
             check_overlap = T )+
  ggtitle("Saison 2019-2020 : plaquages offensifs et points encaissés (après 12 journées)")+
  xlab ("Nombre de plaquage offensif par rencontre")+
  ylab("Classement")

# Période 2015-2020

# Création variable plaq_p_m et pts_e_p_m

bilan15_20$plaq_p_m<-bilan15_20$`Plaquages offensifs réussis`/bilan15_20$Matchs
bilan15_20$pts_e_p_m<-bilan15_20$Pts.E/bilan15_20$J.

# Graphiques plaquages / pts encaissés
ggplot(bilan15_20, aes(x=plaq_p_m, y=pts_e_p_m)) +
  geom_point(color="black",
             fill="orange",
             shape=22,
             alpha=0.5,
             size=6,
             stroke = 1) +
  ggtitle("Période 2015-2020 : plaquages offensifs et points encaissés")+
  xlab ("Nombre de plaquage offensif par rencontre")+
  ylab("Nombre de points encaissés par rencontre")

# Graphiques plaquages / Classement
ggplot(bilan15_20, aes(x=plaq_p_m, y=Rang)) +
  geom_point(color="black",
             fill="orange",
             shape=22,
             alpha=0.5,
             size=6,
             stroke = 1)+
  ggtitle("Période 2015-2020 : plaquages offensifs et classement")+
  xlab ("Nombre de plaquage offensif par rencontre")+
  ylab("Classement")

