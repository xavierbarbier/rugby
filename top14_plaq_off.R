library(dplyr)
library(ggplot2)
library(ggthemes)

# Importation data frame
top14_class2015 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/rugby/master/top14_class2015.csv")
top14_def2015 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/rugby/master/top14_def2015.csv")

top14_class2016 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/rugby/master/top14_class2016.csv")
top14_def2016 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/rugby/master/top14_def2016.csv")

top14_class2017 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/rugby/master/top14_class2017.csv")
top14_def2017 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/rugby/master/top14_def2017.csv")

top14_class2019 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/rugby/master/top14_class2019.csv")
top14_def2019 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/rugby/master/top14_def2019.csv")

top14_class2020 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/rugby/master/top14_class2020.csv")
top14_def2020 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/rugby/master/top14_def2020.csv")

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

# conservation variables d'intérêts

bilan2020<-bilan2020%>% select("Clubs", "saison","Rang","Pts" ,"J.","Pts.M","Pts.E","Plaquages.offensifs.réussis","Matchs" )


# Création variable plaq_p_m et pts_e_p_m

bilan2020$plaq_p_m<-bilan2020$Plaquages.offensifs.réussis/bilan2020$Matchs
bilan2020$pts_e_p_m<-bilan2020$Pts.E/bilan2020$J.

# summary plaquages offensifs
summary(bilan2020$plaq_p_m) # moyenne = 14

# distribution plaquages off par match

ggplot(bilan2020,aes(x=bilan2020$plaq_p_m)) + 
  geom_histogram(fill="#f8766d")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")+
  ggtitle("Distribution du nombre de plaquages offensifs par match (12 matchs saison 2019-2020)")+
  theme(legend.title = element_blank(),
        legend.position=c(0.8, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),legend.text = element_text(size=20, 
                                                                face="bold"))

# Graphiques plaquages / pts encaissés
ggplot(bilan2020, aes(x=plaq_p_m, y=pts_e_p_m)) +
  geom_point() + # Show dots
  geom_label(label= bilan2020$Clubs,nudge_x = 0.25, nudge_y = 0.25, 
             check_overlap = T )+
  ggtitle("Saison 2019-2020 : plaquages offensifs et points encaissés (après 12 journées)")+
  xlab ("Nombre de plaquage offensif par rencontre")+
  ylab("Nombre de points encaissés par rencontre")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")+
  theme(legend.title = element_blank(),
        legend.position=c(0.8, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),legend.text = element_text(size=20, 
                                                                face="bold"))+
  
  geom_smooth(method=lm , color="red", se=FALSE)

# Graphiques plaquages / Classement
ggplot(bilan2020, aes(x=plaq_p_m, y=Rang)) +
  geom_point() + # Show dots
  geom_label(label= bilan2020$Clubs,nudge_x = 0.25, nudge_y = 0.25, 
             check_overlap = T )+
  ggtitle("Saison 2019-2020 : plaquages offensifs et points encaissés (après 12 journées)")+
  xlab ("Nombre de plaquage offensif par rencontre")+
  ylab("Classement")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")+
  theme(legend.title = element_blank(),
        legend.position=c(0.8, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),legend.text = element_text(size=20, 
                                                                face="bold"))+
  
  geom_smooth(method=lm , color="red", se=FALSE)

summary(lm(bilan2020$Rang ~ bilan2020$plaq_p_m)) # coef = -0.27 / R2 = -0.03

# Période 2015-2020

# conservation variables d'intérêts

bilan15_20<-bilan15_20%>% select("Clubs","saison","Rang","Pts" ,"J.","Pts.M","Pts.E","Plaquages.offensifs.réussis","Matchs" )

# Création variable plaq_p_m et pts_e_p_m

bilan15_20$plaq_p_m<-bilan15_20$Plaquages.offensifs.réussis/bilan15_20$Matchs
bilan15_20$pts_e_p_m<-bilan15_20$Pts.E/bilan15_20$J.

# summary plaquages offensifs 15-20
summary(bilan15_20$plaq_p_m) # moyenne = 8.2 / mediane = 6.2

# distribution plaquages off par match

ggplot(bilan15_20,aes(x=bilan15_20$plaq_p_m)) + 
  geom_histogram(fill="#f8766d")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")+
  ggtitle("Distribution du nombre de plaquages offensifs par match (saisons 2015 à 2020)")+
  theme(legend.title = element_blank(),
        legend.position=c(0.8, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),legend.text = element_text(size=20, 
                                                                face="bold"))

# Graphiques plaquages / pts encaissés 15-20
ggplot(bilan15_20, aes(x=plaq_p_m, y=pts_e_p_m)) +
  geom_point(color="blue",size=3) +
  ggtitle("Période 2015-2020 : plaquages offensifs et points encaissés")+
  xlab ("Nombre de plaquage offensif par rencontre")+
  ylab("Nombre de points encaissés par rencontre")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")+
  theme(legend.title = element_blank(),
        legend.position=c(0.8, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),legend.text = element_text(size=20, 
                                                                face="bold"))+
  
  geom_smooth(method=lm , color="red", se=FALSE)

# Graphiques plaquages / Classement 15-20
ggplot(bilan15_20, aes(x=plaq_p_m, y=Rang)) +
  geom_point(color="blue",size=3) +
  ggtitle("Période 2015-2020 : plaquages offensifs et classement")+
  xlab ("Nombre de plaquage offensif par rencontre")+
  ylab("Classement")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")+
  theme(legend.title = element_blank(),
        legend.position=c(0.8, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),legend.text = element_text(size=20, 
                                                                face="bold"))+
  
  geom_smooth(method=lm , color="red", se=FALSE)

summary(lm(bilan15_20$Rang ~ bilan15_20$plaq_p_m)) # coef = -0.05 / R2 = -0.004


# Graphiques plaquages OFF par saison 
ggplot( bilan15_20, aes(x=bilan15_20$saison,y=bilan15_20$plaq_p_m , fill=bilan15_20$saison)) +
  geom_boxplot() +
  ggtitle("Période 2015-2020 : plaquages offensifs par match") +
  xlab("Saison") + ylab("Nombre de plaques offensifs par match")+
  theme(legend.position="none")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")+
  theme(legend.title = element_blank(),
        legend.position=c(0.8, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),legend.text = element_text(size=20, 
                                                                face="bold"))+
  theme(legend.position = "none")


# Graphiques nombre de points encaissés par match 
ggplot( bilan15_20, aes(x=bilan15_20$saison,y=bilan15_20$pts_e_p_m , fill=bilan15_20$saison)) +
  geom_boxplot() +
  ggtitle("Période 2015-2020 : points encaissés par match") +
  xlab("Saison") + ylab("Nombre de points encaissés par match")+
  theme(    legend.position="none")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")+
  theme(legend.title = element_blank(),
        legend.position=c(0.8, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),legend.text = element_text(size=20, 
                                                                face="bold"))+
  theme(legend.position = "none")

# Création variable pts_m_p_m

bilan15_20$pts_m_p_m<-bilan15_20$Pts.M/bilan15_20$Matchs

# Graphiques nombre de points marqués par match 
ggplot( bilan15_20, aes(x=bilan15_20$saison,y=bilan15_20$pts_m_p_m , fill=bilan15_20$saison)) +
  geom_boxplot() +
  ggtitle("Période 2015-2020 : points marqués par match") +
  xlab("Saison") + ylab("Nombre de points marqués par match")+
  theme(    legend.position="none")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")+
  theme(legend.title = element_blank(),
        legend.position=c(0.8, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),legend.text = element_text(size=20, 
                                                                face="bold"))+
  theme(legend.position = "none")

# Anova sur différentes saisons
model1<-aov(bilan15_20$plaq_p_m~bilan15_20$saison) 
summary(model1)
TukeyHSD(model1)
