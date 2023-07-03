# Manipulation des données 
# Importation 

way_1 <- "E:\\KPAM ISEP 2\\Semestre 4\\Programmation R\\Cours R 2023\\PROJET R\\Projet ISEP2 2023\\Ressources"
library(haven)
dahm1 <- read_dta(file = paste0(way_1,"\\s07a1_me_SEN_2021.dta"))
dahm1
View(dahm1)
dahm2 <- read_dta(file = paste0(way_1,"\\s07a2_me_SEN_2021.dta"))
View(dahm2)
# dahm=dépenses alimentaire hors ménage

#Structure des données
dim(dahm1)
dim(dahm2)
# noms des colonnes
names(dahm1)
names(dahm2)

#Voir structuration des données
library(dplyr)
glimpse(dahm1)
glimpse(dahm2)
str(dahm1)
str(dahm2)


# Voir globalement les données 
summary(dahm1)
summary(dahm2)

#Renommer les variables

## Afficher le nom des variables
###Le nom des variables n'est rien d'autre que le nom des colonnes.Par ricochet on peux utiliser (names(dahm))
colnames(dahm1)
colnames(dahm2)
##Pour la base dahm1
nom_col=colnames(dahm1)[1:32]
nom_col
nv_nom <- c(nom_col[1],nom_col[2],nom_col[3],nom_col[4],nom_col[5],"repon_princ","cons_pdej","dep_pdej","dep_cpdej","cons_dej","dep_dej","dep_cdej","cons_diner"
            ,"dep_diner","dep_cdiner","conso_colas","dep_colas","dep_ccolas","conso_bois_ch","dep_bois_ch","dep_cbois_ch","cons_bois_nalco","dep_bois_nalco"
            ,"dep_cbois_nalco","conso_bois_alco","dep_c_bois_alco","dep_cbois_alco",nom_col[28],nom_col[29],nom_col[30],nom_col[31],nom_col[32])


isTRUE(length(nv_nom)==length(nom_col)) 
colnames(dahm1) <- nv_nom
colnames(dahm1)


##Pour la base dahm2
### Supprimer  la variable id in member
dahm2[6]=NULL


nom_col1=colnames(dahm2)[1:32]

nom_col1
nv_nom1 <- c(nom_col1[1],nom_col1[2],nom_col1[3],nom_col1[4],nom_col1[5],"repon_princ","cons_pdej","dep_pdej","dep_cpdej","cons_dej","dep_dej","dep_cdej","cons_diner"
            ,"dep_diner","dep_cdiner","conso_colas","dep_colas","dep_ccolas","conso_bois_ch","dep_bois_ch","dep_cbois_ch","cons_bois_nalco","dep_bois_nalco"
            ,"dep_cbois_nalco","conso_bois_alco","dep_c_bois_alco","dep_cbois_alco",nom_col1[28],nom_col1[29],nom_col1[30],nom_col1[31],nom_col1[32])
isTRUE(length(nv_nom1)==length(nom_col1)) 


colnames(dahm2) <- nv_nom1
colnames(dahm2)

## types des bases
typeof(dahm1)
typeof(dahm2)
library(tidyverse)
class(dahm1)
class(dahm2)


dahm1 <- data.frame(dahm1)
dahm2 <- data.frame(dahm2)

class(dahm1)
class(dahm2)

## Comme notre objectif c'est de calculer les dépenses alimentaires hors ménage .Par ricocher, 
## les variables qui nous interesse sont les variables avec dépenses .
##Selection des variables importantes

library(tidyverse)
dahm1 <- dahm1 %>%
  select(interview__key, dep_pdej, dep_dej, 
         dep_diner, dep_colas, dep_bois_ch, dep_bois_nalco
         ,  dep_c_bois_alco)
dahm2 <- dahm2 %>%
  select(interview__key, dep_pdej,  dep_dej
         , dep_diner,  dep_colas,  dep_bois_ch, dep_bois_nalco
         ,  dep_c_bois_alco)




### Imputation des données manquantes par 0 avec la tidyverse
library(tidyverse)
dahm1 <- replace_na(dahm1, replace = list(dep_pdej = 0,dep_dej=0
                                          ,dep_diner=0,dep_colas=0,dep_bois_ch=0,dep_bois_nalco=0
                                          ,dep_c_bois_alco=0))

dahm2 <- replace_na(dahm2, replace = list(dep_pdej = 0,dep_dej=0
                                          ,dep_diner=0,dep_colas=0,dep_bois_ch=0,dep_bois_nalco=0
                                          ,dep_c_bois_alco=0))

##Agregation des données de la base individuelle des ménages avec comme variable clee interview__key
library(tidyverse)
dahm2_agregaged <- dahm2 %>%
  group_by(interview__key) %>%
  summarise(dep_pdej = sum(dep_pdej),dep_dej=sum(dep_dej)
            ,dep_diner=sum(dep_diner),dep_colas=sum(dep_colas),dep_bois_ch=sum(dep_bois_ch)
            ,dep_bois_nalco=sum(dep_bois_nalco)
            ,dep_c_bois_alco=sum(dep_c_bois_alco))


View(dahm2_agregaged)
#calcul des depenses totales indiviquelles
library(Hmisc)
dahm2_agregaged <- dahm2_agregaged %>%
  mutate(dpenses_ttle = rowSums(across(c( dep_pdej,  dep_dej,        
                                          dep_diner,  dep_colas,  dep_bois_ch,   
                                          dep_bois_nalco,  dep_c_bois_alco ))))
label(data=dahm2_agregaged,dahm2_agregaged$dpenses_ttle, "Depenses totales individuelles durant les 7 derniers jours")
View(dahm2_agregaged)
#calcul des depenses totales pour le menage
dahm1 <- dahm1 %>%
  mutate(dpenses_ttle = rowSums(across(c( dep_pdej,  dep_dej,        
                                          dep_diner,  dep_colas,  dep_bois_ch,   
                                          dep_bois_nalco,  dep_c_bois_alco ))))
label(data=dahm1,dahm1$dpenses_ttle, "Depenses totales menages durant les 7 derniers jours")
View(dahm1)

#Identification des valeurs aberantes pour les dépenses totales/base individuelle agregé
library(ggplot2)
ggplot(dahm2_agregaged, aes(x = "", y = dpenses_ttle)) +
  geom_boxplot() +
  labs(x = "", y = "Valeur") +
  theme_minimal()

## Identifier les valeurs aberrantes dans la base individus à partir du boxplot
##on considère valeur aberantes toute depenses totales superieures à 80000 eton les remplace par la médiane
valeurs_aberrantes <- dahm2_agregaged %>%
  filter(dpenses_ttle > 80000  
           ) %>%
  pull(dpenses_ttle)

## Calculer la médiane de la variable taille_mng
mediane <- median(dahm2_agregaged$dpenses_ttle, na.rm = TRUE)

## Remplacer les valeurs aberrantes par la médiane
dahm2_agregaged <- dahm2_agregaged %>%
  mutate(dpenses_ttle = ifelse(dpenses_ttle %in% valeurs_aberrantes, mediane, dpenses_ttle))
View(dahm2_agregaged)

#Identification des valeurs aberantes pour les dépenses totales/base menage
library(ggplot2)
ggplot(dahm1, aes(x = "", y = dpenses_ttle)) +
  geom_boxplot() +
  labs(x = "", y = "Valeur") +
  theme_minimal()

## Identifier les valeurs aberrantes dans la base ménage à partir du boxplot
##on considère valeur aberantes toute depenses totales superieures à 80000 eton les remplace par la médiane
valeurs_aberrantes <- dahm1 %>%
  filter(dpenses_ttle > 80000  
  ) %>%
  pull(dpenses_ttle)

## Calculer la médiane de la variable taille_mng
mediane <- median(dahm1$dpenses_ttle, na.rm = TRUE)

## Remplacer les valeurs aberrantes par la médiane
dahm1 <- dahm1 %>%
  mutate(dpenses_ttle = ifelse(dpenses_ttle %in% valeurs_aberrantes, mediane, dpenses_ttle))



## On va append les deux bases dahm2_agregaged et dahm1(base menage)
## verification du nombre de variable avant de fusionner (append)
isTRUE(length(dahm1)==length(dahm2_agregaged))
dahm_last <- append(dahm1,dahm2_agregaged)

## changer de type
class(dahm_last)
dahm_last <- data.frame(dahm_last)

## Réagrégation des données de dahm_last pour avoir la base finale 

library(tidyverse)
dahm_last <- dahm_last %>%
  group_by(interview__key) %>%
  summarise(dep_pdej = sum(dep_pdej),dep_dej=sum(dep_dej)
            ,dep_diner=sum(dep_diner),dep_colas=sum(dep_colas),dep_bois_ch=sum(dep_bois_ch)
            ,dep_bois_nalco=sum(dep_bois_nalco)
            ,dep_c_bois_alco=sum(dep_c_bois_alco),dpenses_ttle = sum(dpenses_ttle))


dim(dahm_last)#on voit que le nombre de ménage abservé avec la base dahm1(base ménage) est le meme avec la base_last
## Calcul des dépenses pour le ménage au cours des 7 derniers jours
View(dahm_last)
summary(dahm_last)

## Calcul des dépenses par jour du ménage

dahm_last$dpenses_jr_ttle=(dahm_last$dpenses_ttle/7)
label(data=dahm_last,dahm_last$dpenses_jr_ttle, "Depenses totales par jour du ménage ")
## CALCUL DES DEPENSES PAR ANNEE
dahm_last$dpenses_ann_ttle<-(dahm_last$dpenses_jr_ttle*360)
label(data=dahm_last,dahm_last$dpenses_ann_ttle, "Depenses totales par année du ménage ")

###############################

#### calcul de la taille du ménage .On utilisera la base s01_me_SEN_2021.dta (caractéristiques sociodémographiques des membres du ménage) pour calculer la taille de chaque ménage.

#way_i <- "C:\\Users\\dell\\Desktop\\ENSAE\\ISEP2\\Semestre_2\\Programmation R\\Projet(Calcul des dépenses alimentaires hors menage)\\Projet ISEP2 2023\\Ressources"
library(haven)
indivi<- read_dta(file = paste0(way_1,"\\s01_me_SEN_2021.dta"))
indivi=data.frame(indivi)
View(indivi)
dahm_last$taille_mng=table(indivi$interview__key)
base_socio<-read_dta(file = paste0(way_1,"\\s00_me_SEN_2021.dta"))
View(base_socio)
###calcul des dépenses du ménage par tete 

dahm_last$dpenses_tete<-(dahm_last$dpenses_ann_ttle/dahm_last$taille_mng)
label(data=dahm_last,dahm_last$dpenses_tete, "Depenses totales par tete du menage")


# recodage de la variable depense totale afin d'etudier les correlations 
##Recodage de la variable dpenses_ttle

dahm_last$recode_dpenses_ttle <- ifelse(dahm_last$dpenses_ttle<15000*360,"depenses_faibles","depenses_elevees")

View(dahm_last)

# dans l'analyse bivariée on essayera de voir la repartition des depenses selon les regions ou selon les milieux de residences,selon la situation matrimoniale
# de ce fait on va ajouter ses deux variables à notre base finale
dahm_last$region=base_socio$s00q01
dahm_last$milieu_de_residence=base_socio$s00q04
sum(is.na(dahm_last$region))
# 260 valeurs manquantes sur le departement et region,aucune indication pour imputer cette variable
# on va les traiter en utilisant la variable grappe
grapna<-unique(na.omit(base_socio[is.na(base_socio$s00q01), "grappe"]))
View(unique(base_socio[base_socio$grappe==3,"s00q01"]))


for (i in grapna$grappe){
  reg<-na.omit(base_socio[base_socio$grappe==i,"s00q01"])
  
  base_socio[which(base_socio$grappe==i),"s00q01"]= reg$s00q01[1]
  
}
sum(is.na(base_socio$s00q01)) # il reste 29 valeurs manquantes nous allons les exclures aucours de l'analyse
# puisqu'on ne peut pas les imputer sans d'autres informations
# traitement de la variable milieu de residence
grapna<-unique(na.omit(base_socio[is.na(base_socio$s00q04), "grappe"]))

for (i in grapna$grappe){
  reg<-na.omit(base_socio[base_socio$grappe==i,"s00q04"])
  
  base_socio[which(base_socio$grappe==i),"s00q04"]= reg$s00q04[1]
  
}

sum(is.na(base_socio$s00q04)) # nombre de valeurs manquantes restantes 15 . On va les exclure egalement
## Vérification de la comodité des variables avec la base 
isTRUE(length(dahm_last$interview__key)==length(dahm_last$region)) 

isTRUE(length(dahm_last$interview__key)==length(dahm_last$milieu_de_residence)) 


##Recodage des deux variables

dahm_last$region <- ifelse(dahm_last$region==1,"Dakar",
                           ifelse(dahm_last$region==2,"Ziguinchor",ifelse(dahm_last$region==3,"Diourbel",ifelse(dahm_last$region==4,"Saint-Louis",ifelse(dahm_last$region==5,"Tambacounda",
                                                                                                                                                         ifelse(dahm_last$region==6,"Kaolack",ifelse(dahm_last$region==7,"Thies",ifelse(dahm_last$region==8,"Louga",
                                                                                                                                                                                                                                        ifelse(dahm_last$region==9,"Fatick",ifelse(dahm_last$region==10,"Kolda",ifelse(dahm_last$region==11,"Matam",ifelse(dahm_last$region==12,"Kaffrine",ifelse(dahm_last$region==13,"Kedougou","Sedhiou")))))))))))))

dahm_last$milieu_de_residence <- ifelse(dahm_last$milieu_de_residence==1,"Urbain","Rural")

# afin de faire la regresion logistique on recode la categories des depenses totales en 1 ou 0
dahm_last$recode_dpenses_ttle<-ifelse(dahm_last$recode_dpenses_ttle=="depenses_faibles",0,1)
# base finale
dahm_last<-na.omit(dahm_last)
sum(is.na(dahm_last$region))
#Exportation des données
#.dta
library(haven)
chemin1<-"E:\\KPAM ISEP 2\\Semestre 4\\Programmation R\\Cours R 2023\\PROJET R\\base_appuree_dahm.dta"
write_dta(dahm_last, chemin1)
#base individus
chemin2<-"E:\\KPAM ISEP 2\\Semestre 4\\Programmation R\\Cours R 2023\\PROJET R\\base_individus.dta"
write_dta(dahm2_agregaged, chemin2)

################################STATISTIQUES UNIVARIES##################

##### variable dpenses_ttle
summary(dahm_last$dpenses_ann_ttle)
# on constate que la somme maximale depensée par année est de 2777143 avec une moyenne de 86968
# boxplot
ggplot(dahm_last, aes(y = dahm_last$dpenses_ann_ttle)) +
  geom_boxplot() +
  labs(title = "Boxplot", y = dahm_last$dpenses_ann_ttle)
### Variable region
tab<-table(dahm_last$region)
tab
# on constate que 272 menages enquetés sont dans la region de dakar
### milieu de residence
tab<-table(dahm_last$milieu_de_residence)
tab
## 1084 menages sont en milieu urbain

##################STATISTIQUES BIVARIEES##############

#on va faire le croisement entre les variables dpenses_ann_ttle et les variables region,milieu de résidence pour voir sises dernières ont une influence sur les depenses hors ménages
tabcroise<-table(dahm_last$recode_dpenses_ttle, dahm_last$region)
# Effectuer le test du Chi carré
chi_square_test <- chisq.test(dahm_last$dpenses_ann_ttle, dahm_last$region,simulate.p.value = TRUE)
tabcroise
chi_square_test
##  le test de chi deux nous donne une pvalue non significatif ce qui peut suggerer l'inexistence d'une correlation entre les deux variables
# milieu de residence et depenses totale
chi_square_test <- chisq.test(dahm_last$dpenses_ann_ttle, dahm_last$milieu_de_residence,simulate.p.value = TRUE)
chi_square_test
# la p-value (0.04148)inferieur à 0.05 ce qui signifie que le fait de vivre en milieu rural ou urbain influencerait la consommations hors menages
## Pour le croisement entre les variables quantitatives( cf shiny)
################Analyse économetrique##########
#modelisation linéaire
# entre variable dpenses_ann_ttle et region
mod<-lm(dahm_last$dpenses_ann_ttle~dahm_last$region,data=dahm_last)
coef(mod)
anova(mod)
# entre variable dpenses_ann_ttle et milieu de residence
mod<-lm(dahm_last$dpenses_ann_ttle~dahm_last$milieu_de_residence,data=dahm_last)
coef(mod)
anova(mod)
#Analyse de la covariance
mod1<-lm(dahm_last$dpenses_ann_ttle~dahm_last$milieu_de_residence*dahm_last$region,dahm_last)
summary.aov(mod1)
###  regression logistique 

model <- glm(dahm_last$recode_dpenses_ttle~dahm_last$milieu_de_residence+dahm_last$region, data = dahm_last, family = binomial)
summary(model)



##############                    FIN                       ####################################""