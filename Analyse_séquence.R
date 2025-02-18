############################ Chargement des packages ############################
library(haven)
library(questionr)
library(tidyverse)
library(gtsummary)
########################## Import des packages ##############################

enfants_champ <- read_dta("C:/Users/33636/Desktop/Université/Master/M1/Mémoire de recherche/BdD/MdG_2021/Data/Stata/enfants_champ.dta")
parents <- read_dta("C:/Users/33636/Desktop/Université/Master/M1/Mémoire de recherche/BdD/MdG_2021/Data/Stata/parents.dta")
menages <- read_dta("C:/Users/33636/Desktop/Université/Master/M1/Mémoire de recherche/BdD/MdG_2021/Data/Stata/menages.dta")
calendrier <- read_dta("C:/Users/33636/Desktop/Université/Master/M1/Mémoire de recherche/BdD/MdG_2021/Data/Stata/calendrier.dta")

############################# Création base de données âge au moment de la séparation #################

############################# Création base de données caractéristiques sociodémographique #################
## Recodage de parents$couple en parents$couple_rec
parents$couple_rec <- parents$couple %>%
  as.character() %>%
  fct_recode(
    "En couple" = "1",
    "Monoparentale" = "2",
    "Monoparentale" = "3",
    NULL = "4" #1VM
  )

## Recodage de parents$sitemp en parents$sitemp_rec
parents$sitemp_rec <- parents$sitemp %>%
  as.character() %>%
  fct_recode(
    "Actif" = "1",
    "Actif" = "2",
    "Etudiant" = "3",
    "Chômage" = "4",
    NULL = "5",
    "Inactif" = "6",
    "Inactif" = "7",
    "Inactif" = "8",
    NULL = "9"
  )
#
par <- parents%>%
  select(idmen,noi,sexe,anais,matri,dipl,nbhr,sitemp_rec,couple_rec) #143 parents séparés ont retrouvé un partenaire au moment de l'enquête, est-il possible de les rajouter afin de comparer ? 


############################# Création de la base enfant ##########################

#Recodage des intervenants

## Recodage de enf$hmdg_inter_p1 en enf$hmdg_inter_p1_rec
enfants_champ$hmdg_inter_p1_rec <- enfants_champ$hmdg_inter_p1 %>%
  as.character() %>%
  fct_recode(
    "Les parents" = "1",
    "Les grands-parents" = "2",
    "Un assistant(e) maternel(le) agréé(e)" = "3",
    "Une crèche" = "4",
    "L'école" = "5",
    "Une garde à domicile" = "6",
    "Un autre mode de garde payant" = "7",
    "Un autre mode de garde" = "8"
  )
## Recodage de enf$hmdg_inter_p2 en enf$hmdg_inter_p2_rec
enfants_champ$hmdg_inter_p2_rec <- enfants_champ$hmdg_inter_p2 %>%
  as.character() %>%
  fct_recode(
    "Les parents" = "1",
    "Les grands-parents" = "2",
    "Un assistant(e) maternel(le) agréé(e)" = "3",
    "Une crèche" = "4",
    "L'école" = "5",
    "Une garde à domicile" = "6",
    "Un autre mode de garde payant" = "7",
    "Un autre mode de garde" = "8"
  )
## Recodage de enf$hmdg_inter_p5 en enf$hmdg_inter_p5_rec
enfants_champ$hmdg_inter_p3_rec <- enfants_champ$hmdg_inter_p3 %>%
  as.character() %>%
  fct_recode(
    "Les parents" = "1",
    "Les grands-parents" = "2",
    "Un assistant(e) maternel(le) agréé(e)" = "3",
    "Une crèche" = "4",
    "L'école" = "5",
    "Une garde à domicile" = "6",
    "Un autre mode de garde payant" = "7",
    "Un autre mode de garde" = "8"
  )
## Recodage de enf$hmdg_inter_p4 en enf$hmdg_inter_p4_rec
enfants_champ$hmdg_inter_p4_rec <- enfants_champ$hmdg_inter_p4 %>%
  as.character() %>%
  fct_recode(
    "Les parents" = "1",
    "Les grands-parents" = "2",
    "Un assistant(e) maternel(le) agréé(e)" = "3",
    "Une crèche" = "4",
    "L'école" = "5",
    "Une garde à domicile" = "6",
    "Un autre mode de garde payant" = "7",
    "Un autre mode de garde" = "8"
  )

## Recodage de enf$hmdg_inter_p5 en enf$hmdg_inter_p5_rec
enfants_champ$hmdg_inter_p5_rec <- enfants_champ$hmdg_inter_p5 %>%
  as.character() %>%
  fct_recode(
    "Les parents" = "1",
    "Les grands-parents" = "2",
    "Un assistant(e) maternel(le) agréé(e)" = "3",
    "Une crèche" = "4",
    "L'école" = "5",
    "Une garde à domicile" = "6",
    "Un autre mode de garde payant" = "7",
    "Un autre mode de garde" = "8"
  )
## Recodage de enfants_champ$pres en enfants_champ$pres_rec
enfants_champ$pres_rec <- enfants_champ$pres %>%
  as.character() %>%
  fct_recode(
    "Garde exclusif ou presque" = "1",
    "DVH classique" = "2",
    "Garde alternée" = "3",
    NULL = "6"
  )

enf <- enfants_champ%>%
  filter(age<36)%>%
  #pres justifie la + de la moitié du temps dans le logement
  select(age,idmen,mdgpalsyn, anais_enf, pres_rec,starts_with("hmdg_date"),starts_with("hmdg_inter"))



#Fusion des bases par-enf
seqmdg <- full_join(enf,par)

#Avec la fonction labels
labels <- c(
  par = "Les parents", #1
  g_par = "Les grands-parents" ,#2
  ass_mat = "Un assistant(e) maternel(le) agréé(e)", #3
  crec = "Une crèche",#4
  ecole = "L'école", #5
  g_dom = "Une garde à domicile", #6
  mdg_p = "Un autre mode de garde payant", #7
  mdg = "Un autre mode de garde" #8
)

tbl_summary(seqmdg, include = c(mdgpalsyn, couple_rec),
            by=couple_rec)


################ Création de la base de données pour l'analyse de séquence à partir des données de la base seqmdg ############################

####Ajout des variables des mois de l'année

months <- seq(as.Date("2018-01-01"), as.Date("2021-12-01"), by = "month")
month_names <- format(months, "%d/%m/%Y")
# Initialiser un tibble avec les nouvelles colonnes
new_columns <- tibble(!!!setNames(as.list(rep(NA, length(month_names))), month_names))
# Ajouter les nouvelles colonnes à la base de données existante
seqmdg1 <- bind_cols(seqmdg, new_columns)

## Ajout du jour au date hdmg:
seqmdg1$jour <- "01";

seqmdg1$hmdg_dated_p1 <- paste(seqmdg1$jour, seqmdg1$hmdg_dated_p1, sep = "/")
seqmdg1$hmdg_dated_p2 <- paste(seqmdg1$jour, seqmdg1$hmdg_dated_p2, sep = "/")
seqmdg1$hmdg_dated_p3 <- paste(seqmdg1$jour, seqmdg1$hmdg_dated_p3, sep = "/")
seqmdg1$hmdg_dated_p4 <- paste(seqmdg1$jour, seqmdg1$hmdg_dated_p4, sep = "/")
seqmdg1$hmdg_dated_p5 <- paste(seqmdg1$jour, seqmdg1$hmdg_dated_p5, sep = "/")
seqmdg1$hmdg_datef_p1 <- paste(seqmdg1$jour, seqmdg1$hmdg_datef_p1, sep = "/")
seqmdg1$hmdg_datef_p2 <- paste(seqmdg1$jour, seqmdg1$hmdg_datef_p2, sep = "/")
seqmdg1$hmdg_datef_p3 <- paste(seqmdg1$jour, seqmdg1$hmdg_datef_p3, sep = "/")
seqmdg1$hmdg_datef_p4 <- paste(seqmdg1$jour, seqmdg1$hmdg_datef_p4, sep = "/")
seqmdg1$hmdg_datef_p5 <- paste(seqmdg1$jour, seqmdg1$hmdg_datef_p5, sep = "/")

seqmdg2 <-seqmdg1 %>% 
  mutate(
    hmdg_dated_p1=as.Date(hmdg_dated_p1, format = "%d/%m/%Y"),
    hmdg_dated_p2=as.Date(hmdg_dated_p2, format = "%d/%m/%Y"),
    hmdg_dated_p3=as.Date(hmdg_dated_p3, format = "%d/%m/%Y"),
    hmdg_dated_p4=as.Date(hmdg_dated_p4, format = "%d/%m/%Y"),
    hmdg_dated_p5=as.Date(hmdg_dated_p5, format = "%d/%m/%Y"),
    hmdg_datef_p1=as.Date(hmdg_datef_p1, format = "%d/%m/%Y"),
    hmdg_datef_p2=as.Date(hmdg_datef_p2, format = "%d/%m/%Y"),
    hmdg_datef_p3=as.Date(hmdg_datef_p3, format = "%d/%m/%Y"),
    hmdg_datef_p4=as.Date(hmdg_datef_p4, format = "%d/%m/%Y"),
    hmdg_datef_p5=as.Date(hmdg_datef_p5, format = "%d/%m/%Y")
    )


# Fonction pour assigner les intervenants à chaque période
assign_values <- function(row, month) {
  if (!is.na(date) && !is.na(row$hmdg_dated_p1) && !is.na(row$hmdg_datef_p1) &&
      date >= row$hmdg_dated_p1 && date <= row$hmdg_datef_p1) {
    return(row$hmdg_inter_p1_rec)
  } else if (!is.na(date) && !is.na(row$hmdg_dated_p2) && !is.na(row$hmdg_datef_p2) &&
             date >= row$hmdg_dated_p2 && date <= row$hmdg_datef_p2) {
    return(row$hmdg_inter_p2_rec)
  } else if (!is.na(date) && !is.na(row$hmdg_dated_p3) && !is.na(row$hmdg_datef_p3) &&
             date >= row$hmdg_dated_p3 && date <= row$hmdg_datef_p3) {
    return(row$hmdg_inter_p3_rec)
  } else if (!is.na(date) && !is.na(row$hmdg_dated_p4) && !is.na(row$hmdg_datef_p4) &&
             date >= row$hmdg_dated_p4 && date <= row$hmdg_datef_p4) {
    return(row$hmdg_inter_p4_rec)
  } else if (!is.na(date) && !is.na(row$hmdg_dated_p5) && !is.na(row$hmdg_datef_p5) &&
             date >= row$hmdg_dated_p5 && date <= row$hmdg_datef_p5) {
    return(row$hmdg_inter_p5_rec)
  } else {
    return(NA)
  }
}

# Appliquer la fonction à chaque ligne et chaque mois
for (month in month_names) {
  cat("Processing month:", month, "\n")  # Message de débogage
  seqmdg1[[month]] <- sapply(1:nrow(seqmdg1), function(i) {
    if (i %% 1000 == 0) {  # Message de débogage toutes les 1000 lignes
      cat("Processing row:", i, "\n")
    }
    assign_values(seqmdg1[i, ], month)
  })
}