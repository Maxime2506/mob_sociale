library(tidyverse)
library(haven)
library(labelled)

## Importation des enquêtes
racine <- "//Users/81894/Documents/Enquetes Emplois 82-02"
dossiers_sav <- list.files(racine, pattern = "qi.sav$", full.names = TRUE, recursive = TRUE)

les_vars_82 <- c("AG", "CSTOT", "CSPP", "DIEG", "DIEP", "DIES", "DIPL", "EXTRI", "FI", "NA", "RANSECH", "S", 
                 "SALTR", "SECT14", "STATUT", "TYPMENR")

EE82_89 <- imap(dossiers_sav[1:8], \(x, i) read_sav(x, col_select = all_of(les_vars_82)) |> 
                   mutate(ANNEE = 1981L + i)) |> 
    list_rbind()

les_vars_90 <- c("AG", "CSTOT", "CSPP", "DIEG", "DIES", "DIET", "DIPL", "EXTRI", "FI", "TP", "RANSECH", "S", 
                 "SALRED", "SECT14", "STATUT", "TYMEN90R")

EE90_02 <- imap(dossiers_sav[9:21], \(x, i) read_sav(x, col_select = any_of(les_vars_90)) |> 
                    mutate(ANNEE = 1989L + i)) |> 
    list_rbind()


# TRAITEMENT des DIPLOMES. On passe d'abord du codage 1982 à 1990
EE82_89 <- EE82_89 |> 
    mutate(DIEG = case_match(DIEG, "0" ~ "00", "1" ~ "02", "3" ~ "15", "4" ~ "18", "6" ~ "16", "7" ~"17", "8" ~ "19"),
           DIET = case_match(DIEP, "" ~ "", "2" ~ "23", "3" ~ "23", "4" ~ "23", "5" ~ "21", "6" ~ "29", "7" ~ "36",
                             "8" ~ "39", "9" ~ "36"),
           DIES = case_match(DIES, "1" ~ "41", "2" ~ "45", "3" ~ "43", "4" ~"43", "5"~ "44", "6" ~ "46", "7" ~ "48",
                             "8" ~ "47", "9" ~ "49"))

seuil_SALTR <- c(seq(1000, 5000, 500), seq(6000, 10000, 1000), seq(15000, 30000, 5000))
lab_SALTR <- c(paste("De", lag(seuil_SALTR, default = 0), "à", seuil_SALTR, "francs"),
               "Plus de 30000 francs", "Refus de répondre")
lettre_SALTR <- c(LETTERS[1:19], "X")

EE82_89 <- EE82_89 |> 
    mutate(SALTR = factor(SALTR, levels = lettre_SALTR, labels = lab_SALTR))

EE90_02 <- EE90_02 |> 
    mutate(SALTR = cut(SALRED, breaks = c(0, seuil_SALTR, Inf), labels = lab_SALTR[-20]))

# On ne conserve que le tiers de l'échantillon qui n'est ni entrant ni sortant, soit RANSECH = 2
# TYMEN90R = TYPMENR
# HH = HHAB sauf "SP" devenant "0"
EE82_02 <- bind_rows(
    EE82_89 |> rename(TP = "NA"), 
    EE90_02 |> rename(TYPMENR = TYMEN90R)
    ) |> 
    filter(RANSECH == 2) |> 
    select(-RANSECH) |> 
    relocate(ANNEE, S, AG, CSTOT)

# RECODAGE diplome sur le format 2003
EE82_02 <- EE82_02 |> 
    mutate(
        DIEG = as.integer(DIEG), DIET = as.integer(DIET), DIES = as.integer(DIES), 
        DIP_old = pmax(DIEG, DIET, DIES, na.rm = TRUE),
        diplome = case_match(DIP_old,
                             47 ~ "3e cycle/Tout doctorat",
                             c(48, 49) ~ "Grande école",
                             46 ~ "Licence/Maîtrise",
                             c(41, 42, 43, 44, 45) ~ "Bac+2",
                             40 ~ "Capacité/DAEU",
                             17 ~ "Baccalauréat général",
                             32 ~ "Baccalauréat techno",
                             34 ~ "Baccalauréat pro",
                             c(36, 39) ~ "Brevet technicien",
                             c(21, 23, 25, 27, 29) ~ "CAP/BEP",
                             c(15, 16, 18, 19) ~ "Brevet des collèges", #vieux diplomes type brevet supérieur assimilé à BEPC (sic)
                             2 ~ "CEP",
                             0 ~ "Aucun diplôme",
                             TRUE ~ NA_character_))

##### Labellisation #####
lab_FI <- c("Actif occupé", "Chômeur", "Etudiant", "Militaire du contingent",
            "Retraité", "Retiré des affaires", "Femme au foyer", "Autre inactif")

code_CSTOT <- c(
    11,12,13,21,22,23,31,33,34,35,37,38,
    42,43,44,45,46,47,48,52,53,54,55,56,
    62,63,64,65,67,68,69,71,72,74,75,77,
    78,81,83,84,85,86
)

lab_CSTOT <- c(
    "agriculteurs sur petite exploitation",
    "agriculteurs sur moyenne exploitation",
    "agriculteurs sur grande exploitation",
    "artisans",
    "commerçants et assimilés",
    "chefs d'entreprise de 10 salariés ou plus",
    "professions libérales",
    "cadres de la fonction publique",
    "professeurs, professions scientifiques",
    "professions de l'information, des arts et des spectacles",
    "cadres administratifs et commerciaux d'entreprises",
    "ingénieurs et cadres techniques d'entreprises",
    "instituteurs et assimilés",
    "professions intermédiaires de la santé et du travail social",
    "clergé, religieux",
    "professions intermédiaires administratives de la fonction publique",
    "professions intermédiaires administratives et commerciales des entreprises",
    "techniciens",
    "contremaîtres, agents de maîtrise",
    "employés civils et agents de service de la fonction publique",
    "policiers et militaires",
    "employés administratifs d'entreprises",
    "employés de commerce",
    "personnels des services directs aux particuliers",
    "ouvriers qualifiés de type industriel",
    "ouvriers qualifiés de type artisanal",
    "chauffeurs",
    "ouvriers qualifiés de la manutention, du magasinage et du transport",
    "ouvriers non qualifiés de type industriel",
    "ouvriers non qualifiés de type artisanal",
    "ouvriers agricoles",
    "anciens agriculteurs exploitants",
    "anciens artisans, commerçants, chefs d'entreprise",
    "anciens cadres",
    "anciennes professions intermédiaires",
    "anciens employés",
    "anciens ouvriers",
    "chômeurs n'ayant jamais travaillé",
    "militaires du contingent",
    "élèves, étudiants",
    "personnes diverses sans activité professionnelle de moins de 60 ans (sauf retraités)",
    "personnes diverses sans activité professionnelle de 60 ans et plus (sauf retraités)"
)

code_CSA <- c(
    10,21,22,23,31,33,34,35,37,38,
    42,43,44,45,46,47,48,52,53,54,
    55,56,62,63,64,65,67,68,69
)

lab_CSA <- c(
    "agriculteurs exploitants",
    "artisans",
    "commerçants et assimilés",
    "chefs d'entreprise de 10 salariés ou plus",
    "professions libérales",
    "cadres de la fonction publique",
    "professeurs, professions scientifiques",
    "professions de l'information, des arts et des spectacles",
    "cadres administratifs et commerciaux d'entreprises",
    "ingénieurs et cadres techniques d'entreprises",
    "instituteurs et assimilés",
    "professions intermédiaires de la santé et du travail social",
    "clergé, religieux",
    "professions intermédiaires administratives de la fonction publique",
    "professions intermédiaires administratives et commerciales des entreprises",
    "techniciens",
    "contremaîtres, agents de maîtrise",
    "employés civils et agents de service de la fonction publique",
    "policiers et militaires",
    "employés administratifs d'entreprises",
    "employés de commerce",
    "personnels des services directs aux particuliers",
    "ouvriers qualifiés de type industriel",
    "ouvriers qualifiés de type artisanal",
    "chauffeurs",
    "ouvriers qualifiés de la manutention, du magasinage et du transport",
    "ouvriers non qualifiés de type industriel",
    "ouvriers non qualifiés de type artisanal",
    "ouvriers agricoles"
)


lab_TYPMENR <- c("Célibataire", "Ménage complexe", "Monoparent", "Couple sans enfant", "Couple avec enfant(s)")

lab_SECT14 <- c(
    "Agriculture, sylviculture, pêche (SECT38=01)",
    "Industries agricoles et alimentaires (SECT38=02,03)",
    "Energie (SECT38=04,05,06)",
    "Industrie des biens intermédiaires (SECT38=07 à 11,13,21,23)",
    "Industrie des biens d'équipement (SECT38=14 à 17)",
    "Industrie des biens de consommation courante (SECT38=12,18 à 20,22)",
    "Bâtiment, génie civil et agricole (SECT38=24)",
    "Commerce (SECT38=25 à 28)",
    "Transports et télécommunications (SECT38=31,32)",
    "Services marchands (SECT38=29,30,33,34)",
    "Location et crédit bail immobiliers (SECT38=35)",
    "Assurances (SECT38=36)",
    "Organismes financiers (SECT38=37)",
    "Services non marchands"
)

code_DIPL <- c(
    10,11,30,31,32,33,40,41,42,43,
    50,51,60,70,71
)

lab_DIPL <- c(
    "2ème ou 3ème cycle universitaire",
    "grande école, diplôme d'ingénieur",
    "1er cycle universitaire",
    "BTS, DUT",
    "paramédical ou social avec baccalauréat général",
    "paramédical ou social sans baccalauréat général",
    "baccalauréat général et diplôme technique secondaire",
    "baccalauréat général seul",
    "baccalauréat technologique, BAC pro. et brevet professionnel",
    "BEI, BEC, BEA",
    "CAP, BEP, et BEPC",
    "CAP, BEP seul",
    "BEPC seul",
    "CEP",
    "aucun diplôme"
)

lab_TP <- c("Temps complet", "Temps partiel")

#####
EE82_02 <- EE82_02 |> 
    mutate(S = factor(S, levels = 1:2, labels = c("Homme", "Femme")),
           AG = as.integer(AG),
           FI = factor(FI, levels = 1:8, labels = lab_FI),
           CSTOT = factor(CSTOT, levels = code_CSTOT, labels = lab_CSTOT),
           CSPP = factor(CSPP , levels = code_CSA, labels = lab_CSA),
           TYPMENR = factor(TYPMENR, levels = 1:5, labels = lab_TYPMENR),
           SECT14 = factor(SECT14, levels = 1:14, labels = lab_SECT14),
           DIPL = factor(DIPL, levels = code_DIPL, labels = lab_DIPL),
           TP = factor(TP, levels = 1:2, labels = lab_TP), 
           SAL = SALRED * .152449) |> 
    select(SALRED)

rm(EE82_89, EE90_02, les_vars_82, les_vars_90, code_CSA, code_CSTOT, code_DIPL, lab_CSA, lab_CSTOT, lab_DIPL,
   lab_TP, lab_FI, lab_SALTR, lab_SECT14, lab_TYPMENR)

arrow::write_parquet(EE82_02, "EE82_02.parquet")

