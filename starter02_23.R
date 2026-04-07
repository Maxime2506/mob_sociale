library(tidyverse)
library(arrow)
library(duckdb)
library(haven)
library(labelled)

options(arrow.unsafe_metadata = TRUE)

## Importation des enquêtes
racine <- "//Users/81894/Documents/Enquetes Emplois"
dossiers_racine <- list.files(racine, pattern = "[^z][^i][^p]$", full.names = TRUE)
dossiers_csv <- list.files(racine, pattern = "csv$", full.names = TRUE)
dossiers_parquet <- setdiff(dossiers_racine, dossiers_csv)

chemins_parquet <- list.files(dossiers_parquet, pattern = "PARQUET", full.names = TRUE, ignore.case = TRUE) |> 
    list.files(full.names = TRUE, pattern = "indiv")
chemins_csv <- list.files(paste0(dossiers_csv, "/Csv"), full.names = TRUE, pattern = "indiv", ignore.case = TRUE)

chemins_annees <- map(chemins_csv, \(x) read_csv2(x, col_select = "ANNEE", n_max = 1) |> 
                         distinct(ANNEE) |>
                         mutate(nom_fichier = x)) |> 
    list_rbind() |> 
    arrange(ANNEE)

# TABLE de correspondances pour les variables d'intérêts

# utilitaire pour checker les var
# chemins_annees |>
#     filter(ANNEE >= 2003) |>
#     slice(1, .by = ANNEE) |>
#     mutate(z = map(nom_fichier, \(x) read_delim(x, delim = ";",
#                                                 col_select = c(ANNEE, starts_with("DIP")), n_max=1) |>
#                        names())) |> unnest_auto(z) |> view()
# 
# chemins_parquet |>
#     open_dataset() |>
#     names() |> keep(\(x) str_detect(x, "MATRI"))

tab_corres <- tribble(
    ~ nom_final, ~ pre2013, ~ post2013, ~ post2021, ~ type,
    "ANNEE", "ANNEE", "ANNEE", "ANNEE", "int",
    "RGA", "RGA", "RGA", "RGA", "int",
    "EXTRI", "EXTRI", "extri", "EXTRI", "chr",
    "SEXE", "SEXE", "SEXE", "SEXE", "int",
    "AG", "AG", "AG", "AG", "int",
    "CSTOT", "CSTOT", "CSTOT", "PCS2", "int",
    "MATRI", "MATRI", "MATRI", "ETAMATRI_Y", "int",
    "CSPP", "CSPP", "CSPP", "PCSPAR1_2", "int", # ATTENTION au sexe PAR1 et PAR2
    "CSPM", "CSPM", "CSPM", "PCSPAR2_2", "int",
    "SEXE_PAR1", NA, NA, "SEXE_PAR1", "int",
    "SEXE_PAR2", NA, NA, "SEXE_PAR2", "int",
    "DIP_long", NA, "DIPFIN", "DIP108", "int",
    "DIP_moyen", NA, "DIPDET", "DIP29", "int",
    "DIP_court", "DIP", "DIP", "DIP7", "int",
    "DIPGEN", "DIPGEN", NA, NA, "int",
    "DIPTEC", "DIPTEC", NA, NA, "int",
    "DIPSUP", "DIPSUP", NA, NA, "int",
    "SAL", "SALRED", "SALRED", "SALRED_Y", "num",
    "TYPMEN" , "TYPMEN5", "TYPMEN7", "TYPMEN5", "int",  # Revoir les cas
    "SP00", "SP00", "SP00", "SPR00", "int",
    "TPPRED", "TPPRED", "TPPRED", "TPPRED", "int"
)

type_collector <- list(
    int = col_integer(),
    num = col_double(),
    chr = col_character()
)
type_schema <- list(
    chr = string(),
    num = float64(),
    int = int32()
)

tab_corres <- tab_corres |> 
    mutate(col_spec = map(type, \(x) type_collector[[x]]),
           schema_spec = map(type, \(x) type_schema[[x]]))

## Base de travail
# ATTENTION : On conserve uniquement les individus interrogés lors de la première grappe d'enquête.

# 2023 en csv
var_23 <- tab_corres |> 
    drop_na(post2021)

spec_23 <- var_23 |> pull(col_spec, name = post2021) |> do.call(cols_only, args = _)

EE_23 <- chemins_annees |> 
    filter(ANNEE == 2023) |>
    pull(nom_fichier) |> 
    read_csv2(col_types = spec_23, 
              show_col_types = FALSE) |>
    filter(RGA == 1) |> 
    mutate(EXTRI = as.numeric(EXTRI)) |>
    rename(var_23 |> pull(post2021, name = nom_final))
    

# 2021-2022 en parquet
var_21_22 <- tab_corres |> drop_na(post2021)

schema_21_22 <- map(seq_along(var_21_22$post2021), \(i) field(var_21_22$post2021[i], var_21_22$schema_spec[[i]])) |> 
    do.call(schema, args =_)

EE_21_22 <- open_dataset(chemins_parquet, schema = schema_21_22) |> 
    collect() |> 
    filter(RGA == 1) |> 
    as_tibble() |> 
    mutate(EXTRI = as.numeric(EXTRI)) |>
    rename(var_21_22 |> pull(post2021, name = nom_final))


# 2013-2020 en csv
var_13_20 <- tab_corres |> 
    drop_na(post2013) 

spec_13_20 <- var_13_20 |> pull(col_spec, name = post2013) |> do.call(cols_only, args = _)

EE_13_20 <- chemins_annees |> 
    filter(between(ANNEE, 2013, 2020)) |>
    group_by(ANNEE) |> 
    summarise(les_noms = list(nom_fichier)) |> 
    pull(les_noms) |> 
    map(\(x) read_csv2(x, col_types = spec_13_20, 
                       show_col_types = FALSE) |> 
            filter(RGA == 1) |> 
            rename_with(.fn = \(x) toupper(x))) |>
    list_rbind() |> 
    mutate(EXTRI = as.numeric(EXTRI)) |>
    rename(var_13_20 |> filter(str_detect(nom_final, "^EXTRI", negate = TRUE))|> pull(post2013, name = nom_final))

# 2003-2012 en csv
var_03_12 <- tab_corres |> 
    drop_na(pre2013) 

spec_03_12 <- var_03_12 |> pull(col_spec, name = pre2013) |> do.call(cols_only, args = _)

EE_03_12 <- chemins_annees |> 
    filter(between(ANNEE, 2003, 2012)) |> 
    pull(nom_fichier) |> 
    map(\(x) read_csv2(x, col_types = spec_03_12, 
                       show_col_types = FALSE) |> 
            filter(RGA == 1)) |>
    list_rbind() |> 
    mutate(EXTRI = as.numeric(EXTRI)) |>
    rename(var_03_12 |> pull(pre2013, name = nom_final))


## BASE 2003-2023
# Le diplome court avant 2023 est DIP, dont le 1er chiffre correspond à DIP7 de 2023 (sauf = 9 NA)
EE <- list(EE_03_12, 
           EE_13_20, 
           EE_21_22) |> 
    list_rbind() |> 
    mutate(DIP_court = str_sub(DIP_court, 1, 1) |> as.integer())

EE <- list(EE, EE_23) |> 
    list_rbind()

## LES DICOS
# Récupération dico 2022
sources_labels <- readLines(paste0(racine, "/lil-1619b/lil-1619b-Documentation/Labels_modalités/formats_spss_lil-1619b.txt"))

lignes_labels <- grep("^\\s*/", sources_labels)
lignes_labels <- c(lignes_labels, length(sources_labels) + 1)
label_blocks <- map2(lignes_labels[-length(lignes_labels)], lignes_labels[-1], \(x,y) sources_labels[(x + 1):(y - 1)])
names(label_blocks) <- str_remove(sources_labels[lignes_labels[-length(lignes_labels)]], "^\\s*/\\s*")

parse_labels <- function(block) {
    df <- str_match(block, "^\\s*(\\d+)\\s+\"(.*?)\"$")
    tibble(code = as.integer(df[,2]), label = df[,3])
}

dico22 <- map(label_blocks, parse_labels)

# Récupération dico 2014
sources_labels14 <- readLines(paste0(racine, "/lil-1000b.csv/Doc/Formats/formats_spss.txt"), encoding = "CP1252") |> 
    iconv(from = "CP1252", to = "UTF-8")

lignes_labels14 <- grep("^\\s*/", sources_labels14)
lignes_labels14 <- c(lignes_labels14, length(sources_labels14) + 1)
label_blocks14 <- map2(lignes_labels14[-length(lignes_labels14)], lignes_labels14[-1], \(x,y) sources_labels14[(x + 1):(y - 1)])
names(label_blocks14) <- str_remove(sources_labels14[lignes_labels14[-length(lignes_labels14)]], "^\\s*/\\s*")

dico14 <- map(label_blocks14, parse_labels)

# Récupération dico 2003
dico03 <- foreign::read.dbf(paste0(racine, "/lil-0246b.csv/Doc/varmod.DBF")) |>
    as_tibble() |>
    mutate(MODLIBELLE = iconv(as.character(MODLIBELLE), from = "CP1252", to = "UTF-8"),
           MODLIBELLE = stringi::stri_replace_all_fixed(
               MODLIBELLE,
               pattern = c("‚", "“", "Š", "…", "Œ"),
               replacement = c("é", "ô", "è", "à", "î"),
               vectorize_all = FALSE
           ))

### Recodage des diplomes ###
EE <- EE |> 
    mutate(diplome7 = labelled(DIP_court, labels = dico22[["DIP7"]] |> pull(code, name = label))) |> 
    select(-DIP_court)

EE <- EE |> 
    mutate(dip_temp = case_when(
        DIPSUP > 0 ~ DIPSUP,
        DIPTEC > 0 ~ DIPTEC,
        DIPGEN >= 0 ~ DIPGEN,
        .default = NA_integer_
    )) |> 
    select(-DIPGEN, -DIPTEC, -DIPSUP)

EE <- EE |> 
    mutate(
        # Recodage de diplome29 (codes à 2 chiffres)
        niveau_diplome29 = case_when(
            DIP_moyen == 11 ~ "Doctorat sauf santé",
            DIP_moyen == 12 ~ "Doctorat de santé",
            DIP_moyen %in% c(13, 14) ~ "Grande école",
            DIP_moyen %in% c(15, 16, 17, 18) ~ "Master/DESS/DEA",
            DIP_moyen == 21 ~ "Maîtrise",
            DIP_moyen %in% c(22, 23) ~ "Licence",
            DIP_moyen == 24 ~ "Autres Bac+3 et plus",
            DIP_moyen %in% c(31, 32, 33, 34, 35, 36) ~ "Bac+2",
            DIP_moyen == 41 ~ "Capacité/DAEU",
            DIP_moyen == 42 ~ "Baccalauréat général",
            DIP_moyen == 43 ~ "Baccalauréat techno",
            DIP_moyen == 44 ~ "Baccalauréat pro",
            DIP_moyen == 45 ~ "Brevet technicien",
            DIP_moyen %in% c(51, 52, 53) ~ "CAP/BEP",
            DIP_moyen == 60 ~ "Brevet des collèges",
            DIP_moyen == 70 ~ "CEP",
            DIP_moyen == 71 ~ "Aucun diplôme",
            TRUE ~ NA_character_
        ),
        # Recodage de dip_lab2003 (codes à 1-2 chiffres)
        niveau_dip_lab2003 = case_when(
            dip_temp == 71 ~ "Doctorat sauf santé",
            dip_temp == 72 ~ "Doctorat de santé",
            dip_temp %in% c(63, 64) ~ "Grande école",
            dip_temp %in% c(61, 62) ~ "Master/DESS/DEA",
            dip_temp == 53 ~ "Maîtrise",
            dip_temp == 51 ~ "Licence",
            dip_temp == 55 ~ "Autres Bac+3 et plus",
            dip_temp %in% c(41, 42, 43, 44, 46) ~ "Bac+2",
            dip_temp == 40 ~ "Capacité/DAEU",
            dip_temp == 17 ~ "Baccalauréat général",
            dip_temp == 32 ~ "Baccalauréat techno",
            dip_temp == 34 ~ "Baccalauréat pro",
            dip_temp %in% c(33, 35, 36) ~ "Brevet technicien",
            dip_temp %in% c(21, 23, 24, 25, 27, 28, 29) ~ "CAP/BEP",
            dip_temp == 15 ~ "Brevet des collèges",
            dip_temp == 2 ~ "CEP",
            dip_temp == 0 ~ "Aucun diplôme",
            TRUE ~ NA_character_
        ))

EE <- EE |> 
    mutate(diplome17 = if_else(ANNEE > 2012, niveau_diplome29, niveau_dip_lab2003) |> 
               factor(levels = c("Aucun diplôme", "CEP", "Brevet des collèges", "CAP/BEP", "Brevet technicien",
                                 "Baccalauréat pro", "Baccalauréat techno", "Baccalauréat général", "Capacité/DAEU",
                                 "Bac+2", "Autres Bac+3 et plus", "Licence", "Maîtrise", "Master/DESS/DEA",
                                 "Grande école", "Doctorat de santé", "Doctorat sauf santé"))) |> 
    select(-DIP_moyen, -dip_temp, -niveau_diplome29, -niveau_dip_lab2003)

EE <- EE %>%
    mutate(
        diplome104 = case_when(
            DIP_long == 100 ~ "Diplôme université bac+5",
            DIP_long == 110 ~ "Magistère",
            DIP_long == 111 ~ "Mastère spécialisé",
            DIP_long == 114 ~ "DRT",
            DIP_long == 120 ~ "DEA",
            DIP_long == 121 ~ "Master recherche",
            DIP_long == 130 ~ "DESS",
            DIP_long == 131 ~ "Master professionnel",
            DIP_long == 140 ~ "Master non différencié",
            DIP_long == 142 ~ "DNSEP depuis 2006",
            DIP_long == 145 ~ "BEES 3e degré",
            DIP_long == 150 ~ "Capacité médecine",
            DIP_long == 160 ~ "École sup. commerce (depuis 2000)",
            DIP_long == 164 ~ "Autre titre bac+5",
            DIP_long == 170 ~ "École ingénieur",
            DIP_long == 180 ~ "Prof. ens. secondaire (depuis 2010)",
            DIP_long == 188 ~ "Prof. écoles (depuis 2010)",
            DIP_long == 190 ~ "Agrégation (depuis 2010)",
            DIP_long == 194 ~ "Diplôme santé bac+5 (sage-femme)",
            DIP_long == 195 ~ "Docteur vétérinaire",
            DIP_long == 196 ~ "Doctorat santé",
            DIP_long == 197 ~ "Doctorat recherche",
            DIP_long == 198 ~ "Autre diplôme bac+5+",
            DIP_long == 200 ~ "Licence (L3)",
            DIP_long == 201 ~ "Maîtrise (M1)",
            DIP_long == 220 ~ "MST",
            DIP_long == 230 ~ "Maîtrise IUP",
            DIP_long == 240 ~ "DSAA",
            DIP_long == 241 ~ "DNAT/DNAP depuis 2006",
            DIP_long == 242 ~ "DNSEP jusqu'à 2005",
            DIP_long == 245 ~ "BEES 2e degré/Desjeps",
            DIP_long == 246 ~ "DESE/DEST Cnam",
            DIP_long == 250 ~ "Licence pro/BUT",
            DIP_long == 256 ~ "Diplôme université bac+3/4",
            DIP_long == 258 ~ "Drea",
            DIP_long == 260 ~ "École sup. commerce (jusqu'à 1999)",
            DIP_long == 264 ~ "Autre titre bac+3/4",
            DIP_long == 280 ~ "Prof. ens. secondaire IUFM",
            DIP_long == 288 ~ "Prof. écoles IUFM",
            DIP_long == 290 ~ "Agrégation (jusqu'à 2009)",
            DIP_long == 296 ~ "Diplôme santé bac+3/4 (infirmière)",
            DIP_long == 298 ~ "Autre diplôme bac+3/4",
            DIP_long == 320 ~ "BTS",
            DIP_long == 321 ~ "DMA",
            DIP_long == 322 ~ "DTS/DNTS/DPECF",
            DIP_long == 323 ~ "BTSA",
            DIP_long == 335 ~ "Brevet maîtrise supérieur",
            DIP_long == 340 ~ "DNAT avant 2006",
            DIP_long == 342 ~ "DNAP avant 2006",
            DIP_long == 345 ~ "Dejeps",
            DIP_long == 346 ~ "DPCT/DPCE Cnam",
            DIP_long == 350 ~ "DUT",
            DIP_long == 351 ~ "Propédeutique",
            DIP_long == 352 ~ "Deug/PCEM",
            DIP_long == 355 ~ "Deust",
            DIP_long == 356 ~ "Diplôme université bac+2",
            DIP_long == 363 ~ "CSA bac+2",
            DIP_long == 364 ~ "Autre titre bac+2",
            DIP_long == 388 ~ "École normale instituteur",
            DIP_long == 396 ~ "Diplôme santé bac+2 (infirmière)",
            DIP_long == 398 ~ "Autre diplôme bac+2",
            DIP_long == 400 ~ "Bac pro",
            DIP_long == 401 ~ "BMA",
            DIP_long == 402 ~ "BTM",
            DIP_long == 403 ~ "Bac pro agricole",
            DIP_long == 406 ~ "BMS",
            DIP_long %in% 410:414 ~ "Brevet enseignement",
            DIP_long == 420 ~ "BT",
            DIP_long == 423 ~ "BTA",
            DIP_long == 430 ~ "Bac technologique",
            DIP_long == 433 ~ "Bac technologique agricole",
            DIP_long == 435 ~ "Brevet de maîtrise",
            DIP_long == 437 ~ "Mention complémentaire bac",
            DIP_long == 445 ~ "BEES 1er degré/BPJEPS",
            DIP_long == 450 ~ "BP",
            DIP_long == 453 ~ "BPA",
            DIP_long == 460 ~ "BSEC",
            DIP_long == 463 ~ "CSA niveau bac",
            DIP_long == 464 ~ "Autre titre niveau bac",
            DIP_long == 470 ~ "Bac général",
            DIP_long == 488 ~ "Capacité droit/DAEU",
            DIP_long == 490 ~ "Brevet supérieur",
            DIP_long == 496 ~ "Diplôme santé niveau bac",
            DIP_long == 498 ~ "Autre diplôme niveau bac",
            DIP_long == 500 ~ "CAP",
            DIP_long == 503 ~ "CAPA",
            DIP_long == 510 ~ "BEP",
            DIP_long == 513 ~ "BEPA",
            DIP_long == 523 ~ "BAA",
            DIP_long == 532 ~ "BC",
            DIP_long == 537 ~ "Mention complémentaire CAP-BEP",
            DIP_long == 540 ~ "CFES",
            DIP_long == 545 ~ "Brevet élémentaire/BEPS",
            DIP_long == 553 ~ "BPA niveau CAP",
            DIP_long == 556 ~ "EFAA",
            DIP_long == 563 ~ "CSA niveau CAP-BEP",
            DIP_long == 564 ~ "Titre niveau CAP-BEP",
            DIP_long == 596 ~ "Diplôme santé CAP-BEP",
            DIP_long == 598 ~ "Autre diplôme CAP-BEP",
            DIP_long == 640 ~ "Brevet collèges",
            DIP_long == 641 ~ "CEPRO",
            DIP_long == 740 ~ "Certificat études primaires",
            DIP_long == 751 ~ "CFG",
            DIP_long == 799 ~ "Aucun diplôme",
            TRUE ~ NA_character_
        )
    )

EE <- EE |> 
    mutate(diplome104 = fct_reorder(diplome104, DIP_long, .fun = \(x) -first(x), .na_rm = FALSE)) |> 
    select(-DIP_long)

# inversion parents dans les cas simples
EE <- EE |> 
    mutate(CSPP_tmp = if_else(SEXE_PAR1 == 2 & SEXE_PAR2 == 1, CSPM, CSPP, missing = CSPP),
           CSPM = if_else(SEXE_PAR1 == 2 & SEXE_PAR2 == 1, CSPP, CSPM, missing = CSPM),
           CSPP = CSPP_tmp) |> 
    select(-CSPP_tmp)

# Recodage CSTOT, CSPP, CSPM en code à 1 ou 2 chiffres
# les 10 20 30 40 50 60 seront in fine mis en NA dans le codage à deux chiffres
# CSA1, CSPP1 et CSPM1 prennent le chiffre des dizaines et recyclent si nécessaire les 7X
EE <- EE |> 
    mutate(across(.cols = c(CSTOT, CSPP, CSPM), 
                  .fns = \(x) {
                      z <- str_sub(x, 1, 1)
                      z <- if_else(z == "7", str_sub(x, 2, 2), z)
                      if_else(z == "0", NA, z)
                      as.integer(z)
                  },
                  .names = "{.col}1"))

EE <- EE |> 
    mutate(CSPP1 = labelled(CSPP1, labels = dico22[["PCSPAR1_1"]] |> pull(code, name = label)),
           CSPM1 = labelled(CSPM1, labels = dico22[["PCSPAR1_1"]] |> pull(code, name = label)),
           CSTOT1 = labelled(CSPM1, labels = dico22[["PCS1"]] |> pull(code, name = label)))


EE <- EE |> 
    mutate(across(.cols = c(CSTOT, CSPP, CSPM), 
                  .fns = \(x) if_else(str_sub(x, -1) == "0", NA, x))) |> 
    mutate(CSTOT = labelled(CSTOT, labels = dico14[["CSTOT"]] |> pull(code, name = label)),
           CSPP = labelled(CSPP, labels = dico14[["CSPP"]] |> pull(code, name = label)),
           CSPM = labelled(CSPP, labels = dico14[["CSPP"]] |> pull(code, name = label)))


## Ajout de quelques labels
EE <- EE |> 
    mutate(across(.cols = c("SEXE", "TYPMEN", "TPPRED"),
                  .fns = \(x) {
                      nom_dico <- tab_corres |> filter(nom_final == cur_column()) |> pull(post2021)
                      labelled(x, labels = dico22[[nom_dico]] |> pull(code, name = label))
                      }))

# MATRI change à partir de 2021
EE <- EE |> 
    mutate(MATRI = if_else(ANNEE < 2021, MATRI, 
                           case_match(MATRI,
                                      c(1, 2, 3) ~ 2L,
                                      4 ~ 3L,
                                      c(5, 6, 7) ~ 4L,
                                      8 ~ 1L,
                                      .default = NA_integer_)),
           MATRI = labelled(MATRI, labels = dico14[["MATRI"]] |> pull(code, name = label)))

# SP00 change souvent
EE <- EE |> 
    mutate(statut21 = case_match(SP00,
                                1 ~ 1L, 2 ~ 2L, 3 ~ 3L, 4 ~ 6L, 5 ~ 4L, 6 ~ 5L, 7 ~ 6L, 9 ~ 6L, .default = NA_integer_),
           statut13 = case_match(SP00,
                                1 ~ 1L, 2 ~ 1L, 3 ~ 4L, 4 ~ 2L, 5 ~ 3L, 6 ~ 6L, 7 ~ 5L, 8 ~ 6L, 9 ~ 6L, .default = NA_integer_),
           statut03 = case_match(SP00,
                                1 ~ 1L, 2 ~ 1L, 3 ~ 1L, 4 ~ 2L, 5 ~ 4L, 6 ~ 6L, 7 ~ 3L, 8 ~ 5L, 9 ~ 6L, .default = NA_integer_),
           statut = if_else(ANNEE >= 2021, statut21,
                            if_else(ANNEE >= 2013, statut13, statut03))) |> 
    select(-statut03, -statut13, -statut21, -SP00)

write_parquet(EE, "EE03_23.parquet")

EE82_02 <- read_parquet("EE82_02.parquet") |> 
    transmute(
        ANNEE,
        SEXE = S,
        AG,
        CSTOT,
        CSPP,
        SALTR,
        SAL,
        TP,
        TYPMENR,
        statut = case_match(as.integer(FI), 1 ~ 1L, 2 ~ 2L, 3 ~ 4L, 4 ~ 6L, 5 ~ 3L, 6 ~ 3L, 7 ~ 5L, 8 ~ 6L),
        diplome7 = case_match(diplome,
                              c("Aucun diplôme", "CEP") ~ 7L,
                              "Brevet des collèges" ~ 6L,
                              "CAP/BEP" ~ 5L,
                              c("Brevet technicien", "Baccalauréat techno", "Baccalauréat pro", "Baccalauréat général") ~ 4L,
                              c("Capacité/DAEU", "Bac+2") ~ 3L,
                              "Licence/Maîtrise" ~ 2L,
                              c("3e cycle/Tout doctorat", "Grande école") ~ 1L) |> 
            factor(labels = (EE$diplome7 |> to_factor() |> levels())[1:7]),
        diplome13 = diplome
        )
    
seuil_SALTR <- c(0, seq(1000, 5000, 500), seq(6000, 10000, 1000), seq(15000, 30000, 5000), Inf) * .152449

EE03_23 <- EE |> 
    transmute(
        ANNEE, 
        SEXE = to_factor(SEXE), 
        AG, 
        CSTOT = to_factor(CSTOT),
        CSPP = to_factor(CSPP),
        CSPM = to_factor(CSPM),
        SAL,
        SALTR = cut(SAL, breaks = seuil_SALTR),
        TP = to_factor(TPPRED),
        TYPMENR = to_factor(TYPMEN) |> 
            fct_recode(
                "Célibataire" = "Femme seule", 
                "Ménage complexe" = "Autre",
                "Monoparent" = "Famille monoparentale",
                "Couple avec enfant(s)" = "Couple avec enfant (s)",
                NULL = "6",    # Supprime le niveau "6"
                NULL = "9"     # Supprime le niveau "9"
            ),
        statut,
        diplome7 = to_factor(diplome7),
        diplome17,
        diplome13 = case_match(diplome17,
                               "Aucun diplôme" ~ "Aucun diplôme",
                               "CEP" ~ "CEP",
                               "Brevet des collèges" ~ "Brevet des collèges",
                               "CAP/BEP" ~ "CAP/BEP",
                               "Brevet technicien" ~ "Brevet technicien", 
                               "Baccalauréat pro" ~ "Baccalauréat pro",
                               "Baccalauréat techno" ~ "Baccalauréat techno",
                               "Baccalauréat général" ~ "Baccalauréat général",
                               "Bac+2" ~"Bac+2",
                               "Capacité/DAEU" ~ "Capacité/DAEU",
                               c("Autres Bac+3 et plus", "Licence", "Maîtrise") ~ "Licence/Maîtrise",
                               c("Master/DESS/DEA", "Doctorat de santé", "Doctorat sauf santé") ~ "3e cycle/Tout doctorat",
                               "Grande école" ~ "Grande école"),
        diplome104,
        EXTRI
    )



EE82_23 <- bind_rows(EE82_02, EE03_23) |> 
    mutate(statut = factor(statut, labels = c("En emploi", "Chômeur", "Retraite", "Etudes", "Au foyer", "Autres inactifs"))) 


# Calculs du quantile moyen de la tranche de SALAIRE par ANNEE
corres_centile <- EE82_23 |> 
    drop_na(SALTR) |> 
    filter(SALTR != "Refus de répondre") |> 
    group_by(ANNEE, SALTR) |>
    summarise(n = n(), .groups = "drop_last") |> 
    mutate(prop = 100 *n / sum(n),
           cum = cumsum(prop),
           cum_prec = lag(cum, default = 0),
           centile_moyen = (cum + cum_prec)/2) |> 
    select(ANNEE, SALTR, centile_moyen)

EE82_23 <- EE82_23 |> 
    left_join(corres_centile, by = c("ANNEE", "SALTR"))
        
# Corrections des CS
EE82_23 <- EE82_23 |> 
    mutate(across(.cols = c(CSTOT, CSPP, CSPM), .fns = \(x) str_to_sentence(x))) |> 
    mutate(CSTOT = case_match(CSTOT,
                              "Élèves, étudiants" ~ "Elèves, étudiants",
                              "Employés administratifs d'entreprises" ~ "Employés administratifs d'entreprise",
                              "Instituteurs et assimilés" ~ "Professeurs des écoles, instituteurs et professions assimilées",
                              .default = CSTOT)) |> 
    mutate(across(.cols = c(CSPP, CSPM), 
                  .fns = \(x) case_match(x,
                             "84" ~ "Elèves, étudiants",
                             c("85", "86") ~ "Inactifs divers (autres que retraités)",
                             "Employés administratifs d'entreprises" ~ "Employés administratifs d'entreprise",
                             "Ouvriers agricoles" ~ "Ouvriers agricoles et assimilés",
                             "Instituteurs et assimilés" ~ "Professeurs des écoles, instituteurs et professions assimilées",
                             .default = x)))

EE82_23 <- EE82_23 |> 
    mutate(across(.cols = c(CSTOT, CSPP, CSPM),
                  .names = "{.col}1",
                  .fns = \(x) case_when(
                      str_detect(x, "(A|a)griculteur") ~ "Agriculteurs",
                      str_detect(x, "intermédiaire") ~ "Professions intermédiaires",
                      str_detect(x, "(O|o)uvrier") ~ "Ouvriers",
                      str_detect(x, "(E|e)mployé") ~ "Employés",
                      str_detect(x, "(A|a)rtisan|(C|c)ommerçant|(C|c)hef") ~ "Artisans, commerçants, chefs d'entreprise",
                      str_detect(x, "(C|c)adre") ~ "Cadres et professions intellectuelles",
                      x == "Chauffeurs" ~ "Ouvriers",
                      x %in% c("Professeurs des écoles, instituteurs et professions assimilées", 
                               "Clergé, religieux",
                               "Contremaîtres, agents de maîtrise",
                               "Techniciens") ~ "Professions intermédiaires",
                      x %in% c("Chômeurs n'ayant jamais travaillé", 
                               "Personnes diverses sans activité professionnelle de moins de 60 ans (sauf retraités)",
                               "Personnes diverses sans activité professionnelle de 60 ans et plus (sauf retraités)",
                               "Militaires du contingent",
                               "Elèves, étudiants",
                               "Inactifs divers (autres que retraités)") ~ "Inactifs (n'ayant jamais travaillé)",
                      x %in% c("Personnels des services directs aux particuliers",
                               "Policiers et militaires") ~ "Employés",
                      x %in% c("Professeurs, professions scientifiques",
                               "Professions de l'information, des arts et des spectacles",
                               "Professions libérales") ~ "Cadres et professions intellectuelles"
                  ))) |> 
    mutate(across(.cols = c(CSTOT1, CSPP1, CSPM1),
                  .fns = \(x) factor(x, levels = c("Agriculteurs", 
                                                   "Artisans, commerçants, chefs d'entreprise",
                                                   "Cadres et professions intellectuelles",
                                                   "Professions intermédiaires",
                                                   "Employés",
                                                   "Ouvriers", 
                                                   "Inactifs (n'ayant jamais travaillé)"))))

EE82_23 <- EE82_23 |> 
    mutate(diplome13 = diplome13 |> fct_relevel("Aucun diplôme", "CEP", "Brevet des collèges", "CAP/BEP", 
                                                "Brevet technicien", "Baccalauréat pro", "Baccalauréat techno",
                                                "Baccalauréat général", "Capacité/DAEU", "Bac+2",
                                                "Licence/Maîtrise", "3e cycle/Tout doctorat", "Grande école"))

breaks_gen5 <- seq(1880, 2010, 5)

EE82_23 <- EE82_23 |> 
    mutate(date_naissance = ANNEE - AG,
           gen5 = cut(date_naissance, breaks = breaks_gen5, include.lowest = TRUE, ordered_result = TRUE),
           diplome10 = diplome13 |> 
               fct_collapse("Baccalauréat techno" = c("Brevet technicien", "Baccalauréat techno"),
                            "Bac+2" = c("Bac+2", "Capacité/DAEU"),
                            "Grande école/3e cycle/Tout doctorat" = c("Grande école", "3e cycle/Tout doctorat")) |> 
               fct_relevel("Baccalauréat techno", after = 5))

write_parquet(EE82_23, "EE82_23.parquet")


