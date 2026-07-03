library(tidyverse)

EE <- arrow::read_parquet("EE82_23.parquet")
indice_prix <- read.csv2("serie_011813530_03032026/valeurs_annuelles.csv", skip = 3) |> 
    as_tibble() |> 
    transmute(ANNEE = Période, ipc = as.numeric(X))

##############################################
# LES SALAIRES SONT EN EURO CONSTANT DE 2025 #
##############################################
EE <- EE |> 
    left_join(indice_prix, by = "ANNEE") |> 
    mutate(sal2025 = SAL * ipc / 100)

EE <- EE |> 
    mutate(AG5 = cut(AG, breaks = c(30, 35, 40, 45, 50, 55, 60), include.lowest = TRUE, ordered = TRUE),
           AG10 = cut(AG, breaks = c(30, 40, 50, 60), include.lowest = TRUE, ordered = TRUE))

les_tranches <- 1000 * c(0, seq(1, 5, .5), 6:10, 15, 20, 25, 30, 50)
gros_salaire <- EE |> 
    filter(between(ANNEE, 1990, 1999), SALTR == "Plus de 30000 francs") |> 
    summarise(logsalmax = mean(log(sal2025), na.rm = TRUE) |> exp(),
              ipc_mean = mean(ipc))
######################################################################
# ATTRIBUTION DES SALAIRES MOYENS DANS CHAQUE TRANCHE POUR 1982-1989 #
######################################################################

les_imputations <- tibble(borne_inf = les_tranches[1:19] * 0.1524, 
                          borne_sup = les_tranches[2:20] * 0.1524, 
                          tranches = levels(EE$SALTR)[1:19]) |> 
    mutate(sal_moyen = (borne_inf + borne_sup)/2) |> 
    cross_join(tibble(ANNEE = 1982:1989)) |> 
    left_join(indice_prix, by = "ANNEE") |> 
    mutate(sal_moyen = sal_moyen * ipc /100, 
           sal_moyen = if_else(borne_sup <5000, sal_moyen,
                               gros_salaire$logsalmax / gros_salaire$ipc_mean * ipc))

EE <- EE |> left_join(les_imputations |> select(SALTR = tranches, ANNEE, sal_moyen), by = c("ANNEE", "SALTR"))

EE <- EE |> 
    mutate(sal2025 = if_else(ANNEE < 1990, sal_moyen, sal2025)) |> 
    select(-sal_moyen)

les_périodes <- c(seq(1982, 2017, 5), 2023)
les_périodes_lab <- paste0(str_sub(les_périodes, 3, 4)[-9], "-", str_sub(les_périodes, 3, 4)[-1])

EE <- EE |> 
    filter(between(AG, 30, 60), gen5 >= "(1930,1935]") |> 
    mutate(id = 1:n()) |> 
    filter(SAL > 0) |> 
    drop_na(SAL) |> 
    mutate(logsal = log(sal2025),
           ANby5  = cut(ANNEE, breaks = les_périodes, include.lowest = TRUE, 
                        labels = les_périodes_lab, ordered = TRUE))


csp_origin <- levels(EE$CSPP1)[-7] 
names(csp_origin) <- c("Agriculteur", "ACCE", "Cadre", "Pinter", "Employé", "Ouvrier")

EE <- EE |> 
    filter(CSPP1 != "Inactifs (n'ayant jamais travaillé)") |> 
    mutate(Origin = CSPP1 |> fct_recode(!!!csp_origin))

EE <- EE |> mutate(PCS1 = fct_recode(CSTOT1, !!!csp_origin))

# ON FAIT AVEC diplome7
dip_levels <- EE$diplome7 |> levels()
names(dip_levels) <- c(paste0("Niveau", 6:0), "Non réponse")
EE <- EE |> 
    mutate(dip7 = fct_recode(diplome7, !!!dip_levels))


