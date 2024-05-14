
# INIT -------------------------------------------------------------------------

library(wifo.base)
library(wifo.theme)
library(wifo.data)
library(STATcubeR)
library(tidyverse)
library(readxl)
library(broom)
library(readxl)

filter <- dplyr::filter

setwd("K:/gitea/tariflohnmonitor/scripts/")
options(scipen = 99999)
names <- read_excel("K:/gitea/tariflohnmonitor/scripts/names.xlsx")

months <- matrix(
  data = c(
    "01", "Jän",
    "01", "Jan",
    "02", "Feb",
    "03", "Mär",
    "03", "Mar",
    "04", "Apr",
    "05", "Mai",
    "05", "May",
    "06", "Jun",
    "07", "Jul",
    "08", "Aug",
    "09", "Sep",
    "10", "Okt",
    "10", "Oct",
    "11", "Nov",
    "12", "Dez",
    "12", "Dec"
  ),
  ncol = 2,
  byrow = T
)

# TLWDS ------------------------------------------------------------------------

wds_series <- names |> 
  filter(series == "Tariflohnindex 2006")
TLI <- NULL

for (i in 1:nrow(wds_series)) {
  temp <- wdGetWdsData(wds_series[i,1], db = "wds")
  temp$Sparte <- wds_series[i,2]
  names(temp) <- c("Monat", "Tariflohnindex", "Sparte")
  TLI <- rbind(TLI, temp)
}

TLI <- TLI |>
  map_df(rev)


# TLKV -------------------------------------------------------------------------

od_series <- names |>
  filter(names == "Tariflohnindex")
TLI_KV <- NULL

temp <- od_table("OGD_tli16wk15_TLI_107")
temp <- temp$tabulate()

months <- as.tibble(months)
names(months) <- c("Monat","Monat_ch")

for (i in 1:nrow(od_series)) {
  temp <- od_table(od_series[i,1] |> pull())
  temp <- temp$tabulate()
  names(temp)[4] <- "Tariflohnindex"
  names(temp)[1] <- "Kollektivvertrag"
  temp <- temp |>
    filter(nchar(as.character(Zeitreihe))!=4) |>
    mutate(
      Zeitreihe = str_replace(as.character(Zeitreihe), fixed(" (prel.)"), ""),
      Jahr = paste0("20", substr(Zeitreihe, nchar(as.character(Zeitreihe))-1, nchar(as.character(Zeitreihe)))),
      Monat_ch = fct_recode(substr(Zeitreihe, 1,3))
    ) |>
    left_join(months) |>
    mutate(
      Monat = paste0(Jahr, "-", Monat, "-01")
    ) |>
    select(Monat, Kollektivvertrag, `Soziale Stellung`, Tariflohnindex) 
  temp$series <- od_series[i,3] |> pull()
  TLI_KV <- rbind(TLI_KV, temp)
}

distinct_2016 <- TLI_KV |>
  filter(series == "Tariflohnindex nach KV 2016") |>
  ungroup() |>
  distinct(Kollektivvertrag)

distinct_2006 <- TLI_KV |>
  filter(series == "Tariflohnindex nach KV 2006") |>
  ungroup() |>
  distinct(Kollektivvertrag)

KV_names <- read_excel("KV_names.xlsx")
FOO2010_2015 <- read_csv("FOO2010-2015.csv")

TLI_KV <- TLI_KV |>
  filter(Kollektivvertrag %in% KV_names$`Tariflohnindex nach KV 2006` | series == "Tariflohnindex nach KV 2016") |>
  filter(Kollektivvertrag %in% KV_names$`Tariflohnindex nach KV 2016` | series == "Tariflohnindex nach KV 2006") |>
  left_join(KV_names, by = c("Kollektivvertrag" = "Tariflohnindex nach KV 2006")) |>
  mutate(Kollektivvertrag = ifelse(
    series == "Tariflohnindex nach KV 2006",
    `Tariflohnindex nach KV 2016`,
    Kollektivvertrag
    )) |>
  group_by(Monat, Kollektivvertrag, series) |>
  summarize(Tariflohnindex = mean(Tariflohnindex))
  
adj_factor <- TLI_KV |>
  filter(startsWith(as.character(Monat), "2016")) |> 
  filter(series == "Tariflohnindex nach KV 2006") |>
  group_by(Kollektivvertrag) |>
  summarize(adj_factor = mean(Tariflohnindex))

TLI_KV <- TLI_KV |>
  left_join(adj_factor) |>
  mutate(Tariflohnindex = ifelse(
    series == "Tariflohnindex nach KV 2016",
    Tariflohnindex * adj_factor / 100,
    Tariflohnindex
  )) |>
  mutate(Monat = as.Date(Monat)) |>
  filter(series == "Tariflohnindex nach KV 2016" | Monat < "2017-01-01") |>
  mutate(Sparte = Kollektivvertrag) |>
  ungroup() |>
  select(-series, -Kollektivvertrag, -adj_factor)


# TLWKO ------------------------------------------------------------------------

od_series <- names |>
  filter(names == "Tariflohnindex nach WKO")
TLI_WKO <- NULL

months <- as.tibble(months)
names(months) <- c("Monat","Monat_ch")

for (i in 1:nrow(od_series)) {
  temp <- od_table(od_series[i,1] |> pull())
  temp <- temp$tabulate()
  names(temp)[4] <- "Tariflohnindex"
  names(temp)[1] <- "Sparte"
  temp <- temp |>
    filter(nchar(as.character(Zeitreihe))!=4) |>
    mutate(
      Zeitreihe = str_replace(as.character(Zeitreihe), fixed(" (prel.)"), ""),
      Jahr = paste0("20", substr(Zeitreihe, nchar(as.character(Zeitreihe))-1, nchar(as.character(Zeitreihe)))),
      Monat_ch = fct_recode(substr(Zeitreihe, 1,3))
    ) |>
    left_join(months) |>
    mutate(
      Monat = paste0(Jahr, "-", Monat, "-01")
    ) |>
    select(Monat, Sparte, `Soziale Stellung`, Tariflohnindex) 
  temp$series <- od_series[i,3] |> pull()
  TLI_WKO <- rbind(TLI_WKO, temp |> filter(`Soziale Stellung` == "Insgesamt"))
}

distinct_2016 <- TLI_WKO |>
  filter(series == "Tariflohnindex nach WKO 2016") |>
  ungroup() |>
  distinct(Sparte)

distinct_2006 <- TLI_WKO |>
  filter(series == "Tariflohnindex nach WKO 2006") |>
  ungroup() |>
  distinct(Sparte)

# write_excel_csv(distinct_2006, "WKO_raw_2006.csv")
# write_excel_csv(distinct_2016, "WKO_raw_2016.csv")

WKO_names <- read_excel("KV_names.xlsx", sheet = "WKO_names", trim_ws = F)

TLI_WKO <- TLI_WKO |>
  filter(Sparte %in% WKO_names$`Tariflohnindex nach WKO 2006` | series == "Tariflohnindex nach WKO 2016") |>
  filter(Sparte %in% WKO_names$`Tariflohnindex nach WKO 2016` | series == "Tariflohnindex nach WKO 2006") |>
  left_join(WKO_names, by = c("Sparte" = "Tariflohnindex nach WKO 2006")) |>
  mutate(Sparte = ifelse(
    series == "Tariflohnindex nach WKO 2006",
    `Tariflohnindex nach WKO 2016`,
    Sparte
  )) |>
  group_by(Monat, Sparte, series) |>
  summarize(Tariflohnindex = mean(Tariflohnindex))

adj_factor <- TLI_WKO |>
  filter(startsWith(as.character(Monat), "2016")) |> # 2016
  filter(series == "Tariflohnindex nach WKO 2006") |>
  group_by(Sparte) |>
  summarize(adj_factor = mean(Tariflohnindex))

TLI_WKO <- TLI_WKO |>
  left_join(adj_factor) |>
  mutate(Tariflohnindex = ifelse(
    series == "Tariflohnindex nach WKO 2016",
    Tariflohnindex * adj_factor / 100,
    Tariflohnindex
  )) |>
  mutate(Monat = as.Date(Monat)) |>
  filter(series == "Tariflohnindex nach WKO 2016" | Monat < "2017-01-01") |>
  mutate(Sparte = Sparte) |>
  ungroup() |>
  select(-series, -adj_factor)

# test <- TLI_WKO |> group_by(Sparte) |> count()


# TLÖFF ------------------------------------------------------------------------

od_series <- names |>
  filter(names == "Tariflohnindex nach Gruppen")
TLI_GRU <- NULL

months <- as.tibble(months)
names(months) <- c("Monat","Monat_ch")

for (i in 1:nrow(od_series)) {
  temp <- od_table(od_series[i,1] |> pull())
  temp <- temp$tabulate()
  names(temp)[4] <- "Tariflohnindex"
  names(temp)[1] <- "Sparte"
  temp <- temp |>
    filter(nchar(as.character(Zeitreihe))!=4) |>
    mutate(
      Zeitreihe = str_replace(as.character(Zeitreihe), fixed(" (prel.)"), ""),
      Jahr = paste0("20", substr(Zeitreihe, nchar(as.character(Zeitreihe))-1, nchar(as.character(Zeitreihe)))),
      Monat_ch = fct_recode(substr(Zeitreihe, 1,3))
    ) |>
    left_join(months) |>
    mutate(
      Monat = paste0(Jahr, "-", Monat, "-01")
    ) |>
    select(Monat, Sparte, `Soziale Stellung`, Tariflohnindex) 
  temp$series <- od_series[i,3] |> pull()
  TLI_GRU <- rbind(TLI_GRU, temp |> filter(`Soziale Stellung` == "Insgesamt"))
}

distinct_2016 <- TLI_GRU |>
  filter(series == "Tariflohnindex nach Gruppen 2016") |>
  ungroup() |>
  distinct(Sparte)

distinct_2006 <- TLI_GRU |>
  filter(series == "Tariflohnindex nach Gruppen 2006") |>
  ungroup() |>
  distinct(Sparte)

# write_excel_csv(distinct_2006, "Gruppen_raw_2006.csv")
# write_excel_csv(distinct_2016, "Gruppen_raw_2016.csv")

TLI_GRU <- TLI_GRU |>
  group_by(Monat, Sparte, series) |>
  summarize(Tariflohnindex = mean(Tariflohnindex))

adj_factor <- TLI_GRU |>
  filter(startsWith(as.character(Monat), "2016")) |> # 2016
  filter(series == "Tariflohnindex nach Gruppen 2006") |>
  group_by(Sparte) |>
  summarize(adj_factor = mean(Tariflohnindex))

TLI_GRU <- TLI_GRU |>
  left_join(adj_factor) |>
  mutate(Tariflohnindex = ifelse(
    series == "Tariflohnindex nach Gruppen 2016",
    Tariflohnindex * adj_factor / 100,
    Tariflohnindex
  )) |>
  mutate(Monat = as.Date(Monat)) |>
  filter(series == "Tariflohnindex nach Gruppen 2016" | Monat < "2017-01-01") |>
  mutate(Sparte = Sparte) |>
  ungroup() |>
  select(Monat, Tariflohnindex, Sparte) |>
  filter(Sparte != "Elektrizitätsversorgungsunternehmungen")

test <- TLI_GRU |> group_by(Sparte) |> count()

TLI_GRU <- TLI_GRU |>
  filter(Sparte %in% (test |> filter(n >199) |> select(Sparte) |> pull()))

TLI <- TLI_WKO |>
  add_row(TLI_KV) |>
  add_row(TLI_GRU)

# test <- TLI |> group_by(Sparte) |> count()



# PROD --------------------------------------------------------------------

l = od_list()

od_series <- names |> 
  filter(
    series %in% c(
      "Produktivitätsindex 2010", 
      "Produktivitätsindex 2015"
      )
    )

PROD <- NULL

for (i in 1:nrow(od_series)) {
  temp <- od_table(od_series[i,1] |> pull())
  temp <- temp$tabulate()
  temp <- gather(temp, "Sektor", "Produktivitätsindex", 2:ncol(temp))
  names(temp) <- c("Monat", "Sektor", "Produktivitätsindex")
  temp$Einheit <- od_series[i,2] |> pull()
  temp$series <- od_series[i,3] |> pull()
  PROD <- rbind(PROD, temp)
}

PROD <- PROD |>
  # filter(Sektor == "Productivity index per employed") |>
  arrange(Monat, Einheit, Sektor) |>
  mutate(
    Sektor = ifelse(
      Sektor %in% c(
        "Productivity index per hour worked in total",
        "Productivity index per employed"
      ),
      "Total",
      Sektor
    )
  )

adj_factor <- PROD |>
  filter(startsWith(as.character(Monat), "2015")) |>
  spread(key = series, value = Produktivitätsindex) |>
  mutate(adj_factor = `Produktivitätsindex 2015`/`Produktivitätsindex 2010`) |>
  group_by(Sektor, Einheit) |>
  summarize(adj_factor = mean(adj_factor))

PROD <- PROD |>
  left_join(adj_factor) |>
  mutate(Produktivitätsindex = ifelse(
    series == "Produktivitätsindex 2015", 
    Produktivitätsindex / adj_factor, 
    Produktivitätsindex
    )) |>
  filter(series == "Produktivitätsindex 2015" | Monat < "2015-01-01")


# PVGR -------------------------------------------------------------------------

wds_series <- names |> 
  filter(series %in% c("Produktivitätsindex VGR"))
PVGR <- NULL

for (i in 1:nrow(wds_series)) {
  temp <- wdGetWdsData(wds_series[i,1], db = "wds")
  temp$Sparte <- wds_series[i,2]
  names(temp) <- c("Monat", "Produktivitätsindex", "Indikator")
  temp$series <- wds_series[i,3] |> pull()
  PVGR <- rbind(PVGR, temp)
}

PVGR <- PVGR |> 
  filter(Monat > "2009-12-01") |>
  spread(key = "Indikator", value = "Produktivitätsindex") |>
  full_join(PROD |> select(Monat) |> distinct()) |>
  mutate(
    `Produktivitätsindex je unselbstständig Beschäftigten` = `Bruttoinlandsprodukt, real - unbereinigt` / `Erwerbstätige (Beschäftigungsverhältnisse) Insgesamt - unbereinigt`,
    `Produktivitätsindex je geleisteter Arbeitsstunde` = `Bruttoinlandsprodukt, real - unbereinigt` / `Arbeitszeitvolumen Erwerbstätige Insgesamt - unbereinigt`
  ) |>
  select(
    Monat, 
    `Produktivitätsindex je unselbstständig Beschäftigten`, 
    `Produktivitätsindex je geleisteter Arbeitsstunde`, 
    series
    ) |>
  arrange(Monat) |>
  fill(2:4, .direction = c("down")) |>
  gather(value = "Produktivitätsindex", key = "Einheit", 2:3) |>
  mutate(Sektor = "Gesamt (VGR-Daten; Alle Sektoren)") 

PROD <- PROD |>
  select(-adj_factor) |>
  add_row(PVGR) 


# INFL -------------------------------------------------------------------------

wds_series <- names |> 
  filter(series %in% c(
    "VPI2010 Obergruppen", 
    "VPI2015 Obergruppen", 
    "VPI2020 Obergruppen"
    ))

VPI <- NULL

for (i in 1:nrow(wds_series)) {
  temp <- wdGetWdsData(wds_series[i,1], db = "wds")
  temp$Sparte <- wds_series[i,2]
  names(temp) <- c("Monat", "Verbraucherpreisindex", "Produktgruppe")
  temp$series <- wds_series[i,3] |> pull()
  VPI <- rbind(VPI, temp)
}

VPI <- VPI |> 
  mutate(Produktgruppe = str_replace(Produktgruppe, "VPI 2010 - ", "")) |>
  mutate(Produktgruppe = str_replace(Produktgruppe, "VPI 2015 - ", "")) |>
  mutate(Produktgruppe = str_replace(Produktgruppe, "VPI 2020 - ", "")) 

adj_factor <- VPI |>
  filter(startsWith(as.character(Monat), "2021-01-01")) |> # 2016
  spread(key = series, value = Verbraucherpreisindex) |>
  mutate(adj_factor = `VPI2020 Obergruppen`/`VPI2015 Obergruppen`) |>
  group_by(Produktgruppe) |>
  summarize(adj_factor = mean(adj_factor))

VPI <- VPI |>
  left_join(adj_factor) |>
  mutate(Verbraucherpreisindex = ifelse(
    series == "VPI2015 Obergruppen",
    Verbraucherpreisindex * adj_factor,
    Verbraucherpreisindex
  )) |>
  mutate(Monat = as.Date(Monat)) |>
  filter(series == "VPI2020 Obergruppen" | Monat < "2021-01-01") |>
  filter(!(series == "VPI2020 Obergruppen" & Monat < "2021-01-01")) |>
  select(-adj_factor)

adj_factor <- VPI |>
  filter(startsWith(as.character(Monat), "2016-01-01")) |> # 2016
  spread(key = series, value = Verbraucherpreisindex) |>
  mutate(adj_factor = `VPI2015 Obergruppen`/`VPI2010 Obergruppen`) |>
  group_by(Produktgruppe) |>
  summarize(adj_factor = mean(adj_factor))

VPI <- VPI |>
  left_join(adj_factor) |>
  mutate(Verbraucherpreisindex = ifelse(
    series == "VPI2010 Obergruppen",
    Verbraucherpreisindex * adj_factor,
    Verbraucherpreisindex
  )) |>
  mutate(Monat = as.Date(Monat)) |>
  filter(series == "VPI2015 Obergruppen" | series == "VPI2020 Obergruppen" | Monat < "2016-01-01") |>
  filter(!(series == "VPI2015 Obergruppen" & Monat < "2016-01-01")) |>
  select(-series, -adj_factor) |>
  arrange(Monat)

test <- VPI |> filter(Produktgruppe == "Gesamtindex")
 
temp <- od_table("OGD_vpi20_VPI_2020_1")
temp <- temp$tabulate()



# ADJU --------------------------------------------------------------------

PROD <- PROD |>
  filter(Monat > "2009-12-01") |>
  filter(!is.nan(Produktivitätsindex)) |>
  filter(Sektor %in% c(
    "Total",
    "Gesamt (VGR-Daten; Alle Sektoren)",
    # "Intermediate goods <A>",
    # "Mining and quarrying <B>",
    # "Capital goods <B>",
    "Manufacturing <C>",
    # "Electricity, gas, steam and air conditioning supply <D>",
    # "Energy <E>",
    "Construction <F>"
  )) |>
  mutate(Sektor = fct_recode(
    Sektor,
    "Produzierender Bereich (ÖNACE Abschnitte B-F)" = "Total",
    "Herstellung von Waren" = "Manufacturing <C>",
    # "Energieversorgung" = "Energy <E>",
    "Bau" = "Construction <F>",
  ))
  

VPI <- VPI |>
  filter(Monat > "2009-12-01") |>
  mutate(Produktgruppe = fct_recode(
    Produktgruppe,
    "Gesamt" = "Gesamtindex"
  )) 

# test <- VPI |> group_by(Monat, Produktgruppe) |> count()
# test <- VPI |> group_by(Monat, Produktgruppe)

TLI <- TLI |>
  filter(Monat > "2009-12-01") |>
  mutate(Sparte = factor(
    Sparte,
    levels = unique(c(
      WKO_names$`Tariflohnindex nach WKO 2016`, 
      KV_names$`Tariflohnindex nach KV 2016`,
      as.character(TLI_GRU$Sparte)
    ))
  )) |>
  drop_na() |>
  arrange(Sparte)

# test <- TLI |> filter(Sparte == "Gesamt")


# SAVE -------------------------------------------------------------------------

write_csv(PROD, "K:/gitea/tariflohnmonitor/PROD.csv")
write_csv(VPI, "K:/gitea/tariflohnmonitor/VPI.csv")
write_csv(TLI, "K:/gitea/tariflohnmonitor/TLI.csv")


# LYDIA ------------------------------------------------------------------------

pathBase <- "//HFUSR/Nabu/Bereiche/FB2/Daten/Einkommen/Tariflohnindex/"

tableToWifoStyle(
  data = TLI |> 
    spread(key = "Monat", value = "Tariflohnindex") |> 
    data.table(),
  filePath = paste0(
    pathBase, 
    "AKTUELLSTE DATEN TLI Updates/Aktuell_Verkettet/STATcube API/TLI_wide.xlsx"
    )
)

tableToWifoStyle(
  data = TLI_GRU |> 
    spread(key = "Monat", value = "Tariflohnindex") |> 
    data.table(),
  filePath = paste0(
    pathBase, 
    "AKTUELLSTE DATEN TLI Updates/Aktuell_Verkettet/STATcube API/TLI_GRU.xlsx"
  )
)

tableToWifoStyle(
  data = TLI_KV |> 
    spread(key = "Monat", value = "Tariflohnindex") |> 
    data.table(),
  filePath = paste0(
    pathBase, 
    "AKTUELLSTE DATEN TLI Updates/Aktuell_Verkettet/STATcube API/TLI_KV.xlsx"
  )
)

tableToWifoStyle(
  data = TLI_WKO |> 
    spread(key = "Monat", value = "Tariflohnindex") |> 
    data.table(),
  filePath = paste0(
    pathBase, 
    "AKTUELLSTE DATEN TLI Updates/Aktuell_Verkettet/STATcube API/TLI_WKO.xlsx"
  )
)
