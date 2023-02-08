
# Lê arquivo de correções
correcoes <- read_csv2("inputs/correcoes.csv") %>%
  mutate(Data = as.Date(Data,"%d/%m/%Y"),
         #DataHora = paste(Data, Hora))
         DataHora = as.POSIXct(paste(Data, Hora), "%Y-%m-%d %H:%M", tz = Sys.timezone()))


tail(correcoes)
