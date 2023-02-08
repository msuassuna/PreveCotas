# Dados hidrotelemetria
Dados <- tryCatch(read_delim(paste0("dados/",Codigos[est],".txt"),
                             delim = " ", col_types = "cTnnn"),
                  error = function(e){
                    message("Algum problema aconteceu na leitura.\n",e)
                  },
                  warning = function(w){
                    message("Algum problema ocorreu na leitura.\nPossivelmente os dados sao indisponiveis.\n",w)
                  })

DatasSub <- data.frame(DataHora = seq(as.POSIXct(min(Dados$DataHora), tz = Sys.timezone()),
                                      as.POSIXct(max(Dados$DataHora) + 5 * 3600, tz = Sys.timezone()),
                                      by = '15 mins'))

Dados <- merge(DatasSub, Dados, all.x = TRUE)

# Dados hidroweb
Est <- read_delim(paste0("dadosHidroWeb/",Codigos[est],"/cotas_T_",Codigos[est],".zip"),
                  delim = ";", skip = 12)
Est <- NivelConsist(Est, 3)

Est2 <- Est %>%
  group_by(Dia_Ano) %>%
  dplyr::summarize(Mediana = median(Cota, na.rm = TRUE),
                   Max =  max(Cota, na.rm = TRUE),
                   Min =  min(Cota, na.rm = TRUE),
                   Q10 =  quantile(Cota, 0.9, na.rm = TRUE),
                   Q90 =  quantile(Cota, 0.1, na.rm = TRUE)) %>%
  filter(Dia_Ano != "366")

DiaHoje <- format(tail(Dados$DataHora[complete.cases(Dados$Nivel)], 1), "%j")
CotaHoje <- tail(Dados$Nivel[complete.cases(Dados$Nivel)], 1)
DeclividadeHoje <- CotaHoje - tail(Dados$Nivel[complete.cases(Dados$Nivel)], 10)[1]

EstHoje <- Est[Est$Dia_Ano == DiaHoje,]
EstHoje_10 <- Est[Est$Dia_Ano == str_pad(as.character(as.numeric(DiaHoje)-10), 3, pad = "0"),]
EstHoje$Declividade <- EstHoje$Cota - EstHoje_10$Cota

EstHoje$Dist <- rank(abs(EstHoje$Cota - CotaHoje))
EstHoje$Dist_Declividade <- rank(abs(EstHoje$Declividade - DeclividadeHoje))

#Media_Dist <- mean(EstHoje$Dist, na.rm = TRUE)
#SD_Dist <- sd(EstHoje$Dist_Declividade, na.rm = TRUE)

#Media_Dist_Declividade <- mean(EstHoje$Dist_Declividade, na.rm = TRUE)
#SD_Dist_Declividade <- sd(EstHoje$Dist_Declividade, na.rm = TRUE)
P <- 2
EstHoje$Dist_Total <- EstHoje$Dist * P + EstHoje$Dist_Declividade
EstHoje <- EstHoje %>% arrange(Dist_Total)

Anos <- head(EstHoje$Ano, k)

# Faz a previsão
ref <- referencia %>% filter(codigo == Codigos[est])

png(paste0("previsoes/",format(Sys.Date(),"%y%m%d"),"_previsao_",Codigos[est],".png"),
    height = 500*5.5, width = 500*11, res = 75 * 7, )

par(mar = c(4.5,4.5,2.5,1), mfrow = c(1,1))

plot(c(1:365) + as.Date("2019-01-01"),
     ylim = c(min(unlist(Est2[,c(2:6)])[is.finite(unlist(Est2[,c(2:6)]))]),
              max(unlist(Est2[,c(2:6)])[is.finite(unlist(Est2[,c(2:6)]))])),
     xlim = c(1,365) + as.Date("2019-01-01"),
     ylab = "", xlab = "", type = "n",
     yaxt = "n", xaxt = "n", bty="n")

AXIS1 <- seq.Date(as.Date("2019-01-01"),
                  as.Date("2020-01-01"), "month")

axis(side=1, AXIS1, format(AXIS1, "%b"), lwd=2)
axis(side=2, at = seq(-100,3000,100), lwd=2)

abline(v = seq(-30,390, 30) + as.Date("2019-01-01"), col = "grey", lty = 3)
abline(h = seq(0,3000, 100), col = "grey", lty = 3)

lines(c(1:365) + as.Date("2019-01-01"),
      Est2$Min[c(1:365)], col = rgb(1,0.5,0,0.5), lwd = 2)

lines(c(1:365) + as.Date("2019-12-31"),
      Est2$Min[c(1:365)], col = rgb(1,0.5,0,0.5), lwd = 2)

lines(c(1:365) + as.Date("2019-01-01"),
      Est2$Max[c(1:365)], col = rgb(1,0.5,0,0.5), lwd = 2)

lines(c(1:365) + as.Date("2019-12-31"),
      Est2$Max[c(1:365)], col = rgb(1,0.5,0,0.5), lwd = 2)

polygon(c(as.numeric(Est2$Dia_Ano)[1]:as.numeric(Est2$Dia_Ano)[365],
          as.numeric(Est2$Dia_Ano)[365]:as.numeric(Est2$Dia_Ano)[1])+
          as.Date("2019-01-01"),
        c(Est2$Q10, rev(Est2$Q90)),
        col = rgb(0.3,0.5,0.8,0.5),
        border = FALSE)

polygon(c(as.numeric(Est2$Dia_Ano)[1]:as.numeric(Est2$Dia_Ano)[365],
          as.numeric(Est2$Dia_Ano)[365]:as.numeric(Est2$Dia_Ano)[1])+
          as.Date("2019-12-31"),
        c(Est2$Q10, rev(Est2$Q90)),
        col = rgb(0.3,0.5,0.8,0.5),
        border = FALSE)

DadosDia <- Dados %>% group_by(Dia = as.Date(DataHora)) %>%
  dplyr::summarize(Media = mean(Nivel, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Dia) %>%
  mutate(DiaJ = format(Dia, "%j")) %>%
  filter(year(Dia) == year(Sys.Date()))

Dias <- data.frame("Dia" = seq.Date(min(DadosDia$Dia), max(DadosDia$Dia), by = "day"))
DadosDia <- merge(Dias, DadosDia, all.x = TRUE, by = "Dia") %>%
  filter(complete.cases(Media))

lines(as.numeric(as.character(DadosDia$DiaJ))+as.Date("2019-01-01"),
      as.numeric(DadosDia$Media), lwd = 3,
      col = rgb(0.2,0.2,0.8,0.9))

abline(h = ref$Inunda, col = "red", lty = 2)
abline(h = ref$Alerta, col = "orange", lty = 2)
abline(h = ref$Atencao, col = "yellow", lty = 2)

mtext("Dia do Ano", side = 1, line = 2.5)
mtext("Cota (cm)", side = 2, line = 2.5)

mtext(paste0("Níveis observados no ano de ",
             format(as.Date(DataInic,"%d/%m/%Y"),"%Y"),
             " e comparação com níveis históricos"),
      line = 1.5, side = 3, font = 2)

mtext(paste("Estação", Nomes[est]),
      line = 0.5, side = 3, font = 3)

legend(c(280) + as.Date("2019-01-01"),max(unlist(Est2[,c(2:6)])),
       cex = 0.8,
       legend = c("Zona de Normalidade",
                  paste0("Níveis ",format(as.Date(DataInic,"%d/%m/%Y"),"%Y")),
                  "Máximas e mínimas diárias",
                  "Previsões 7 a 28 dias", "IC 80% da previsão"),
       lty = c(1, 1, 1, NA, 1),
       pch = c(NA, 20, NA, 20, NA),
       col = c(rgb(0.3,0.5,0.8,0.5),
               rgb(0.2,0.2,0.8,0.9),
               rgb(1,0.5,0,0.5),
               2, rgb(0.5,0.5,0.5,0.5)),
       lwd = c(8,2,2,1,8), bty = "n")

DadosDia <- Dados %>% group_by(Dia = as.Date(DataHora)) %>%
  dplyr::summarize(Media = mean(Nivel, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Dia) %>%
  mutate(DiaJ = format(Dia, "%j"))
DadosDia <- merge(Dias, DadosDia, all.x = TRUE, by = "Dia")

i <- 8
DiaHoje <- as.numeric(DiaHoje)
Prev <- matrix(NA, nrow = H + 1, ncol = k)
for(i in 1:k){
  
  #Sub_Prev <- Est[Est$Ano %in% c(Anos[i], as.character(as.numeric(Anos[i])+1)),] %>%
  #  mutate(Dia_Ano = as.numeric(Dia_Ano) + as.Date("2019-01-01"))
  
  Sub_Prev <- Est[Est$Ano %in% c(Anos[i], as.character(as.numeric(Anos[i])+1)),]
  
  DatasSub <- data.frame(Data = seq(as.Date(paste0(Anos[i],"-01-01")),
                                    as.Date(paste0(as.character(as.numeric(Anos[i])+1),"-12-31")),
                                            by = "day"))
  
  Sub_Prev <- merge(DatasSub, Sub_Prev, all.x = TRUE) %>%
    mutate(Dia_Ano = seq_along(Cota) + as.Date("2018-12-31"))
  
  Sub_Prev <- Sub_Prev[DiaHoje:I(DiaHoje+H),] %>%
    arrange(Dia_Ano)
  
  Vies <- Sub_Prev$Cota[1] - CotaHoje
  Sub_Prev$Corrigido <- Sub_Prev$Cota - Vies
  
  #with(Sub_Prev,
  #     lines(Dia_Ano, Corrigido,
  #            col = rgb(0.5,0.5,0.5,0.8)))
  
  Prev[,i] <- Sub_Prev$Corrigido
}

P10 <- apply(Prev, 1, function(x) quantile(x, 0.1, na.rm = TRUE))
P90 <- apply(Prev, 1, function(x) quantile(x, 0.9, na.rm = TRUE))

polygon(c(Sub_Prev$Dia_Ano[complete.cases(P10)],
          rev(Sub_Prev$Dia_Ano[complete.cases(P90)])),
        c(P10[complete.cases(P10)],
          rev(P90)[complete.cases(P90)]), border = NA, col = rgb(0.5,0.5,0.5,0.5))

points(Sub_Prev$Dia_Ano[c(which(1:H %% 7 == 0)+1)],
       rowMeans(Prev, na.rm = TRUE)[c(which(1:H %% 7 == 0)+1)], col = 2, pch = 20, cex = 1)

dev.off()

Prev_Ladario <- rowMeans(Prev, na.rm = TRUE)[c(8,15,22,29)]

DadosDia[which.max(DadosDia$Media),]

Est %>% filter(Ano == "2021") %>%
  arrange(desc(Cota))

Est %>%
  group_by(Ano) %>%
  dplyr::summarize(Maxima = max(Cota, na.rm = TRUE)) %>%
  ungroup() %>%
  pull(Maxima) %>% mean()
  

Est %>%
  group_by(Ano) %>%
  dplyr::summarize(DiaMin = Dia_Ano[which.min(Cota)]) %>%
  ungroup() %>%
  pull(DiaMin) %>%
  as.numeric(as.character()) %>%
  mean(na.rm = TRUE)

qnorm(0.5, 241.2828, 144.6613) + as.Date("2021-12-31")
