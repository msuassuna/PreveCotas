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


Dados_List <- merge(Dados, Dados2, "DataHora", all.x = TRUE) %>%
  filter(complete.cases(DataHora))

prev_BOAVISTA_17 <- array(NA, nrow(Dados_List))

Dados_List$Nivel.x[which(is.na(Dados_List$Nivel.x))] <- 
  approx(Dados_List$DataHora,
         Dados_List$Nivel.x,
         xout = Dados_List$DataHora)$y[which(is.na(Dados_List$Nivel.x))]
#plot(Dados_List$DataHora, Dados_List$Nivel.x)

for(i in seq_along(Dados_List$DataHora)){
  if(i > 17){
    prev_BOAVISTA_17[i] <- Prev_BOAVISTA_17_2(Dados_List$Nivel.x[i],
                                          Dados_List$Nivel.y[i])
  }
}

ultimo <- min(tail(Dados_List$DataHora[complete.cases(Dados_List$Nivel.x)],1),
              tail(Dados_List$DataHora[complete.cases(Dados_List$Nivel.y)],1))


limites <- range(c(Dados_List$Nivel.x, 850,
                   tail(prev_BOAVISTA_17[complete.cases(prev_BOAVISTA_17)],1)),
                   na.rm = TRUE)


png(paste0("previsoes/",format(Sys.Date(),"%y%m%d"),"_previsao_",14620000,".png"),
    height = 500*5.5, width = 500*11, res = 75 * 7, )

par(mar = c(4.5,4.5,2,1))

plot(Dados_List$DataHora, Dados_List$Nivel.x, type = "l", bty = "n",xaxt="n",yaxt="n",
     xlab = "Data", ylab = "Cota (cm)",
     xlim = c(ultimo-Dias_eixo_x*24*3600, ultimo+36*3600),
     ylim = c(limites[1]-80,limites[2]+80))

abline(v = c(1:730)*24*3600 + as.POSIXct("2020-12-31"), col = "grey", lty = 3)
abline(h = seq(0, 3000, 100), col = "grey", lty = 3)

lines(Dados_List$DataHora, Dados_List$Nivel.x, lwd = 2)
points(ultimo+17*3600, tail(prev_BOAVISTA_17[complete.cases(prev_BOAVISTA_17)],1), pch = 20, cex = 2)

abline(h = 850, col = "red", lty = 2)
abline(h = 800, col = "orange", lty = 2)
abline(h = 750, col = "yellow", lty = 2)

legend(ultimo-Dias_eixo_x*24*3600,
       limites[2]+80,
       legend = c("Dados observados", "Previsão 17 horas",
                                    "Atenção","Alerta","Inundação"),
       lty = c(1,NA,2,2,2),
       lwd = c(2,NA,1,1,1),
       pch = c(NA,20,NA,NA,NA),
       col = c(1,1,"yellow","orange","red"), box.col = rgb(0.1,0.1,0.1,0.1),
       cex = 0.8)

axis(1,
     seq.POSIXt(ultimo-Dias_eixo_x*24*3600, ultimo+36*3600, by = "day"),
     as.Date(seq.POSIXt(ultimo-Dias_eixo_x*24*3600, ultimo+36*3600, by = "day")),
     col.axis=1)

axis(2,
     seq(0,2000,100),
     seq(0,2000,100),
     col.axis=1)

mtext("Previsão de níveis em Boa Vista", side = 3, font = 2, line = 0.5)

dev.off()


plot(Dados_List$DataHora, Dados_List$Nivel.x, type = "l", bty = "n",
     xlim = c(ultimo-Dias_eixo_x*24*3600, ultimo+17*3600), ylim = c(300,1000))
points(ultimo+17*3600, tail(prev_BOAVISTA_17[complete.cases(prev_BOAVISTA_17)],1), col = 2, pch = 20, cex = 3)

lines(Dados_List$DataHora+17*3600, prev_BOAVISTA_17, col = 2, pch = 20)

