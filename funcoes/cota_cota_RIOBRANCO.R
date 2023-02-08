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


# Dados hidrotelemetria
Dados2 <- tryCatch(read_delim(paste0("dados/",Codigos[est2],".txt"),
                             delim = " ", col_types = "cTnnn"),
                  error = function(e){
                    message("Algum problema aconteceu na leitura.\n",e)
                  },
                  warning = function(w){
                    message("Algum problema ocorreu na leitura.\nPossivelmente os dados sao indisponiveis.\n",w)
                  })

Dados_List <- merge(Dados, Dados2, "DataHora", all.x = TRUE) %>%
  filter(complete.cases(DataHora))

prev_RBR_30 <- array(NA, nrow(Dados_List))
prev_RBR_24 <- array(NA, nrow(Dados_List))
prev_RBR_12 <- array(NA, nrow(Dados_List))
prev_RBR_6 <- array(NA, nrow(Dados_List))

Dados_List$Nivel.x[which(is.na(Dados_List$Nivel.x))] <- 
  approx(Dados_List$DataHora,
         Dados_List$Nivel.x,
         xout = Dados_List$DataHora)$y[which(is.na(Dados_List$Nivel.x))]
# plot(Dados_List$DataHora, Dados_List$Nivel.x)

# Dados_List %>%
#  filter(complete.cases(Nivel.x)) %>%
#  tail()

Dados_List$Nivel.y[which(is.na(Dados_List$Nivel.y))] <- 
  approx(Dados_List$DataHora,
         Dados_List$Nivel.y,
         xout = Dados_List$DataHora)$y[which(is.na(Dados_List$Nivel.y))]


for(i in seq_along(Dados_List$DataHora)){
  if(i > 30){
    prev_RBR_30[i] <- Cota_RBR_30H(Dados_List$Nivel.x[i],
                                   Dados_List$Nivel.x[i-5],
                                   Dados_List$Nivel.x[i-10],
                                   Dados_List$Nivel.x[i-15],
                                   Dados_List$Nivel.x[i-20],
                                   Dados_List$Nivel.y[i],
                                   Dados_List$Nivel.y[i-5],
                                   Dados_List$Nivel.y[i-10],
                                   Dados_List$Nivel.y[i-15],
                                   Dados_List$Nivel.y[i-20],
                                   Dados_List$Nivel.y[i-25],
                                   Dados_List$Nivel.y[i-30])
  }
  
  if(i > 24){
    prev_RBR_24[i] <- Cota_RBR_24H(Dados_List$Nivel.x[i],
                                   Dados_List$Nivel.x[i-4],
                                   Dados_List$Nivel.x[i-12],
                                   Dados_List$Nivel.x[i-16],
                                   Dados_List$Nivel.x[i-20],
                                   Dados_List$Nivel.y[i],
                                   Dados_List$Nivel.y[i-4],
                                   Dados_List$Nivel.y[i-8],
                                   Dados_List$Nivel.y[i-12],
                                   Dados_List$Nivel.y[i-16],
                                   Dados_List$Nivel.y[i-20],
                                   Dados_List$Nivel.y[i-24])
  }
  
  if(i > 12){
    prev_RBR_12[i] <- Cota_RBR_12H(Dados_List$Nivel.x[i],
                                   Dados_List$Nivel.x[i-2],
                                   Dados_List$Nivel.x[i-4],
                                   Dados_List$Nivel.x[i-6],
                                   Dados_List$Nivel.x[i-8],
                                   Dados_List$Nivel.x[i-10],
                                   Dados_List$Nivel.x[i-12],
                                   Dados_List$Nivel.y[i],
                                   Dados_List$Nivel.y[i-2],
                                   Dados_List$Nivel.y[i-4],
                                   Dados_List$Nivel.y[i-6],
                                   Dados_List$Nivel.y[i-8],
                                   Dados_List$Nivel.y[i-10],
                                   Dados_List$Nivel.y[i-12])
  }
  
  if(i > 12){
    prev_RBR_6[i] <- Cota_RBR_6H(Dados_List$Nivel.x[i],
                                   Dados_List$Nivel.x[i-1],
                                   Dados_List$Nivel.x[i-2],
                                   Dados_List$Nivel.x[i-3],
                                   Dados_List$Nivel.x[i-4],
                                   Dados_List$Nivel.x[i-5],
                                   Dados_List$Nivel.x[i-6],
                                   Dados_List$Nivel.y[i],
                                   Dados_List$Nivel.y[i-1],
                                   Dados_List$Nivel.y[i-2],
                                   Dados_List$Nivel.y[i-3])
  }
  
}

ultimo <- min(tail(Dados_List$DataHora[complete.cases(Dados_List$Nivel.x)],1),
              tail(Dados_List$DataHora[complete.cases(Dados_List$Nivel.y)],1))


limites <- range(c(Dados_List$Nivel.x,1400,
                   tail(prev_RBR_30[complete.cases(prev_RBR_30)],1),
                   tail(prev_RBR_24[complete.cases(prev_RBR_24)],1),
                   tail(prev_RBR_12[complete.cases(prev_RBR_12)],1),
                   tail(prev_RBR_6[complete.cases(prev_RBR_6)],1)), na.rm = TRUE)

limites[1] <- ifelse(limites[1] < 0, 100, limites[1])
limites[2] <- ifelse(limites[2] > 2000, 2000, limites[2])

png(paste0("previsoes/",format(Sys.Date(),"%y%m%d"),"_previsao_",13600002,".png"),
    height = 500*5.5, width = 500*11, res = 75 * 7, )

par(mar = c(4.5,4.5,2,1))

plot(Dados_List$DataHora, Dados_List$Nivel.x, type = "l", bty = "n",xaxt="n",yaxt="n",
     xlab = "Data", ylab = "Cota (cm)",
     xlim = c(ultimo-Dias_eixo_x*24*3600, ultimo+36*3600),
     ylim = c(limites[1]-80,limites[2]+80))

abline(v = c(1:730)*24*3600 + as.POSIXct("2020-12-31"), col = "grey", lty = 3)
abline(h = seq(0, 3000, 100), col = "grey", lty = 3)

lines(Dados_List$DataHora, Dados_List$Nivel.x, lwd = 2)
points(ultimo+30*3600, tail(prev_RBR_30[complete.cases(prev_RBR_30)],1), pch = 20, cex = 2)
points(ultimo+24*3600, tail(prev_RBR_24[complete.cases(prev_RBR_24)],1), pch = 20, cex = 2)
points(ultimo+12*3600, tail(prev_RBR_12[complete.cases(prev_RBR_12)],1), pch = 20, cex = 2)
points(ultimo+6*3600, tail(prev_RBR_6[complete.cases(prev_RBR_6)],1), pch = 20, cex = 2)

abline(h = 1400, col = "red", lty = 2)
abline(h = 1350, col = "orange", lty = 2)
abline(h = 1250, col = "yellow", lty = 2)

legend(ultimo-Dias_eixo_x*24*3600,
       limites[2]+80,
       legend = c("Dados observados", "Previsões de nível 6, 12, 24 e 30 horas",
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

mtext("Previsão de níveis em Rio Branco", side = 3, font = 2, line = 0.5)

dev.off()


#plot(Dados_List$DataHora, Dados_List$Nivel.x, type = "l", bty = "n",
#     xlim = c(ultimo-Dias_eixo_x*24*3600, ultimo+36*3600), ylim = c(100,1600))
#points(ultimo+30*3600, tail(prev_RBR_30[complete.cases(prev_RBR_30)],1), col = 2, pch = 20, cex = 3)
#points(ultimo+24*3600, tail(prev_RBR_24[complete.cases(prev_RBR_24)],1), col = 3, pch = 20, cex = 3)
#points(ultimo+12*3600, tail(prev_RBR_12[complete.cases(prev_RBR_12)],1), col = 4, pch = 20, cex = 3)
#points(ultimo+6*3600, tail(prev_RBR_6[complete.cases(prev_RBR_6)],1), col = 5, pch = 20, cex = 3)

#lines(Dados_List$DataHora+30*3600, prev_RBR_30, col = 2, pch = 20)
#lines(Dados_List$DataHora+24*3600, prev_RBR_24, col = 3, pch = 20)
#lines(Dados_List$DataHora+12*3600, prev_RBR_12, col = 4, pch = 20)
#lines(Dados_List$DataHora+6*3600, prev_RBR_6, col = 5, pch = 20)

head(Dados_List)
