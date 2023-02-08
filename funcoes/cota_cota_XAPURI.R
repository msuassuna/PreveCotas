# Dados hidrotelemetria
Dados <- tryCatch(read_delim(paste0("dados/",Codigos[est],".txt"),
                             delim = " ", col_types = "cTnnn"),
                  error = function(e){
                    message("Algum problema aconteceu na leitura.\n",e)
                  },
                  warning = function(w){
                    message("Algum problema ocorreu na leitura.\nPossivelmente os dados sao indisponiveis.\n",w)
                  })


# Dados hidrotelemetria
Dados2 <- tryCatch(read_delim(paste0("dados/",Codigos[est2],".txt"),
                              delim = " ", col_types = "cTnnn"),
                   error = function(e){
                     message("Algum problema aconteceu na leitura.\n",e)
                   },
                   warning = function(w){
                     message("Algum problema ocorreu na leitura.\nPossivelmente os dados sao indisponiveis.\n",w)
                   })



Dados_List <- merge(Dados, Dados2, "DataHora")
head(Dados_List)

Dados_List$Nivel.x[which(is.na(Dados_List$Nivel.x))] <- 
  approx(Dados_List$DataHora,
         Dados_List$Nivel.x,
         xout = Dados_List$DataHora)$y[which(is.na(Dados_List$Nivel.x))]

Dados_List$Nivel.y[which(is.na(Dados_List$Nivel.y))] <- 
  approx(Dados_List$DataHora,
         Dados_List$Nivel.y,
         xout = Dados_List$DataHora)$y[which(is.na(Dados_List$Nivel.y))]

prev_XPU_18 <- array(NA, nrow(Dados_List))
prev_XPU_12 <- array(NA, nrow(Dados_List))
prev_XPU_6 <- array(NA, nrow(Dados_List))

for(i in seq_along(Dados_List$DataHora)){
  if(i > 18){
    prev_XPU_18[i] <- Cota_XPU_18H(Dados_List$Nivel.x[i],
                                   Dados_List$Nivel.x[i-3],
                                   Dados_List$Nivel.x[i-6],
                                   Dados_List$Nivel.x[i-9],
                                   Dados_List$Nivel.x[i-12],
                                   Dados_List$Nivel.x[i-15],
                                   Dados_List$Nivel.y[i],
                                   Dados_List$Nivel.y[i-3],
                                   Dados_List$Nivel.y[i-6],
                                   Dados_List$Nivel.y[i-9],
                                   Dados_List$Nivel.y[i-18])
  }
  
  if(i > 12){
    prev_XPU_12[i] <- Cota_XPU_12H(Dados_List$Nivel.x[i],
                                   Dados_List$Nivel.x[i-2],
                                   Dados_List$Nivel.x[i-4],
                                   Dados_List$Nivel.x[i-6],
                                   Dados_List$Nivel.x[i-8],
                                   Dados_List$Nivel.x[i-12],
                                   Dados_List$Nivel.y[i],
                                   Dados_List$Nivel.y[i-2],
                                   Dados_List$Nivel.y[i-4],
                                   Dados_List$Nivel.y[i-6],
                                   Dados_List$Nivel.y[i-8],
                                   Dados_List$Nivel.y[i-10])
  }
  
  if(i > 6){
    prev_XPU_6[i] <- Cota_XPU_6H(Dados_List$Nivel.x[i],
                                 Dados_List$Nivel.x[i-1],
                                 Dados_List$Nivel.x[i-2],
                                 Dados_List$Nivel.x[i-4],
                                 Dados_List$Nivel.x[i-5],
                                 Dados_List$Nivel.y[i],
                                 Dados_List$Nivel.y[i-1])
  }
  
}

ultimo <- min(tail(Dados_List$DataHora[complete.cases(Dados_List$Nivel.y)],1),
              tail(Dados_List$DataHora[complete.cases(Dados_List$Nivel.x)],1))

limites <- range(c(Dados_List$Nivel.x,1340,
                   tail(prev_XPU_12[complete.cases(prev_XPU_12)],1),
                   tail(prev_XPU_18[complete.cases(prev_XPU_18)],1),
                   tail(prev_XPU_6[complete.cases(prev_XPU_6)],1)), na.rm = TRUE)


png(paste0("previsoes/",format(Sys.Date(),"%y%m%d"),"_previsao_",13550000,".png"),
    height = 500*5.5, width = 500*11, res = 75 * 7, )

par(mar = c(4.5,4.5,2,1))

plot(Dados_List$DataHora, Dados_List$Nivel.x, type = "l", bty = "n",xaxt="n",yaxt="n",
     xlab = "Data", ylab = "Cota (cm)",
     xlim = c(ultimo-Dias_eixo_x*24*3600, ultimo+36*3600),
     ylim = c(limites[1],limites[2]))

abline(v = c(1:365)*24*3600 + as.POSIXct("2021-11-30"), col = "grey", lty = 3)
abline(h = seq(0, 3000, 100), col = "grey", lty = 3)


#polygon(c(1:730,730:1)*24*3600+
#          as.POSIXct("2020-12-31"),
#        c(Est2$Q10,Est2$Q10, rev(Est2$Q90),rev(Est2$Q90)),
#        col = rgb(0.9,0.9,0.9),
#        border = FALSE)

#lines(c(1:730)*24*3600 + as.POSIXct("2020-12-31"),
#      c(Est2$Min[c(1:365)],Est2$Min[c(1:365)]),
#      col = rgb(0.2,0.2,0.8,0.9))

#lines(c(1:730)*24*3600 + as.POSIXct("2020-12-31"),
#      c(Est2$Max[c(1:365)],Est2$Max[c(1:365)]), col = rgb(0.2,0.2,0.8,0.9))

lines(Dados_List$DataHora, Dados_List$Nivel.x, lwd = 2)
points(ultimo+18*3600, tail(prev_XPU_18[complete.cases(prev_XPU_18)],1), pch = 20, cex = 2)
points(ultimo+12*3600, tail(prev_XPU_12[complete.cases(prev_XPU_12)],1), pch = 20, cex = 2)
points(ultimo+6*3600, tail(prev_XPU_6[complete.cases(prev_XPU_6)],1), pch = 20, cex = 2)

mtext("Previsão de níveis em Xapuri", side = 3, font = 2, line = 0.5)

abline(h = 1340, col = "red", lty = 2)
abline(h = 1250, col = "orange", lty = 2)
abline(h = 1150, col = "yellow", lty = 2)

legend(ultimo-Dias_eixo_x*24*3600,
       limites[2],
       legend = c("Dados observados", "Previsões de nível 6, 12 e 18 horas",
                  "Faixa de normalidade", "Máximos e mínimos históricos",
                  "Atenção","Alerta","Inundação"),
       lty = c(1,NA,1,1,2,2,2),
       lwd = c(2,NA,10,1,1,1,1),
       pch = c(NA,20,NA,NA,NA,NA,NA),
       col = c(1,1,rgb(0.9,0.9,0.9),
               rgb(0.2,0.2,0.8,0.9),
               "yellow","orange","red"), box.col = rgb(0.1,0.1,0.1,0.1),
       cex = 0.8)

axis(1,
     seq.POSIXt(ultimo-Dias_eixo_x*24*3600, ultimo+36*3600, by = "day"),
     as.Date(seq.POSIXt(ultimo-Dias_eixo_x*24*3600, ultimo+36*3600, by = "day")),
     col.axis=1)

axis(2,
     seq(0,2000,100),
     seq(0,2000,100),
     col.axis=1)

dev.off()


plot(Dados_List$DataHora, Dados_List$Nivel.x, type = "l", bty = "n",
     xlim = c(ultimo-Dias_eixo_x*24*3600, ultimo+36*3600))
points(ultimo+18*3600, tail(prev_XPU_18[complete.cases(prev_XPU_18)],1), col = 2, pch = 20, cex = 3)
points(ultimo+12*3600, tail(prev_XPU_12[complete.cases(prev_XPU_12)],1), col = 3, pch = 20, cex = 3)
points(ultimo+6*3600, tail(prev_XPU_6[complete.cases(prev_XPU_6)],1), col = 4, pch = 20, cex = 3)

lines(Dados_List$DataHora+18*3600, prev_XPU_18, col = 2)
lines(Dados_List$DataHora+12*3600, prev_XPU_12, col = 3)
lines(Dados_List$DataHora+6*3600, prev_XPU_6, col = 4)


