#### Leitura de dados ####

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
#Dados_List <- Dados_List %>%
#    rename(Codigo.z = Codigo,
#           ChuvaHoraria.z = ChuvaHoraria,
#           Nivel.z = Nivel,
#           Vazao.z = Vazao)
head(Dados_List)

# Interpola dados
Dados_List$Nivel.x[which(is.na(Dados_List$Nivel.x))] <- 
    approx(Dados_List$DataHora,
           Dados_List$Nivel.x,
           xout = Dados_List$DataHora)$y[which(is.na(Dados_List$Nivel.x))]

Dados_List$Nivel.y[which(is.na(Dados_List$Nivel.y))] <- 
    approx(Dados_List$DataHora,
           Dados_List$Nivel.y,
           xout = Dados_List$DataHora)$y[which(is.na(Dados_List$Nivel.y))]

Dados_List$Vazao.x[which(is.na(Dados_List$Vazao.x))] <- 
    approx(Dados_List$DataHora,
           Dados_List$Vazao.x,
           xout = Dados_List$DataHora)$y[which(is.na(Dados_List$Vazao.x))]

Dados_List$Vazao.y[which(is.na(Dados_List$Vazao.y))] <- 
    approx(Dados_List$DataHora,
           Dados_List$Vazao.y,
           xout = Dados_List$DataHora)$y[which(is.na(Dados_List$Vazao.y))]

#### Leitura de dados ####

ref <- referencia %>% filter(codigo == 15400000)

ultimo <- min(tail(Dados_List$DataHora[complete.cases(Dados_List$Nivel.x)],1),
              tail(Dados_List$DataHora[complete.cases(Dados_List$Nivel.y)],1))

Previsao <- Cota_PVH(Dados_List[which(Dados_List$DataHora == ultimo),"Nivel.x"],
                     Dados_List[which(Dados_List$DataHora == ultimo),"Nivel.y"])

limites <- range(c(Dados_List$Nivel.x,1700,
                   Previsao), na.rm = TRUE)

limites[1] <- 200
png(paste0("previsoes/",format(Sys.Date(),"%y%m%d"),"_previsao_",15400000,"_cota_3.png"),
    height = 500*5.5, width = 500*11, res = 75 * 7, )

par(mar = c(4.5,4.5,2,1))

plot(Dados_List$DataHora, Dados_List$Nivel.x, type = "l", bty = "n",xaxt="n",yaxt="n",
     xlab = "Data", ylab = "Cota (cm)",
     xlim = c(ultimo-Dias_eixo_x*24*3600, ultimo+80*3600), ylim = c(limites[1],limites[2]))

abline(v = c(1:365)*24*3600 + as.POSIXct("2021-11-30"), col = "grey", lty = 3)
abline(h = seq(0, 3000, 100), col = "grey", lty = 3)

lines(Dados_List$DataHora, Dados_List$Nivel.x, lwd = 2)
points(ultimo + seq(6,72,6)*3600, Previsao, pch = 20, cex = 2)

mtext("Previsão de níveis em Porto Velho", side = 3, font = 2, line = 0.5)

abline(h = ref$Inunda, col = "red", lty = 2)
abline(h = ref$Alerta, col = "orange", lty = 2)
abline(h = ref$Atencao, col = "yellow", lty = 2)

legend("bottomleft",
       legend = c("Dados observados", "Previsão de nível 30 horas",
                  "Atenção","Alerta","Inundação"),
       lty = c(1,NA,2,2,2),
       lwd = c(2,NA,1,1,1),
       pch = c(NA,20,NA,NA,NA),
       col = c(1,1,
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

