Ultima_Hora_Bsb <- array(NA, length(Codigos))
Ultima_Hora_Rondonia <- array(NA, length(Codigos))
Ultima_Hora_Acre <- array(NA, length(Codigos))
Ultimo_Nivel <- array(NA, length(Codigos))
Var_Nivel_24 <- array(NA, length(Codigos))
Var_Nivel_7d <- array(NA, length(Codigos))
Var_Nivel_14d <- array(NA, length(Codigos))
Acum_Chuva_24 <- array(NA, length(Codigos))
Acum_Chuva_96 <- array(NA, length(Codigos))
Cota_Inund <- array(NA, length(Codigos))

i <- 19
for(i in 1:length(Codigos)){
  
  print(paste("Lendo dados da estacao",Codigos[i]))
  
  Dados <- tryCatch(read_delim(paste0("dados/",Codigos[i],".txt"),
                               delim = " ", col_types = "ccnnn"),
                    error = function(e){
                      message("Algum problema aconteceu na leitura.\n",e)
                      return(NULL)
                      },
                    warning = function(w){
                      message("Algum problema ocorreu na leitura.\nPossivelmente os dados sao indisponiveis.\n",w)
                      return(NULL)
                      })
  
  Dados$Codigo <- Codigos[i]
  
  if(nrow(Dados) == 0){Dados <- NULL}
  
  if(nrow(Dados %>% filter(complete.cases(Nivel))) != 0){
    
    Dados <- Dados %>%
      arrange(Nivel) %>%
      mutate(DataHora = as.POSIXct(DataHora)) %>%
      filter(!duplicated(DataHora)) %>%
      arrange(DataHora)
    
    Ultima_Hora_Bsb[i] <- Dados %>% filter(complete.cases(Nivel)) %>% arrange(DataHora) %>% tail(1) %>% pull(DataHora)
    Ultima_Hora_Rondonia[i] <- Dados %>% filter(complete.cases(Nivel)) %>% arrange(DataHora) %>% tail(1) %>% pull(DataHora) - 1*60*60
    Ultima_Hora_Acre[i] <- Dados %>% filter(complete.cases(Nivel)) %>% arrange(DataHora) %>% tail(1) %>% pull(DataHora) - 2*60*60
    
    Var24 <- Dados %>%
      filter(complete.cases(Nivel)) %>%
      arrange(DataHora) %>%
      filter(DataHora %in% c(max(DataHora, na.rm = TRUE),
                             max(DataHora, na.rm = TRUE) - 24* 3600))
    
    Ultimo_Nivel[i] <- Var24 %>% tail(1) %>% pull(Nivel)
    
    if(nrow(Var24) < 2){
      Var_Nivel_24[i] <- NA
    } else {
      Var_Nivel_24[i] <- Var24$Nivel[2] - Var24$Nivel[1]
    }
    
    Var7 <- Dados %>%
      filter(complete.cases(Nivel)) %>%
      arrange(DataHora) %>%
      filter(DataHora %in% c(max(DataHora, na.rm = TRUE),
                             max(DataHora, na.rm = TRUE) - 24 * 7 * 3600))
    
    if(nrow(Var7) < 2){
      Var_Nivel_7d[i] <- NA
    } else {
      Var_Nivel_7d[i] <- Var7$Nivel[2] - Var7$Nivel[1]
    }
    
    Var14 <- Dados %>%
      filter(complete.cases(Nivel)) %>%
      arrange(DataHora) %>%
      filter(DataHora %in% c(max(DataHora, na.rm = TRUE),
                             max(DataHora, na.rm = TRUE) - 24 * 14 * 3600))
    
    if(nrow(Var14) < 2){
      Var_Nivel_14d[i] <- NA
    } else {
      Var_Nivel_14d[i] <- Var14$Nivel[2] - Var14$Nivel[1]
    }
    
    
  } else {
    Ultimo_Nivel[i] <- NA
    Var_Nivel_24[i] <- NA
    Var_Nivel_7d[i] <- NA
    Var_Nivel_14d[i] <- NA
  }
  
  Chuva24 <- Dados %>%
    arrange(DataHora) %>%
    filter(DataHora >= max(DataHora, na.rm = TRUE) - 24*60*60) %>%
    dplyr::summarize(Chuva24 = sum(ChuvaHoraria, na.rm = TRUE))
  
  Acum_Chuva_24[i] <- as.numeric(Chuva24)
  
  Chuva96 <- Dados %>%
    arrange(DataHora) %>%
    filter(DataHora >= max(DataHora, na.rm = TRUE) - 96*60*60) %>%
    dplyr::summarize(Chuva24 = sum(ChuvaHoraria, na.rm = TRUE))
                           
  Acum_Chuva_96[i] <- as.numeric(Chuva96)
  
}


Tab_Boletim <- data.frame(
  
  "Estacao" = Codigos,
  "Nome" = Nomes,
  "Horario_Brasilia" = as.POSIXct(Ultima_Hora_Bsb, tz = Sys.timezone(),
                                  origin = "1970-01-01 00:00.00 UTC"),
  "Horario_Rondonia" = as.POSIXct(Ultima_Hora_Rondonia, tz = Sys.timezone(),
                                  origin = "1970-01-01 00:00.00 UTC"),
  "Horario_Acre" = as.POSIXct(Ultima_Hora_Acre, tz = Sys.timezone(),
                              origin = "1970-01-01 00:00.00 UTC"),
  "Nivel_Atual" = Ultimo_Nivel,
  "Var_Nivel_24" = Var_Nivel_24,
  "Var_Nivel_7d" = Var_Nivel_7d,
  "Var_Nivel_14d" = Var_Nivel_14d,
  "Acum_Chuva_24" = Acum_Chuva_24,
  "Acum_Chuva_96" = Acum_Chuva_96
)
