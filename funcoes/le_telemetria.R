
i <- 1
for(i in 1:length(Codigos)){
  
  if(file.exists(paste0("dados/",Codigos[i],".txt"))){
    if(file.size(paste0("dados/",Codigos[i],".txt")) <= 4) next
    
    DadosAnteriores <- read.table(paste0("dados/",Codigos[i],".txt"),
                                  header = TRUE)
    
    Sub_DataInic <- max(as.Date(DataInic, "%d/%m/%Y"),
                        max(as.Date(DadosAnteriores$DataHora)))
    
  } else {
    Sub_DataInic <- as.Date(DataInic, "%d/%m/%Y")
  }
  
  if(file.exists(paste0("dados/",Codigos[i],".txt"))){
    if(file.size(paste0("dados/",Codigos[i],".txt")) <= 4) next
  } 
  
  print(paste("Baixando dados da estação", Nomes[i], "-", Codigos[i]))
  
  Url <- url_add(Codigos[i],
                 format(Sub_DataInic, "%d/%m/%Y"),
                 format(Sys.Date(), "%d/%m/%Y"))
  
  doc <- xmlTreeParse(Url, useInternal = TRUE)
  rootNode <- xmlRoot(doc)
  
  if(substr(Codigos[i],1,1) == 4 | substr(Codigos[i],1,1) == 6){
    
    if(Codigos[i] %in% Manual){
      
      Dados <- data.frame("Codigo" = xpathSApply(rootNode[[2]][[1]],"//CodEstacao",xmlValue),
                          "DataHora" = xpathSApply(rootNode[[2]][[1]],"//DataHora",xmlValue),
                          "ChuvaHoraria" = xpathSApply(rootNode[[2]][[1]],"//ChuvaFinal",xmlValue),
                          "Nivel" = xpathSApply(rootNode[[2]][[1]],"//NivelManual",xmlValue),
                          "Vazao" = xpathSApply(rootNode[[2]][[1]],"//VazaoFinal",xmlValue))
      
    } else {
    
      Dados <- data.frame("Codigo" = xpathSApply(rootNode[[2]][[1]],"//CodEstacao",xmlValue),
                          "DataHora" = xpathSApply(rootNode[[2]][[1]],"//DataHora",xmlValue),
                          "ChuvaHoraria" = xpathSApply(rootNode[[2]][[1]],"//ChuvaFinal",xmlValue),
                          "Nivel" = xpathSApply(rootNode[[2]][[1]],"//NivelFinal",xmlValue),
                          "Vazao" = xpathSApply(rootNode[[2]][[1]],"//VazaoFinal",xmlValue))
        
    }
    
  } else {
    Dados <- data.frame("Codigo" = xpathSApply(rootNode[[2]][[1]],"//CodEstacao",xmlValue),
                        "DataHora" = xpathSApply(rootNode[[2]][[1]],"//DataHora",xmlValue),
                        "ChuvaHoraria" = xpathSApply(rootNode[[2]][[1]],"//ChuvaFinal",xmlValue),
                        "Nivel" = xpathSApply(rootNode[[2]][[1]],"//NivelSensor",xmlValue),
                        "Vazao" = xpathSApply(rootNode[[2]][[1]],"//VazaoFinal",xmlValue))
  }
  
  
  if(file.exists(paste0("dados/",Codigos[i],".txt"))){
    Dados <- rbind.data.frame(Dados,
                              DadosAnteriores %>%
                                filter(as.Date(DataHora) < max(as.Date(DadosAnteriores$DataHora))))
    
  }
  
  write.table(Dados, paste0("dados/",Codigos[i],".txt"),
              row.names = FALSE, sep = " ", dec = ".")
  
}
