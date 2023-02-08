# Função que organiza os dados do HidroWeb:
# Nos casos 1 e 2 a função filtra dados brutos ou consistidos, respectivamente
# Caso contrário ela prioriza os dados consistidos mas mantém os brutos caso
# os consistidos não estejam disponíveis.


NivelConsist_Q <- function(Est, NC = "Pref"){
  if(NC == 1){
    
    Est <- Est %>%
      filter(MediaDiaria == "1" & NivelConsistencia == "1") %>%
      select(Data,NivelConsistencia,grep("[0123456789]$", names(Est), value = TRUE)) %>%
      mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
      gather(key = Dia, value = Vazao, -Data, - NivelConsistencia) %>%
      mutate(Dia = as.numeric(substr(Dia,6,7)),
             Data = Data + Dia - 1,
             Mes = format(Data, "%m"),
             Ano = format(Data, "%Y")) %>%
      arrange(Data) %>%
      mutate(Dia_Ano = format(Data, "%j"),
             Vazao = as.numeric(as.character(Vazao)))
    
  } else if(NC == 2) {
    
    Est <- Est %>%
      filter(MediaDiaria == "1" & NivelConsistencia == "2") %>%
      select(Data,NivelConsistencia,grep("[0123456789]$", names(Est), value = TRUE)) %>%
      mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
      gather(key = Dia, value = Vazao, -Data, - NivelConsistencia) %>%
      mutate(Dia = as.numeric(substr(Dia,6,7)),
             Data = Data + Dia - 1,
             Mes = format(Data, "%m"),
             Ano = format(Data, "%Y")) %>%
      arrange(Data) %>%
      mutate(Dia_Ano = format(Data, "%j"),
             Vazao = as.numeric(as.character(Vazao)))
    
  } else {
    
    Est <- Est %>%
      filter(MediaDiaria == "1") %>%
      select(Data,NivelConsistencia,grep("[0123456789]$", names(Est), value = TRUE)) %>%
      mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
      gather(key = Dia, value = Vazao, -Data, - NivelConsistencia) %>%
      mutate(Dia = as.numeric(substr(Dia,6,7)),
             Data = Data + Dia - 1,
             Mes = format(Data, "%m"),
             Ano = format(Data, "%Y")) %>%
      arrange(desc(NivelConsistencia)) %>%
      filter(!duplicated(Data)) %>%
      arrange(Data) %>%
      mutate(Dia_Ano = format(Data, "%j"),
             Vazao = as.numeric(as.character(Vazao)))
    
  }
  Est <- Est[,c(1,2,3,5,6,4,7)]
}
