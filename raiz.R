########################################################################
########################################################################
##
## Código para gerar as previsões cota-cota e gerar as figuras e tabelas
## Autor: Marcus Suassuna Santos
## Instituição: CPRM - Serviço Geológico do Brasil
## Data da última revisão: 15/12/2022
## Inclui:
##     - Bloco 1:
##     - Bloco 2:
##     - Bloco 3:
##     - Bloco 4:
##
########################################################################
########################################################################

# Linhas de código preparatórias: define pasta de trabalho, limpa memória
# e guarda hora de início e local de trabalho nas em variáveis
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
Old_Time <- Sys.time()
OldDir <- getwd()

# Carrega todos os pacotes que serão utilizados
library(downloader)
library(tidyr)
library(readr)
library(dplyr)
library(XML)
library(plotly)
library(stringr)
library(lubridate)
library(RCurl)
library(png)
library(ggplot2);cat('\014')

############
############
####
#### Nesses scripts são carregadas funções que serão utilizadas ao longo do script
#### As funções são apenas carregadas, ainda não roda nada
####
############
############

# Função acessória para buscar dados do Hidrotelemetria
source("funcoes/url_add.R")

# Funções dos modelos de previsão
source("funcoes/modelos.R")
Modelos <- Modelos[!(Modelos %in% c(44290003,44500000,45298000))]

# Curvas-chave
source("funcoes/curvasChave.R");cat('\014')

# Função para ler arquivos no formato HidroWeb
source("funcoes/seleciona_dados.R")
source("funcoes/seleciona_dados_Q.R")

########################################################################
########################################################################
##
## Agora começa a ler os dados que serão usados
##
########################################################################
########################################################################

# Lê arquivos com as informações das estações que serão usadas no script
Ests <- read_csv2("inputs/Est_SACE.csv", locale = readr::locale(encoding = "latin1")) %>%
  filter(!duplicated(Codigo)) %>%
  filter(Codigo %in% Modelos) %>%
  arrange(Codigo);cat('\014')
Codigos <- Ests$Codigo
Nomes <- Ests$Nome

# Níveis de referência
referencia <- read_csv2("inputs/referencia.csv", locale = readr::locale(encoding = "latin1"))

# Baixa dados do HidroWeb
source("funcoes/download_Hidroweb.R")

# Baixa dados do HidroTelemetria
DataInic <- Sys.Date() - 21

# Estações que têm PCD, mas que vamos usar dados de cota online
Manual <- c(66825000, 67100000)
eval(parse("funcoes/le_telemetria.R", encoding = "UTF-8"));cat('\014')

# Incluir aqui nessa rotina a leitura de um .csv pra
# corrigir dados
source("funcoes/correcoes.R")

# Gera tabelas sumário para os boletins
source("funcoes/tabela_sumario.R")
Tab_Boletim

########################################################################
########################################################################
##
## A partir daqui, as previsões são feitas.
## Modelos Cota-cota
##
########################################################################
########################################################################

unlink(paste0("previsoes/", dir("previsoes")))
# Para os gráficos da previsão, o domínio do eixo x pode ser alterado

#####
#
# Roda Previsão Ladário
#
#####
est <- which(Codigos == 66825000)
H <- 28
k <- 20
eval(parse("funcoes/cota_LADARIO.R", encoding = "UTF-8"))
Anos
round(Prev_Ladario)

#####
#
# Roda Previsão Porto Murtinho
#
#####
#est <- which(Codigos == 67100000)
#H <- 28
#k <- 20
#eval(parse("funcoes/cota_PORTOMURTINHO.R", encoding = "UTF-8"))
#Anos
#round(Prev_PMurtinho)

#####
#
# Roda Previsão Forte Coimbra
#
#####
#est <- which(Codigos == 66970000)
#H <- 28
#k <- 20
#eval(parse("funcoes/cota_FORTECOIMBRA.R", encoding = "UTF-8"))
#Anos
#round(Prev_FCoimbra)

#####
#
# Roda Previsão Bela Vista do Norte
#
#####
#est <- which(Codigos == 66125000)
#H <- 14
#k <- 20
#eval(parse("funcoes/cota_BVNORTE.R", encoding = "UTF-8"))
#Anos
#round(Prev_BVNorte)

#####
#
# Roda Previsão Cáceres
#
#####
est <- which(Codigos == 66070004)
H <- 14
k <- 20
eval(parse("funcoes/cota_CACERES.R", encoding = "UTF-8"))
Anos
round(Prev_Caceres)

Previsoes <- rbind(Prev_Caceres,
                   #Prev_BVNorte,
                   #Prev_FCoimbra
                   #Prev_PMurtinho
                   Prev_Ladario) %>%
  round() %>%
  as.data.frame()
names(Previsoes) <- c("Dia + 7","Dia + 14","Dia + 21","Dia + 28")
write.csv2(Previsoes, "outputs/Previsoes_Pantanal.csv", na = "-")

#####
#
# Roda Cota-Cota CARACARAÍ
#
#####
est <- which(Codigos == 14710000)
est2 <- which(Codigos == 14620000)
Dias_eixo_x <- 15
eval(parse("funcoes/cota_cota_CARACARAI.R", encoding = "UTF-8"))
tail(prev_CRC_36[complete.cases(prev_CRC_36)],1)

#####
#
# Roda Cota-Cota BOA VISTA
#
#####
est <- which(Codigos == 14620000)
est2 <- which(Codigos == 14527000)
Dias_eixo_x <- 10
eval(parse("funcoes/cota_cota_BOAVISTA.R", encoding = "UTF-8"))
tail(prev_BOAVISTA_17[complete.cases(prev_BOAVISTA_17)],1)

#####
#
# Roda Cota-Cota RIO BRANCO
#
#####
est <- which(Codigos == 13600002)
est2 <- which(Codigos == 13550000)
Dias_eixo_x <- 10
eval(parse("funcoes/cota_cota_RIOBRANCO.R", encoding = "UTF-8"))

#####
#
# Roda Cota-Cota XAPURI
#
#####
est <- which(Codigos == 13550000)
est2 <- which(Codigos == 13470000)
Dias_eixo_x <- 10
# Roda Cota-Cota Xapuri
eval(parse("funcoes/cota_cota_XAPURI.R", encoding = "UTF-8"))

#####
#
# Roda PORTO VELHO
#
#####

# Vazão-vazão em Porto Velho usando JJB e MorNova
#est <- 14
#est2 <- 15
#est3 <- 109
#Dias_eixo_x <- 10
#eval(parse("funcoes_v2/vazao_vazao_PVLH.R", encoding = "UTF-8"))
#x <- curvaChave_QN_PV(tail(prev_30[complete.cases(prev_30)],1))
#tendencia <- "estabilidade"
#hora <- round_date(Sys.time()+30*3600, "hour")
#paste("Nível com tendência de",tendencia,"podendo atingir a cota de",round(x),"cm até às", format(hora,"%H"), "horas do dia", format(hora,"%d/%m"))

# Vazão-vazão em Porto Velho usando JJB
#est <- 14
#est2 <- 15
#est3 <- 109
#Dias_eixo_x <- 10
# Roda Vazao-Vazao Porto Velho
#eval(parse("funcoes_v2/vazao_vazao_PVLH_2.R", encoding = "UTF-8"))
#x <- curvaChave_QN_PV(tail(prev_30[complete.cases(prev_30)],1))
#tendencia <- "estabilidade"
#hora <- round_date(Sys.time()+30*3600, "hour")
#paste("Nível com tendência de",tendencia,"podendo atingir a cota de",round(x),"cm até às", format(hora,"%H"), "horas do dia", format(hora,"%d/%m"))

# Roda Cota-cota Porto Velho
est <- which(Codigos == 15400000)
est2 <- which(Codigos == 15318000)
Dias_eixo_x <- 10
eval(parse("funcoes/cota_cota_PORTOVELHO.R", encoding = "UTF-8"))

x <- tail(Previsao,1)
tendencia <- "estabilidade"
hora <- round_date(Sys.time()+72*3600, "hour")
paste("Nível com tendência de",tendencia,"podendo atingir a cota de",round(x),"cm até às", format(hora,"%H"), "horas do dia", format(hora,"%d/%m"))

#####
#
# Roda SÃO FRANCISCO
#
#####
#est <- 12
#est2 <- 13
#Dias_eixo_x <- 15
#eval(parse("funcoes_v2/vazao_vazao_SAF.R", encoding = "UTF-8"))
#x <- curvaChave_QN_SAOF(tail(prev_16[complete.cases(prev_16)],1))
#tendencia <- "descenso"
#hora <- floor_date(Sys.time()+16*3600, "hour")
#paste("Nível com tendência de",tendencia,"podendo atingir a cota de",round(x),
#      "cm até às", format(hora,"%H"), "horas do dia", format(hora,"%d/%Y"))

#####
#
# Roda CACHOEIRA DA MANTEIGA
#
#####
#Dias_eixo_x <- 15
#est <- 10
#est2 <- 11
#eval(parse("funcoes_v2/vazao_vazao_CDM.R", encoding = "UTF-8"))
#curvaChave_QN_CDM(tail(prev_34[complete.cases(prev_34)],1))

#####
#
# Roda PIRAPORA
#
#####
#Dias_eixo_x <- 15
#est <- which(Codigos==41135000)
#est2 <- which(Codigos==41020002)
#est3 <- which(Codigos==41090002)
#eval(parse("funcoes_v2/vazao_vazao_PIR_1.R", encoding = "UTF-8"))
#curvaChave_QN_PIR(tail(prev_20[complete.cases(prev_20)],1))


#Dias_eixo_x <- 15
#est <- which(Codigos==41135000)
#est2 <- which(Codigos==41020002)
#est3 <- which(Codigos==41090002)
#eval(parse("funcoes_v2/vazao_vazao_PIR_2.R", encoding = "UTF-8"))
#curvaChave_QN_PIR(tail(prev_20[complete.cases(prev_20)],1))

#####
#
# Roda SÃO ROMÃO
#
#####
#Dias_eixo_x <- 15
#est <- which(Codigos==42210000)
#est2 <- which(Codigos==43200000)
#eval(parse("funcoes_v2/vazao_vazao_SRM.R", encoding = "UTF-8"))
#curvaChave_QN_SAR(tail(prev_9[complete.cases(prev_9)],1))

#####
#
# Roda MANGA
#
#####
#Dias_eixo_x <- 15
#est <- which(Codigos==44500000)
#est2 <- which(Codigos==44290003)
#eval(parse("funcoes_v2/vazao_vazao_MAN.R", encoding = "UTF-8"))
#curvaChave_QN_MAN(tail(prev_26_2[complete.cases(prev_26_2)],1))

#####
#
# Roda BOM JESUS DA LAPA
#
#####
#Dias_eixo_x <- 15
#est <- which(Codigos==45480000)
#est2 <- which(Codigos==45298000)
#eval(parse("funcoes_v2/cota_cota_BJL.R", encoding = "UTF-8"))
#tail(prev_24[complete.cases(prev_24)],24)


#####
#
# Roda CARINHANHA
#
#####
#Dias_eixo_x <- 15
#est <- which(Codigos==45298000)
#est2 <- which(Codigos==44500000)
#eval(parse("funcoes_v2/vazao_vazao_CAR.R", encoding = "UTF-8"))
#curvaChave_QN_CAR(tail(prev_24[complete.cases(prev_24)],1))

#####
#
# Roda MORPARÁ
#
#####
#Dias_eixo_x <- 15
#eval(parse("funcoes_v2/vazao_vazao_MPRA.R", encoding = "UTF-8"))
#curvaChave_QN_MPR(tail(prev_72[complete.cases(prev_72)],1))

Sys.time() - Old_Time
