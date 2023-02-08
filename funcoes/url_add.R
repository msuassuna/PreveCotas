url_add <- function(Estacao,Inicio,Fim){
  paste0(
    "http://telemetriaws1.ana.gov.br/ServiceANA.asmx/DadosHidrometeorologicosGerais?CodEstacao=",
    Estacao,
    "&DataInicio=",Inicio,"&DataFim=",Fim)
}