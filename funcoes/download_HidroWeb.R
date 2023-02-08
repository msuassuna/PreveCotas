Url <- paste0("http://www.snirh.gov.br/hidroweb/rest/api/documento/convencionais?tipo=2&documentos=",Codigos)

i <- 150
for(i in 1:nrow(Ests)){
  if(Codigos[i] == 47771000) next
  download(Url[i], paste0("dadosHidroWeb/",Codigos[i],".zip"), mode="wb") 
  unzip(paste0("dadosHidroWeb/",Codigos[i],".zip"), exdir = paste0("dadosHidroWeb/",Codigos[i]))
}
