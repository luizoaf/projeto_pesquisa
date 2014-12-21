dado_semestre_retorna_media_serie_retornos_por_setor = function(semestre){
  # semestre = 2008
  semestre_acoes = subset(dados,dados$datas==semestre)
  # nrow(semestre_acoes)
  serie_retornos_por_semestre = semestre_acoes[,2:ncol(semestre_acoes)]
  # head(serie_retornos_por_semestre)
  #   serie_retornos_por_semestre = cria_tabela_serie_retornos_de_todas_as_acoes(semestre_acoes)
  setores_media_acoes = data.frame(1)
  setores_100_porcento = setores_100_porcento_por_periodo(semestre)
  for( setor in setores_100_porcento){
    #   setor = setores_100_porcento[1]
    relacao_setores_acoes_menos_acoes = relacao_setores_acoes(dados,semestre)
    acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
    if(length(acoes_do_setor)!=1){ # nao ira calcular a media quando tiver apenas 1 acao
      medias_por_setor = apply(serie_retornos_por_semestre[,acoes_do_setor],MARGIN=1,FUN=mean)
    }else{
      medias_por_setor = serie_retornos_por_semestre[,acoes_do_setor]
    }
    setores_media_acoes = cbind(setores_media_acoes,medias_por_setor)
  }
  setores_media_acoes = setores_media_acoes[,-1]
  #   head(setores_media_acoes)
  ### mudanca de ordem ###
  # setores_media_acoes = cria_tabela_serie_retornos_de_todas_as_acoes(setores_media_acoes)
  
  colnames(setores_media_acoes) = setores_100_porcento_por_periodo(semestre)
  return(setores_media_acoes)
}
dados_com_bovespa = read.csv(file="papeis_da_ibovespa_2008_a_2014_2_com_IBOVESPA.csv")
dados_com_bovespa$datas  = as.Date(dados_com_bovespa$datas)

#anual
dados_com_bovespa$datas = as.numeric(format( dados_com_bovespa$datas,"%Y"))

#semestral

# meses = as.numeric(format( dados_com_bovespa$datas,"%m"))
# datas = meses

# datas[which(meses <= 6)] = paste(format( dados_com_bovespa$datas[which(meses <= 6)],"%Y"),".1",sep="")
# datas[which(meses > 6 )] = paste(format( dados_com_bovespa$datas[which(meses > 6 )] ,"%Y"),".2",sep="")

# dados_com_bovespa$datas = datas

faixa_temporal = unique(dados_com_bovespa$datas)
dados_bovespa_setores = data.frame()
for(i in 1:length(faixa_temporal)){
  dados_bovespa_setores = rbind(dados_bovespa_setores,dado_semestre_retorna_media_serie_retornos_por_setor(faixa_temporal[i]))
}
dados_bovespa_setores$BVSP = dados_com_bovespa$X.BVSP
dados_bovespa_setores$datas = dados_com_bovespa$datas

aggregate(dados_bovespa_setores,list(dados_bovespa_setores$datas),FUN=var)
aggregate(dados_bovespa_setores,list(dados_bovespa_setores$datas),FUN=cov)

((dados_bovespa_setores[2,1]/dados_bovespa_setores[1,1])-1)*100
# 1:3
acao = subset(dados_bovespa_setores[,1],dados_bovespa_setores$datas==2008)
ibovespa = dados_bovespa_setores[dados_bovespa_setores$datas==2008,"BVSP"]
variacoes_acao = c()
variacoes_ibovespa = c()
for(i in 1:(length(acao)-1)){
  variacoes_acao[i] = ((acao[i+1]/acao[i])-1)*100
  variacoes_ibovespa[i] = ((ibovespa[i+1]/ibovespa[i])-1)*100
}
# plot(variacoes_ibovespa)
variancia = var(variacoes_ibovespa)
covariancia = cov(variacoes_ibovespa,variacoes_acao)
beta = covariancia/variancia
