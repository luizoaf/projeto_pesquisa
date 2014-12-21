source("../1_funcoes.R")
dados_com_bovespa = read.csv(file="papeis_da_ibovespa_2008_a_2014_2_com_IBOVESPA.csv")
dados_com_bovespa$datas  = as.Date(dados_com_bovespa$datas)

#anual
dados_com_bovespa$datas = as.numeric(format( dados_com_bovespa$datas,"%Y"))

# semestral

# meses = as.numeric(format( dados_com_bovespa$datas,"%m"))
# datas = meses

# datas[which(meses <= 6)] = paste(format( dados_com_bovespa$datas[which(meses <= 6)],"%Y"),".1",sep="")
# datas[which(meses > 6 )] = paste(format( dados_com_bovespa$datas[which(meses > 6 )] ,"%Y"),".2",sep="")

# dados_com_bovespa$datas = datas
# 
faixa_temporal = unique(dados_com_bovespa$datas)
dados_bovespa_setores = data.frame()
for(i in 1:length(faixa_temporal)){
  dados_bovespa_setores = rbind(dados_bovespa_setores,setor_por_periodo(faixa_temporal[i],dados_com_bovespa[,-2])) # sem o ibovespa
}
dados_bovespa_setores$BVSP = dados_com_bovespa$X.BVSP
dados_bovespa_setores$datas = dados_com_bovespa$datas


risco_beta = data.frame()
for(tempo in unique(dados_bovespa_setores$datas)){
  for(indice_setor in 1:(ncol(dados_bovespa_setores)-2)){
    risco_beta = rbind(risco_beta,calcula_risco_beta(indice_setor,tempo,dados_bovespa_setores))
  }
}

risco_beta
