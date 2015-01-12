source("../1_funcoes.R")
dados = read.csv(file="papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv")

dados = dados[,-2]
dados$datas  = as.Date(dados$datas)

#anual
dados$datas = as.numeric(format( dados$datas,"%Y"))


# serie_retornos_normalizado = dado_semestre_retorna_media_serie_retornos_por_setor(2010,dados)
# plot(main = "Ação ",serie_retornos_normalizado$"Consumo Cíclico / Tecid Vest Calç",type="l",ylab="valor da ação")

# serie_retornos_normalizado = dado_semestre_retorna_media_serie_retornos_por_setor(2014,dados)
# plot(main = "Ação ",serie_retornos_normalizado$ "Const e Transp / Constr e Engenh",type="l",ylab="valor da ação")

serie_retornos_normalizado = dado_semestre_retorna_media_serie_retornos_por_setor(2008,dados)
plot(main = "Ação ",serie_retornos_normalizado$ "Cons N Cíclico / Bebidas",type="l",ylab="valor da ação")

serie_retornos_normalizado = dado_semestre_retorna_media_serie_retornos_por_setor(2009,dados)
plot(main = "Ação ",serie_retornos_normalizado$ "Cons N Cíclico / Bebidas",type="l",ylab="valor da ação")







# 
# acoes_const_e_transp = c("CYRE3.SA","EVEN3.SA","GFSA3.SA","MRVE3.SA","PDGR3.SA","RSID3.SA")
# const_e_transp =  dados[,c(acoes_const_e_transp)]
# const_e_transp = cbind(dados$datas,data.frame(valor_acao = apply(const_e_transp,MARGIN=1,FUN=mean)))
# dados = const_e_transp
# cria_tabela_serie_retornos_de_todas_as_acoes(const_e_transp)
# plot(main = "Setor Const e Transp / Constr e Engenh",const_e_transp$valor_acao,type="l",ylab="valor da ação")
# 
# # outlier = dados[dados$datas==2010,c("datas","HGTX3.SA")]
# outlier = dados[,c("datas","HGTX3.SA")]
# plot(main = "Ação ",outlier$HGTX3.SA,type="l",ylab="valor da ação")
# 
# outlier = dados[,c("datas","ABEV3.SA")]
# plot(main = "Ação ",outlier$ABEV3.SA,type="l",ylab="valor da ação")
# 
# 
# outlier = dados[,c("datas","CRUZ3.SA")]
# plot(main = "Ação ",outlier$CRUZ3.SA,type="l",ylab="valor da ação")
# 
# outlier = dados[,c("datas","NATU3.SA")]
# plot(main = "Ação ",outlier$NATU3.SA,type="l",ylab="valor da ação")
# 
# outlier = dados[,c("datas","SBSP3.SA")]
# plot(main = "Ação ",outlier$SBSP3.SA,type="l",ylab="valor da ação")










names(dados)
dados2014 = subset(dados,dados$datas==2014)
outlier2 = apply(dados2014[,acoes],MARGIN=1,FUN=mean)
plot(main = "Const e Transp / Constr e Engenh, ano 2014",outlier2,type="l",ylab="valor da ação")
