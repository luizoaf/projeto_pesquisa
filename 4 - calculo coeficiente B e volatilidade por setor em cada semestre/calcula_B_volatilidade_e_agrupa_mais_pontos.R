setwd("C:/Users/V1d4 L0k4/Desktop/Projetos SourceTree/4 - calculo coeficiente B e volatilidade por setor em cada semestre")
source("../1_funcoes.R")

dados = read.csv(file="papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv")
names(dados)


#Datas precisam estar da mais recente para a mais antiga
dados = inverte_indices_data_frame(dados)

#eliminação da data
dados = dados[,-1]

series_temporais_setores = calcula_series_temporais_dos_setores(dados)

##################### BETA #######################
beta_series_temporais_setores = cbind(data.frame(BVSP = dados$X.BVSP),series_temporais_setores)

#precisa fazer o janelamento

##################################################
# eliminação da ibovespa
dados = dados[,-1]


# serie_temporal_normalizada = c()
# serie_temporal = c()
# coluna = 1
# for(i in 1:(nrow(series_temporais_setores)-1)) {
#   serie_temporal_normalizada[i] = serie_temporal   
# }
# par(mfrow=c(3,3))
# plot(main = names(series_temporais_setores)[1],series_temporais_setores[,1],ylab="valor")
# plot(main = names(series_temporais_setores)[2],series_temporais_setores[,2],ylab="valor")
# plot(main = names(series_temporais_setores)[3],series_temporais_setores[,3],ylab="valor")
# plot(main = names(series_temporais_setores)[4],series_temporais_setores[,4],ylab="valor")
# plot(main = names(series_temporais_setores)[5],series_temporais_setores[,5],ylab="valor")
# plot(main = names(series_temporais_setores)[6],series_temporais_setores[,6],ylab="valor")
# plot(main = names(series_temporais_setores)[7],series_temporais_setores[,7],ylab="valor")

# head(series_temporais_setores)

series_temporais_setores = cria_serie_retornos(series_temporais_setores)

# 
# par(mfrow=c(3,3))
# plot(main = names(series_temporais_setores)[1],series_temporais_setores[,1],ylab="valor",type="l")
# plot(main = names(series_temporais_setores)[2],series_temporais_setores[,2],ylab="valor",type="l")
# plot(main = names(series_temporais_setores)[3],series_temporais_setores[,3],ylab="valor",type="l")
# plot(main = names(series_temporais_setores)[4],series_temporais_setores[,4],ylab="valor",type="l")
# plot(main = names(series_temporais_setores)[5],series_temporais_setores[,5],ylab="valor",type="l")
# plot(main = names(series_temporais_setores)[6],series_temporais_setores[,6],ylab="valor",type="l")
# plot(main = names(series_temporais_setores)[7],series_temporais_setores[,7],ylab="valor",type="l")


# serie_retornos_normalizado_por_setores = dado_semestre_retorna_media_serie_retornos_por_setor_sem_periodo(dados)

# write.table(x = series_temporais_setores,file="series_temporais_setores.csv",row.names=F)


dias_mes = 20
dias_ano = 240
total_dias = nrow(series_temporais_setores)
inicio_janelamento = seq(from=1,to=total_dias,by=dias_mes)
fim_janelamento = seq(from=1,to=total_dias,by=dias_mes)+dias_ano
# inicio_janelamento[fim_janelamento<total_dias]
# fim_janelamento[fim_janelamento<total_dias]

janelamentos_indices = data.frame(inicio = inicio_janelamento,fim = fim_janelamento)
janelamentos_indices = janelamentos_indices[janelamentos_indices$fim<=total_dias,]

# write.table(x = janelamentos_indices,file="janelamentos_indices.csv",row.names=F)

eixo_x_y = data.frame()
# setor = 1
j=1
grupo =1
colunas = c()
# i=1
for(i in 1:nrow(janelamentos_indices)){
  #   serie_retornos_normalizado = dado_semestre_retorna_media_serie_retornos_por_setor(periodo,dados)
  series_temporais = series_temporais_setores[janelamentos_indices$inicio[i]:janelamentos_indices$fim[i],]
  beta_series_temporais = beta_series_temporais_setores[janelamentos_indices$inicio[i]:janelamentos_indices$fim[i],]
  #   dado_semestre_retorna_media_serie_retornos_por_setor_sem_periodo
  
  #   plot(serie,type="l")
  for(coluna in 1:length(names(series_temporais))){
    #              coluna = 1
    colunas[j] = names(series_temporais)[coluna]
    serie = series_temporais[,coluna]
    ############ VaR = Value At Risk #############
    
    #probabilidade de perda_anormal% de perdas anormais
    #     z_1 = qnorm(.01)
    #     z_5= qnorm(.05)
    #     z_10 = qnorm(.1)
    #     var_1 = mean(serie) + z_1 * sd(serie)
    #     var_5 = mean(serie) + z_5 * sd(serie)
    #     var_10 = mean(serie) + z_10 * sd(serie)
    
    beta = calcula_risco_beta_sem_periodo(indice_setor = coluna + 1,beta_series_temporais)
    
    #### b e beta ####
    #     ibovespa = beta_series_temporais[,"BVSP"]
    #     variacoes_ibovespa = c()
    #     for(i in 1:(length(ibovespa)-1)){
    #       #       variacoes_acao[i] = ((acao[i+1]/acao[i])-1)*100
    #       variacoes_ibovespa[i] = ((ibovespa[i+1]/ibovespa[i])-1)*100
    #     }
    #     # plot(variacoes_ibovespa)
    variancia = var(beta_series_temporais$BVSP)
    media = mean(beta_series_temporais$BVSP)
    #     covariancia = cov(variacoes_ibovespa,variacoes_acao)
    #     beta = covariancia/variancia
    
    ####################
    
    #     media = mean(beta_series_temporais[,"BVSP"])
    #     variancia = var(beta_series_temporais[,"BVSP"])
    #         var_1 = mean(serie) - sqrt(var(serie))*qnorm(.99)
    #         var_5 = mean(serie) - sqrt(var(serie))*qnorm(.95)
    #         var_10 = mean(serie) - sqrt(var(serie))*qnorm(.9)
    ###########################################
    #     z_99 = qnorm(.99)
    #     z_95= qnorm(.95)
    #     z_90 = qnorm(.9)
    #     
    #     var_1 = mean(serie) - z_99 * sqrt(calcula_volatilidade(serie))
    #     var_5 = mean(serie) - z_95 * sqrt(calcula_volatilidade(serie))
    #     var_10 = mean(serie) - z_90 * sqrt(calcula_volatilidade(serie))
    ##################################3
    
    #   var_1 = as.numeric(VaR(serie,p=.99,method = "gaussian"))
    #    var_5 =  as.numeric(VaR(serie,p=.95,method="gaussian"))
    #    var_10 =  as.numeric(VaR(serie,p=.9,method ="gaussian"))
    ##############################################
    
    
    #     png(filename=paste(j,".png",sep=""),bg="transparent")
    #     par(mfrow=c(2,1))
    
    source("previsao_exponencial.R")
    #     eixo_x_y = rbind(eixo_x_y, cbind(eixo_x_frequencias,alvo,previsao, sse,a,coeficiente_B,volatilidade,i))
    #     colnames(eixo_x_y) = c("eixo_x_frequencias","alvo","previsao","sse","a","coeficiente_B","volatilidade","i")
    #     
    ###### OK #####
    #     eixo_x_y = rbind(eixo_x_y, cbind(sse,a,coeficiente_B,volatilidade,grupo,var_1,var_5,var_10))
    #     colnames(eixo_x_y) = c("sse","a","coeficiente_B","volatilidade","grupo","var_1","var_5","var_10")
    eixo_x_y = rbind(eixo_x_y, cbind(sse,a,coeficiente_B,volatilidade,grupo,beta,media,variancia))
    colnames(eixo_x_y) = c("sse","a","coeficiente_B","volatilidade","grupo","beta","media","variancia")
    ###############
    ###### ADAPTACAO ######
    #     eixo_x_y = rbind(eixo_x_y, cbind(eixo_x_frequencias,alvo,previsao,sse,a,coeficiente_B,volatilidade,i))
    #     colnames(eixo_x_y) = c("eixo_x_frequencias","alvo","previsao","sse","a","coeficiente_B","volatilidade","i")
    ########################
    
    #     a = 1
    #     source("define_coeficiente_B_sem_a.R")
    #     eixo_x_y_sem_a = rbind(eixo_x_y_sem_a, cbind(eixo_x_frequencias,alvo,previsao, sse,a,coeficiente_B,volatilidade,i))
    #     #,periodo,setor,coeficiente_B*volatilidade))
    #     colnames(eixo_x_y_sem_a) =  c("eixo_x_frequencias","alvo","previsao","sse","a","coeficiente_B","volatilidade","i")
    #     dev.off()
    j= j+1
    # write.table(eixo_x_y,paste(names(series_temporais)[i],grupo_janelamento,"combinacao_janelamento_30_incrementos_0_01_0_28_todas_acoes.csv"),row.names=F,sep=",")
  }
  grupo = grupo + 1
}

eixo_x_y$setor = colunas[1:nrow(eixo_x_y)]
eixo_x_y$b_volatilidade = eixo_x_y$coeficiente_B*eixo_x_y$volatilidade

setor_Beta_b = data.frame(setor=c(),beta =c())
for(setor in unique(eixo_x_y$setor)){
  analise = eixo_x_y[eixo_x_y$setor==as.character(setor),]
  # 
  # relacao_b_beta = cov(eixo_x_y$coeficiente_B,eixo_x_y$media)/eixo_x_y$variancia
  # boxplot(relacao_b_beta)
  
  # plot(analise$media)
  # plot(eixo_x_y$variancia)
  
  coeficiente_B = analise$coeficiente_B
  variacoes_coeficiente_B = c()
  for(i in 1:(length(coeficiente_B)-1)){
    #       variacoes_acao[i] = ((acao[i+1]/acao[i])-1)*100
    variacoes_coeficiente_B[i] = ((coeficiente_B[i+1]/coeficiente_B[i])-1)*100
  }
  
  ibovespa = analise$media
  variacoes_ibovespa = c()
  for(i in 1:(length(ibovespa)-1)){
    #       variacoes_acao[i] = ((acao[i+1]/acao[i])-1)*100
    variacoes_ibovespa[i] = ((ibovespa[i+1]/ibovespa[i])-1)*100
  }
  # plot(variacoes_ibovespa)
  variancia = var(variacoes_ibovespa)
  # plot(variacoes_coeficiente_B)
  # variancia = var(variacoes_coeficiente_B)
  # media = mean(variacoes_coeficiente_B)
  
  saida = cov(variacoes_ibovespa,variacoes_coeficiente_B)/(variancia)
  df = data.frame(setor=setor,beta =saida)
  setor_Beta_b = rbind(setor_Beta_b,df)
}



cor(saida,])
boxplot(saida)

plot(variacoes_coeficiente_B,type="l")
lines(eixo_x_y$media,col=2)
# cor(data.frame(eixo_x_y$a,eixo_x_y$coeficiente_B,eixo_x_y$volatilidade,eixo_x_y$var_1,eixo_x_y$var_5,eixo_x_y$var_10))

# correlacao = data.frame(beta = eixo_x_y$beta,b = eixo_x_y$coeficiente_B,grupo = eixo_x_y$grupo)
# 
# teste = function(entrada){
#   #   return(cor.test(entrada[,1],(entrada[,2]))$estimate)
#   #   return(cor.test(entrada[,1],(entrada[,2]),method="kendall")$estimate)
#   return(cor.test(entrada[,1],(entrada[,2]),method="spearman")$estimate)
#   #   return(cov(entrada[,1],entrada[,2]))#$estimate
# }
# saida = data.frame()
# i=1
# for(i in 1:length(unique(correlacao$grupo))){
#   valor_correlacao = teste(subset(correlacao,correlacao$grupo==i))
#   #   print(paste("Grupo: ",i," Correlacao: ",valor_correlacao))
#   saida = rbind(saida,data.frame(grupo = i,valor_correlacao = valor_correlacao))
# }
# hist(saida$valor_correlacao)
# boxplot(saida$valor_correlacao)


# resultado = data.frame(eixo_x_y$a,eixo_x_y$coeficiente_B,eixo_x_y$volatilidade,eixo_x_y$var_1,eixo_x_y$var_5,eixo_x_y$var_10)
# cor.test(resultado$eixo_x_y.coeficiente_B,resultado$eixo_x_y.var_5,method = "spearman")
# plot(resultado$eixo_x_y.coeficiente_B,resultado$eixo_x_y.var_5)



# write.table(eixo_x_y,row.names=F,file="VaR.csv")


# plot(eixo_x_y$coeficiente_B,type="l",ylim=c(-2,3))
# lines(eixo_x_y$var_1,col="red")
# lines(eixo_x_y$var_5,col="blue")
# lines(eixo_x_y$var_10,col="green")
# 
# nrow(eixo_x_y)
# nrow(na.omit(eixo_x_y))












dados$datas  = as.Date(dados$datas)

# outlier = dados[dados$datas==2010,c("datas","HGTX3.SA")]
# outlier = dados[,c("datas","HGTX3.SA")]
# plot(main = "Ação HGTX3.SA, ano 2010",outlier$HGTX3.SA,type="l",ylab="valor da ação")
# plot(main = "Ação HGTX3.SA, ano 2010",outlier$HGTX3.SA~dados$data,type="l",ylab="valor da ação")

# names(dados)
# acoes = c("CYRE3.SA","EVEN3.SA","GFSA3.SA","MRVE3.SA","PDGR3.SA","RSID3.SA")
# dados2014 = subset(dados,dados$datas==2014)
# outlier2 = apply(dados2014[,acoes],MARGIN=1,FUN=mean)
# plot(main = "Const e Transp / Constr e Engenh, ano 2014",outlier2,type="l",ylab="valor da ação")

# meses = as.numeric(format( dados$datas,"%m"))
# datas = meses

#mensal
# dados$datas = paste(format(dados$datas ,"%Y"),".",meses,sep="")

#trimestre
# datas[which(meses <= 3)] = paste(format( dados$datas[which(meses <= 3)],"%Y"),".1",sep="")
# datas[which(meses >3 & meses <=6 )] = paste(format( dados$datas[which(meses >3 & meses <=6 )] ,"%Y"),".2",sep="")
# datas[which(meses >6 & meses <=9 )] = paste(format( dados$datas[which(meses >6 & meses <=9 )],"%Y"),".3",sep="")
# datas[which(meses >9 & meses <=12 )] = paste(format( dados$datas[which(meses >9 & meses <=12 )],"%Y"),".4",sep="")

#semestre
# datas[which(meses <= 6)] = paste(format( dados$datas[which(meses <= 6)],"%Y"),".1",sep="")
# datas[which(meses > 6 )] = paste(format( dados$datas[which(meses > 6 )] ,"%Y"),".2",sep="")

#anual
dados$datas = as.numeric(format( dados$datas,"%Y"))


faixa_temporal = unique(dados$datas)

eixo_x_y = data.frame()
eixo_x_y_sem_a = data.frame()
i=1
colunas = c()
faixa_temporal_por_setores = c()
# periodo = faixa_temporal[1]
# coluna = 1


# só comentar para ficar sem serie de retorno setores_media_acoes = cria_tabela_serie_retornos_de_todas_as_acoes(setores_media_acoes)
setEPS()
postscript("imagens/com_serie_retorno_serie_temporal_2014_Cons_N_Básico_Alimentos_Processados.eps")
periodo = 2014
serie_retornos_normalizado = dado_semestre_retorna_media_serie_retornos_por_setor(periodo,dados)
plot(main="Sector Consumption Not Cyclical / Processed Food in 2014",
     serie_retornos_normalizado$"Cons N Básico / Alimentos Processados", ylab="Price (R$)",xlab="Days",
     type="l",las=1)

dev.off()
# # periodo = 2008
# for(periodo in faixa_temporal){
#   serie_retornos_normalizado = dado_semestre_retorna_media_serie_retornos_por_setor(periodo,dados)
#   for(coluna in 1:length(names(serie_retornos_normalizado))){
#     colunas[i] = names(serie_retornos_normalizado)[coluna]
#     faixa_temporal_por_setores[i] = periodo
#     serie = serie_retornos_normalizado[,coluna]
#     
#     #             png(filename=paste(i,".png",sep=""),bg="transparent")
#     #     par(mfrow=c(2,1))
#     
#     source("previsao_exponencial.R")
#     #     eixo_x_y = rbind(eixo_x_y, cbind(eixo_x_frequencias,alvo,previsao, sse,a,coeficiente_B,volatilidade,i))
#     #     colnames(eixo_x_y) = c("eixo_x_frequencias","alvo","previsao","sse","a","coeficiente_B","volatilidade","i")
#     #     
#     ###### OK #####
#     eixo_x_y = rbind(eixo_x_y, cbind(sse,a,coeficiente_B,volatilidade,i))
#     colnames(eixo_x_y) = c("sse","a","coeficiente_B","volatilidade","i")
#     ###############
#     ###### ADAPTACAO ######
#     #     eixo_x_y = rbind(eixo_x_y, cbind(eixo_x_frequencias,alvo,previsao,sse,a,coeficiente_B,volatilidade,i))
#     #     colnames(eixo_x_y) = c("eixo_x_frequencias","alvo","previsao","sse","a","coeficiente_B","volatilidade","i")
#     ########################
#     
#     #     a = 1
#     #     source("define_coeficiente_B_sem_a.R")
#     #     eixo_x_y_sem_a = rbind(eixo_x_y_sem_a, cbind(eixo_x_frequencias,alvo,previsao, sse,a,coeficiente_B,volatilidade,i))
#     #     #,periodo,setor,coeficiente_B*volatilidade))
#     #     colnames(eixo_x_y_sem_a) =  c("eixo_x_frequencias","alvo","previsao","sse","a","coeficiente_B","volatilidade","i")
#     #             dev.off()
#     i= i+1
#     # write.table(eixo_x_y,paste(names(serie_retornos_normalizado)[i],grupo_janelamento,"combinacao_janelamento_30_incrementos_0_01_0_28_todas_acoes.csv"),row.names=F,sep=",")
#   }
# }
# eixo_x_y$tempo = faixa_temporal_por_setores
# eixo_x_y$setor = colunas
# eixo_x_y$b_volatilidade = eixo_x_y$coeficiente_B*eixo_x_y$volatilidade
# #remocao dos outliers
# eixo_x_y_sem_outliers = eixo_x_y[eixo_x_y$coeficiente_B < 1.5,]
# eixo_x_y = eixo_x_y_sem_outliers
# 
# # plot(eixo_x_y$b_volatilidade,ylim=c(0,2))
# unique(eixo_x_y$i[eixo_x_y_sem_a$sse < eixo_x_y$sse])
# boxplot(main="SSE",data.frame(com_a = eixo_x_y$sse,sem_a = eixo_x_y_sem_a$sse))
# boxplot(main="SSE com outliers",eixo_x_y$sse)
# boxplot(main="SSE sem outliers",eixo_x_y_sem_outliers$sse)

# boxplot(main="MAPE",eixo_x_y$mape)
# 
# eixo_x_y_pior_caso = eixo_x_y[order(eixo_x_y$sse,decreasing=T),]
eixo_x_y_pior_caso = eixo_x_y[order(eixo_x_y$sse,decreasing=T),]
head(eixo_x_y_pior_caso)
# # eixo_x_y_pior_caso = eixo_x_y_pior_caso[eixo_x_y_pior_caso$,]
# eixo_x_y = eixo_x_y_pior_caso
# head(eixo_x_y_pior_caso,n=200)$i
# # 23 54
# unique(head(eixo_x_y$i,n=250))
# unique(head(eixo_x_y_sem_a$i,n=250))
# pior_1 = subset(eixo_x_y,eixo_x_y$i==2)[,1:2]
# pior_2 = subset(eixo_x_y,eixo_x_y$i==35)[,1:2]
# pior_3 = subset(eixo_x_y,eixo_x_y$i==84)[,1:2]
# pior_4 = subset(eixo_x_y,eixo_x_y$i==88)[,1:2]
# pior_5 = subset(eixo_x_y,eixo_x_y$i==3)[,1:2]
# 
# pior_2 = subset(eixo_x_y,eixo_x_y$i==6)
# serie=pior_2$alvo
# pior_3 = subset(eixo_x_y,eixo_x_y$i==28)
# pior_4 = subset(eixo_x_y,eixo_x_y$i==3)
# pior_5 = subset(eixo_x_y,eixo_x_y$i==15)
# 
# # pior_1$coeficiente_B
# unique(eixo_x_y$setor)
formato_ponto = 21
######################## enviar  pra paulo ########################
# LOG LOG
# "Cons N Básico / Alimentos Processados" # colocar em inglÊs
setEPS()
postscript("imagens/melhor_exponencial_log_log.eps")
# plot(rnorm(100), main="Hey Some Data")
pior_1 = subset(eixo_x_y,eixo_x_y$i==571 )
atual = pior_1[,"alvo"]
previsao = pior_1[,"previsao"]
# main = paste("Outlier\nSSE: ",unique(pior_1$sse),"\nB: ",unique(pior_1$coeficiente_B)," volat.: ",unique(pior_1$volatilidade))
# "Consumo não Cíclico/Alimentos Processados"
plot( log(pior_1[,c(1,2)]),xlab="Returns",ylab="Sector Consumption Not Cyclical / Processed Food",pch=formato_ponto,las=1)
# ,pch=20
lines(log(pior_1[,c(1,3)]),col="gray",lwd=2)
dev.off()

###################################################################
#SEMILOG
# LOG no Y, normal no X
#"Cons N Cíclico / Pr Pessoal Limp"  # colocar em inglÊs
setEPS()
postscript("imagens/segunda_melhor_exponencial_semi_log.eps")
# unique(eixo_x_y$setor)
pior_1 = subset(eixo_x_y,eixo_x_y$i==29 )
atual = pior_1[,"alvo"]
previsao = pior_1[,"previsao"]
# main = paste("Outlier\nSSE: ",unique(pior_1$sse),"\nB: ",unique(pior_1$coeficiente_B)," volat.: ",unique(pior_1$volatilidade))
# Consumo não Cíclico/Produtos de uso Pessoal e Limpeza
plot( pior_1[,"eixo_x_frequencias"],log(pior_1[,"alvo"]),xlab="Returns",ylab="Sector Consumption Not Cyclical /  Products of use Personal and Cleaning" ,pch=formato_ponto,las=1)
lines(pior_1[,"eixo_x_frequencias"],log(pior_1[,"previsao"]),col="gray",lwd=2)
# mtext("aaaaaaaaaaaaaaadownvar",side=1,line=2,col=1)
dev.off()


# # sum(( atual-previsao)^2)
# 
# # # 
# write.csv(pior_1,file="pior_1.csv",row.names=F)
# write.csv(pior_2,file="pior_2.csv",row.names=F)
# write.csv(pior_3,file="pior_3.csv",row.names=F)
# write.csv(pior_4,file="pior_4.csv",row.names=F)
# write.csv(pior_5,file="pior_5.csv",row.names=F)
# 
# # write.csv(eixo_x_y,file="por_ano_0_35_b_volatilidade_sse_mape_setores.csv")
# # write.csv(eixo_x_y,file="por_ano_b_volatilidade_sse_mape_setores.csv")
# # write.csv(eixo_x_y,file="por_semestre_b_volatilidade_sse_mape_setores.csv")
# # write.csv(eixo_x_y,file="por_trimestre_b_volatilidade_sse_mape_setores.csv")
# # write.csv(eixo_x_y,file="por_mensal_b_volatilidade_sse_mape_setores.csv")
# 
# # dados = read.csv("por_ano_b_volatilidade_sse_mape_setores.csv")
# # dados = read.csv("por_semestre_b_volatilidade_sse_mape_setores.csv")
# # dados = read.csv("por_trimestre_b_volatilidade_sse_mape_setores.csv")
# # dados = read.csv("por_mensal_b_volatilidade_sse_mape_setores.csv")
# # boxplot(main="MAPE mensal",dados$mape)
# 
# # 

# tem que ser o mesmo número
# length(unique(eixo_x_y$b_volatilidade))
# eixo_x_y$i

######################## volatilidade * B ########################
formato_ponto = 21
setEPS()
postscript("imagens/constante.eps")
volatilidade = (unique(eixo_x_y$volatilidade))
B = (unique(eixo_x_y$coeficiente_B))
# ,pch=16
plot(volatilidade*B,ylim=c(0,2),cex=1.3,ylab="Volatility x Coefficient B",xlab="Indexes of the sectors",pch=formato_ponto,las=1)
intercept = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[1]
slope = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[2]
abline(a=intercept,b =slope,col=2)
# abline(a=1,b =0,lwd=2,col="gray") #"gray"
dev.off()
########################################################################


# 
# 
# 
######################### nversamente_proporcionais_b_volatilidade #################################
setEPS()
postscript("imagens/inversamente_proporcionais_b_volatilidade.eps")
legenda = c("Coefficient B","Volatility")
plot(unique(eixo_x_y$coeficiente_B[order(eixo_x_y$coeficiente_B,decreasing=T)]),xlab="Indexes of the sectors",ylab="Amplitudes",ylim=c(0, 3),col="black",pch=formato_ponto,las=1)
points(unique(eixo_x_y$volatilidade[order(eixo_x_y$volatilidade,decreasing=F)]),xlab="Indice",ylab="Volatility",col="black",pch=4)
legend("topright", inset=.04,legenda ,col =c("black","black") ,pch=c(formato_ponto,4), horiz=F)
dev.off()

####################################################################################################
# title="Legend",
# com_b_menor_que_2 = unique(eixo_x_y[eixo_x_y$coeficiente_B>3,"i"])
# plot(com_b_menor_que_2$coeficiente_B~com_b_menor_que_2$volatilidade,xlab="Volatility",ylab="Coefficient B")
# 
plot(eixo_x_y$coeficiente_B~eixo_x_y$volatilidade,xlab="Volatility",ylab="Coefficient B")
# 
# 
# dados = eixo_x_y
# k =3
retorna_cluster = function(dados,k){
  #   dados = eixo_x_y
  #   k = 3
  #   dados = subset(eixo_x_y,eixo_x_y$tempo==semestre)
  iter = 45
  agrupamento = dados[,c("coeficiente_B","volatilidade")]
  #     dados[,1] = log(dados[,1])
  #     dados[,2] = log(dados[,2])
  
  km = kmeans (x = agrupamento, centers = k, iter.max = iter)
  agrupamento$cluster = km$cluster
  grupos = unique(km$cluster)
  #   print(grupos)
  dados$cluster = agrupamento$cluster
  dados$risco_b = ""
  dados$risco_b[dados$cluster == grupos[1]] = "moderado"
  dados$risco_b[dados$cluster == grupos[2]] = "conservador"
  dados$risco_b[dados$cluster == grupos[3]] = "arrojado"
  
  dados$cor = ""
  #   dados$cor[dados$cluster == grupos[1]] = "black"
  #   dados$cor[dados$cluster == grupos[2]] = "green"
  #   dados$cor[dados$cluster == grupos[3]] = "red"
  dados$cor[dados$cluster == grupos[1]] = "black"
  dados$cor[dados$cluster == grupos[2]] = "green"
  dados$cor[dados$cluster == grupos[3]] = "red"
  
  
  cor = c()
  #   dados$cor[dados$cluster == grupos[1]] = "black"
  #   dados$cor[dados$cluster == grupos[2]] = "green"
  #   dados$cor[dados$cluster == grupos[3]] = "red"
  cor[dados$cluster == grupos[1]] = "black"
  cor[dados$cluster == grupos[2]] = "green"
  cor[dados$cluster == grupos[3]] = "red"
  head(dados)
  #   cluster_ordem = unique( km$cluster)
  legenda = c("conservador","moderado","arrojado")
  plot(main= paste("Para K = ",k,sep =""),agrupamento$coeficiente_B~agrupamento$volatilidade,xlab="Volatility",ylab="Coefficient B", col = cor,pch = 20, cex = 0.9)
  #   points(km$centers[,1]~km$centers[,2],col=4, pch = 8,lwd=2)
  legend("topright", inset=.05,legenda , lwd= 3,col =c("green","black","red") , horiz=TRUE)
  # points(km$centers,col=1:k, pch = 8,lwd=3)
  
  #   agrupamento = agrupamento[order(agrupamento$cluster),]
  
  # dados
  return(dados)
}
# # eixo_x_y_sem_outlier = eixo_x_y[eixo_x_y$coeficiente_B<3,]
agrupamento_dados = retorna_cluster(eixo_x_y,3)
# # outlier = eixo_x_y[eixo_x_y$coeficiente_B>3,]
# # outlier$cor = "green"
# # outlier$cluster = unique(agrupamento_dados$cluster[agrupamento_dados$risco_b=="conservador"])
# # outlier$risco_b = "conservador"
# # agrupamento_dados = rbind( agrupamento_dados,outlier)
# # head(agrupamento_dados)
# legenda = c("conservador","moderado","arrojado")
# plot(main= paste("Para K = ",3,sep =""),agrupamento_dados$coeficiente_B~agrupamento_dados$volatilidade,xlab="Volatility",ylab="Coefficient B", col = agrupamento_dados$cor,pch = 20, cex = 0.9)
# #   points(km$centers[,1]~km$centers[,2],col=4, pch = 8,lwd=2)
# legend("topright", inset=.05,legenda , lwd= 3,col =c("green","black","red") , horiz=TRUE)
# 
# # semestre = "2014"
# # retorna_cluster ("2014")
# # cluster_2014_1 = cbind(data.frame(cluster = retorna_cluster(semestre)),eixo_x_y[eixo_x_y$tempo==semestre,c("colunas","coeficiente_B","volatilidade","tempo" )])
# 
# # write.table(cluster_2014_1,"cluster_2014_1.csv",row.names=F,sep=",")


write.table(eixo_x_y,"calculo_b_volatilidade_mais_pontos.csv",row.names=F,sep=",")
