source("../1_funcoes.R")
# papeis_da_ibovespa_2007_2012
# dados = read.csv(file="papeis_da_ibovespa_2007_2012.csv")
# dados = read.csv(file="papeis_da_ibovespa_2008_a_2014_2.csv")
dados = read.csv(file="papeis_da_ibovespa_2008_a_2014_2_com_IBOVESPA.csv")
dados = dados[,-2]
dados$datas  = as.Date(dados$datas)

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

# dados$datas = datas

df_setores = read.csv("setores.csv")
df_setores_codigo_acao = data.frame(codigo = df_setores$Código[2:nrow(df_setores)],
                                    acao = df_setores$Ação[2:nrow(df_setores)],
                                    setores = correcao_coluna_setores(df_setores))

relacao_setores_acoes = function(dados,periodo){
  periodo_acoes = dados[dados$datas==periodo,]
  papeis = names(periodo_acoes)[2:ncol(periodo_acoes)]
  codigo_acoes = substr(papeis, 0, nchar(papeis)-3) # remocao do ".SA"
  df_codigo_menos_acoes = data.frame(codigo = codigo_acoes)
  relacao_setores_acoes_menos_acoes = merge(x=df_setores_codigo_acao,y=df_codigo_menos_acoes,by="codigo",all=FALSE)
  return(relacao_setores_acoes_menos_acoes)
}
acoes_por_setores_por_periodo = function(dados,periodo){
  relacao_setores_acoes_menos_acoes = relacao_setores_acoes(dados,periodo)
  quantidade_acoes_por_setor_menos_acoes = aggregate(relacao_setores_acoes_menos_acoes$setores,list(relacao_setores_acoes_menos_acoes$setores),FUN=length)
  colnames(quantidade_acoes_por_setor_menos_acoes) = c("Setor","Quantidade_de_Acoes_pesquisadas")
  quantidade_acoes_por_setor_menos_acoes = quantidade_acoes_por_setor_menos_acoes[order(quantidade_acoes_por_setor_menos_acoes$Quantidade_de_Acoes,decreasing=T),]
  
  # Todas acoes
  codigos =as.character(df_setores$Código[2:length(df_setores$Código)])
  df_codigo = data.frame(codigo = codigos)
  relacao_setores_acoes = merge(x=df_setores_codigo_acao,y=df_codigo,by="codigo",all=FALSE)
  quantidade_acoes_por_setor = aggregate(relacao_setores_acoes$setores,list(relacao_setores_acoes$setores),FUN=length)
  colnames(quantidade_acoes_por_setor) = c("Setor","Quantidade_de_Acoes_todas_acoes")
  quantidade_acoes_por_setor = quantidade_acoes_por_setor[order(quantidade_acoes_por_setor$Quantidade_de_Acoes,decreasing=T),]
  
  # merge dos setores do periodo com o de todos os setores com todas as acoes possiveis
  setores = merge(quantidade_acoes_por_setor,quantidade_acoes_por_setor_menos_acoes,by="Setor",all=TRUE)
  setores[is.na(setores)] = 0
  setores$porcentagem = 100*(setores$Quantidade_de_Acoes_pesquisadas/setores$Quantidade_de_Acoes_todas_acoes)
  setores = setores[order(setores$porcentagem,decreasing=T),]
  return(setores)
}
# write.table(quantidade_acoes_por_setor_menos_acoes,"quantidade_acoes_por_setor_49_acoes.csv",sep=",",row.names=F)

setores_100_porcento_por_periodo = function(periodo){
  setores_100_porcento = acoes_por_setores_por_periodo(dados,periodo) 
  setores_100_porcento = setores_100_porcento$Setor[setores_100_porcento$porcentagem ==100]
  setores_100_porcento = as.character(setores_100_porcento)
  return(setores_100_porcento)
}

dado_semestre_retorna_media_serie_retornos_por_setor = function(semestre){
  semestre_acoes = subset(dados,dados$datas==semestre)
  serie_retornos_por_semestre = semestre_acoes[,2:ncol(semestre_acoes)]
  #   serie_retornos_por_semestre = cria_tabela_serie_retornos_de_todas_as_acoes(semestre_acoes)
  setores_media_acoes = data.frame(1)
  setores_100_porcento = setores_100_porcento_por_periodo(semestre)
  for( setor in setores_100_porcento){
    relacao_setores_acoes_menos_acoes = relacao_setores_acoes(dados,semestre)
    acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
    if(length(acoes_do_setor)!=1){ # nao ira calcular a media quando tiver apenas 1 acao
      medias_por_setor = apply(serie_retornos_por_semestre[,acoes_do_setor],MARGIN=1,FUN=mean)
    }else{
      medias_por_setor = serie_retornos_por_semestre[,acoes_do_setor]
    }
    setores_media_acoes = cbind(setores_media_acoes,medias_por_setor)
  }
  #   setores_media_acoes = setores_media_acoes[,-1]
  #   head(setores_media_acoes)
  ### mudanca de ordem ###
  setores_media_acoes = cria_tabela_serie_retornos_de_todas_as_acoes(setores_media_acoes)
  
  colnames(setores_media_acoes) = setores_100_porcento_por_periodo(semestre)
  return(setores_media_acoes)
}

faixa_temporal = unique(dados$datas)

eixo_x_y = data.frame()
eixo_x_y_sem_a = data.frame()
i=1
colunas = c()
faixa_temporal_por_setores = c()
# periodo = faixa_temporal[1]
# coluna = 1
for(periodo in faixa_temporal){
  serie_retornos_normalizado = dado_semestre_retorna_media_serie_retornos_por_setor(periodo)
  for(coluna in 1:length(names(serie_retornos_normalizado))){
    colunas[i] = names(serie_retornos_normalizado)[coluna]
    faixa_temporal_por_setores[i] = periodo
    serie = serie_retornos_normalizado[,coluna]
    
    #         png(filename=paste(i,".png",sep=""),bg="transparent")
    #     par(mfrow=c(2,1))
    
    source("previsao_exponencial.R")
    eixo_x_y = rbind(eixo_x_y, cbind(eixo_x_frequencias,alvo,previsao, sse,a,coeficiente_B,volatilidade,i))
    colnames(eixo_x_y) = c("eixo_x_frequencias","alvo","previsao","sse","a","coeficiente_B","volatilidade","i")
    
    #     a = 1
    #     source("define_coeficiente_B_sem_a.R")
    #     eixo_x_y_sem_a = rbind(eixo_x_y_sem_a, cbind(eixo_x_frequencias,alvo,previsao, sse,a,coeficiente_B,volatilidade,i))
    #     #,periodo,setor,coeficiente_B*volatilidade))
    #     colnames(eixo_x_y_sem_a) =  c("eixo_x_frequencias","alvo","previsao","sse","a","coeficiente_B","volatilidade","i")
    #         dev.off()
    i= i+1
    # write.table(eixo_x_y,paste(names(serie_retornos_normalizado)[i],grupo_janelamento,"combinacao_janelamento_30_incrementos_0_01_0_28_todas_acoes.csv"),row.names=F,sep=",")
  }
}
eixo_x_y$tempo = faixa_temporal_por_setores
eixo_x_y$setor = colunas
eixo_x_y$b_volatilidade = eixo_x_y$coeficiente_B*eixo_x_y$volatilidade

# plot(eixo_x_y$b_volatilidade,ylim=c(0,2))
# unique(eixo_x_y$i[eixo_x_y_sem_a$sse < eixo_x_y$sse])
# boxplot(main="SSE",data.frame(com_a = eixo_x_y$sse,sem_a = eixo_x_y_sem_a$sse))
boxplot(main="SSE",eixo_x_y$sse)
# boxplot(main="MAPE",eixo_x_y$mape)
# 
eixo_x_y_pior_caso = eixo_x_y[order(eixo_x_y$sse,decreasing=T),]
# eixo_x_y_sem_a = eixo_x_y_sem_a[order(eixo_x_y_sem_a$sse,decreasing=T),]
# 2 35 84 88  3
head(eixo_x_y_pior_caso,n=1)$i
# unique(head(eixo_x_y$i,n=250))
# unique(head(eixo_x_y_sem_a$i,n=250))
# pior_1 = subset(eixo_x_y,eixo_x_y$i==2)[,1:2]
# pior_2 = subset(eixo_x_y,eixo_x_y$i==35)[,1:2]
# pior_3 = subset(eixo_x_y,eixo_x_y$i==84)[,1:2]
# pior_4 = subset(eixo_x_y,eixo_x_y$i==88)[,1:2]
# pior_5 = subset(eixo_x_y,eixo_x_y$i==3)[,1:2]
# 
pior_1 = subset(eixo_x_y,eixo_x_y$i==2)
# pior_2 = subset(eixo_x_y,eixo_x_y$i==6)
# serie=pior_2$alvo
# pior_3 = subset(eixo_x_y,eixo_x_y$i==28)
# pior_4 = subset(eixo_x_y,eixo_x_y$i==3)
# pior_5 = subset(eixo_x_y,eixo_x_y$i==15)
# 
# # pior_1$coeficiente_B
atual = pior_1[,"alvo"]
previsao = pior_1[,"previsao"]
plot(main = paste("Pior caso\nSSE: ",unique(pior_1$sse)),pior_1[,1:2],pch=20)
lines(pior_1[,c(1,3)],col="blue")

# 
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
volatilidade = (unique(eixo_x_y$volatilidade))
B = (unique(eixo_x_y$coeficiente_B))
plot(volatilidade*B,ylim=c(0,2),ylab="sqrt(Volatility) x sqrt(Coefficient B)",xlab="Indices",pch=20)
intercept = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[1]
slope = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[2]
abline(a=intercept,b =slope,col=2)
# 
# 
# 
# 
plot(unique(eixo_x_y$coeficiente_B[order(eixo_x_y$coeficiente_B,decreasing=T)]),xlab="Volatility",ylab="Coefficient B",ylim=c(0, 5),col="red",pch=20)
points(unique(eixo_x_y$volatilidade[order(eixo_x_y$volatilidade,decreasing=F)]),xlab="Indice",ylab="Volatility",col="blue",pch=20)
# 
com_b_menor_que_2 = unique(eixo_x_y[eixo_x_y$coeficiente_B>3,"i"])
# plot(com_b_menor_que_2$coeficiente_B~com_b_menor_que_2$volatilidade,xlab="Volatility",ylab="Coefficient B")
# 
plot(eixo_x_y$coeficiente_B~eixo_x_y$volatilidade,xlab="Volatility",ylab="Coefficient B")
# 
# 
retorna_cluster = function(){
  #   dados = subset(eixo_x_y,eixo_x_y$tempo==semestre)
  dados = eixo_x_y
  k=3
  iter = 45
  dados = dados[,c("coeficiente_B","volatilidade")]
  #   dados[,1] = log(dados[,1])
  #   dados[,2] = log(dados[,2])
  
  km = kmeans (x = dados, centers = k, iter.max = iter)
  dados$cluster = km$cluster
  
  plot(main= paste("Para K = ",k,sep =""),dados$coeficiente_B~dados$volatilidade,xlab="Volatility",ylab="Coefficient B", col = km$cluster,pch = 20, cex = 0.9)
  points(km$centers[,1]~km$centers[,2],col=4, pch = 8,lwd=2)
  # points(km$centers,col=1:k, pch = 8,lwd=3)
  
  dados = dados[order(dados$cluster),]
  # dados
  return(dados$cluster)
}
retorna_cluster()
semestre = "2014"
retorna_cluster ("2014")
cluster_2014_1 = cbind(data.frame(cluster = retorna_cluster(semestre)),eixo_x_y[eixo_x_y$tempo==semestre,c("colunas","coeficiente_B","volatilidade","tempo" )])

# write.table(cluster_2014_1,"cluster_2014_1.csv",row.names=F,sep=",")