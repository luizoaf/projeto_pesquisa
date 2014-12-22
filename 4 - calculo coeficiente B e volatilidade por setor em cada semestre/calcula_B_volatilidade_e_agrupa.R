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


faixa_temporal = unique(dados$datas)

eixo_x_y = data.frame()
eixo_x_y_sem_a = data.frame()
i=1
colunas = c()
faixa_temporal_por_setores = c()
# periodo = faixa_temporal[1]
# coluna = 1
for(periodo in faixa_temporal){
  serie_retornos_normalizado = dado_semestre_retorna_media_serie_retornos_por_setor(periodo,dados)
  for(coluna in 1:length(names(serie_retornos_normalizado))){
    colunas[i] = names(serie_retornos_normalizado)[coluna]
    faixa_temporal_por_setores[i] = periodo
    serie = serie_retornos_normalizado[,coluna]
    
    #         png(filename=paste(i,".png",sep=""),bg="transparent")
    #     par(mfrow=c(2,1))
    
    source("previsao_exponencial.R")
    #     eixo_x_y = rbind(eixo_x_y, cbind(eixo_x_frequencias,alvo,previsao, sse,a,coeficiente_B,volatilidade,i))
    #     colnames(eixo_x_y) = c("eixo_x_frequencias","alvo","previsao","sse","a","coeficiente_B","volatilidade","i")
    #     
    eixo_x_y = rbind(eixo_x_y, cbind(sse,a,coeficiente_B,volatilidade,i))
    colnames(eixo_x_y) = c("sse","a","coeficiente_B","volatilidade","i")
    
    
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
  dados$risco_b[dados$cluster == grupos[2]] = "arrojado"
  dados$risco_b[dados$cluster == grupos[3]] = "conservador"
  
  dados$cor = ""
  #   dados$cor[dados$cluster == grupos[1]] = "black"
  #   dados$cor[dados$cluster == grupos[2]] = "green"
  #   dados$cor[dados$cluster == grupos[3]] = "red"
  dados$cor[dados$cluster == grupos[1]] = "black"
  dados$cor[dados$cluster == grupos[2]] = "red"
  dados$cor[dados$cluster == grupos[3]] = "green"
  
  
  cor = c()
  #   dados$cor[dados$cluster == grupos[1]] = "black"
  #   dados$cor[dados$cluster == grupos[2]] = "green"
  #   dados$cor[dados$cluster == grupos[3]] = "red"
  cor[dados$cluster == grupos[1]] = "black"
  cor[dados$cluster == grupos[2]] = "red"
  cor[dados$cluster == grupos[3]] = "green"
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
eixo_x_y_sem_outlier = eixo_x_y[eixo_x_y$coeficiente_B<3,]
dados = retorna_cluster(eixo_x_y_sem_outlier,3)
outlier = eixo_x_y[eixo_x_y$coeficiente_B>3,]
outlier$cor = "green"
outlier$cluster = unique(dados$cluster[dados$risco_b=="conservador"])
outlier$risco_b = "conservador"
dados = rbind( dados,outlier)
head(dados)
legenda = c("conservador","moderado","arrojado")
plot(main= paste("Para K = ",3,sep =""),dados$coeficiente_B~dados$volatilidade,xlab="Volatility",ylab="Coefficient B", col = dados$cor,pch = 20, cex = 0.9)
#   points(km$centers[,1]~km$centers[,2],col=4, pch = 8,lwd=2)
legend("topright", inset=.05,legenda , lwd= 3,col =c("green","black","red") , horiz=TRUE)

# semestre = "2014"
# retorna_cluster ("2014")
# cluster_2014_1 = cbind(data.frame(cluster = retorna_cluster(semestre)),eixo_x_y[eixo_x_y$tempo==semestre,c("colunas","coeficiente_B","volatilidade","tempo" )])

# write.table(cluster_2014_1,"cluster_2014_1.csv",row.names=F,sep=",")