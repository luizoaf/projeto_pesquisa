dados = read.table("calculo_b_volatilidade.csv",sep=",",head=T)

retorna_cluster = function(dados,k){
  #   dados = eixo_x_y
  # k = 3
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
  
  
  #   cor = c()
  #   #   dados$cor[dados$cluster == grupos[1]] = "black"
  #   #   dados$cor[dados$cluster == grupos[2]] = "green"
  #   #   dados$cor[dados$cluster == grupos[3]] = "red"
  #   cor[dados$cluster == grupos[1]] = "red"
  #   cor[dados$cluster == grupos[2]] = "black"
  #   cor[dados$cluster == grupos[3]] = "green"
  #   head(dados)
  #   cluster_ordem = unique( km$cluster)
  legenda = c("conservador","moderado","arrojado")
  plot(main= paste("Para K = ",k,sep =""),agrupamento$coeficiente_B~agrupamento$volatilidade,xlab="Volatility",ylab="Coefficient B", col = dados$cor,pch = 20, cex = 0.9)
  #   points(km$centers[,1]~km$centers[,2],col=4, pch = 8,lwd=2)
  legend("topright", inset=.05,legenda , lwd= 3,col =c("green","black","red") , horiz=TRUE)
  # points(km$centers,col=1:k, pch = 8,lwd=3)
  
  #   agrupamento = agrupamento[order(agrupamento$cluster),]
  
  # dados
  #   return(dados)
  names(dados)
  # centroides = cbind(km$centers,data.frame(risco = c("conservador","moderado","arrojado")))
  centroides = as.data.frame(km$centers)
  centroides = centroides[order(centroides$volatilidade,decreasing=T),]
  centroides = cbind(centroides,data.frame(risco = c("arrojado","moderado","conservador")))
  centroides$cor = ""
  #   dados$cor[dados$cluster == grupos[1]] = "black"
  #   dados$cor[dados$cluster == grupos[2]] = "green"
  #   dados$cor[dados$cluster == grupos[3]] = "red"
  centroides$cor[centroides$risco == "conservador"] = "green"
  centroides$cor[centroides$risco == "moderado"] = "black"
  centroides$cor[centroides$risco == "arrojado"] = "red"
  
  plot( centroides$coeficiente_B~centroides$volatilidade,col=centroides$cor, pch = 8,lwd=3,xlim=c(0,2.5),ylim=c(0,4.5))
  return(centroides)
}
# return(km$centers)
# }
# eixo_x_y_sem_outlier = eixo_x_y[eixo_x_y$coeficiente_B<3,]
# agrupamento_dados = retorna_cluster(dados,3)
# outlier = eixo_x_y[eixo_x_y$coeficiente_B>3,]
# outlier$cor = "green"
# outlier$cluster = unique(agrupamento_dados$cluster[agrupamento_dados$risco_b=="conservador"])
# outlier$risco_b = "conservador"
# agrupamento_dados = rbind( agrupamento_dados,outlier)
# head(agrupamento_dados)
# legenda = c("conservador","moderado","arrojado")
# plot(main= paste("Para K = ",3,sep =""),agrupamento_dados$coeficiente_B~agrupamento_dados$volatilidade,xlab="Volatility",ylab="Coefficient B", col = agrupamento_dados$cor,pch = 20, cex = 0.9)
# #   points(km$centers[,1]~km$centers[,2],col=4, pch = 8,lwd=2)
# legend("topright", inset=.05,legenda , lwd= 3,col =c("green","black","red") , horiz=TRUE)



treino = subset(dados,dados$tempo< 2014)
teste = subset(dados,dados$tempo == 2014)
centroides = retorna_cluster(treino,3)
# pontos_novos = data.frame(volatilidade = c(0.9,1.1),coeficiente_B = c(1.50,0.8))
pontos_novos = teste[,c("volatilidade","coeficiente_B")]
points(pontos_novos$coeficiente_B~pontos_novos$volatilidade,col="violet")

novos_pontos_classificados = data.frame()
# ponto_estudado = 1# indices de todos os pontos

for(ponto_estudado in 1:nrow(pontos_novos)){
  ponto_centroide = 1:3 # os 3 possiveis centroides dos riscos
  distancia = sqrt((pontos_novos$volatilidade[ponto_estudado] - centroides$volatilidade[ponto_centroide])^2 + (pontos_novos$coeficiente_B[ponto_estudado] - centroides$coeficiente_B[ponto_centroide])^2)
  todas_distancias = data.frame(distancias = distancia,risco =centroides$risco)
  indice_menor_distancia = which.min(todas_distancias$distancias)
  risco = as.character(todas_distancias$risco[indice_menor_distancia])
  agrupamento = cbind(pontos_novos[ponto_estudado,],risco)
  distancia_euclidiana = distancia[indice_menor_distancia]
  agrupamento = cbind(agrupamento,distancia_euclidiana)
  
  # calcula SSE de B e volatilidade
  alvo_volatilidade = centroides$volatilidade[ponto_centroide][indice_menor_distancia]
  alvo_B = centroides$coeficiente_B[ponto_centroide][indice_menor_distancia]
  
  volatilidade = pontos_novos$volatilidade[ponto_estudado]
  coeficiente_b = pontos_novos$coeficiente_B[ponto_estudado]
  
  sse_volatilidade = sum(( volatilidade-alvo_volatilidade)^2)
  sse_coeficiente_b = sum(( coeficiente_b-alvo_B)^2)
  
  agrupamento = cbind(agrupamento,sse_volatilidade)
  agrupamento = cbind(agrupamento,sse_coeficiente_b)
  
  novos_pontos_classificados = rbind(novos_pontos_classificados,agrupamento)
}
# novos_pontos_classificados = data.frame(pontos_novos$volatilidade)

novos_pontos_classificados$cor = ""
#   dados$cor[dados$cluster == grupos[1]] = "black"
#   dados$cor[dados$cluster == grupos[2]] = "green"
#   dados$cor[dados$cluster == grupos[3]] = "red"
novos_pontos_classificados$cor[novos_pontos_classificados$risco == "moderado"] = "black"
novos_pontos_classificados$cor[novos_pontos_classificados$risco == "arrojado"] = "red"
novos_pontos_classificados$cor[novos_pontos_classificados$risco == "conservador"] = "green"

points(novos_pontos_classificados$volatilidade,novos_pontos_classificados$coeficiente_B,col=novos_pontos_classificados$cor,lwd=1)
# aggregate(dados$a,list(dados$tempo),FUN=length)



# distancia = sqrt((pontos_novos$Sepal.Length[ponto_estudado] - pontos$Sepal.Length[ponto_vizinho])^2 + (pontos_novos$Sepal.Width[ponto_estudado] - pontos$Sepal.Width[ponto_vizinho])^2)
