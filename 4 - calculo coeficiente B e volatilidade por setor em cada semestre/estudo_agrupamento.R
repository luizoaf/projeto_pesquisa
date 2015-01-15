b_volatilidade = read.table("agrupamento_sse_b_e_volatilidade_k_2.csv",head=T)
b = read.table("agrupamento_sse_b_k_2.csv",head=T)
volatilidade = read.table("agrupamento_sse_volatilidade_k_2.csv",head=T)

tamanho_amostra = nrow(b_volatilidade)
k = 3
# ,rep(1,tamanho_amostra)
dados = b[b$periodo==2014,]
dados = b_volatilidade[b_volatilidade$periodo ==2014,]
dados = volatilidade[volatilidade$periodo ==2014,]

plot(main= paste("Para K = ",k,sep =""),dados$sse_volatilidade ,ylab="Coefficient B", col = dados$cor,pch = 20, cex = 0.9)

# legenda = c("conservador","moderado","arrojado")
# plot(main= paste("Para K = ",k,sep =""),agrupamento$coeficiente_B~agrupamento$volatilidade,xlab="Volatility",ylab="Coefficient B", col = dados$cor,pch = 20, cex = 0.9)
# #   points(km$centers[,1]~km$centers[,2],col=4, pch = 8,lwd=2)
# legend("topright", inset=.05,legenda , lwd= 3,col =c("green","black","red") , horiz=TRUE)

sse = data.frame(b_volatilidade_sse_volatilidade = b_volatilidade$sse_volatilidade,
                 b_volatilidade_sse_coeficiente_B = b_volatilidade$sse_coeficiente_b,
                 coeficiente_b_sse = b$sse_coeficiente_b,
                 volatilidade_sse = volatilidade$sse_volatilidade)

summary(sse)

boxplot(sse,ylim=c(0,0.5))
boxplot(sse)




b_volatilidade = read.table("agrupamento_sse_b_e_volatilidade.csv",head=T)
b = read.table("agrupamento_sse_b.csv",head=T)
volatilidade = read.table("agrupamento_sse_volatilidade.csv",head=T)

b_volatilidade_sse_volatilidade = aggregate(b_volatilidade$sse_volatilidade,list(b_volatilidade$periodo),FUN=mean)
colnames(b_volatilidade_sse_volatilidade) = c("ano","b_volatilidade_sse_volatilidade")

b_volatilidade_sse_b = aggregate(b_volatilidade$sse_coeficiente_b,list(b_volatilidade$periodo),FUN=mean)
b_volatilidade_sse_volatilidade = cbind(b_volatilidade_sse_volatilidade,data.frame(b_volatilidade_sse_b = b_volatilidade_sse_b[,2]))

volatilidade_sse = aggregate(volatilidade$sse_volatilidade,list(volatilidade$periodo),FUN=mean)
b_volatilidade_sse_volatilidade = cbind(b_volatilidade_sse_volatilidade,data.frame(volatilidade_sse = volatilidade_sse[,2]))

b_sse = aggregate(b$sse_coeficiente_b,list(b$periodo),FUN=mean)
b_volatilidade_sse_volatilidade = cbind(b_volatilidade_sse_volatilidade,data.frame(b_sse = b_sse[,2]))
# boxplot(b_volatilidade_sse_volatilidade[,2:3])

# write.table(file="sse_agrupamentos_diferentes.csv",b_volatilidade_sse_volatilidade,row.names=F)
write.table(file="sse_agrupamentos_diferentes_sem_4_outliers.csv",b_volatilidade_sse_volatilidade,row.names=F)





b_volatilidade = read.table("agrupamento_sse_b_e_volatilidade.csv",head=T)
b = read.table("agrupamento_sse_b.csv",head=T)
volatilidade = read.table("agrupamento_sse_volatilidade.csv",head=T)


# merge(b_volatilidade,b,)
b_volatilidade_b = merge(x=b_volatilidade,y=b, by = c("setor","periodo"), all = TRUE)

100 -100*length(b_volatilidade_b$risco.x[b_volatilidade_b$risco.y == b_volatilidade_b$risco.x])/length(b_volatilidade_b$risco.x)

b_volatilidade_volatilidade = merge(b_volatilidade,volatilidade, by = c("setor","periodo"), all = TRUE)
100 -100*length(b_volatilidade_volatilidade$risco.x[b_volatilidade_volatilidade$risco.y == b_volatilidade_volatilidade$risco.x])/length(b_volatilidade_volatilidade$risco.x)

volatilidade_b = merge(b,volatilidade, by = c("setor","periodo"), all = TRUE)
100 -100*length(volatilidade_b$risco.x[volatilidade_b$risco.y == volatilidade_b$risco.x])/length(volatilidade_b$risco.x)




b_volatilidade = read.table("agrupamento_distancias_b_e_volatilidade_mais_pontos.csv",head=T)
b = read.table("agrupamento_distancias_B_mais_pontos.csv",head=T)
volatilidade = read.table("agrupamento_distancias_volatilidade_mais_pontos.csv",head=T)



b_volatilidade_b = merge(x=b_volatilidade,y=b, by = c("coeficiente_B","volatilidade","periodo"), all = TRUE)
length(b_volatilidade_b$distancia_euclidiana.x[b_volatilidade_b$distancia_euclidiana.x > b_volatilidade_b$distancia_euclidiana.y ])/length(b_volatilidade_b$distancia_euclidiana.x)


b_volatilidade_b_vol = merge(x=b_volatilidade,y=volatilidade, by = c("coeficiente_B","volatilidade","periodo"), all = TRUE)
length(b_volatilidade_b$distancia_euclidiana.x[b_volatilidade_b$distancia_euclidiana.x > b_volatilidade_b$distancia_euclidiana.y ])/length(b_volatilidade_b$distancia_euclidiana.x)


b_volatilidade_vol = merge(x=volatilidade,y=b, by = c("coeficiente_B","volatilidade","periodo"), all = TRUE)
length(b_volatilidade_b$distancia_euclidiana.x[b_volatilidade_b$distancia_euclidiana.x > b_volatilidade_b$distancia_euclidiana.y ])/length(b_volatilidade_b$distancia_euclidiana.x)

boxplot(data.frame(dist_b_vol = b_volatilidade$distancia_euclidiana,
                   dist_b = b$distancia_euclidiana,
                   dist_vol = volatilidade$distancia_euclidiana))



plot(main="Agrupamento por B e volatilidade",b_volatilidade$volatilidade,b_volatilidade$coeficiente_B,col=b_volatilidade$cor,las=1,xlab="Volatility",ylab="Coefficient B")
plot(main="Agrupamento por B",b$volatilidade,b$coeficiente_B,col=b$cor,las=1,xlab="Volatility",ylab="Coefficient B")
plot(main="Agrupamento por volatilidade",volatilidade$volatilidade,volatilidade$coeficiente_B,col=volatilidade$cor,las=1,xlab="Volatility",ylab="Coefficient B")


plot(main="Agrupamento por B ",b_volatilidade_b$volatilidade.y,b_volatilidade_b$coeficiente_B.y,col=b_volatilidade_b$cor.y)


plot(main="Agrupamento por B e volatilidade",b_volatilidade_b$volatilidade.y,b_volatilidade_b$coeficiente_B.y,col=b_volatilidade_b$cor.x)
