# ########### calculo do B mais rapido ############# 

eixo_x_frequencias = funcao_distribuicao_probabilidade(serie)$valor_serie_retorno_eixo_x
alvo = funcao_distribuicao_probabilidade(serie)$frequencia_eixo_y

incremento = c(2,1,0.5,0.1,0.05,0.01,0.005,0.001,0.0001)
i_incremento = 1
coeficiente_B = incremento[length(incremento)] #tenho sse, comparo o sse atual com o antigo
i_erros =1
incremento_atual = incremento[1]
coeficiente_B_e_erros = data.frame()
transicao = F

while(T){
  valores_a = data.frame()
  
  for(a in seq(from=0.1,to = 1.5,by=0.1)){
    previsao = resultado_funcao_exponencial(a,eixo_x_frequencias,coeficiente_B)
    sse = sum(( previsao-alvo)^2)
    valores_a = rbind(valores_a,c(a,sse))
  }
  colnames(valores_a) = c("a","sse")
  menor_sse = valores_a$sse[which.min(valores_a$sse)]
  coeficiente_B_e_erros_menor_sse = subset(valores_a,valores_a$sse == menor_sse)
  a = coeficiente_B_e_erros_menor_sse$a
  
  #   previsao = resultado_funcao_exponencial(a,eixo_x_frequencias,coeficiente_B)
  #   sse = sum(( previsao-alvo)^2)
  
  coeficiente_B_e_erros = rbind(coeficiente_B_e_erros, cbind(a,menor_sse,coeficiente_B))
  colnames(coeficiente_B_e_erros) = c("a","menor_sse","coeficiente_B")
  
  
  
  if(i_erros>2 && coeficiente_B_e_erros$menor_sse[i_erros-1] >= coeficiente_B_e_erros$menor_sse[i_erros] && coeficiente_B_e_erros$coeficiente_B > 0){
    incremento_atual = incremento[i_incremento]
    if(transicao){
      coeficiente_B = coeficiente_B - ifelse(is.na(incremento_atual),0,incremento_atual)
    }else{
      coeficiente_B = coeficiente_B + ifelse(is.na(incremento_atual),0,incremento_atual)
    }
  }else{
    i_incremento = i_incremento + 1
    transicao = !transicao
    incremento_atual = incremento[i_incremento]
    if(transicao){
      coeficiente_B = coeficiente_B - ifelse(is.na(incremento_atual),0,incremento_atual)
    }else{
      coeficiente_B = coeficiente_B + ifelse(is.na(incremento_atual),0,incremento_atual)
    }
  }
  
  if(i_erros==1){
    incremento_atual = incremento[i_incremento]
    coeficiente_B = coeficiente_B + ifelse(is.na(incremento_atual),0,incremento_atual)
  }
  if(is.na(incremento_atual)){
    break
  }
  i_erros = i_erros + 1
}


sse = coeficiente_B_e_erros$menor_sse[which.min(coeficiente_B_e_erros$menor_sse)]
coeficiente_B_e_erros_menor_sse = subset(coeficiente_B_e_erros,coeficiente_B_e_erros$menor_sse == sse)

volatilidade = calcula_volatilidade(serie)
coeficiente_B = coeficiente_B_e_erros_menor_sse$coeficiente_B
a = coeficiente_B_e_erros_menor_sse$a
previsao = resultado_funcao_exponencial(a,eixo_x_frequencias,coeficiente_B)

# png(filename=paste(sse,".png",sep=""),bg="transparent")
plot_previsao_com_B_e_exponencial(eixo_x_frequencias,previsao)
# dev.off()