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
  
  previsao = resultado_funcao_exponencial(1,eixo_x_frequencias,coeficiente_B)
  sse = sum(( previsao-alvo)^2)
  
  coeficiente_B_e_erros = rbind(coeficiente_B_e_erros, cbind(sse,coeficiente_B))
  colnames(coeficiente_B_e_erros) = c("sse","coeficiente_B")
  
  if(i_erros>2 && coeficiente_B_e_erros$sse[i_erros-1] >= coeficiente_B_e_erros$sse[i_erros] && coeficiente_B_e_erros$coeficiente_B > 0){
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


menor_sse_sem_a = coeficiente_B_e_erros$sse[which.min(coeficiente_B_e_erros$sse)]
coeficiente_B_e_erros_menor_sse = subset(coeficiente_B_e_erros,coeficiente_B_e_erros$sse == menor_sse_sem_a)

volatilidade = calcula_volatilidade(serie)
sse = menor_sse_sem_a
coeficiente_B = coeficiente_B_e_erros_menor_sse$coeficiente_B
previsao = resultado_funcao_exponencial(1,eixo_x_frequencias,coeficiente_B)

# png(filename=paste(sse,".png",sep=""),bg="transparent")
# a= 1
print(plot_previsao_com_B_e_exponencial(eixo_x_frequencias,previsao))
# dev.off()