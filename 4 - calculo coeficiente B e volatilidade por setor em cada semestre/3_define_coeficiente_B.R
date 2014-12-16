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
  #   valores_a = data.frame()
  #   # a : ajudar no calculo
  #   i_incremento_a = 1
  #   a = incremento_a[length(incremento_a)]
  #   a_atual = incremento_a[1]
  #   i_erros_a =1
  #   transicao_a = F
  #   #   chegou_no_max= FALSE
  #   while(T){
  #     previsao = resultado_funcao_exponencial(a,eixo_x_frequencias,coeficiente_B)
  #     sse = sum(( previsao-alvo)^2)
  #     valores_a = rbind(valores_a,c(a,sse))
  #     colnames(valores_a) = c("a","sse")
  #     valores_a
  #     if(i_erros_a>2 && valores_a$sse[i_erros_a-1] >= valores_a$sse[i_erros_a] && valores_a$a > 0){
  #       a_atual = incremento_a[i_incremento_a]
  #       if(transicao){
  #         a = a - ifelse(is.na(a_atual),0,a_atual)
  #       }else{
  #         a = a + ifelse(is.na(a_atual),0,a_atual)
  #       }
  #     }else{
  #       i_incremento_a = i_incremento_a + 1
  #       transicao_a = !transicao_a
  #       a_atual = incremento_a[i_incremento_a]
  #       if(transicao_a){
  #         a = a - ifelse(is.na(a_atual),0,a_atual)
  #       }else{
  #         a = a + ifelse(is.na(a_atual),0,a_atual)
  #       }
  #     }
  #     
  #     if(i_erros_a==1){
  #       a_atual = incremento_a[i_incremento_a]
  #       a = a + ifelse(is.na(a_atual),0,a_atual)
  #     }
  #     if(is.na(a_atual)){
  #       break
  #     }
  #     i_erros_a = i_erros_a + 1
  #   }
  #   #   colnames(valores_a) = c("a","sse")
  #   menor_sse = valores_a$sse[which.min(valores_a$sse)]
  #   coeficiente_B_e_erros_menor_sse = subset(valores_a,valores_a$sse == menor_sse)
  #   #   a = coeficiente_B_e_erros_menor_sse$a
  a = 1
  #   previsao = resultado_funcao_exponencial(a,eixo_x_frequencias,coeficiente_B)
  #   sse = sum(( previsao-alvo)^2)
  
  previsao = resultado_funcao_exponencial(a,eixo_x_frequencias,coeficiente_B)
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

vetor_a = c()
sse_a = c()
for(i in 1:nrow(coeficiente_B_e_erros)){
  coeficiente_B = coeficiente_B_e_erros$coeficiente_B[i]
  valores_a = data.frame()
  # a : ajudar no calculo
  i_incremento_a = 1
  a = incremento_a[length(incremento_a)]
  a_atual = incremento_a[1]
  i_erros_a =1
  transicao_a = F
  #   chegou_no_max= FALSE
  while(T){
    previsao = resultado_funcao_exponencial(a,eixo_x_frequencias,coeficiente_B)
    sse = sum(( previsao-alvo)^2)
    valores_a = rbind(valores_a,c(a,sse))
    colnames(valores_a) = c("a","sse")
    valores_a
    if(i_erros_a>2 && valores_a$sse[i_erros_a-1] >= valores_a$sse[i_erros_a] && valores_a$a > 0){
      a_atual = incremento_a[i_incremento_a]
      if(transicao){
        a = a - ifelse(is.na(a_atual),0,a_atual)
      }else{
        a = a + ifelse(is.na(a_atual),0,a_atual)
      }
    }else{
      i_incremento_a = i_incremento_a + 1
      transicao_a = !transicao_a
      a_atual = incremento_a[i_incremento_a]
      if(transicao_a){
        a = a - ifelse(is.na(a_atual),0,a_atual)
      }else{
        a = a + ifelse(is.na(a_atual),0,a_atual)
      }
    }
    
    if(i_erros_a==1){
      a_atual = incremento_a[i_incremento_a]
      a = a + ifelse(is.na(a_atual),0,a_atual)
    }
    if(is.na(a_atual)){
      break
    }
    i_erros_a = i_erros_a + 1
  }
  #   colnames(valores_a) = c("a","sse")
  menor_sse = valores_a$sse[which.min(valores_a$sse)]
  coeficiente_B_e_erros_menor_sse = subset(valores_a,valores_a$sse == menor_sse)
  a = coeficiente_B_e_erros_menor_sse$a
  #   a = 1
  vetor_a[i] = a
  sse_a[i] = menor_sse
  
}


teste = cbind(coeficiente_B_e_erros,data.frame(vetor_a,sse_a))
sse = teste$sse_a[which.min(teste$sse_a)]


sse = coeficiente_B_e_erros$sse[which.min(coeficiente_B_e_erros$sse)]
coeficiente_B_e_erros_menor_sse = subset(coeficiente_B_e_erros,coeficiente_B_e_erros$menor_sse == sse)

volatilidade = calcula_volatilidade(serie)
coeficiente_B = coeficiente_B_e_erros_menor_sse$coeficiente_B
a = coeficiente_B_e_erros_menor_sse$a
previsao = resultado_funcao_exponencial(a,eixo_x_frequencias,coeficiente_B)

# png(filename=paste(sse,".png",sep=""),bg="transparent")
print(plot_previsao_com_B_e_exponencial(eixo_x_frequencias,previsao))
# dev.off()