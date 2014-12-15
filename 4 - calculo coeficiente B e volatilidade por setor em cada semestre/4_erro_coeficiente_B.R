# previsao = resultado_funcao_exponencial(eixo_x_frequencias,coeficiente_B)
# sse = sum(( previsao-alvo)^2)

# volatilidade = calcula_volatilidade(serie)
# plot(funcao_distribuicao_probabilidade(serie))
# lines(exponencial~eixo_x_frequencias,col=2)


# tamanho_serie = length(serie_retornos_frequencia)
# exponencial_serie_retornos_frequencia_norm = data.frame(exponencial=exponencial,serie_retornos_frequencia = serie_retornos_frequencia)

# sse = sum(( exponencial_serie_retornos_frequencia_norm$exponencial-exponencial_serie_retornos_frequencia_norm$serie_retornos_frequencia)^2)
# rmse = sqrt(mean((exponencial_serie_retornos_frequencia_norm$exponencial-exponencial_serie_retornos_frequencia_norm$serie_retornos_frequencia)^2))

# exponencial_serie_retornos_frequencia_norm = normalizacao_transformacao_linear(0.15,0.85,exponencial_serie_retornos_frequencia_norm)
# mape = sum(abs(exponencial_serie_retornos_frequencia_norm$exponencial-exponencial_serie_retornos_frequencia_norm$serie_retornos_frequencia)/exponencial_serie_retornos_frequencia_norm$serie_retornos_frequencia)/length(exponencial)

# mean(100*abs(exponencial_serie_retornos_frequencia_norm$exponencial-exponencial_serie_retornos_frequencia_norm$serie_retornos_frequencia)/exponencial_serie_retornos_frequencia_norm$serie_retornos_frequencia)
# sse = sum(( exponencial-serie_retornos_frequencia)^2)

