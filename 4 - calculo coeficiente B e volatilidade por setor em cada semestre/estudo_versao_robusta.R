require(robustbase)
require(RSNNS)
require(minpack.lm)
dados = read.csv(file="pior_1.csv")
require(nls2)
eixo_x_frequencias = (dados$eixo_x_frequencias)
alvo = (dados$alvo)

# eixo_x_frequencias = funcao_distribuicao_probabilidade(serie)$valor_serie_retorno_eixo_x
# alvo = funcao_distribuicao_probabilidade(serie)$frequencia_eixo_y

# temp = data.frame(y = alvo, x = 1:length(alvo))
temp = data.frame(x= eixo_x_frequencias,y = alvo)
# temp_norm = normalizeData(temp,type="0_1")
# temp = data.frame(y = temp_norm[,1],x = temp_norm[,2])

f <- function(b,x,a) {a*exp(-b*x) } # 0.0602209
mod_padrao <- nlsLM(y ~ f(b,x,a), data = temp,lower=c(a=0.1,b=0.1), start = list(a=0.1,b = 0.1),algorithm = "LM", control = list(maxiter = 500, tol = 1e-05, minFactor = 0.0009765625, printEval = F, warnOnly = FALSE, trace = T))

previsao = predict(mod_padrao, list(x = temp$x))
# previsao_residuos = as.vector(denormalizeData(fitted(mod_padrao),getNormParameters(temp_norm)))
sse = sum(( previsao-alvo)^2)
sse
# previsao_otima = 1.24*exp(-1.96*temp$x)
# residuos = alvo - previsao_otima
# plot(residuos~eixo_x_frequencias)
# sse_otimo = sum(( previsao_otima-alvo)^2)
n=20
plot(main = paste("SSE: ",sse),temp$x, temp$y,pch=n)
lines(fitted(mod_padrao)~temp$x , col="red",lwd=1)
# lines(previsao_otima~eixo_x_frequencias , col="blue",lwd=1)
# lines(previsao~eixo_x_frequencias , col="red",lwd=1)
# 
# residuos = residuals(mod_padrao)
# temp$residuos = residuos
# residuos_norm = normalizeData(temp,type="0_1")
# residuos_norm_df = data.frame(x = residuos_norm[,1],y = residuos_norm[,2],residuos = residuos_norm[,3])
# 
# plot(main = paste("SSE: ",sse),residuos_norm_df$x, residuos_norm_df$residuos)
# mod_padrao <- nlsLM(residuos_norm_df$residuos ~ f(b,x,a), data = residuos_norm_df[,1:2],lower=c(a=0,b=0), start = list(a=0,b = 0),algorithm = "LM", control = list(maxiter = 500, tol = 1e-05, minFactor = 0.0009765625, printEval = F, warnOnly = FALSE, trace = T))
# # predict(mod_padrao, list(x = temp$x))
# previsao_residuos = as.vector(denormalizeData(fitted(mod_padrao),getNormParameters(residuos_norm)))
# lines(previsao_residuos~residuos_norm_df$x , col="blue",lwd=1)
# 

# x = eixo_x_frequencias
# # y = alvo
# 
# g <- function (arg) {
#   a <- arg[1]
#   b <- arg[2]
#   sum(abs(temp$y-a-b*temp$x)) 
# }
# r <- optim( c(0,0), g )$par
# 
# # a = coefficients(lm(y~x))[1]
# # b = coefficients(lm(y~x))[2]
# # previsao_1 = a*x+b
# # lines(eixo_x_frequencias,previsao_1)
# # y = y+1
# # temp$y = temp$y +1 
# # temp$y = temp$y+1
# plot( temp$y~temp$x ,ylim=c(0,1),xlim=c(0,1.2))
# mod_1 = lm(temp$y~temp$x)
# previsao_1 = fitted.values(mod_1)
# # previsao_1 = normalizacao_transformacao_linear(0.0001,1.0,previsao_1)
# lines(temp$x,temp$previsao1,col="blue")
# temp$previsao1 = previsao_1
# temp_norm = normalizeData(temp,type="0_1")
# temp = data.frame(y = temp_norm[,2],x = temp_norm[,1],previsao1= temp_norm[,3])
# 
# mod_padrao <- nlsLM(y ~ f(b,x,a), data = temp,lower=c(a=0,b=0), weights=temp$previsao_1,start = list(a=0,b = 0),algorithm = "LM", control = list(maxiter = 500, tol = 1e-05, minFactor = 0.0009765625, printEval = F, warnOnly = FALSE, trace = T))
# 
# previsao = predict(mod_padrao, list(x = temp$x))
# sse = sum(( previsao-alvo)^2)
# sse
# plot(main = paste("SSE: ",sse),eixo_x_frequencias, temp$y)
# lines(fitted(mod_padrao)~eixo_x_frequencias , col="blue",lwd=1)
# 
# 
# 
# 
# # abline(mod_1, col='red', lty=2)
# abline(r[1], r[2])
# legend( par("usr")[1], par("usr")[4], yjust=1,
#         c("Least Squares", "Least Absolute Values"),
#         lwd=1, lty=c(2,1),
#         col=c(par('fg'),'red'))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
