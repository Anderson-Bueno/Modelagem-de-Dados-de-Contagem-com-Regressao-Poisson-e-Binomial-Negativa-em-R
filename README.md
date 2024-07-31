# Case de Sucesso: Modelagem de Dados de Contagem com Regressão Poisson e Binomial Negativa em R

Neste estudo de caso, utilizamos técnicas de regressão Poisson e Binomial Negativa para modelar dados de contagem, especificamente a quantidade de atrasos em voos. O objetivo é entender os fatores que influenciam os atrasos e prever a quantidade de atrasos com base em variáveis explicativas. Utilizaremos a linguagem R para realizar todas as etapas do processo, desde o carregamento dos dados até a avaliação do modelo.

## Instalação e Carregamento de Pacotes Necessários

Primeiro, instalamos e carregamos os pacotes necessários:

# Pacotes utilizados
pacotes <- c("plotly", "tidyverse", "knitr", "kableExtra", "fastDummies", "reshape2",
             "lmtest", "splines", "jtools", "questionr", "MASS", "pscl", "overdisp")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()
  }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}


## Distribuição Poisson - Parte Conceitual

Estabelecemos funções para a distribuição Poisson com diferentes valores de lambda e plotamos essas funções:


# Funções da distribuição Poisson
poisson_fun_1 <- function(x) {
  lambda <- 1
  (exp(-lambda) * lambda ^ x) / factorial(x)
}

poisson_fun_4 <- function(x) {
  lambda <- 4
  (exp(-lambda) * lambda ^ x) / factorial(x)
}

poisson_fun_10 <- function(x) {
  lambda <- 10
  (exp(-lambda) * lambda ^ x) / factorial(x)
}

# Plotagem das funções
ggplotly(
  ggplot(data.frame(x = 0:20), aes(x = x)) +
    stat_function(fun = poisson_fun_1, size = 1.2, aes(color = "Lambda igual a 01")) +
    stat_function(fun = poisson_fun_4, size = 1.2, aes(color = "Lambda igual a 04")) +
    stat_function(fun = poisson_fun_10, size = 1.2, aes(color = "Lambda igual a 10")) +
    scale_color_manual("Valores de lambda", values = c("darkorchid", "orange", "black")) +
    labs(y = "Probabilidades", x = "m") +
    theme_bw()
)

## Carregamento da Base de Dados

Carregamos os dados da base `atrasos_poisson.RData`:


# Carregando a base de dados
load(file = "atrasos_poisson.RData")


## Observação da Base de Dados

Visualizamos a base de dados e calculamos estatísticas descritivas:


# Visualizando a base de dados
atrasos_poisson %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 12)

# Estrutura da base de dados
glimpse(atrasos_poisson)

# Estatísticas descritivas univariadas da base de dados
summary(atrasos_poisson)

# Tabela de frequências da variável dependente
freq(atrasos_poisson$atrasos) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = T, font_size = 12)

# Diagnóstico preliminar da presença da superdispersão na variável dependente
atrasos_poisson %>%
  summarise(média = mean(atrasos_poisson$atrasos), variância = var(atrasos_poisson$atrasos)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = T, font_size = 12)

# Visualização da distribuição da variável dependente
ggplotly(
  atrasos_poisson %>%
    ggplot() +
    geom_histogram(aes(x = atrasos, fill = ..count..), bins = 5, color = "black") +
    labs(x = "Quantidade de Atrasos", y = "Frequência") +
    scale_fill_gradient("Contagem", low = "darkorchid", high = "orange") +
    theme_bw()
)


## Estimação do Modelo Poisson

Estimamos um modelo de regressão Poisson para os dados:

# Estimando o modelo de regressão Poisson
modelo_poisson <- glm(formula = atrasos ~ dist + sem + per, data = atrasos_poisson, family = "poisson")

# Parâmetros do modelo_poisson
summary(modelo_poisson)

# Outro modo de apresentar os outputs do modelo
summ(modelo_poisson, confint = TRUE, digits = 3, ci.width = 0.95)
export_summs(modelo_poisson, scale = FALSE, digits = 6)

# Extração do valor do LL
logLik(modelo_poisson)

# LR Test - função lrtest do pacote lmtest
# (likelihood ratio test para comparação dos LL's entre modelos)
lrtest(modelo_poisson)


## Teste de Cameron & Trivedi para a Detecção da Superdispersão

Realizamos o teste de Cameron & Trivedi para verificar a presença de superdispersão nos dados:


# Acrescentando os fitted values de lambda à base de dados
atrasos_poisson$lambda <- modelo_poisson$fitted.values

# Criando a nova variável dependente Y* para o modelo auxiliar OLS
atrasos_poisson$ystar <- (((atrasos - lambda) ^ 2) - atrasos) / lambda

# Estimando o modelo auxiliar OLS sem o intercepto
modelo_auxiliar_OLS <- lm(formula = ystar ~ 0 + lambda, data = atrasos_poisson)

# Parâmetros do modelo auxiliar OLS
summary(modelo_auxiliar_OLS)

# Teste de superdispersão
overdisp(x = atrasos_poisson, dependent.position = 2, predictor.position = 3:5)


## Predições

Realizamos predições com o modelo Poisson:


# Fazendo predições
predict(modelo_poisson, data.frame(dist = 12, sem = 17, per = "tarde"), type = "response")


## Plotagens

Visualizamos a evolução da quantidade prevista de atrasos:


# Função de suavização da curva Poisson
poisson_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "poisson"), ...)
}

# Plotagem
ggplotly(
  atrasos_poisson %>%
    ggplot(aes(x = dist, y = lambda)) +
    geom_point(color = "orange", alpha = .7) +
    poisson_smooth(aes(color = "Fitted Values"), formula = y ~ bs(x, df = 6), se = F) +
    scale_color_manual("Legenda:", values = "darkorchid") +
    labs(x = "Distância até a Escola", y = "Lambda") +
    theme_bw()
)


## Comparação entre Estimação Poisson e Log-Linear

Comparamos os modelos Poisson e log-linear:


# Criando a nova variável dependente ln(atrasos)
atrasos_poisson$lnatrasos <- log(atrasos)

# Corrigindo valores que tendem ao infinito negativo
atrasos_poisson$lnatrasos <- ifelse(atrasos_poisson$lnatrasos == -Inf, yes = NA, no = atrasos_poisson$lnatrasos)

# Estimando o modelo log-linear
modelo_loglinear_OLS <- lm(formula = lnatrasos ~ dist + sem + per, data = atrasos_poisson)

# Parâmetros da estimação log-linear
summary(modelo_loglinear_OLS)

# Procedimento Stepwise
step_loglinear_OLS <- step(object = modelo_loglinear_OLS, k = qchisq(p = 0.10, df = 1, lower.tail = FALSE))

# Parâmetros do modelo step_loglinear_OLS
summary(step_loglinear_OLS)

# Integrando yhat do modelo log-linear à base de dados atrasos_poisson:
atrasos_poisson <- atrasos_poisson %>%
  mutate(lnatrasos = ifelse(is.na(lnatrasos), yes = 0, no = lnatrasos),
         yhat = step_loglinear_OLS$coefficients[[1]] +
           step_loglinear_OLS$coefficients[[2]] * dist +
           step_loglinear_OLS$coefficients[[3]] * as.numeric(per),
         expyhat = exp(yhat))

# Gráfico para verificação das diferenças dos valores previstos
ggplotly(
  atrasos_poisson %>%
    ggplot() +
    geom_smooth(aes(x = atrasos, y = lambda, color = "Estimação Poisson"), method = "lm", se = F) +

    geom_smooth(aes(x = atrasos, y = expyhat, color = "Estimação Log-Linear OLS"), method = "lm", se = F) +
    geom_smooth(aes(x = atrasos, y = atrasos, color = "Valores Observados"), method = "lm", linetype = "dotted") +
    scale_color_manual("Legenda:", values = c("orange", "darkorchid", "black")) +
    labs(x = NULL, y = NULL) +
    theme_bw()
)

# Gráfico com loess fit, scatter plots e ICs por modelo
ggplotly(
  atrasos_poisson %>%
    ggplot() +
    geom_smooth(aes(x = atrasos, y = lambda, color = "Estimação Poisson"), method = "loess", se = T) +
    geom_point(aes(x = atrasos, y = lambda), color = "darkorchid") +
    geom_smooth(aes(x = atrasos, y = expyhat, color = "Estimação Log-Linear OLS"), method = "loess", se = T) +
    geom_point(aes(x = atrasos, y = expyhat), color = "orange") +
    geom_smooth(aes(x = atrasos, y = atrasos, color = "Valores Observados"), method = "lm", linetype = "dotted") +
    scale_color_manual("Legenda:", values = c("orange", "darkorchid", "black")) +
    labs(x = NULL, y = NULL) +
    theme_bw()
)


## Distribuição Binomial Negativa do Tipo 2 - Parte Conceitual

Estabelecemos funções para a distribuição Binomial Negativa do Tipo 2 e plotamos essas funções:


# Funções da distribuição NB2
bneg_fun_p2_d2 <- function(x) {
  psi <- 2
  delta <- 2
  ((delta ^ psi) * (x ^ (psi - 1)) * (exp(-x * delta))) / factorial(psi - 1)
}

bneg_fun_p3_d1 <- function(x) {
  psi <- 3
  delta <- 1
  ((delta ^ psi) * (x ^ (psi - 1)) * (exp(-x * delta))) / factorial(psi - 1)
}

bneg_fun_p3_d05 <- function(x) {
  psi <- 3
  delta <- 0.5
  ((delta ^ psi) * (x ^ (psi - 1)) * (exp(-x * delta))) / factorial(psi - 1)
}

# Plotagem das funções
ggplotly(
  ggplot(data.frame(x = 1:20), aes(x = x)) +
    stat_function(fun = bneg_fun_p2_d2, size = 1.2, aes(color = "Psi igual a 02 e Delta igual a 02")) +
    stat_function(fun = bneg_fun_p3_d1, size = 1.2, aes(color = "Psi igual a 03 e Delta igual a 01")) +
    stat_function(fun = bneg_fun_p3_d05, size = 1.2, aes(color = "Psi igual a 03 e Delta igual a 0,5")) +
    scale_color_manual("Valores de psi e de delta", values = c("darkorchid", "orange", "black")) +
    labs(y = "Probabilidades", x = "v") +
    theme_classic()
)

## Conclusão

Este estudo de caso demonstrou como técnicas de regressão Poisson e Binomial Negativa podem ser utilizadas para modelar dados de contagem. Através da integração de dados, limpeza, modelagem e avaliação, fomos capazes de gerar insights valiosos para a tomada de decisões estratégicas.

### Pontos Importantes

1. **Introdução**: Fornece uma visão geral do projeto e sua importância.
2. **Instalação e Carregamento de Pacotes Necessários**: Descreve a instalação e carregamento dos pacotes necessários.
3. **Distribuição Poisson - Parte Conceitual**: Detalha a criação de funções para a distribuição Poisson e sua plotagem.
4. **Carregamento da Base de Dados**: Descreve o carregamento dos dados.
5. **Observação da Base de Dados**: Visualização e estatísticas descritivas dos dados.
6. **Estimação do Modelo Poisson**: Explica a estimação do modelo de regressão Poisson.
7. **Teste de Cameron & Trivedi para a Detecção da Superdispersão**: Descreve a detecção de superdispersão nos dados.
8. **Predições**: Realiza predições com o modelo Poisson.
9. **Plotagens**: Visualiza a evolução da quantidade prevista de atrasos.
10. **Comparação entre Estimação Poisson e Log-Linear**: Compara os modelos Poisson e log-linear.
11. **Distribuição Binomial Negativa do Tipo 2 - Parte Conceitual**: Detalha a criação de funções para a distribuição Binomial Negativa do Tipo 2 e sua plotagem.
12. **Conclusão**: Reflete sobre a experiência e o impacto do projeto.
