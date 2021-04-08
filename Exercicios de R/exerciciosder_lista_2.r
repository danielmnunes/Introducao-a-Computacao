rm(list=ls())

# --------------------  Questão 1 ------------------------------

# item a) *******************
n =  sample(4:20, 1)
x = rnorm(n)
soma = 0

for (y in x){
  soma =  y^2 
}

item_1a = soma - (sum(x)^2)/n
cat("Questão1, item a) ", item_1a)

# item b) *******************
item_b <- function(x) {
  soma = 0
  
  for (y in x){
    soma =  y^2 
  }
  
  sol = soma - (sum(x)^2)/n
  
  return(sol)
}

cat("Questão1, item b)" , item_b(x))

# item c) *******************
item_c <- function(x) {

    if( length(x) > 20 ){
        return("Vetor x não adequado.")
    }

    soma = 0

    for (y in x){
        if (y < -10 | y >= 10  ){
            return("Vetor x não adequado.")
        }
        soma =  y^2 
    }

    sol = soma - (sum(x)^2)/n
  
    return(sol)
}
cat("Questão1, item c)" , item_c(x))


# item d) *******************
item_d <- function(x) {
  
  if( length(x) > 20 | length(x) <= 4  ){
    return("Vetor x não adequado.")
  }
  
  soma = 0
  for (y in x){
    if (y < -10 | y >= 10  ){
      return("Vetor x não adequado.")
    }
    soma =  abs(y)^3 
  }
  
  p = (prod(x)^2)/ sqrt(n)
  q = log10(n) + gamma(n)
  sol1 = (soma - p)/q
  
  
  
  sol2 = log(n) + exp( sqrt(n) )
  
  return(c(sol1, sol2))
}

cat("Questão1, item d)" , item_d(x))


# item e) *******************
item_e <- function(x, y, plotar) {
  c = cor(x, y)
  if (plotar){
    p = plot(x, y)
  }
  return( c )
}

a = c(1,2,3)
b = c(6,5,4)
d = c(0.000001,0.01,0)

ex1 = item_e(a, b, TRUE)
ex2 = item_e(a, d, FALSE)
ex3 = item_e(a, a, FALSE)

cat("alta correlação negativa", ex1, "\n")
cat("baixa correlação negativa", ex2, "\n")
cat("alta correlação positiva", ex3)


# --------------------  Questão 2 ------------------------------

item_2a <- function(m) {
  mcol = c()
  mrow = c()
  
  d1 = dim(m)[1]
  d2 = dim(m)[2]
  
  for (i in 1:d1){
    mrow <- append(mrow, mean(m[i, ]))
    
  }
  
  
  for (j in 1:d2){
    mcol <- append(mcol, mean(m[,j]))
    
  }
  
  media = mean(m)
  
  y = cbind(m, mrow)
  y = rbind(y, c(mcol, media))
  
  
  cat("média das linhas", round(mrow, digits=2) , "\n")
  cat("média das colunas", round(mcol, digits=2), "\n")
  cat("média geral", round(media, digits=2), "\n")
  cat("\n")
  cat("Y modificada - item C")
  print(round(y, digits=2))
  
}

m = matrix( c(2,3,4,2,5,6,7,2,4,9,7,8), 3, 4 )

# itens a e c
item_2a(m)

# item b
print(rowMeans(m))
print(colMeans(m))
print(mean(m))



# --------------------  Questão 3 ------------------------------

# item a) *******************
item_3a <- function(n) {
  filhos = c(0,1,2,3,4)
  porcentagem = c(0.1,0.2,0.3,0.25,0.15)
  amostra = sample( filhos, prob = porcentagem, size = n, replace = TRUE)
  tabela = table(amostra)
  barplot(tabela)
  
  return(tabela)
  
}

n = 10
print(item_3a(n))


# item b) *******************
item_3b <- function() {
  sol = data.frame(item1 = c(0), item2 = c(0) )
  cont = 1
  for (i in 0:4){
    for (j in 0:4){
      sol[cont, ]= c(i,j)
      cont = cont + 1
    }
  }
  
  return(sol)
  
}

print(item_3b())


# --------------------  Questão 4 ------------------------------
questao_4 <- function(n) {
  # populacao = norm <- rnorm(10000, 10, 3)
  populacao = read.csv(file = 'base1.csv')
  B=1000
  amostras = matrix(ncol = n+2, nrow = B)
  for (i in 0:B){
    amostra = sample( populacao$x,  size = n, replace = TRUE)
    amostras[i, ] = c(amostra, mean(amostra), var(amostra))
  }

  old.par = par(mfrow = c(2, 1))
  hist(populacao$x)
  hist(amostras[, n+1])
  
}

# itens a, b, c) *******************
questao_4(1000)


# itens d) *******************

# para n = 10
questao_4(10)

# para n = 100
questao_4(100)


# para n = 500
questao_4(500)


# itens e) *******************

# A questão 4 ilutra o teorema do Teorema central do limite, Se a variável de
# interesse não segue uma distribuição normal na população (ou não
# se sabe qual é a sua distribuição) a distribuição amostral das médias de amostras
# aleatórias retiradas desta população será normal se o tamanho destas amostras for
# suficientemente grande, com uma média igual à média populacional e uma variância igual
# à variância populacional dividida pelo tamanho da amostra.


