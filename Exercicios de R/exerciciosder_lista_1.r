
rm(list=ls())

# item A

valor = sample(seq(from = 500, to = 4000, by=100), 100, replace = TRUE)
produto = sample( c("tv", "notebook", "impressora", "smartphone"), prob = c(0.3, 0.2, 0.1, 0.4), size = 100, replace = TRUE)
quantidade = sample(10:4000, 100)
mes = sample(1:12, 100, replace = TRUE)
margem = runif(min = 0.01, max = 0.35, n=100)
ano = sample(c(2017, 2018, 2019), prob = c(0.5, 0.2, 0.3), 100,replace = TRUE)
ls()

# item B

vendas =  data.frame(ano, mes, produto,quantidade, valor, margem)
rm(ano, margem, mes, produto, quantidade, valor)
ls()

# item C/D

vendas$lucro = vendas$quantidade *  vendas$valor * vendas$margem


library(dplyr);
library(tibble);

# item E

vendas %>% group_by(ano) %>% summarise( min(lucro) , mean(lucro), median(lucro), max(lucro), sd(lucro))

# item F

stats_lucro = vendas %>% group_by(ano) %>% summarise( min(lucro) , mean(lucro), median(lucro), max(lucro), sd(lucro))

lucro2017 = vendas %>% filter(ano == 2017)
lucro2018 = vendas %>% filter(ano == 2018)
lucro2019 = vendas %>% filter(ano == 2019)

# item G

hist(lucro2017$lucro)
hist(lucro2018$lucro)
hist(lucro2019$lucro)

boxplot(vendas$lucro ~ vendas$ano)

# item H

tabela = table(vendas$produto)
prop.table(tabela)
margin.table(tabela, 1)

# item I

xtabs(~ano+produto,data=vendas)

# item J

tapply(vendas$lucro,list(vendas$ano, vendas$mes),mean, default=0)

# item K

tapply(vendas$lucro,vendas$ano, summary , default=0)

# item L

tabela2 = data.frame(tapply(vendas$lucro,list(vendas$ano, vendas$mes),mean, default=0))
tabela2$mean <- rowMeans(tabela2[,1:12])
plot( y=tabela2$mean , x=list("2017", "2018", "2019"), type = "l")

# item M

vendas$valor_total = vendas$quantidade * vendas$valor
tapply(vendas$valor_total,list(vendas$produto, vendas$ano), sum, default=0)

vendas <- subset (vendas, select = -valor_total)

# Item N

subset(vendas, margem >= 0.33)

# item O

MaioresMargens = subset(vendas, margem >= 0.34, select = c(ano, mes, produto, margem))

# item P

MenoresMargens = subset(vendas, margem <= 0.02, select = c(ano, mes, produto, margem))

# item Q
MargensExtremas = cbind(MaioresMargens, MenoresMargens)

# item R
vendas <- tibble::rowid_to_column(vendas, "id")


# item S
subset(vendas, margem >= 0.32, select = c(id, produto))

# item T
subset(vendas, margem >= 0.32, select = c(id))

# item U
dim(subset(vendas, margem >= 0.2 & produto=="impressora", select = c(id, produto, margem)))[1]

# item V
write.csv(vendas,"VendasAtualizado.csv", row.names = FALSE)

# item W
vendasaleatorias20 = vendas[sample(1:100, 20), c(2,3,4,8)]

# item X
l1 = c(101, "2020", 1, "tv", 50, 2000, 0.15, 50 * 2000 * 15)
l2 = c(102, "2020", 1, "notebook", 350, 2700, 0.20, 350 * 2700 * 20)

vendas = rbind(vendas, l1, l2)

# item Y
vendas = vendas %>% mutate(produto=recode(produto, `tv`="TV"))


# item Z
write.csv(vendas,"VendasAtualizado2.csv", row.names = FALSE)

