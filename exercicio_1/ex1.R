
# bibs --------------------------------------------------------------------

library(rio) #importa dados
library(tidyverse) #manipula dados
library(naniar)

getwd() #procura a pasta

setwd("C:/Users/mn_sa/Documents/R/Version control/metodos-quantitativos/exercicio_1") #define a pasta de trabalho

# exercicio 1  ------------------------------------------------------------

ex1 <- import("Ex_1.dta") #importa o arquivo e armazena como objeto

glimpse(ex1) #observa as colunas do banco de dados

#A partir da base Ex1.dta, obtenha: 
        
# a) Salário anual --------------------------------------------------------

sal_ano <- ex1 %>% #cria novo objeto
        mutate(sal_anual = salario_mensal * 12) %>% #cria nova coluna
        select(nome, sobrenome, sal_anual) #seleciona colunas


# b) Variável dummy para gênero (1 se é mulher e 0 se é homem) ------------

dum_gen <- ex1 %>% 
        mutate(dummy = ifelse(genero == "Masculino", 0, 1))
     
# c) Variável dummy se é mulher e maior do que 30 anos de idade -----------

d_m_30 <- ex1 %>% 
        mutate(idade = na_if(idade, "Ignorado"),
               dummy = ifelse(genero == "Feminino" & idade >30, 1, 0)
                ) 
        
# d) Variável de Nome Completo --------------------------------------------

nome_completo <- ex1 %>% 
        mutate(nome_comp = paste(nome, sobrenome))


# e) Dummy de Engenheiro; -------------------------------------------------

dummy_eng <- ex1 %>% 
        mutate(dummy_eng = ifelse(genero = "Masculino" & profissao = "Engenheiro", 1, 0))


# f) Transformar a variável idade em número -------------------------------

ex1$idade <- is.numeric(ex1$idade)


# g) Dummy de Mulher com idade até 30 anos e Engenheira; ------------------

dm_30_eng <- ex1 %>% 
        mutate(dummy = ifelse(genero == "Feminino" & idade < 30 & profissao == "Engenheiro", 1, 0))


# h) Tabela de frequência de Gênero; --------------------------------------

freq_gen <- ex1 %>% 
        group_by(genero) %>% 
        summarise(contagem = n())

# i) Tabela de frequência da Dummy de Mulher; ----------------------------

dum_m <- dum_gen %>% 
        group_by(genero) %>% 
        summarise(contagem = n())

# j) Tabela de frequência cruzada de Gênero e Dummy de Mulher; ------------

freq_cruz <- dum_gen %>% 
        group_by(genero, dummy) %>% 
        summarise(contagem = n()) %>% 
        pivot_wider(values_from = contagem,
                    names_from = genero,
                    values_fill = 0)

# k) Tabela de estatísticas descritivas do Salário Anual; ----------------

summary(sal_ano$sal_anual)

# l) Variável de Família; ------------------------------------------------

var_fam <- ex1 %>% 
        group_by(sobrenome) %>% 
        summarise(familia = n())

# m) Renda anual média dos membros de cada Família; -----------------------

renda_anual_media <- sal_ano %>% 
        group_by(sobrenome) %>% 
        summarise(media = mean(sal_anual, na.rm = TRUE))
        

# n) Excluir a variável de Salário Mensal; --------------------------------

sem_sal_men <- ex1 %>% 
        subset(select = -salario_mensal)

# o) Manter na base apenas as variáveis de Nome Completo, Família, --------



# p) Ordenar as variáveis na sequência descrita no item anterior; ---------



# q) Ordenar as observações por ordem alfabética (do Nome Completo --------



# r) Excluir observações para as quais não temos informação de ida --------



# s) Salvar a nova base de Dados ------------------------------------------




