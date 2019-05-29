# Pacotes -----------------------------------------------------------------

library(tidyverse)

# Base de dados -----------------------------------------------------------

imdb <- read_rds("dados/imdb.rds")

# filter ------------------------------------------------------------------

# exemplo 1
imdb %>% filter(nota_imdb > 9)

# exemplo 2
filmes_bons <- imdb %>% filter(nota_imdb > 9)
filmes_bons

# exemplo 3
filmes_bons <- filmes_bons %>% filter(orcamento < 1000000)
filmes_bons

# exemplo 4 - Relembrando as comparações com o R

1 == 1
"a" == "b"

# Cuidado
sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1

near(sqrt(2) ^ 2,  2)
near(1 / 49 * 49, 1)

bons <- imdb %>% filter(nota_imdb > 9)


# exercício 1
# Criar uma variável chamada `filmes_baratos` com filmes com orçamento menor do 
# que 1 milhão de dólares.



# exemplo 5
# operadores lógicos

imdb %>% filter(ano > 2010 & nota_imdb > 8.5)
imdb %>% filter(orcamento < 100000 & receita > 1000000)

imdb %>% filter(receita > orcamento)
imdb %>% filter(receita > orcamento + 500000000)
imdb %>% filter(receita > orcamento + 500000000 | nota_imdb > 9)

imdb %>% filter(ano > 2010)
imdb %>% filter(!ano > 2010)
imdb %>% filter(!receita > orcamento)

# exercício 2
# Criar um objeto chamado bons_baratos com filmes que tiveram nota no imdb 
# maior do que 8.5 e um orcamento menor do que 1 milhão de dólares.

# exercício 3
# Criar um objeto chamado curtos_legais com filmes de até 1h30 e nota no imdb
# maior do que 8.5.

# exercício 4
# Criar um objeto antigo_colorido com filmes de antes de 1940 que são 
# coloridos. Crie também um objeto antigo_bw com filmes antigos que não são coloridos.

# exercício 5
# Criar um objeto ww com filmes do Wes Anderson ou do Woody Allen.

# exemplo 6
# %in%

pitts <- imdb %>% filter(ator_1 %in% c('Angelina Jolie Pitt', "Brad Pitt"))

# exercicio 6
# Refaça o exercício 5 usando o %in%.

# exemplo 7
# Relembrando as operações com NA

NA > 5

10 == NA

NA + 10

NA / 2

NA == NA

# Seja x a idade de Maria. Não sabemos a idade de Maria:
x <- NA

# Seja y a idade de João. Não sabemos a idade de João:
y <- NA

# Maria e João têm a mesma idade?
x == y
#> [1] NA
# Não sabemos.

is.na(x)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

imdb %>% filter(is.na(orcamento))

# exercício 7
# Identifique os filmes que não possuem informação tanto de receita quanto de orcamento
# e salve em um objeto com nome sem_info.

# exemplo 8
# str_detect

imdb %>% filter(str_detect(generos, "Action"))

# exercício 8
# Salve em um objeto os filmes de Ação e Comédia com nota no imdb maior do que 8.


# arrange -----------------------------------------------------------------

# exemplo 1

imdb %>% arrange(orcamento)

# exemplo 2

imdb %>% arrange(desc(orcamento))

# exemplo 3

imdb %>% arrange(desc(ano), titulo)

# exercício 1
# Ordene os filmes em ordem crescente de ano e decrescente de lucro e salve 
# em um objeto chamado filmes_ordenados

# exemplo 4
# NA

df <- tibble(x = c(NA, 2, 1), y = c(1, 2, 3))

df %>% arrange(x)
df %>% arrange(!is.na(x), x)

# exemplo 5

imdb %>% filter(ano == 2010) %>% arrange(desc(orcamento))

# exercício 2 
# Ordene por ordem decrescente do orçamento os filmes de um diretor a sua escolha.
# Salve o resultado em um objeto chamado diretor_ordenado

# select ------------------------------------------------------------------

# exemplo 1

imdb %>% select(titulo, ano, orcamento)

# exemplo 2 

imdb %>% select(starts_with("ator"))

# exemplo 3

imdb %>% select(-starts_with("ator"), -titulo)

# exercício 1
# Crie uma tabela com apenas as colunas titulo, diretor, e orcamento. Salve em um
# objeto chamado imdb_simples.

# exercício 2
# Remova as colunas ator_1, ator_2 e ator_3 de três formas diferentes. Salve em um
# objeto chamado imdb_sem_ator.

# exercício 3
# Crie uma tabela apenas com filmes do Woody Allen e as colunas titulo e ano
# ordenada por ano.


# mutate ------------------------------------------------------------------

# exemplo 1

imdb %>% mutate(duracao = duracao/60)

# exemplo 2

imdb %>% mutate(duracao_horas = duracao/60)

# exercício 1
# Crie uma variável chamada lucro. Salve em um objeto chamado imdb_lucro.

# exercicio 2
# Modifique a variável lucro para ficar na escala de milhões de dólares.

# exercício 3
# Filtre apenas os filmes com prejuízo maior do que 3 milhões de dólares. 
# Deixe essa tabela ordenada com o maior prejuízo primeiro. Salve o resultado em 
# um objeto chamado filmes_prejuizo.

# exemplo 3
# gêneros

# install.packages("gender")
library(gender)

gender("daniel", method = "kantrowitz")
gender("william", method = "kantrowitz")
gender("nina", method = "kantrowitz")

obter_genero <- function(nome) {
  gender(nome, method = "kantrowitz")$gender
}

obter_genero("william")

imdb <- imdb %>% 
  mutate(
    diretor_primeiro_nome = str_extract(diretor, ".* ") %>% str_trim()
  )

imdb_generos <- imdb %>%
  distinct(diretor_primeiro_nome) %>%
  mutate(
    genero = obter_genero(diretor_primeiro_nome)
  )

# saveRDS(imdb_generos, "dados/imdb_generos.rds")
imdb_generos <- read_rds("dados/imdb_generos.rds")

# https://github.com/meirelesff/genderBR

# group_by + summarise ----------------------------------------------------

# exemplo 1

imdb %>% group_by(ano)

# exemplo 2

imdb %>% 
  group_by(ano) %>% 
  summarise(qtd_filmes = n())

# exemplo 3

imdb %>% 
  group_by(diretor) %>% 
  summarise(qtd_filmes = n())

# exercício 1
# Crie uma tabela com apenas o nome dos diretores com mais de 10 filmes.

# exercício 2
# Crie uma tabela com a receita média e mediana dos filmes por ano.

# exercício 3
# Crie uma tabela com a nota média do imdb dos filmes por tipo de classificacao.

# exemplo 4

imdb %>%
  filter(str_detect(generos, "Action"), !is.na(diretor)) %>%
  group_by(diretor) %>%
  summarise(qtd_filmes = n()) %>%
  arrange(desc(qtd_filmes))

# exemplo 5

imdb %>% 
  filter(ator_1 %in% c("Brad Pitt", "Angelina Jolie Pitt")) %>%
  group_by(ator_1) %>%
  summarise(orcamento = mean(orcamento), receita = mean(receita), qtd = n())

# left join ---------------------------------------------------------------

# exemplo 1

imdb_generos2 <- imdb %>%
  left_join(imdb_generos, by = "diretor_primeiro_nome")

# exemplo 2

depara_cores <- tibble(
  cor = c("Color", "Black and White"),
  cor2 = c("colorido", "pretoEbranco")
)

imdb_cor <- left_join(imdb, depara_cores, by = c("cor"))

# exercicio 1
# Calcule a média dos orçamentos e receitas para filmes feitos por
# genero do diretor.


# gather ------------------------------------------------------------------

# exemplo 1

imdb_gather <- gather(imdb, "importancia_ator", "nome_ator", starts_with("ator"))

# spread ------------------------------------------------------------------

# exemplo 1

imdb_generos2 %>% 
  group_by(genero, ano) %>% 
  summarise(media_orcamento = mean(orcamento, na.rm = TRUE)) %>% 
  spread(genero, media_orcamento) %>% 
  filter(ano > 2000)

