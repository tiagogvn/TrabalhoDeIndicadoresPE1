# Este é o script onde construiremos a base de dados sobre mortalidade para os muni-
#cípios do Rio de Janeiro. Feita a análise exploratória da nossa base de dados, se-
#lecionamos 12 indicadores que consideramos de alguma forma estratégicos para a aná-
#lise da mortalidade no Estado. 
# Aqui, iremos identificar as informações que devemos utilizar como variáveis nos
#indicadores que criamos, renomeá-las para fácil visualização e padronização, e
#calculá-las para cada município.

# O objetivo final é uma base onde cada linha é um Município do Rio de Janeiro, e
# cada coluna um dos indicadores criados, preenchidos com seus valores calculados
#para cada município.

library(geobr) #pacote para extrair código, nome e dados de cada município
library(dplyr) #pacote para manipulação de dados
library(sf)

# Baixar lista de municípios do RJ
mun_rj <- read_municipality(code_muni = "RJ", year = 2023) |>
  st_drop_geometry() |>
  select(code_muni, name_muni)

# Criando base com 12 colunas preenchidas apenas com o nome dos indicadores, e
#todos os municípios do Rio de Janeiro nas linhas
base_indicadores <- mun_rj |>
  mutate(
    TxMFAG23 = NA_real_,
    TxMNDC23 = NA_real_,
    TxMPADR23 = NA_real_,
    TxMMVP23 = NA_real_,
    TxMPMDPNN23 = NA_real_,
    TxMCEPP23 = NA_real_,
    TxFNM23 = NA_real_,
    TxMAT23 = NA_real_,
    TxMLAP23 = NA_real_,
    TxMDAF23 = NA_real_,
    PMDCv23 = NA_real_,
    PMCMD23 = NA_real_
  )

library(microdatasus) #daqui vamos extrair a quantidade de nascidos vivos por município do Rio de Janeiro em 2023, e outras informações de mortalidade

#Buscando do datasus, do sistema SINASC as quantidades de nascidos vivos
nascidos_2023 <- fetch_datasus(
  year_start = 2023,
  year_end = 2023,
  uf = "RJ",
  information_system = "SINASC"
) %>%
  process_sinasc()

# Agrupando por município:
nv_mun <- nascidos_2023 %>%
  group_by(code_muni = CODMUNRES) %>% 
  summarise(nascidos_vivos = n(), .groups = "drop")

nv_mun #o data frame resultante possui o código do município, e o número de nascidos vivos nesse município em 2023

library(readr) #agora iremos ler a planilha com as estimativas de população residente masculina, feminina e total para 2023 dos municípios do Rio de Janeiro, adquirida do estudo de estimativas populacionais pactuadas pela SES/RJ
library(tidyr)
library(stringr)

df_raw <- readr::read_csv2(
  "pop_populacao_ripsa202417635071428.csv",
  locale = locale(encoding = "Latin1")
)

df_raw %>% head() #data frame resultante tem nome do município nas linhas, e estimativas de população residente masculina, feminina e total para 2023 nas colunas

library(stringi) #vamos facilitar os cálculos adicionando o código de município no data grame df_raw

#Padronizando textos para garantir correspondência
df_clean <- df_raw %>%
  mutate(
    muni_norm = Município %>%
      str_to_lower() %>%
      stri_trans_general("Latin-ASCII")
  )

muni_rj <- geobr::read_municipality(code_muni = "RJ", year = 2023) %>%
  select(code_muni, name_muni)

muni_clean <- muni_rj %>%
  mutate(
    muni_norm = name_muni %>%
      str_to_lower() %>%
      stri_trans_general("Latin-ASCII")
  )

#Juntando os códigos com o df_raw
df_final <- df_clean %>%
  left_join(
    muni_clean %>% select(code_muni, muni_norm),
    by = "muni_norm"
  ) %>%
  select(-muni_norm,-geom)

df_final <- df_final %>% filter(Município != "Total") #retirando linha do total
df_final
#Agora que temos todos os valores que serão utilizados nos denominadores das
#fórmulas dos indicadores, basta calcular cada indicador pegando a informação
#de mortalidade.
#Começando pelo indicador de Mortalidade Feminina por Doenças do Aparelho Geniturinário:

#Baixando SIM 2023 para o RJ, que contém dados que serão utilizados para o cálculo de quase todos os indicadores
sim_2023 <- fetch_datasus(
  year_start = 2023,
  year_end = 2023,
  uf = "RJ",
  information_system = "SIM-DO",
  vars = c("SEXO", "CAUSABAS", "CODMUNRES")
)

#Calculando quantidade de óbitos por N00-99 nos municípios do Rio de Janeiro em 2023
obitos_genit_fem <- sim_2023 %>%
  filter(
    SEXO == "2",                           # apenas mulheres
    str_detect(CAUSABAS, "^N[0-9]{2}")     # CID N00 a N99
  ) %>%
  count(CODMUNRES, name = "obitos_genit_fem") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_genit_fem = 0)) %>%
  arrange(CODMUNRES)

obitos_genit_fem <- obitos_genit_fem %>% #retirando indivíduos de município de residência desconhecido
  filter(CODMUNRES != "330000")

obitos_genit_fem #o valor dessa variável no município m será o numerador da fórmula do indicador TxMFAG23 para o município em questão
                 #já o numerador, para esse indicador, será o valor da coluna "Feminino" do data frame df_final no município m
                 #por fim, o resultado dessa divisão será multiplicado pelo fator de multiplicação

#Calculando:
tabela_ind1 <- (obitos_genit_fem$obitos_genit_fem / df_final$Feminino) * 1e5
tabela_ind1

base_indicadores$TxMFAG23 <- tabela_ind1 #armazenando na base de indicadores


#Agora calcularemos o indicador de Mortalidade Feminina por Neoplasias Malignas:
#Calculando quantidade de óbitos por C00-99 nos municípios do Rio de Janeiro em 2023
obitos_neopl_fem <- sim_2023 %>%
  filter(
    SEXO == "2",                           # apenas mulheres
    str_detect(CAUSABAS, "^C[0-9]{2}")     # CID C00 a C99
  ) %>%
  count(CODMUNRES, name = "obitos_neopl_fem") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_neopl_fem = 0)) %>%
  arrange(CODMUNRES)

obitos_neopl_fem <- obitos_neopl_fem %>% #retirando indivíduos de município de residência desconhecido
  filter(CODMUNRES != "330000")

obitos_neopl_fem #o valor dessa variável no município m será o numerador da fórmula do indicador TxFNM23 para o município em questão
#já o numerador, para esse indicador, será o valor da coluna "Feminino" do data frame df_final no município m
#por fim, o resultado dessa divisão será multiplicado pelo fator de multiplicação

#Calculando:
tabela_ind7 <- (obitos_neopl_fem$obitos_neopl_fem / df_final$Feminino) * 1e5
tabela_ind7

base_indicadores$TxFNM23 <- tabela_ind7 #armazenando na base de indicadores


#Agora, o indicador de mortalidade por disparo de arma de fogo

#Obtendo número de mortes por armas de fogo por município em 2023
obitos_armas <- sim_2023 %>%
  filter(str_detect(CAUSABAS, "^W3[2-4]|^X7[2-4]|^Y2[2-4]|^X9[3-5]")) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_armas = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_armas = 0)) %>%
  arrange(CODMUNRES)

obitos_armas <- obitos_armas %>% #retirando indivíduos de município de residência desconhecido
  filter(CODMUNRES != "330000")

obitos_armas #numerador do indicador

tabela_ind10 <- (obitos_armas$obitos_armas / df_final$Total) * 1e5
tabela_ind10

base_indicadores$TxMDAF23 <- tabela_ind10







