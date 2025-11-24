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
nascidos_vivos <- nascidos_2023 %>%
  group_by(code_muni = CODMUNRES) %>% 
  summarise(nascidos_vivos_col = n(), .groups = "drop")

nascidos_vivos <- nascidos_vivos %>% #retirando indivíduos de município de residência desconhecido
  filter(code_muni != "330000")

nascidos_vivos #o data frame resultante possui o código do município, e o número de nascidos vivos nesse município em 2023

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


#
#Indicador da Mortalidade Neonatal por Deformidades Cromossômicas em 2023 a cada 1000 nascidos vivos

#Incluindo apenas as causas pertinentes
causas_cromossomicas <- "^(Q9[0-3]|Q9[5-9])"

Anomalias_cromossomicas_NCOP <- sim_2023 %>%
  filter(
    str_detect(CAUSABAS, causas_cromossomicas)
  ) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_def_crom = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_def_crom = 0)) %>%
  arrange(CODMUNRES)

Anomalias_cromossomicas_NCOP <- Anomalias_cromossomicas_NCOP %>%
  filter(CODMUNRES != "330000")

Anomalias_cromossomicas_NCOP

tabela_ind2 <- (Anomalias_cromossomicas_NCOP$obitos_def_crom / nascidos_vivos$nascidos_vivos_col) * 1e3
tabela_ind2

base_indicadores$TxMNDC23 <- tabela_ind2 #armazenando na base de indicadores


#
#Indicador da Mortalidade Neonatal por Deformidades Cromossômicas em 2023 a cada 1000 nascidos vivos

#Incluindo apenas as causas pertinentes
causas_respiratorias <- "^^(J0[0-6]|J0[9]|J1[0-8]|J2[0-2]|J3[0-9]|J4[0-7]|J6[0-9]|J8[0-4]|J9[0-9])"

Doenças_do_aparelho_respiratorio <- sim_2023 %>%
  filter(
    str_detect(CAUSABAS, causas_respiratorias)
  ) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_respiratorios = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_respiratorios = 0)) %>%
  arrange(CODMUNRES)

Doenças_do_aparelho_respiratorio <- Doenças_do_aparelho_respiratorio %>%
  filter(CODMUNRES != "330000")

Doenças_do_aparelho_respiratorio

tabela_ind3 <- (Doenças_do_aparelho_respiratorio$obitos_respiratorios / nascidos_vivos$nascidos_vivos_col) * 1e3
tabela_ind3

base_indicadores$TxMPADR23 <- tabela_ind3 #armazenando na base de indicadores


#
#Taxa de Mortalidade Masculina em Vias Públicas em 2023 a cada 100.000 mil habitantes
sim_2023 <- fetch_datasus(
  year_start = 2023,
  year_end = 2023,
  uf = "RJ",
  information_system = "SIM-DO", 
  vars = c("SEXO", "CAUSABAS", "CODMUNRES","LOCOCOR") #adicionando local de ocorrência às variáveis no sim_2023
)

obitos_masc_vias <- sim_2023 %>%
  filter(
    SEXO == "1",    #homens
    LOCOCOR == "3"  #local de ocorrência
  ) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_masc_vias = n(), .groups = "drop") %>%
  arrange(CODMUNRES)

obitos_masc_vias <- obitos_masc_vias %>%
  filter(CODMUNRES != "330000")

obitos_masc_vias
sum(obitos_masc_vias$obitos_masc_vias)

tabela_ind4 <- (obitos_masc_vias$obitos_masc_vias / df_final$Total) * 1e5
tabela_ind4

base_indicadores$TxMMVP23 <- tabela_ind4 #armazenando na base de indicadores


#
#Indicador da Mortalidade de 0-27 dias por Doenças do Período Neo-Natal em 2023 

#Incluindo apenas as causas pertinentes
causas_perinatal <- "^(P0[0-4]|P0[5-8]|P1[0-5]|P2[0-9]|P3[5-9]|P5[0-9]|P6[0-1]|P7[0-4]|P9[0-6])"

obitos_doen_neo <- sim_2023 %>%
  filter(
    str_detect(CAUSABAS, causas_perinatal)
  ) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_doen_neo = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_doen_neo = 0)) %>%
  arrange(CODMUNRES)

obitos_doen_neo <- obitos_doen_neo %>%
  filter(CODMUNRES != "330000")

obitos_doen_neo

tabela_ind5 <- (obitos_doen_neo$obitos_doen_neo / nascidos_vivos$nascidos_vivos_col) * 1e3
tabela_ind5

base_indicadores$TxMPMDPNN23 <- tabela_ind5 #armazenando na base de indicadores


#
#Indicador de mortalidade por causas externas na população parda
sim_2023 <- fetch_datasus(
  year_start = 2023,
  year_end = 2023,
  uf = "RJ",
  information_system = "SIM-DO",
  vars = c("SEXO", "CAUSABAS", "CODMUNRES","LOCOCOR","RACACOR") #adicionando variável raça ao sim_2023
)

obitos_ext_parda <- sim_2023 %>%
  filter(
    str_detect(CAUSABAS, "^[V-Y][0-9]{2}"),
    RACACOR == "4"
  ) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_ext_parda = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_ext_parda = 0)) %>% #garantindo que se houver município com 0 ocorrências de óbito pelas dependências específicas, será incluído no data frame
  arrange(CODMUNRES)

obitos_ext_parda <- obitos_ext_parda %>%
  filter(CODMUNRES != "330000")

tabela_ind6 <- (obitos_ext_parda$obitos_ext_parda / df_final$Total) * 1e5
tabela_ind6

base_indicadores$TxMCEPP23 <- tabela_ind6 #armazenando na base de indicadores


#
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

obitos_neopl_fem <- obitos_neopl_fem %>%
  filter(CODMUNRES != "330000")

obitos_neopl_fem #o valor dessa variável no município m será o numerador da fórmula do indicador TxFNM23 para o município em questão
#já o numerador, para esse indicador, será o valor da coluna "Feminino" do data frame df_final no município m
#por fim, o resultado dessa divisão será multiplicado pelo fator de multiplicação

#Calculando:
tabela_ind7 <- (obitos_neopl_fem$obitos_neopl_fem / df_final$Feminino) * 1e5
tabela_ind7

base_indicadores$TxFNM23 <- tabela_ind7 #armazenando na base de indicadores


#
#Taxa de mortalidade por acidentes de trânsito em 2023 a cada 100 mil habitantes
causas_transito <- "^V(0[1-9]|[1-9][0-9])"

Acidentes_de_Transporte <- sim_2023 %>%
  filter(
    str_detect(CAUSABAS, causas_transito)
  ) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_acidentes_transito = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_acidentes_transito = 0)) %>%
  arrange(CODMUNRES)

Acidentes_de_Transporte <- Acidentes_de_Transporte %>%
  filter(CODMUNRES != "330000")

Acidentes_de_Transporte

tabela_ind8 <- (Acidentes_de_Transporte$obitos_acidentes_transito / df_final$Total) * 1e5
tabela_ind8

base_indicadores$TxMAT23 <- tabela_ind8 #armazenando na base de indicadores


#
#Taxa de mortalidade por lesões auto provocadas em 2023 a cada 100 mil habitantes
causas_auto_prov <- "^X(6[0-9]|7[0-9]|8[0-4])"

Lesoes_autoprovocadas_intencionalmente <- sim_2023 %>%
  filter(
    str_detect(CAUSABAS, causas_auto_prov)
  ) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_auto_prov = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_auto_prov = 0)) %>%
  arrange(CODMUNRES)

Lesoes_autoprovocadas_intencionalmente <- Lesoes_autoprovocadas_intencionalmente %>%
  filter(CODMUNRES != "330000")

Lesoes_autoprovocadas_intencionalmente

tabela_ind9 <- (Lesoes_autoprovocadas_intencionalmente$obitos_auto_prov / df_final$Total) * 1e5
tabela_ind9

base_indicadores$TxMLAP23 <- tabela_ind9 #armazenando na base de indicadores


#
#Agora, o indicador de mortalidade por disparo de arma de fogo

#Obtendo número de mortes por armas de fogo por município em 2023
obitos_armas <- sim_2023 %>%
  filter(str_detect(CAUSABAS, "^W3[2-4]|^X7[2-4]|^Y2[2-4]|^X9[3-5]")) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_armas = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_armas = 0)) %>%
  arrange(CODMUNRES)

obitos_armas <- obitos_armas %>% 
  filter(CODMUNRES != "330000")

obitos_armas #numerador do indicador

tabela_ind10 <- (obitos_armas$obitos_armas / df_final$Total) * 1e5
tabela_ind10

base_indicadores$TxMDAF23 <- tabela_ind10







