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
#Começando pelo indicador da Taxa de Mortalidade Feminina por Doenças do Aparelho Geniturinário em 2023 a cada 10.000 habitantes:

#Baixando SIM 2023 para o RJ, que contém dados que serão utilizados para o cálculo de quase todos os indicadores
sim_2023 <- fetch_datasus(
  year_start = 2023,
  year_end = 2023,
  uf = "RJ",
  information_system = "SIM-DO",
  vars = c("SEXO", "CAUSABAS", "CODMUNRES")
)

#Calculando quantidade de óbitos por N00-99 nos municípios do Rio de Janeiro em 2023
Doencas_do_aparelho_geniturinario <- sim_2023 %>%
  filter(
    SEXO == "2",                           # apenas mulheres
    str_detect(CAUSABAS, "^N[0-9]{2}")     # CID N00 a N99
  ) %>%
  count(CODMUNRES, name = "obitos_genit_fem") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_genit_fem = 0)) %>%
  arrange(CODMUNRES)

Doencas_do_aparelho_geniturinario <- Doencas_do_aparelho_geniturinario %>% #retirando indivíduos de município de residência desconhecido
  filter(CODMUNRES != "330000")

Doencas_do_aparelho_geniturinario #o valor dessa variável no município m será o numerador da fórmula do indicador TxMFAG23 para o município em questão
                                  #já o numerador, para esse indicador, será o valor da coluna "Feminino" do data frame df_final no município m
                                  #por fim, o resultado dessa divisão será multiplicado pelo fator de multiplicação

#Calculando:
tabela_ind1 <- (Doencas_do_aparelho_geniturinario$obitos_genit_fem / df_final$Feminino) * 1e4
tabela_ind1

base_indicadores$TxMFAG23 <- tabela_ind1 #armazenando na base de indicadores


#
#Indicador da Taxa de Mortalidade Neonatal por Deformidades Cromossômicas em 2023 a cada 1000 nascidos vivos

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
#Indicador da Taxa de Mortalidade de 28-365 dias por Doenças do Aparelho Respiratório em 2023 a cada 1000 nascidos vivos

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
#Taxa de Mortalidade Masculina em Vias Públicas em 2023 a cada 10.000 habitantes
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

tabela_ind4 <- (obitos_masc_vias$obitos_masc_vias / df_final$Total) * 1e4
tabela_ind4

base_indicadores$TxMMVP23 <- tabela_ind4 #armazenando na base de indicadores


#
#Indicador da Mortalidade de 0-27 dias por Doenças do Período Neo-Natal em 2023 a cada 1000 nascidos vivos

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
#Taxa de mortalidade por causas externas na população parda a cada 10.000 habitantes
sim_2023 <- fetch_datasus(
  year_start = 2023,
  year_end = 2023,
  uf = "RJ",
  information_system = "SIM-DO",
  vars = c("SEXO", "CAUSABAS", "CODMUNRES","LOCOCOR","RACACOR") #adicionando variável raça ao sim_2023
)

Causas_externas_de_morbidade_e_mortalidade <- sim_2023 %>%
  filter(
    str_detect(CAUSABAS, "^[V-Y][0-9]{2}"),
    RACACOR == "4"
  ) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_ext_parda = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_ext_parda = 0)) %>% #garantindo que se houver município com 0 ocorrências de óbito pelas dependências específicas, será incluído no data frame
  arrange(CODMUNRES)

Causas_externas_de_morbidade_e_mortalidade <- Causas_externas_de_morbidade_e_mortalidade %>%
  filter(CODMUNRES != "330000")

tabela_ind6 <- (Causas_externas_de_morbidade_e_mortalidade$obitos_ext_parda / df_final$Total) * 1e4
tabela_ind6

base_indicadores$TxMCEPP23 <- tabela_ind6 #armazenando na base de indicadores


#
#Agora calcularemos o indicador da Taxa de Mortalidade Feminina por Neoplasias Malignas a cada 10.000 habitantes:
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
tabela_ind7 <- (obitos_neopl_fem$obitos_neopl_fem / df_final$Feminino) * 1e4
tabela_ind7

base_indicadores$TxFNM23 <- tabela_ind7 #armazenando na base de indicadores


#
#Taxa de mortalidade por acidentes de trânsito em 2023 a cada 10.000 habitantes
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

tabela_ind8 <- (Acidentes_de_Transporte$obitos_acidentes_transito / df_final$Total) * 1e4
tabela_ind8

base_indicadores$TxMAT23 <- tabela_ind8 #armazenando na base de indicadores


#
#Taxa de mortalidade por lesões auto provocadas em 2023 a cada 10.000 habitantes
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

tabela_ind9 <- (Lesoes_autoprovocadas_intencionalmente$obitos_auto_prov / df_final$Total) * 1e4
tabela_ind9

base_indicadores$TxMLAP23 <- tabela_ind9 #armazenando na base de indicadores


#
#Agora, o indicador da Taxa de mortalidade por disparo de arma de fogo em 2023 a cada 10.000 habitantes

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

tabela_ind10 <- (obitos_armas$obitos_armas / df_final$Total) * 1e4
tabela_ind10

base_indicadores$TxMDAF23 <- tabela_ind10


#
#Taxa de Mortalidade por Doença cardiovascular >= 25% das mortes no município em 2023 a cada 10.000 habitantes

causa_cardio <- "^I[0-9]{2}"

Doencas_do_aparelho_circulatorio <- sim_2023 %>%
  filter(str_detect(CAUSABAS, causa_cardio)) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_cardio = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_cardio = 0)) %>%
  arrange(CODMUNRES)

Doencas_do_aparelho_circulatorio <- Doencas_do_aparelho_circulatorio %>% 
  filter(CODMUNRES != "330000")

Doencas_do_aparelho_circulatorio #total de óbitos nos municípios por doenças cardiovasculares

TOTAL <- sim_2023 %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_todos = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_todos = 0)) %>%
  arrange(CODMUNRES)

TOTAL <- TOTAL %>% 
  filter(CODMUNRES != "330000")

TOTAL #total de óbitos nos municípios, por todas as causas

#Criando objeto com as informações
prop_cardio <- TOTAL %>%
  left_join(Doencas_do_aparelho_circulatorio, by = "CODMUNRES") %>%
  mutate(obitos_cardio = replace_na(obitos_cardio, 0))

#Calculando proporção de óbitos de doença cardiovascular
prop_cardio <- prop_cardio %>%
  mutate(prop_cardio = obitos_cardio / obitos_todos)
prop_cardio <- prop_cardio %>%
  mutate(ind_cardiovasc_quali = if_else(prop_cardio < 0.25, 0, 1))

base_indicadores$PMDCv23 <- prop_cardio$ind_cardiovasc_quali #armazenando na base


#
#Indicador de Causa mal definida >= 25% das mortes no município em 2023

causa_mal_def <- "^R[0-9]{2}"

Causas_mal_definidas_e_desconhecidas_mortalidade <- sim_2023 %>%
  filter(str_detect(CAUSABAS, causa_mal_def)) %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_mal_def = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_mal_def = 0)) %>%
  arrange(CODMUNRES)

Causas_mal_definidas_e_desconhecidas_mortalidade <- Causas_mal_definidas_e_desconhecidas_mortalidade %>% 
  filter(CODMUNRES != "330000")

Causas_mal_definidas_e_desconhecidas_mortalidade #total de óbitos com causa mal definida

TOTAL <- sim_2023 %>%
  group_by(CODMUNRES) %>%
  summarise(obitos_todos = n(), .groups = "drop") %>%
  complete(CODMUNRES = sim_2023$CODMUNRES,
           fill = list(obitos_todos = 0)) %>%
  arrange(CODMUNRES)

TOTAL <- TOTAL %>% 
  filter(CODMUNRES != "330000")

TOTAL #total de óbitos nos municípios, por todas as causas

#criando data frame com as informações
prop_mal_def <- TOTAL %>%
  left_join(Causas_mal_definidas_e_desconhecidas_mortalidade, by = "CODMUNRES") %>%
  mutate(obitos_mal_def = replace_na(obitos_mal_def, 0))

#Calculando proporção de óbitos com causa mal definida
prop_mal_def <- prop_mal_def %>%
  mutate(prop_mal_def = obitos_mal_def / obitos_todos)
prop_mal_def <- prop_mal_def %>%
  mutate(ind_mal_def = if_else(prop_mal_def < 0.25, 0, 1))

base_indicadores$PMCMD23 <- prop_mal_def$ind_mal_def #armazenando na base



