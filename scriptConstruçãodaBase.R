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
mun_rj <- read_municipality(code_muni = "RJ", year = 2024) |>
  st_drop_geometry() |>
  select(code_muni, name_muni)

# Criando base com 12 colunas preenchidas apenas com o nome dos indicadores, e
#todos os municípios do Rio de Janeiro nas linhas
base_indicadores <- mun_rj |>
  mutate(
    TxMFAG24 = NA_real_,
    TxMNDC24 = NA_real_,
    TxMPADR24 = NA_real_,
    TxMMVP24 = NA_real_,
    TxMPMDPNN24 = NA_real_,
    TxMCEPP24 = NA_real_,
    TxFNM24 = NA_real_,
    TxMAT24 = NA_real_,
    TxMLAP24 = NA_real_,
    TxMDAF24 = NA_real_,
    PMDCv24 = NA_real_,
    PMCMD24 = NA_real_
  )

library(microdatasus) #pacote para obter informações sobre a mortalidade

dados_sim <- fetch_datasus(
  year_start = 2024, year_end = 2024,
  uf = "RJ",
  information_system = "SIM-DO"
) |>
  process_sim()

dados_sim #data frame resultante tem informações da mortalidade por indivíduo,
          #agruparemos por código do município para então extrair as informações
          #para nós relevantes: sexo, cor, causa de morte, etc.

obitos_mun <- dados_sim |>
  group_by(code_muni = CODMUNRES, causa = CAUSABAS) |> #para o cálculo dos nossos indicadores, consideramos o município de residência, portanto agruparemos por 'CODMUNRES'
  summarise(obitos = n(), .groups = "drop")

obitos_mun #data frame resultante tem código do município, a causa específica e quantidade de ocorrências no município

library(microdatasus) #daqui vamos extrair a quantidade de nascidos vivos por município do Rio de Janeiro em 2024

#Buscando do datasus, do sistema SINASC as quantidades de nascidos vivos
nascidos_2022 <- fetch_datasus(
  year_start = 2024,
  year_end = 2024,
  uf = "RJ",
  information_system = "SINASC"
) %>%
  process_sinasc()

# Agrupando por município:
nv_mun <- nascidos_2024 %>%
  group_by(code_muni = CODMUNRES) %>% 
  summarise(nascidos_vivos = n(), .groups = "drop")

nv_mun #o data frame resultante possui o código do município, e o número de nascidos vivos nesse município em 2024

library(readr) #agora iremos ler a planilha com as estimativas de população residente masculina, feminina e total para 2024 dos municípios do Rio de Janeiro, adquirida do estudo de estimativas populacionais pactuadas pela SES/RJ
library(tidyr)
library(stringr)

df_raw <- readr::read_csv2(
  "pop_populacao_ripsa202417634975410.csv",
  locale = locale(encoding = "Latin1")
)

df_raw %>% head() #data frame resultante tem nome do município nas linhas, e estimativas de população residente masculina, feminina e total para 2024 nas colunas

library(stringi) #vamos facilitar os cálculos adicionando o código de município no data grame df_raw

#Padronizando textos para garantir correspondência
df_clean <- df_raw %>%
  mutate(
    muni_norm = Município %>%
      str_to_lower() %>%
      stri_trans_general("Latin-ASCII")
  )

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

df_final



