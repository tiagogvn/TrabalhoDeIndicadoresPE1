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

library(microdatasus) #pacote para obter informações sobre a mortalidade

dados_sim <- fetch_datasus(
  year_start = 2023, year_end = 2023,
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

library(censobr) #pacote para obter a população total residente masculina e feminina nos municípios, informação não contida nas fontes anteriores

# Verificar dicionário para saber os nomes/códigos das variáveis (último ano disponível é 2010)
data_dictionary(dataset = "population", year = 2010)



