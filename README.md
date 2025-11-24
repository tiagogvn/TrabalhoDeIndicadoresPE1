Este é o repositório sendo usado pelos alunos Felipe Ceacero Rodrigues Maia, Maria Eduarda Noira Passos da Costa Leal, Sarah de Lima Veríssimo Ivanicska Costa, Tiago Goulart Veloso Nunes e Vitor Eduardo Bezerra Ferreira da Universidade Federal Fluminense (UFF) no segundo semestre dentro da cadeira Prática Estatística 1. 

A cadeira de Prática Estatística 1 é uma das diversas propostas de extensão realizadas pela UFF, a extensão universitária é a ponte entre a universidade e a sociedade. Ela leva o conhecimento acadêmico para além da sala de aula, contribuindo para solucionar problemas sociais e aproximar a universidade da comunidade. 

Neste projeto fomos encarregados de propor indicadores municipais nos baseando na base de dados SIM (Sistema de informações sobre mortalidade) do Datasus, e esperamos que nossos indicadores propostos sejam capazes de facilitar pesquisas e discussões sobre as causas de morte que eles exploram. Segue abaixo uma breve explicação sobre cada um de nossos 12 indicadores:

TxMFAG23: Indicador que mede o número de óbitos causados por complicações envolvendo o sistema geniturinário da população feminina do munícipio em relação a população local no ano de 2023, acreditamos que esse indicador pode ser útil em contextos médicos, principalmente na área da ginecologia.

TxMNDC23: Indicador que mede o número de óbitos causados por complicações oriundas de deformidades cromossômicas em crianças entre 28 a 365 dias de vida no munícipio em relação ao número de nascidos vivos no ano de 2023, acreditamos que esse indicador pode ser útil em contextos médicos, principalmente na área da pediatria.

TxMPADR23: Indicador que mede o número de óbitos causados por complicações relacionadas ao sistema respiratório em crianças entre 28 a 365 dias de vida no munícipio em relação ao número de nascidos vivos no ano de 2023, acreditamos que esse indicador pode ser útil em contextos médicos, principalmente na área da pediatria.

TxMMVP23: Indicador que mede o número de óbitos da população masculina do munícipio em vias públicas relacionado a população local no ano de 2023, acreditamos que esse indicador pode ser útil em contextos de segurança pública e urbanismo, podendo explicitar se certas medidas de segurança foram efetivas no ano em análise.

TxMPMDPNN23: Indicador que mede o número de óbitos causados por doenças do período neo-natal em crianças com até 27 dias de vida no munícipio em relação ao número de nascidos vivos no ano de 2023, acreditamos que esse indicador pode ser útil em contextos médicos, principalmente na área da pediatria.

TxMCEPP23: Indicador que mede o número de óbitos da população parda do munícipio devido a causas externas relacionado a população local no ano de 2023, acreditamos que esse indicador pode ser útil em contextos de segurança pública, urbanismo e sociologia, podendo explicitar se certas medidas de segurança foram efetivas no ano em análise ou se certos munícipios apresentam complicações sociais maiores para a população parda do que outros.

TxFNM23: Indicador que mede o número de óbitos causados por complicações envolvendo neoplasias malignas na população feminina do munícipio em relação a população local no ano de 2023, acreditamos que esse indicador pode ser útil em contextos médicos.

TxMAT23: Indicador que mede o número de óbitos causados por acidentes de trânsito no munícipio em relação a população local no ano de 2023, acreditamos que esse indicador pode ser útil em contextos de urbanismo, podendo apontar para municípios com falhas em seu planejamento rodoviário.

TxMLAP23: Indicador que mede o número de óbitos causados por lesões autoprovocadas no munícipio em relação a população local no ano de 2023, acreditamos que esse indicador pode ser útil em contextos psicologia e psiquiatria, pondendo indicar municípios onde a população local apresenta tendências auto destrutivas muitas vezes ligadas a qualidade de vida no local.

TxMDAF23: Indicador que mede o número de óbitos causados por disparos de armas de fogo no munícipio em relação a população local no ano de 2023, acreditamos que esse indicador pode ser útil em contextos de segurança pública, podendo indicar o nível de violência daquele município.

PMDCv23: Indicador que confere se as doenças cardiovasculares são 25% ou mais das causas de morte naquele ano no município, está costuma ser a maior causa de mortes ao redor do mundo, então é um indicador que pode apontar para municípios anomalos que não apresentam essa causa como principal.

PMCMD23: Indicador que confere se as causas não definidas são 25% ou mais das causas de morte naquele ano no município, causas mal definidas costumam envolver complicações no processo forense e são muito comuns em bases de dados sobre mortalidade, esse indicador confere a prevalência desses casos em cada município.


-Segue abaixo uma explicação breve sobre o que cada arquivo neste repositório representa:

pop_populacao_ripsa202417635071428.csv: Arquivo com a população de cada município do do Rio de janeiro, separado em população total, população masculina e população feminina. Esses dados foram essenciais para calcular as variáveis em nosso script do R.

scriptConstruçãodaBase.R: Este é o script na linguagem R onde colocamos em prática os indicadores que propomos para o projeto, lá é possível encontrar o passo a passo a ser seguido para realizar as mesmas análises que fizemos por conta própria caso isso seja de seu interesse. No final, ao terminar de rodar os códigos você terá uma tabela onde cada linha é um munícipio do Rio de Janeiro e cada coluna é um de nossos 12 indicadores propostos. 
