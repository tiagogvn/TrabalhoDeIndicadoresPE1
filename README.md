Este é o repositório sendo usado pelos alunos Felipe Ceacero Rodrigues Maia, Maria Eduarda Noira Passos da Costa Leal, Sarah de Lima Veríssimo Ivanicska Costa, Tiago Goulart Veloso Nunes e Vitor Eduardo Bezerra Ferreira da Universidade Federal Fluminense (UFF) no segundo semestre de 2025 dentro da cadeira Prática Estatística 1. 

A cadeira de Prática Estatística 1 é uma das diversas propostas de extensão realizadas pela UFF, a extensão universitária é a ponte entre a universidade e a sociedade. Ela leva o conhecimento acadêmico para além da sala de aula, contribuindo para solucionar problemas sociais e aproximar a universidade da comunidade. 

Neste projeto fomos encarregados de propor indicadores municipais para o estado do Rio de Janeiro nos baseando na base de dados SIM (Sistema de informações sobre mortalidade) do Datasus, esperamos que nossos indicadores propostos sejam capazes de facilitar pesquisas e discussões sobre as causas de morte que eles exploram, sendo que essas remetem aos mais diversos tópicos como contextos médicos, ginecologia, pediatria, segurança pública, urbanismo, sociologia, planejamento rodoviário, psicologia e psiquiatria. Nós propomos um total de 12 destes indicadores e eles variam de análises de partes específicas das populações desses municípios sendo aflingidas por comorbidades também particulares, até análises focadas nas causas mais comuns de óbito que afligem a população como um todo. É por motivos como esse, que nós como um gupo, acreditamos que pesquisadores e estudantes das mais diversas áreas podem encontrar em nosso repositório informações essenciais para facilitar suas pesquisas ou saciar suas curiosidades.


-Segue abaixo uma explicação breve sobre o que cada arquivo neste repositório representa:

pop_populacao_ripsa202417635071428.csv: Arquivo com a população de cada município do do Rio de janeiro, separado em população total, população masculina e população feminina. Esses dados foram essenciais para calcular as variáveis em nosso script do R.

scriptConstruçãodaBase.R: Este é o script na linguagem R onde colocamos em prática os indicadores que propomos para o projeto, lá é possível encontrar o passo a passo a ser seguido para realizar as mesmas análises que fizemos por conta própria caso isso seja de seu interesse. No final, ao terminar de rodar os códigos você terá uma tabela onde cada linha é um munícipio do Rio de Janeiro e cada coluna é um de nossos 12 indicadores propostos. 

Indicadores_Grupo_4.pdf: Neste arquivo será encontrada a versão final compilada do relatório, com todos os indicadores, suas relevâncias e modos de calcular.

DicionarioDoGrupo.xlsx: Este dicionário fornece as explicações atreladas a cada indicador, bem como a fonte e as variações de taxas esperadas para cada objeto.

tabela_indicadores.csv: Arquivo CSV contendo a tabela final obtida pelo nosso script no R, aqui você poderá encontrar os valores que cada variável assumiu para cada município do Rio de Janeiro.
