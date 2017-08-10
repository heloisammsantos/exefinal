# exefinal da disciplina linguagem estatística curso Big data FIAP - turma BDT18
#Escolha de um estado, no nosso caso, Rondonia e classificação das melhores IES para se cursar neste estado. 
# Após receber as planilhas em uma única tabela e recebe-las já classificadas
# Neste Trabalho mostro o Log do dt_final(tabela final) e suas categorias para visualizar diferentes correlações e suas importâncias.
#Execução de um Summary contendo: correlações e variáveis estatísticas. 
#1a parte - importação das diferentes tabelas em .CSV, formatacao da tabela dt_final - junção das tabelas DM_IES.csv, DM_CPC.csv, DM_CURSO.csv, DM_DOCENTE.csv, DM_enade_conve.csv, DM_IGC.csv, municipios_ro_distancia.csv

#biblioteca a ser utilizada:
require('sqldf')
require('stringi')

#caminho no computador aonde está as tabelas e aonde vamos criar a dt_final:

setwd('C:\\CursoBigdata\\LinguagemEstatistica\\ExercicioFinal\\dados') #definindo o diretório de trabalho

#carregando as tableas .csv em R studio
ds_ies <- read.csv2('DM_IES.csv', header = TRUE) #carregando o arquivo IES
ds_cpc <- read.csv2('DM_CPC.csv', header = TRUE) #carregando o arquivo CPC
ds_curso <- read.csv2('DM_CURSO.csv', header = TRUE) #carregando o arquivo de CURSOS
ds_docente <- read.csv2('DM_DOCENTE.csv', header = TRUE) #carregando o arquivo de DOCENTES
ds_enade <- read.csv2('DM_enade_conven.csv', header = TRUE) #carregando o arquivo ENADE
ds_igc <- read.csv2('DM_IGC.csv', header = TRUE) #carregando o arquivo IGC
distancia_minha_cidade <- read.csv2('municipios_ro_distancia.csv', header = TRUE, encoding = 'UTF-8')

#Filtro do Estado Rondonia na tabela ds_ies, docentes_ro, igc_ro, cpc_ro, distancia_faculdades

ies_ro <- ds_ies[ds_ies$SGL_UF_IES == 'RO', ] #filtrando as faculdades de Rondônia (RO)
cursos_ro <- sqldf('SELECT c.*
                      FROM ds_curso c
                      JOIN ies_ro ies ON c.co_ies = ies.co_ies
                    ') #filtrando cursos em Rondônia (RO)
docentes_ro <- sqldf('SELECT d.*
                      FROM ds_docente d
                      JOIN ies_ro ies ON d.co_ies = ies.co_ies
                    ') #filtrando docentes em Rondônia (RO)

igc_ro <- sqldf('SELECT i.*
                      FROM ds_igc i
                      JOIN ies_ro ies ON i.co_ies = ies.co_ies
                    ') #filtrando IGC em Rondônia (RO)

cpc_ro <- sqldf('SELECT c.co_ies,
            max(CASE
              WHEN c.cpc_faixa = "SC" THEN 0
              WHEN c.cpc_faixa = "" THEN -1
              ELSE c.cpc_faixa
            END) AS cpc_faixa
                      FROM ds_cpc c
                      JOIN ies_ro ies ON c.co_ies = ies.co_ies
                      GROUP BY c.co_ies
                    ') #filtrando CPF em Rondônia (RO)

distancia_faculdades <- sqldf('SELECT i.co_ies, d.nome_municipio, d.distancia_vilhena_km
                      FROM distancia_minha_cidade d
                      JOIN ies_ro i ON i.co_municipio_ies = d.codigo_ibge')


dt_final <- data.frame()

### ******* MODELO de criação de notas de cada item, criando a relevância de cada tabela na nota total****** ###
for(i in 1:nrow(ies_ro)) {
  cod_ies <- ies_ro[i, ]$CO_IES
  nome_ies <- ies_ro[i, ]$NO_IES
  
  #notas dos itens
  w_categoria_ies <- 0              #15%
  w_situacao_curso <- 0             #5%
  w_situacao_docentes <- 0          #15%
  w_escolaridade_docentes <- 0      #10%
  w_regime_trabalho_docentes <- 0   #15%
  w_cpc_faixa <- 0                  #10%
  w_distancia_minha_casa <- 0       #30%
  
  #Verificando a distância da minha casa (variável mais importante)
  distancia_minha_casa <- distancia_faculdades[distancia_faculdades$CO_IES == cod_ies, ]$distancia_vilhena_km
  
  if (distancia_minha_casa <= 15) {
    w_distancia_minha_casa = 30 #Bem perto
  } else if (distancia_minha_casa > 15 && distancia_minha_casa <= 90) {
    w_distancia_minha_casa = 23 #OK, dá pra ir e voltar todo dia
  } else if (distancia_minha_casa > 90 && distancia_minha_casa <= 250) {
    w_distancia_minha_casa = 12 #Tenho que morar e fazer bate e volta no final de semana
  } else if (distancia_minha_casa > 250 && distancia_minha_casa <= 500) {
    w_distancia_minha_casa = 7 #Não posso voltar todo final de semana
  } else if (distancia_minha_casa > 500) {
    w_distancia_minha_casa = 1 #Bem longe de casa... Tenho que mudar
  }
  
  #Categoria da IES (Federal, Estadual, Municipal, Privada ou Especial)
  categoria_ies <- ies_ro[i, ]$CO_CATEGORIA_ADMINISTRATIVA
  
  if (categoria_ies == 1) { #Federal
    w_categoria_ies = 12
  } else if (categoria_ies == 2) { #Estadual
    w_categoria_ies = 15
  } else if (categoria_ies == 3) { #Municipal
    w_categoria_ies = 10
  } else if (categoria_ies == 4) { #Privada com fins lucrativos
    w_categoria_ies = 7
  } else if (categoria_ies == 5) { #Privada sem fins lucrativos
    w_categoria_ies = 8
  } else if (categoria_ies == 7) { #Especial
    w_categoria_ies = 9
  }
  
  #Situação dos docentes
  docentes_ies <- docentes_ro[docentes_ro$CO_IES == cod_ies, ]
  total_docentes_ies <- nrow(docentes_ies)
  total_docentes_ativos_ies <- nrow(docentes_ies[docentes_ies$CO_SITUACAO_DOCENTE == 1, ]) #Docentes ativos na instituição
  total_docentes_qualificacao_ies <- nrow(docentes_ies[docentes_ies$CO_SITUACAO_DOCENTE == 2, ]) #Docentes em afastados para qualificação
  pct_docentes_ativos <- total_docentes_ativos_ies / total_docentes_ies
  pct_docentes_qualificacao <- total_docentes_qualificacao_ies / total_docentes_ies
  
  if (pct_docentes_ativos == 1) {
    w_situacao_docentes = 15
  } else if (pct_docentes_ativos < 1 && pct_docentes_ativos > .90) {
    if ((pct_docentes_ativos + pct_docentes_qualificacao) == 1) {
      w_situacao_docentes = 13
    } else {
      w_situacao_docentes = 11  
    }
  } else if (pct_docentes_ativos <= .90 && pct_docentes_ativos > .75) {
    w_situacao_docentes = 9
  } else if (pct_docentes_ativos <= .75 && pct_docentes_ativos > .50) {
    if ((pct_docentes_ativos + pct_docentes_qualificacao) > .75) {
      w_situacao_docentes = 8
    } else {
      w_situacao_docentes = 5
    }
  } else if (pct_docentes_ativos <= .50 && pct_docentes_ativos > .25) {
    w_situacao_docentes = 3
  }
  
  #Escolaridade dos docentes
  total_docentes_ies_doutorado <- nrow(docentes_ies[docentes_ies$CO_ESCOLARIDADE_DOCENTE == 5, ]) #Doutorado
  total_docentes_ies_mestrado <- nrow(docentes_ies[docentes_ies$CO_ESCOLARIDADE_DOCENTE == 4, ]) #Mestres
  pct_docentes_doutores <- total_docentes_ies_doutorado / total_docentes_ies
  pct_docentes_mestrado <- total_docentes_ies_mestrado / total_docentes_ies
  
  if (pct_docentes_doutores == 1) {
    w_escolaridade_docentes = 10
  } else if ((pct_docentes_doutores + pct_docentes_mestrado) == 1) {
    w_escolaridade_docentes = 9
  } else if ((pct_docentes_doutores > .9)) {
    w_escolaridade_docentes = 8
  } else if ((pct_docentes_doutores + pct_docentes_mestrado) > .9) {
    w_escolaridade_docentes = 7
  } else if ((pct_docentes_doutores > .7)) {
    w_escolaridade_docentes = 5
  } else if ((pct_docentes_doutores + pct_docentes_mestrado) > .7) {
    w_escolaridade_docentes = 4
  } else if (pct_docentes_doutores > .49) {
    w_escolaridade_docentes = 3
  } else if ((pct_docentes_doutores + pct_docentes_mestrado) > .49) {
    w_escolaridade_docentes = 2
  }
  
  #Regime de trabalho dos docentes
  total_docentes_ies_exclusiva <- nrow(docentes_ies[docentes_ies$CO_REGIME_TRABALHO == 1, ]) #Tempo integral com dedicação exclusiva
  total_docentes_ies_sem_exclusiva <- nrow(docentes_ies[docentes_ies$CO_REGIME_TRABALHO == 2, ]) #Tempo integral sem dedicação exclusiva
  pct_docentes_exclusivos <- total_docentes_ies_exclusiva / total_docentes_ies
  pct_docentes_nao_exclusivos <- total_docentes_ies_sem_exclusiva / total_docentes_ies
  
  if (pct_docentes_exclusivos == 1) {
    w_regime_trabalho_docentes = 15
  } else if ((pct_docentes_exclusivos + pct_docentes_nao_exclusivos) == 1) {
    w_regime_trabalho_docentes = 13
  } else if (pct_docentes_exclusivos > .9) {
    w_regime_trabalho_docentes = 12
  } else if (pct_docentes_exclusivos > .8) {
    w_regime_trabalho_docentes = 11
  } else if ((pct_docentes_exclusivos + pct_docentes_nao_exclusivos) > .8) {
    w_regime_trabalho_docentes = 10
  } else if (pct_docentes_exclusivos > .7) {
    w_regime_trabalho_docentes = 8
  } else if ((pct_docentes_exclusivos + pct_docentes_nao_exclusivos) > .7) {
    w_regime_trabalho_docentes = 7
  } else if (pct_docentes_exclusivos > .6) {
    w_regime_trabalho_docentes = 6
  } else if ((pct_docentes_exclusivos + pct_docentes_nao_exclusivos) > .6) {
    w_regime_trabalho_docentes = 5
  } else if (pct_docentes_exclusivos > .5) {
    w_regime_trabalho_docentes = 4
  } else if ((pct_docentes_exclusivos + pct_docentes_nao_exclusivos) > .5) {
    w_regime_trabalho_docentes = 3
  } else if (pct_docentes_exclusivos > .4) {
    w_regime_trabalho_docentes = 2
  }
  
  #CPC
  cpc_ies <- cpc_ro[cpc_ro$co_ies == cod_ies, ]$cpc_faixa
  
  if (length(cpc_ies) > 0) {
    if (cpc_ies == 5) {
      w_cpc_faixa = 10
    } else if (cpc_ies == 4) {
      w_cpc_faixa = 8
    } else if (cpc_ies == 3) {
      w_cpc_faixa = 6
    } else if (cpc_ies == 2) {
      w_cpc_faixa = 4
    } else if (cpc_ies == 1) {
      w_cpc_faixa = 3
    } else if (cpc_ies == 0) {
      w_cpc_faixa = 2
    } else if (cpc_ies == -1) {
      w_cpc_faixa = 1
    }

}
  
  #PCT de cursos ativos
  cursos_ies <- cursos_ro[cursos_ro$CO_IES == cod_ies, ]
  total_cursos_ativos <- nrow(cursos_ies[cursos_ies$CO_SITUACAO_CURSO == 10056, ])
  pct_cursos_ativos <- total_cursos_ativos / nrow(cursos_ies)
  
  if (pct_cursos_ativos == 1) {
    w_situacao_curso = 5
  } else if (pct_cursos_ativos > .85) {
    w_situacao_curso = 3
  } else if (pct_cursos_ativos > .75) {
    w_situacao_curso = 2
  } else if (pct_cursos_ativos > .55) {
    w_situacao_curso = 1
  }
  
  p_final = w_categoria_ies + w_situacao_curso + w_situacao_docentes + w_escolaridade_docentes + w_regime_trabalho_docentes + w_cpc_faixa + w_distancia_minha_casa
  
  dt_final <- rbind(dt_final,
    data.frame(
      cod_ies, nome_ies
      ,distancia_minha_casa, w_distancia_minha_casa
      ,categoria_ies, w_categoria_ies
      ,total_docentes_ies, total_docentes_ativos_ies, total_docentes_qualificacao_ies, w_situacao_docentes
      ,total_docentes_ies_doutorado, total_docentes_ies_mestrado, w_escolaridade_docentes
      ,total_docentes_ies_exclusiva, total_docentes_ies_sem_exclusiva, w_regime_trabalho_docentes
      ,ifelse(length(cpc_ies) > 0, cpc_ies, ''), w_cpc_faixa
      ,nrow(cursos_ies), total_cursos_ativos, w_situacao_curso
      ,p_final
    )
  )
  
  
}

names(dt_final) <- c('Código da IES', 'Nome da IES'
                     ,'Distância da Minha Casa (KM)', 'Nota - Distância da Minha Casa'
                     ,'Categoria da IES', 'Nota - Categoria da IES'
                     ,'Total de Docentes', 'Total de Docentes Ativos', 'Total de Docentes Afastados para Qualificação', 'Nota - Total de Docentes'
                     ,'Total de Docentes Doutores', 'Total de Docentes Mestres', 'Nota - Escolaridade Docentes'
                     ,'Total de Docentes - Dedicado Exclusivo', 'Total de Docentes - Dedicado sem Exclusividade', 'Nota - Docentes Dedicados'
                     ,'Classificação CPC da IES', 'Nota - Classificação CPC da IES'
                     ,'Total de Cursos na IES', 'Total de Cursos Ativos', 'Nota - Situação Curso'
                     ,'Nota Final - IES'
                   )
View(dt_final)


### PCA ####
#com o objetivo de desenvolver e analisar as informações da tabela levantamos os 5 informações de cada coluna.


require(data.table)

data(dt_final)

head(dt_final, 5)

#geração dos ínidices estatísticos como média, mediana e os padrões de notas que estamos investigando#
summary(dt_final)

#kmeans para iniciar a correlação e as aproximações de dados
#Cluster variando 

cluster.dt_final<- kmeans(dt_final[,18:19], 10,nstart = 3)
cluster.dt_final

Cluster.docente <- kmeans(dt_final[,11:12],10, nstart = 3)
Cluster.docente

Cluster_parcial <- kmeans(dt_final[,18:20], 10, nstart = 3)
Cluster_parcial

cluster


##correlacoes estatisticas - se faz para entender a importancia entre elas, e quais estão mais relacionadas e menos relacionadas###

library(data.table)
library(knitr)

View(dt_final)
dt_final

#Fiz diversar correlções e daí essas foram as maiores!!!!
x <- c(dt_final$`Nota Final - IES`)
y <- c(dt_final$`Total de Docentes Doutores`)
z <- c(dt_final$`Distância da Minha Casa (KM)`)
k <- c(dt_final$`Categoria da IES`)
l <- c(dt_final$`Nota - Escolaridade Docentes`)
m <- c(dt_final$`Total de Docentes Mestres`)
n <- c(dt_final$`Total de Docentes`)
o <- c(dt_final$`Total de Docentes - Dedicado Exclusivo`)
p <- c(dt_final$`Nota - Classificação CPC da IES`)
q <- c(dt_final$`Classificação CPC da IES`)
r <- c(dt_final$`Nota - Situação Curso`)
s <- c(dt_final$`Total de Docentes Afastados para Qualificação`)
t <- c(dt_final$`Nota - Total de Docentes`)
u <- c(dt_final$`Total de Cursos Ativos`)

Assim visualmente consegue enxergar uma correlação entre as seguintes colunas:
Total de Docentes AtivosX total de Docentes Doutores X Classificação CPC da IES X Nota Final da IEs X Distância da Minha casa
 Ainda flata o pcrcomp e mais cluster que apontem o que já está aparecendo nas correlações

Fiz diversar correlções e daí essas foram as maiores!!!!

cor(n,m)
0.97maior correlação com total de docentes com mestrados
cor(m,y)
0,95correlação entre professores mestres com professores doutores
cor(l,y)
0,44 há correlação media entre notas dos docentes e docentes doutores
cor(n,y)
0,90 de correlação entre docentes doutores e total de docentes
cor(x,y)
0,629 depos a nota da IEs com numeros de Docentes doutores
cor(m,o)
0,90 correlação entre docentes mestres e docentes exclusivos
cor(q,y)
0,39 correlação entre a nota classificação CPC da IES e Total de docentes doutores
cor(q,n)
0,5701 correlação Nota de Classificação CPC da IES e Total de Docentes
cor(p,q)
0,7188 correlação Classificação CPC da IES e Nota da Classificação Cpc da IEs
cor(x,u)
0,91 correlação em Nota da IEs e total de cursos ativos
cor(u,y)
0,911 total de cursos ativos X total de docentes doutores
cor(u,z)
0,12  total de cursos ativos X distancia da minha casa
cor(u,p)
0,57 total de cursos ativos X Nota - Classificação CPC da IES`
cor(u,l)
0,41 total de cursos ativos X Nota da escolaridade docentes
cor(u,m)
0,97  total de cursos ativos X total de docentes com mestrado
cor(u,n)
0,95  total de cursos ativos X total de docentes
cor(u,o)
0,83 total de cursos ativos X total de Docentes exclusivos





