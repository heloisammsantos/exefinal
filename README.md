# exefinal
parte do exercicio final da disciplina linguagem estatística

Fiz o Log do dt_final para entender aonde estava a correlação

plotei um Grafico que não funcionou...
depois criei um Summary contendo correlações, variáveis estatísticas, 
assim visualmente consegue enxergar uma correlação entre as seguintes colunas:
Total de Docentes AtivosX total de Docentes Doutores X Classificação CPC da IES X Nota Final da IEs X Distância da Minha casa
 Ainda flata o pcrcomp e mais cluster que apontem o que já está aparecendo nas correlações

Nas 5 Melhores seria baseado também na distância aonde moro para a IES:
Faculdade Amazônia
Faculdade de Ciências Biomédicas de CACOAL
Faculdade de Educação de JAru
Fundação Universidade Federal de Rondonia (melhor de todas mas está há 707 km)
Faculdade São Lucas

Fiz diversar correlções e daí essas foram as maiores!!!!

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





