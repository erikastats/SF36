############################################
#  Análise Questionário Saúde Ocupacional  #
#          Funções a serem usadas          #
############################################

#' Apresentar a tabela das medidas resumo de cada Domínio
#'
#' @param data Dados com as colunas que serão utilizadas para o resumo, contendo os Domínios
#'
#' @return DataFrame contendo o valor Mínimo, Máximo, Mediana, Média, Desvio Padrão e Coeficiente de Variabilidade
#'  como colunas, e tendo dos domínios como linhas

Resumo = function(data){
    
    rd = lapply(data, function(x){
      data.frame(Minimo = min(x), 
                 Maximo = max(x),
                 Mediana = median(x), 
                 Media = mean(x),
                 DP = sd(x))
    }
    )
    
    rd = do.call(rbind, rd)
    ResDominios = rbind(rd, round(apply(rd, 2, mean),2))
    row.names(ResDominios)[nrow(ResDominios)] = "Média"
    ResDominios
  }

  
#' Histograma na diagonal
#'
#' @param x Dados que comporão o plot
#'
#' @return Parâmetro que adiciona o histograma dos dados na diagonal da tabela de correlação
#' 
#' 
  
  ### Desabilitado pois não foi usado nesse estudo -----------------------------------------------------------
    # panel.hist <- function(x, ...) 
    # {
    #   usr <- par("usr"); on.exit(par(usr))
    #   par(usr = c(usr[1:2], 0, 1.5) )
    #   h <- hist(x, plot = FALSE)
    #   breaks <- h$breaks; nB <- length(breaks)
    #   y <- h$counts; y <- y/max(y)
    #   rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
    # }
  
#' Coeficientes de Correlação somente em uma parte do painel
#'
#' @param x Dados que comporão o plot
#'
#' @return Parâmetro que adiciona os coeficientes das correlações em um dos painéis triangulares, 
#' condicionando o tamanho dos números ao valor da correlação
#'   
  ### Desabilitado pois não foi usado nesse estudo -----------------------------------------------------------

# panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) 
#   {
#     usr <- par("usr"); on.exit(par(usr))
#     par(usr = c(0, 1, 0, 1))
#     r <- abs(cor(x, y))
#     txt <- format(c(r, 0.123456789), digits = digits)[1]
#     txt <- paste0(prefix, txt)
#     if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
#     text(0.5, 0.5, txt, cex = cex.cor * r)}
    
#' Reta de regressão 
#'
#' @param x Dados que comporão o plot
#'
#' @return Parâmetro que adiciona uma reta de regressão nos gráficos de dispersão

### Desabilitado pois não foi usado nesse estudo -----------------------------------------------------------
  # panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
  #                       cex = 1, col.line="red") {
  #   points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  #   ok <- is.finite(x) & is.finite(y)
  #   if (any(ok)) {
  #     abline(lm(y[ok]~x[ok]), col = col.line)
  #   }
  # }

#' Gráficos múltiplos ggplot 
#'
#' @param x Gráficos armazenados em um objeto
#'
#' @return Painel de gráficos de acordo com o número de colunas indicadas 
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
### Desabilitado pois não foi usado nesse estudo -----------------------------------------------------------

# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#     require(grid)
#     
#     # Make a list from the ... arguments and plotlist
#     plots <- c(list(...), plotlist)
#     
#     numPlots = length(plots)
#     
#     # If layout is NULL, then use 'cols' to determine layout
#     if (is.null(layout)) {
#       # Make the panel
#       # ncol: Number of columns of plots
#       # nrow: Number of rows needed, calculated from # of cols
#       layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                        ncol = cols, nrow = ceiling(numPlots/cols))
#     }
#     
#     if (numPlots==1) {
#       print(plots[[1]])
#       
#     } else {
#       # Set up the page
#       grid.newpage()
#       pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#       
#       # Make each plot, in the correct location
#       for (i in 1:numPlots) {
#         # Get the i,j matrix positions of the regions that contain this subplot
#         matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#           
#         print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                         layout.pos.col = matchidx$col))
#       }
#     }
#   }
  

#' Plotando densindade
#'
#' @param col Coluna com os dados que serão usados para gerar o plot de Densidade
#'
#' @return Gráfico de densidade, filtrado pelo parâmetro "por" e entitulado pelo parâmtro "nome"
 
### Desabilitado pois não foi usado nesse estudo -----------------------------------------------------------   
  # plose = function(col , por, nome){
  #   ggplot(Respfinal, aes(col, colours = por, fill = por)) + 
  #     geom_density(alpha = 0.55) + ggtitle(nome)
  # }
   
#' Multiplot específico dos domínos GGplot
#'
#' @param comp Dados que serão usados para filtrar os dados para ser usado como comparação
#'
#' @return Grade com gráficos de densidade

### Desabilitado pois não foi usado nesse estudo -----------------------------------------------------------    
  # multicomp1 = function(comp){
  #     
  #   pcfs = plose(Respfinal[,42], comp, "Capacidade Funcional")
  #     
  #   pafs =   plose(Respfinal[,43], comp, "Aspectos Físicos")
  #     
  #   pds = plose(Respfinal[,44], comp, "Dor")
  #     
  #   pegss = plose(Respfinal[,45], comp, "Estado Geral de Saúde")
  #   
  #   multiplot(pcfs, pafs, pds, pegss, cols = 2)
  # }
  # 
  # multicomp2 = function(comp){
  #   
  #   pvs = plose(Respfinal[,46], comp, "Vitalidade")
  #   
  #   pass = plose(Respfinal[,47], comp, "Aspectos Sociais")
  #   
  #   paEs = plose(Respfinal[,48], comp, "Aspectos Emocionais")
  #   
  #   psms = plose(Respfinal[,48], comp, "Saúde Mental")
  #   
  #   multiplot(pvs, pass, paEs, psms, cols = 2)
  # }


  #' Gráfico de barras da frequência
  #'
  #' @param dado;nome Coluna que será usada como base; Nome do gráfico
  #'
  #' @return Plot de barras com a frequência do dado selecionado

  
  plotfreqQ = function(dado, nome){
    a = dado %>% 
      table() %>%
      data.frame() 
      
    
    colnames(a) = c("x", "y")
    prop = round(prop.table(a$y)*100,2)
    colori = rev(viridis(nrow(a), alpha = 0.9))
    
    plot_ly(a, x = ~x, y = ~y, type = 'bar', name = nome, 
            text = paste0(prop, "%"),
            marker = list(color = colori)) %>%
      layout( title = nome,
              xaxis = list(title = ""),
              yaxis = list(title = ""), showlegend = F)
  }
  
#' Tabela de frequências
#'
#' @param col1;col2 Colunas que será usada como base
#'
#' @return Tabela de frequências
#' 

tabfreq = function(col1, col2){
  
  tabela = table(col1, col2)
  ptabela = tabela %>%
    prop.table()
  tf = data.frame(tabela, ptabela)
  
}
  
  
  #' DunnetquEstoes
  #'
  #' @param dado;nome;legenda Coluna que será usada como base; Nome do histograma; Legenda das partes 
  #'
  #' @return Gráfico de Dunnet para as questões
  
  
DunnetQuestoes =  function(dado, nome, legenda){
  a = dado %>% 
    table() 
  b = data.frame(a, Legenda = legenda) %>% arrange(Freq)
  colnames(b) = c("Opcoes", "Freq", "Resposta")
  
  plot_ly(b, values= ~Freq, labels = ~Resposta, 
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          marker = list(colors = brewer.pal(nrow(b), "Blues"))) %>% 
    add_pie(hole = 0.6) %>% 
    layout(title = nome,  showlegend = F,
                                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
}

#' DunnetquEstoes
#'
#' @param dado;nome Coluna que será usada como base; Nome do histograma; Legenda das partes 
#'
#' @return Gráfico de Dunnet para os dominios


DunnetDominio =  function(dado, indic, nome){
  
  Dchao = function(x){
    a = x[ x == 0] %>%
      length()
    a/length(x)
  }
  Dteto = function(x){
    a = x[ x == 100] %>%
      length()
    a/length(x)}
  dc = Dchao(dado[,indic])
  dt = Dteto(dado[,indic])
    
    chaoteto = data.frame(Dchao = dc, Dteto = dt, Dmeio= 1-(dc+dt)) %>% 
      t() %>% 
      as.data.frame() 
    chaoteto$efeito = row.names(chaoteto)
    names(chaoteto)[1] = "Grupo" 
  
  
  plot_ly(chaoteto, values= ~Grupo, labels = ~efeito, 
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          marker = list(colors = rev(viridis(nrow(chaoteto), alpha = 0.9)))) %>% 
    add_pie(hole = 0.6) %>% 
    layout(title = nome,  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
}






#' Outlier
#'
#' @param dado Coluna que será usada como base
#'
#' @return As linhas que possuem outliers

identOutlier = function(dado){ 
  OutVals = boxplot(dado)$out %>% unique()
  which(dado %in% OutVals)
  }

#' Faixa Etária
#'
#' @param data Coluna que será usada como base
#'
#' @return Vetor com as idades encaixadas em Faixa etária
#' 
### Desabilitado pois não foi usado na análise descritiva ----------------------------------------------------------- 
# c("<=24 anos", "25-34 anos", "35-44 anos", "45-54 anos", "55-64 anos", "65-74 anos" )
# 
# faixa = function(data){
#   idade = data
#   idade[between(data, 0, 24)] = 1
#   idade[between(data, 25, 34)] = 2
#   idade[between(data, 35, 44)] = 3
#   idade[between(data, 45, 54)] = 4
#   idade[between(data, 55, 64)] = 5
#   idade[between(data, 65, 74)] = 6
#   idade[between(data, 75,100)] = 7
#   idade 
# }

#' Plot Faixa Etária
#'
#' @param data,coluna,nome Coluna que será usada como base, título do gráfico
#'
#' @return Scaterplot das médias dos domínios por faixa etária

### Desabilitado pois não foi usado na análise descritiva ----------------------------------------------------------- 
# plotfEtaria = function(data,coluna, nome){
#   plot_ly(data, x = ~FaixaEtaria, y = ~coluna, 
#            color = ~Sexo, type = "scatter", mode = "lines", name = nome
#           ) %>%
#     layout(showlegend = F)
# }

#' Efeito Chão
#'
#' @param data,limite Coluna que será usada como base, limite do efeito chão
#'
#' @return porcentagem dos dados que estão abaixo do limite chão

chao =  function(data, limite){
  a = data[ data <= limite] %>%
    length()
}

#' Efeito Teto
#'
#' @param data,limite Coluna que será usada como base, limite do efeito teto
#'
#' @return porcentagem dos dados que estão abaixo do limite teto

teto =  function(data, limite){
  a = data[ data >= limite] %>%
    length()
  
}

####################################### Agrupamento


#' Cluster Hierárquico (Método Ward)
#'
#' @param subdados Subconjunto de dados a serem agrupados
#'
#' @return modelo do Método Ward
### Desabilitado pois não foi usado na análise descritiva ----------------------------------------------------------- 
# ClusWard = function(subdados){
#   hclustfunc <- function(x) hclust(x, method="ward.D")
#   distfunc <- function(x) {as.dist((1-cor(t(x)))/2)}
#   subdados$SEXO = subdados$SEXO %>% #transformando os dados em numérico
#     as.factor() %>% 
#     as.numeric()
#   d <- distfunc(subdados)
#   fit <- hclustfunc(d)
#   fit
# }

#' Agrupamento dos domínios
#'
#' @param subdados;num Subconjunto de dados a serem agrupados, número de agrupamentos
#'
#' @return novo dataframe com os subgrupos
### Desabilitado pois não foi usado na análise descritiva ----------------------------------------------------------- 
# Dataagrup = function(subdados, num){
#   subdados$SEXO = subdados$SEXO %>% #transformando os dados em numérico
#     as.factor() %>% 
#     as.numeric()
#   ag = kmeans(subdados, num)
#   membros = ag$cluster
#   dg = data.frame(subdados, membros)
#   dg
# }

#' Gráfico Barras Domínio por algo
#'
#' @param tabela, col1,col2,titulo Tabela de dados completa, Numero da coluna a ser agrupada, Numero da coluna a ser feita a proporção
#'
#' @return tabela de frequências


DominioBarras_tab =  function(tabela,col1, col2){
  ## Fazendo um novo df agrupando pelo col1 escolhido, contando as informações de col2 e fazendo sua proporção
  tes =  tabela %>% 
    group_by(tabela[,col1], tabela[,col2]) %>%
    tally()
  names(tes) = c("Grupo", "Dominio", "n")
  tes %<>%  
    arrange(Grupo, Dominio) %>%  
    mutate( prop = lapply(unique(tes$Grupo), function(a){ 
      da = tes %>% 
        filter(Grupo == a ) 
      da$n/sum(da$n)*100}) %>% 
        unlist(use.names = F)) 
  ## Criando uma nova coluna com as cores de cada Grupo
  tes %<>% 
    mutate(cores = lapply(unique(tes$Grupo), function(s){
      a = tes %>% 
        filter(Grupo == s) %>% 
        nrow()
      rev(viridis(a, alpha = 0.9))}) %>% 
        unlist(use.names = F)) 
  return(tes)}


#' Gráfico Barras Domínio por algo
#'
#' @param tabeladefrequencias,titulo Tabela de frequências gerada com a função DominioBarras_tab, Título do gráfico
#'
#' @return Gráfico de barras horizontais com a proporção de algum domínio
#' 

DominioBarras_plot = function(tes, titulo){
  
  ## Fazendo o plot
  plot_ly(tes, x = ~prop, y = ~Grupo, 
          orientation = "h", type = "bar",
          text = paste0(round(tes$prop,2),"%<br>","Quantidade: ", tes$n, "<br>", tes$Dominio),
          marker = list(color = ~cores,
                        line = list(color = 'rgb(248, 248, 249)', width = 1))) %>% 
    layout(title = titulo,
           xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        zeroline = FALSE),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        zeroline = FALSE),
           barmode = 'stack')}

#' Tabela de frequÊncia Perguntas com um opção
#'
#' @param dado,ind tabela de Dados completa, índice das questões únicas [usar só os índices (1,2,6,7,8,10)]
#'
#' @return Tabela com as frequências das respostas 
#' 

TabfreqPerguntasUnica = function(dado, ind){
  indi = case_when(
    ind == 1 ~ 9,
    ind == 2 ~ 10,
    ind == 6 ~ 28,
    ind == 7 ~ 29,
    ind == 8 ~ 30,
    ind == 10 ~ 40
  )
  tes =  dado %>% 
    group_by(dado[,indi]) %>% 
    na.omit() %>% 
    tally()
  names(tes) = c("Grupo", "n")
  tes %<>%  
    arrange(Grupo) %>%  
    mutate( prop = round((n/sum(n))*100,2)) 
  
  leg = list(Q1 = c("Excelente",	"Muito Boa",	"Boa",	"Ruim"),
             Q2 = c("Muito Melhor",	"Um Pouco Melhor",	"Quase a Mesma",	"Um Pouco Pior",	"Muito Pior"),
             Q6 = c("De forma nenhuma",	"Ligeiramente",	"Moderadamente",	"Bastante",	"Extremamente"),
             Q7 = c("Nenhuma",	"Muito leve",	"Leve",	"Moderada",	"Grave",	"Muito grave"),
             Q8 = c("De maneira alguma",	"Um pouco",	"Moderadamente","	Bastante",	"Extremamente"),
             Q10 = c("Todo Tempo","	A maior parte do tempo",	"Alguma parte do tempo",	"Uma pequena parte do tempo",	"Nenhuma parte do tempo"))
  ind = case_when(
    ind == 6 ~3,
    ind == 7 ~4,
    ind == 8 ~5,
    ind == 10 ~6,
    TRUE ~ ind
  )
  ## Criando uma nova coluna com as cores de cada Grupo
  tes %<>% arrange(n) %>% 
    mutate(cores = rev(viridis(nrow(tes), alpha = 0.9))) %>% 
    arrange(Grupo) %>% 
    mutate(leg = leg[[ind]] %>% factor(levels = leg[[ind]]))
  return(tes)
}


#' Gráfico de barras de frequências de Perguntas únicas
#'
#' @param tes,titulo tabela de frequencia criada pela função TabfreqPerguntasUnica, título do gráfico
#'
#' @return Gráfico de barras com as frequências de PErguntas únicas 
#' 
Plot_freqPerguntasUnica = function(tes, titulo){
  plot_ly(tes, x = ~n, y = ~Grupo, 
          orientation = "h", type = "bar",
          text = paste0(round(tes$prop,2),"% <br>", tes$leg),
          marker = list(color = ~cores,
                        line = list(color = 'rgb(248, 248, 249)', width = 1))) %>% 
    layout(title = titulo,
           xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        zeroline = FALSE),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        zeroline = FALSE),
           barmode = 'stack')}


#' Tabela de frequências perguntas multiplas
#'
#' @param dado,ianda,enunciado Dados filtrados das colunas referente a uma pergunta, significado de cada resposta, enunciado de cada letra
#'
#' @return Tabela com as frequências de PErguntas multiplas 
#' 

#data.frame(b$Group.1, b$x) %>% str()


dadosQuestoesMultiplas = function(dado, legenda, enunciado){
  
  # Gerando a frequência das respostas para cada letra da questão
  vtQmultiplas = apply(dado,2, function(a){
    a %>% count()
  }) 
  
  # Juntando os resultados em uma tabela
  vtQmultiplas = do.call(cbind, vtQmultiplas)
  
  vtQmultiplas=   vtQmultiplas[,c(1,seq(2,ncol(vtQmultiplas),2))] %>% 
    mutate(leg =factor(legenda,  
                       levels = legenda))
  tirar = nrow(vtQmultiplas)
  a = melt(vtQmultiplas)
  a = a[-c(1:tirar),]
  
  b = aggregate(a$value, by = list(a$variable), FUN = prop.table)
  d = melt(cbind(b[1],as.matrix(b[-1])), id = "Group.1") %>% arrange(Group.1)
  dado = cbind(a,prop = d$value)
  quant = unique(dado$leg) %>% length()
  dado %<>% mutate(cores = rep(rev(viridis(quant, alpha = 0.9)),nrow(dado)/quant),
                   enunciadoq = factor(variable, labels = enunciado))
  dado
} # Fim da função

#' Plot de frequência perguntas multiplas
#'
#' @param tabelaQuestoesMultiplas Tabela frequência de perguntas multiplas criado pela função dadosQuestoesMultiplas
#'
#' @return Gráfico de barras com as frequências de PErguntas multiplas
#' 

PlotBarras_questoesMultiplas = function(tabelaQuestoesMultiplas, titulo){
  plot_ly(tabelaQuestoesMultiplas, x = ~value, y = ~variable, type = "bar", 
          orientation = "h", 
          text = paste0(tabelaQuestoesMultiplas$leg, "<br>",round((tabelaQuestoesMultiplas$prop)*100,2),"% <br>", tabelaQuestoesMultiplas$enunciadoq),
          marker = list(color = ~cores,
                        line = list(color = 'rgb(248, 248, 249)', width = 1))) %>% 
    layout(title = titulo,
           xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        zeroline = FALSE),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        zeroline = FALSE),
           barmode = 'stack')}
