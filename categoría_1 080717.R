pacman::p_load(NLP,tm,SnowballC,readxl,wordcloud,RColorBrewer,dplyr)


path<-"Data.xlsx"
xlsx_excel<-read_excel(path)
excel_sheets(path)
DataMaestra<-read_excel(path, sheet = "Hoja4")

View(DataMaestra)

Test<-filter(DataMaestra,Test==1);View(Test)
DataMaestra<-filter(DataMaestra,Test==0);View(DataMaestra)


#cargar el corpus de documentos
#DataMaestra=as.data.frame(read.csv2("C:/Users/Claudia/OneDrive/Documents/Publicaciones/AbstractsFinales.csv"))

################################ funciones #########################################################################

#Recibe un vector de character y entrega la lista de los más frecuentes ordenada.
Doc_term_Matrix<-function (StringVector)
{
  StringVector<-Corpus(VectorSource(StringVector))
  
  toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
  
  vector_cleaning<-c("!","#","$","%","&","/", "•", "‘", "’", "-","<", ">")
  
  for (i in 1:length(vector_cleaning))
  {
    StringVector <- tm_map(StringVector, toSpace, vector_cleaning[i])
  }
  
  
  #remover signos de  puntuación
  StringVector <- tm_map(StringVector, removePunctuation)
  
  #remover número
  StringVector <- tm_map(StringVector, removeNumbers)
  
  StringVector <-tm_map(StringVector,content_transformer(tolower))
  
  #cargar la lista de stopwords
  stopwords <- c(stopwords(kind = "english"))
  
  #remover stopwords
  StringVector <- tm_map(StringVector, removeWords, stopwords)
  
  #remover whitespace
  StringVector <- tm_map(StringVector, stripWhitespace)
  
  #cargar la lista de generic terms
  genericterms <- readLines("genericterms.txt")
  
  #remover generic terms
  StringVector <- tm_map(StringVector, removeWords, genericterms)
  
  #aplicar stemming al documento
  StringVector <- tm_map(StringVector,stemDocument, language ="english")
  
  #remover generic terms
  StringVector <- tm_map(StringVector, removeWords, genericterms)
  
  dtm <- DocumentTermMatrix(StringVector)
  
  dtm
}




Keyword<-function (StringVector)
{
  StringVector<-Corpus(VectorSource(StringVector))
  
  toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})

  vector_cleaning<-c("!","#","$","%","&","/", "•", "‘", "’", "-","<", ">")
  
  for (i in 1:length(vector_cleaning))
  {
    StringVector <- tm_map(StringVector, toSpace, vector_cleaning[i])
  }


  #remover signos de  puntuación
  StringVector <- tm_map(StringVector, removePunctuation)
  
  #remover número
  StringVector <- tm_map(StringVector, removeNumbers)

  StringVector <-tm_map(StringVector,content_transformer(tolower))
  
  #cargar la lista de stopwords
  stopwords <- c(stopwords(kind = "english"))
  
  #remover stopwords
  StringVector <- tm_map(StringVector, removeWords, stopwords)
  
  #remover whitespace
  StringVector <- tm_map(StringVector, stripWhitespace)
  
  #cargar la lista de generic terms
  genericterms <- readLines("genericterms.txt")
  
  #remover generic terms
  StringVector <- tm_map(StringVector, removeWords, genericterms)
    
  #aplicar stemming al documento
  StringVector <- tm_map(StringVector,stemDocument, language ="english")
  
  #remover generic terms
  StringVector <- tm_map(StringVector, removeWords, genericterms)

  dtm <- DocumentTermMatrix(StringVector)
  
  #agrupar sumando por columnas
  freq <- colSums(as.matrix(dtm))
  
  #el largo debe ser igual a total de términos
  length(freq)-ncol(dtm)==0
  
  #ordenar por frecuencia (descendiente)
  ord <- order(freq,decreasing=TRUE)
  
  #Enlistar los términos por frecuencia
  data.frame(freq[ord])
  
}


Keyword_rev<-function(StringVector)
{
  StringVector<-Corpus(VectorSource(StringVector))
  
  toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
  
  vector_cleaning<-c("!","#","$","%","&","/", "•", "‘", "’", "-","<", ">")
  
  for (i in 1:length(vector_cleaning))
  {
    StringVector <- tm_map(StringVector, toSpace, vector_cleaning[i])
  }
  
  
  #cargar la lista de generic terms
  genericterms_rev <- readLines("genericterms_revista.txt")
  
  #remover generic terms
  StringVector <- tm_map(StringVector, removeWords, genericterms_rev)
  
  #remover signos de  puntuación
  StringVector <- tm_map(StringVector, removePunctuation)
  
  #remover número
  StringVector <- tm_map(StringVector, removeNumbers)
  
  StringVector <-tm_map(StringVector,content_transformer(tolower))
  
  #cargar la lista de stopwords
  stopwords <- c(stopwords(kind = "english"))
  
  #remover stopwords
  StringVector <- tm_map(StringVector, removeWords, stopwords)
  
  #remover whitespace
  StringVector <- tm_map(StringVector, stripWhitespace)
  

  

  #aplicar stemming al documento
  StringVector <- tm_map(StringVector,stemDocument, language ="english")
  
  
  #remover generic terms
  StringVector <- tm_map(StringVector, removeWords, genericterms_rev)
  
  
  dtm <- DocumentTermMatrix(StringVector)
  
  #agrupar sumando por columnas
  freq <- colSums(as.matrix(dtm))
  
  #el largo debe ser igual a total de términos
  length(freq)-ncol(dtm)==0
  
  #ordenar por frecuencia (descendiente)
  ord <- order(freq,decreasing=TRUE)
  
  #Enlistar los términos por frecuencia
  data.frame(freq[ord])
  
}



#Busca dentro de un texto una palabra y retorna TRUE si es que existe.
existe_palabra<-function(abstract,p_encontrada)
{
  existe=FALSE
  abs2<-unlist(strsplit(abstract," "))
  if(is.element(p_encontrada,abs2))
  {
    existe=TRUE
  }
  return(existe)
}

#######################  Metadata de la respuesta ##################################################################

#Se inicializa la primera palabra
palabra_encontrada=""

#Se inicializan las categorías con las palabras claves del texto. Se le añade la frecuencia, categoría, y la iteración.
DataMaestra=mutate(DataMaestra,key1="",key2="",key3="",key4="",key5="",
                   Categoria_1="",Frecuencia_1=0,j_1=0,Porcentaje=0.0
)


#Todavía no ha terminado, y la iteración es cero
Terminar=FALSE
j=0

Aux<-DataMaestra

#Todo el corpus como un vector
AbstractVector=as.vector(Aux$Abstract)

#Vector vacío
TotalKeyWords=vector(mode = "character",length=length(AbstractVector))

######################################## Palabras Frecuentes        #########################################


for(i in 1:nrow(DataMaestra)){
  
  #Cada abstract como un vector
  NewAbstractVector=as.character(DataMaestra[i,"Abstract"])
  
  KeyWordsAbsi=Keyword(NewAbstractVector)
  rownamesFreq=rownames(KeyWordsAbsi)
  DataMaestra[i,"key1"]=rownamesFreq[1]
  DataMaestra[i,"key2"]=rownamesFreq[2]
  DataMaestra[i,"key3"]=rownamesFreq[3]
  DataMaestra[i,"key4"]=rownamesFreq[4]
  DataMaestra[i,"key5"]=rownamesFreq[5]
}

View(DataMaestra)

############################################################################################################
Aux<-DataMaestra



while(!Terminar)
{
    
      #El conjunto de las palabras más frecuentes de cada abstract <-- 
      #Aquí la diferencia de haber sacado las más frecuentes del conjunto completo, 
      #esto es más representativo de lo que es importante en cada uno
      prop<-1
      TotalKeyWords=paste(Aux[,"key1"],Aux[,"key2"],Aux[,"key3"],Aux[,"key4"],Aux[,"key5"],sep=" ")
      TotalKeyWordsC<-Corpus(VectorSource(TotalKeyWords))
      TotalKeyWordsC <-tm_map(TotalKeyWordsC,content_transformer(tolower))
      toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
      dtm <- DocumentTermMatrix(TotalKeyWordsC)
      
      #agrupar sumando por columnas
      freq <- colSums(as.matrix(dtm))
      
      #el largo debe ser igual a total de términos
      length(freq)-ncol(dtm)==0
      
      #ordenar por frecuencia (descendiente)
      ord <- order(freq,decreasing=TRUE)
      
      #Enlistar los términos por frecuencia
      FrecuenciaFinal=data.frame(freq[ord])
            
      palabra_encontrada=rownames(FrecuenciaFinal)[1]

      
      #Colocar la etiqueta de categoria
      for(i in 1:nrow(DataMaestra))
      {
        if(existe_palabra(paste(DataMaestra[i,"key1"],DataMaestra[i,"key2"],DataMaestra[i,"key3"],DataMaestra[i,"key4"],DataMaestra[i,"key5"],sep=" "),palabra_encontrada) && DataMaestra[i,"Categoria_1"]=="")
        {
          DataMaestra[i,"Categoria_1"]=palabra_encontrada
          DataMaestra[i,"Frecuencia_1"]=FrecuenciaFinal[1,1]
          DataMaestra[i,"j_1"]=j
          DataMaestra[i,"Porcentaje"]=FrecuenciaFinal[1,1]/nrow(DataMaestra)
          prop<-FrecuenciaFinal[1,1]/nrow(DataMaestra)
        }
      }

      print(j)
      print(prop)
      print(palabra_encontrada)
      j=j+1
      Aux<-filter(DataMaestra,Categoria_1=="")
      print(nrow(Aux))
      if(nrow(Aux)==0 || prop<0.01)
      {
        Terminar=TRUE
      }
      
}


# NivelesCategorias<-levels(as.factor(DataMaestra$Categoria_1))
# for (i in 1:length(NivelesCategorias))
# {
#   #DataMaestra[,NivelesCategorias[i]]
#   Words<-filter(DataMaestra,Categoria_1==NivelesCategorias[i]) 
#   print(i)
#   FrecuenciaFinal<-Keyword(Words$Abstract)
#   archivo<-paste(NivelesCategorias[i],"_wordcloud_11_9.png")
#   png(filename=archivo)
#   layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
#   par(mar=rep(0, 4))
#   plot.new()
#   text(x=0.5, y=0.5, paste(NivelesCategorias[i]," Wordcloud"))
#   
#   wordcloud(words = rownames(FrecuenciaFinal)[2:length(rownames(FrecuenciaFinal))], freq = FrecuenciaFinal$freq.ord.[2:length(FrecuenciaFinal$freq.ord.)], min.freq = 1,
#             max.words=200, random.order=FALSE, rot.per=0.35, 
#             colors=brewer.pal(8, "Dark2"),main="Title")
#   dev.off()
# }



###################  K-Means ##########################################



####Document Term Matrix Sobre el Corpus
NewAbstractVector=unlist(as.vector(DataMaestra[,"Abstract"]))
mat<-Doc_term_Matrix(NewAbstractVector);mat

 
#Term Frequency Inverse Term
mat4 <- weightTfIdf(mat)
mat4 <- as.matrix(mat4)

#Normalizar los Scores por distancia Euclidiana
norm_eucl <- function(m)
  m/apply(m,1,function(x) sum(x^2)^.5)
mat_norm <- norm_eucl(mat4)


set.seed(5)
k <- 30
kmeansResult <- kmeans(mat_norm, k)
DataMaestra$Cluster_15<-kmeansResult$cluster
View(DataMaestra)

#NivelesCategorias<-levels(as.factor(DataMaestra$Cluster_3))

# for (i in 1:length(NivelesCategorias))
# {
#   #DataMaestra[,NivelesCategorias[i]]
#   Words<-filter(DataMaestra,Cluster_3==NivelesCategorias[i]) 
#   print(i)
#   FrecuenciaFinal<-Keyword(Words$Abstract)
#   archivo<-paste(NivelesCategorias[i],"_wordcloud_Cluster_k3.png")
#   png(filename=archivo)
#   layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
#   par(mar=rep(0, 4))
#   plot.new()
#   text(x=0.5, y=0.5, paste(NivelesCategorias[i]," Wordcloud"))
#   
#   wordcloud(words = rownames(FrecuenciaFinal), freq = FrecuenciaFinal$freq.ord., min.freq = 1,
#             max.words=200, random.order=FALSE, rot.per=0.35, 
#             colors=brewer.pal(8, "Dark2"),main="Title")
#   dev.off()
# }



set.seed(5)
k <- 40
kmeansResult <- kmeans(mat_norm, k)
DataMaestra$Cluster_8<-kmeansResult$cluster
View(DataMaestra)

#NivelesCategorias<-levels(as.factor(DataMaestra$Cluster_8))

# for (i in 1:length(NivelesCategorias))
# {
#   #DataMaestra[,NivelesCategorias[i]]
#   Words<-filter(DataMaestra,Cluster_8==NivelesCategorias[i]) 
#   print(i)
#   FrecuenciaFinal<-Keyword(Words$Abstract)
#   archivo<-paste(NivelesCategorias[i],"_wordcloud_Cluster_k8.png")
#   png(filename=archivo)
#   layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
#   par(mar=rep(0, 4))
#   plot.new()
#   text(x=0.5, y=0.5, paste(NivelesCategorias[i]," Wordcloud"))
#   
#   wordcloud(words = rownames(FrecuenciaFinal), freq = FrecuenciaFinal$freq.ord., min.freq = 1,
#             max.words=200, random.order=FALSE, rot.per=0.35, 
#             colors=brewer.pal(8, "Dark2"),main="Title")
#   dev.off()
# }

set.seed(5)
k <- 50
kmeansResult <- kmeans(mat_norm, k)
saveRDS(kmeansResult,"kmeans.rds")
DataMaestra$Cluster_20<-kmeansResult$cluster
View(DataMaestra)


# NivelesCategorias<-levels(as.factor(DataMaestra$Cluster_10))
# 
# 
# for (i in 1:length(NivelesCategorias))
# {
#   #DataMaestra[,NivelesCategorias[i]]
#   Words<-filter(DataMaestra,Cluster_10==NivelesCategorias[i]) 
#   print(i)
#   FrecuenciaFinal<-Keyword(Words$Abstract)
#   archivo<-paste(NivelesCategorias[i],"_wordcloud_Cluster_k10.png")
#   png(filename=archivo)
#   layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
#   par(mar=rep(0, 4))
#   plot.new()
#   text(x=0.5, y=0.5, paste(NivelesCategorias[i]," Wordcloud"))
#   
#   wordcloud(words = rownames(FrecuenciaFinal), freq = FrecuenciaFinal$freq.ord., min.freq = 1,
#             max.words=200, random.order=FALSE, rot.per=0.35, 
#             colors=brewer.pal(8, "Dark2"),main="Title")
#   dev.off()
# }


####################### LDA #############################################
library(topicmodels)
k <- 35
lda <- LDA(mat, k)
terms(lda)

saveRDS(lda,"lda.rds")

DataMaestra$lda_10<-topics(lda)

#Exportar resultados
#docs en cada tópico
lda.topicos <- as.matrix(topics(lda))
write.csv(lda.topicos,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"DocsTopicos.csv"))

#palabras con mayor frecuencia en cada tópico
lda.palabras <- as.matrix(terms(lda, 10))
write.csv(lda.palabras,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"TopicosPalabras.csv"))


#probabilidad de que cada documento esté asociado a cada topico 
ProbDocTopico <- as.data.frame(lda@gamma)
write.csv(ProbDocTopico,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"ProbTopico.csv"))

#probabilidad de cada palabra esté asociada a un tópico
ProbPalabraTopico <- as.data.frame(exp(lda@beta))
write.csv(ProbPalabraTopico,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"ProbPalabraTopico.csv"))

library(topicmodels)
k <- 40
lda <- LDA(mat, k)
terms(lda)

saveRDS(lda,"lda.rds")

DataMaestra$lda_10<-topics(lda)

#Exportar resultados
#docs en cada tópico
lda.topicos <- as.matrix(topics(lda))
write.csv(lda.topicos,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"DocsTopicos.csv"))

#palabras con mayor frecuencia en cada tópico
lda.palabras <- as.matrix(terms(lda, 10))
write.csv(lda.palabras,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"TopicosPalabras.csv"))


#probabilidad de que cada documento esté asociado a cada topico 
ProbDocTopico <- as.data.frame(lda@gamma)
write.csv(ProbDocTopico,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"ProbTopico.csv"))

#probabilidad de cada palabra esté asociada a un tópico
write.csv(ProbPalabraTopico,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"ProbPalabraTopico.csv"))P
robPalabraTopico <- as.data.frame(exp(lda@beta))

library(topicmodels)
k <- 20
lda <- LDA(mat, k)
terms(lda)

saveRDS(lda,"lda.rds")

DataMaestra$lda_10<-topics(lda)

#Exportar resultados
#docs en cada tópico
lda.topicos <- as.matrix(topics(lda))
write.csv(lda.topicos,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"DocsTopicos.csv"))

#palabras con mayor frecuencia en cada tópico
lda.palabras <- as.matrix(terms(lda, 10))
write.csv(lda.palabras,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"TopicosPalabras.csv"))


#probabilidad de que cada documento esté asociado a cada topico 
ProbDocTopico <- as.data.frame(lda@gamma)
write.csv(ProbDocTopico,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"ProbTopico.csv"))

#probabilidad de cada palabra esté asociada a un tópico
ProbPalabraTopico <- as.data.frame(exp(lda@beta))
write.csv(ProbPalabraTopico,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"ProbPalabraTopico.csv"))






library(topicmodels)
k <- 50
lda <- LDA(mat, k)
terms(lda)

saveRDS(lda,"lda.rds")

DataMaestra$lda_10<-topics(lda)

# NivelesCategorias<-levels(as.factor(DataMaestra$lda_10))
# 
# for (i in 1:length(NivelesCategorias))
# {
#   #DataMaestra[,NivelesCategorias[i]]
#   Words<-filter(DataMaestra,lda_10==NivelesCategorias[i]) 
#   print(i)
#   FrecuenciaFinal<-Keyword(Words$Abstract)
#   archivo<-paste(NivelesCategorias[i],"_wordcloud_lda_10.png")
#   png(filename=archivo)
#   layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
#   par(mar=rep(0, 4))
#   plot.new()
#   text(x=0.5, y=0.5, paste(NivelesCategorias[i]," Wordcloud"))
#   
#   wordcloud(words = rownames(FrecuenciaFinal), freq = FrecuenciaFinal$freq.ord., min.freq = 1,
#             max.words=200, random.order=FALSE, rot.per=0.35, 
#             colors=brewer.pal(8, "Dark2"),main="Title")
#   dev.off()
# }

#Exportar resultados
#docs en cada tópico
lda.topicos <- as.matrix(topics(lda))
write.csv(lda.topicos,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"DocsTopicos.csv"))

#palabras con mayor frecuencia en cada tópico
lda.palabras <- as.matrix(terms(lda, 10))
write.csv(lda.palabras,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"TopicosPalabras.csv"))


#probabilidad de que cada documento esté asociado a cada topico 
ProbDocTopico <- as.data.frame(lda@gamma)
write.csv(ProbDocTopico,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"ProbTopico.csv"))

#probabilidad de cada palabra esté asociada a un tópico
ProbPalabraTopico <- as.data.frame(exp(lda@beta))

library(topicmodels)
k <- 55
lda <- LDA(mat, k)
terms(lda)

saveRDS(lda,"lda30.rds")

DataMaestra$lda_30<-topics(lda)

saveRDS(DataMaestra,"DataFinal.rds")

#Exportar resultados
#docs en cada tópico
lda.topicos <- as.matrix(topics(lda))
write.csv(lda.topicos,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"DocsTopicos.csv"))

#palabras con mayor frecuencia en cada tópico
lda.palabras <- as.matrix(terms(lda, 10))
write.csv(lda.palabras,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"TopicosPalabras.csv"))


#probabilidad de que cada documento esté asociado a cada topico 
ProbDocTopico <- as.data.frame(lda@gamma)
write.csv(ProbDocTopico,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"ProbTopico.csv"))

#probabilidad de cada palabra esté asociada a un tópico
ProbPalabraTopico <- as.data.frame(exp(lda@beta))

library(topicmodels)
k <- 48
lda <- LDA(mat, k)
terms(lda)

saveRDS(lda,"lda45.rds")

DataMaestra$lda_45<-topics(lda)

saveRDS(DataMaestra,"DataFinal.rds")


TesteoRDS<-readRDS("DataFinal.rds")

DataMaestra<-readRDS("DataFinal.rds")

#Exportar resultados
#docs en cada tópico
lda.topicos <- as.matrix(topics(lda))
write.csv(lda.topicos,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"DocsTopicos.csv"))

#palabras con mayor frecuencia en cada tópico
lda.palabras <- as.matrix(terms(lda, 10))
write.csv(lda.palabras,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"TopicosPalabras.csv"))


#probabilidad de que cada documento esté asociado a cada topico 
ProbDocTopico <- as.data.frame(lda@gamma)
write.csv(ProbDocTopico,file=paste("C:/Users/Claudia/Documents/Tesis/LDA",k,"ProbTopico.csv"))

#probabilidad de cada palabra esté asociada a un tópico
ProbPalabraTopico <- as.data.frame(exp(lda@beta))
######################## Evaluación  Modelos no Supervisados ###########################################
26/11/17 17:36 creo que funcionó hasta aquí

View(as.data.frame(table(DataMaestra$Disciplina,DataMaestra$lda_30)))
#install.packages("reshape")
library(reshape)

as.data.frame(table(DataMaestra$Disciplina,DataMaestra$lda_30))%>%cast(Var1~Var2)->tablaResultadosLDA30;tablaResultadosLDA30%>%View()


tablaResultadosLDA30%>%select(2:31)%>%rowSums()->TotalCat
tablaResultadosLDA30%>%select(2:31)/as.vector(TotalCat)->Porcentaje

cbind(tablaResultadosLDA30[,1],Porcentaje)->df
df[df == 0] <- NA
View(df)
df%>%as.data.frame()%>%write.csv2("DATOS.csv")

####Ver como exportar

#########################################################################
#################### Aprendizaje Supervisado ############################
#########################################################################

library(caTools)

stratified = function(df, group, size) {
  #  USE: * Specify your data frame and grouping variable (as column 
  #         number) as the first two arguments.
  #       * Decide on your sample size. For a sample proportional to the
  #         population, enter "size" as a decimal. For an equal number 
  #         of samples from each group, enter "size" as a whole number.
  #
  #  Example 1: Sample 10% of each group from a data frame named "z",
  #             where the grouping variable is the fourth variable, use:
  # 
  #                 > stratified(z, 4, .1)
  #
  #  Example 2: Sample 5 observations from each group from a data frame
  #             named "z"; grouping variable is the third variable:
  #
  #                 > stratified(z, 3, 5)
  #
  require(sampling)
  temp = df[order(df[group]),]
  if (size < 1) {
    size = ceiling(table(temp[group]) * size)
  } else if (size >= 1) {
    size = rep(size, times=length(table(temp[group])))
  }  
  strat = strata(temp, stratanames = names(temp[group]), 
                 size = size, method = "srswor")
  (dsample = getdata(temp, strat))
}

########################################################################
############################## Tratamiento de Revistas #################
########################################################################

#Keyword_rev(DataMaestra$Titulo_Revista)%>%View()


#######################  Metadata de la respuesta ##################################################################

#Se inicializa la primera palabra
palabra_encontrada=""

#Se inicializan las categorías con las palabras claves del texto. Se le añade la frecuencia, categoría, y la iteración.
DataMaestra=mutate(DataMaestra,keyR1="",keyR2="",keyR3="",keyR4="",keyR5="",
                   Categoria_R="",Frecuencia_R=0,j_R=0,PorcentajeR=0.0, Categoria_R1="",Categoria_R2=""
)


#Todavía no ha terminado, y la iteración es cero
Terminar=FALSE
j=0

Aux<-DataMaestra

#Todo el corpus como un vector
AbstractVector=as.vector(Aux$Titulo_Revista)

#Vector vacío
TotalKeyWords=vector(mode = "character",length=length(AbstractVector))

######################################## Palabras Frecuentes        #########################################
#names(DataMaestra)

#Sacar las que no sirven (HECHO)

for(i in 1:nrow(DataMaestra)){
  
  #Cada abstract como un vector
  NewAbstractVector=as.character(DataMaestra[i,"Titulo_Revista"])
  
  KeyWordsAbsi=Keyword_rev(NewAbstractVector)
  rownamesFreq=rownames(KeyWordsAbsi)
  DataMaestra[i,"keyR1"]=rownamesFreq[1]
  DataMaestra[i,"keyR2"]=rownamesFreq[2]
  DataMaestra[i,"keyR3"]=rownamesFreq[3]
  DataMaestra[i,"keyR4"]=rownamesFreq[4]
  DataMaestra[i,"keyR5"]=rownamesFreq[5]
}


View(DataMaestra)

############################################################################################################
Aux<-DataMaestra

j=0

# while(!Terminar)
# {
  
  #El conjunto de las palabras más frecuentes de cada abstract <-- 
  #Aquí la diferencia de haber sacado las más frecuentes del conjunto completo, 
  #esto es más representativo de lo que es importante en cada uno
  prop<-1
  TotalKeyWords=paste(Aux[,"keyR1"],Aux[,"keyR2"],Aux[,"keyR3"],Aux[,"keyR4"],Aux[,"keyR5"],sep=" ")
  TotalKeyWordsC<-Corpus(VectorSource(TotalKeyWords))
  TotalKeyWordsC <-tm_map(TotalKeyWordsC,content_transformer(tolower))
  toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
  dtm <- DocumentTermMatrix(TotalKeyWordsC)
  
  #agrupar sumando por columnas
  freq <- colSums(as.matrix(dtm))
  
  #el largo debe ser igual a total de términos
  length(freq)-ncol(dtm)==0
  
  #ordenar por frecuencia (descendiente)
  ord <- order(freq,decreasing=TRUE)
  
  #Enlistar los términos por frecuencia
  FrecuenciaFinal=data.frame(freq[ord])
  
  palabra_encontrada=rownames(FrecuenciaFinal)
  
  
  
  
  
  #Colocar la etiqueta de categoria
  for(i in 1:nrow(DataMaestra))
  {
    keys_revista=c("","")
    m=1
    for(h in 1:length(palabra_encontrada))
    {
      print(i)
      if(existe_palabra(paste(DataMaestra[i,"keyR1"],DataMaestra[i,"keyR2"],DataMaestra[i,"keyR3"],DataMaestra[i,"keyR4"],DataMaestra[i,"keyR5"],sep=" "),palabra_encontrada[h]) )
      {
        if(m<3)
        {
          keys_revista[m]=palabra_encontrada[h]
          m=m+1
        }
        else
        {
          break;
          
        }
        
        # 
        # DataMaestra[i,"Categoria_R"]=palabra_encontrada
        # DataMaestra[i,"Frecuencia_R"]=FrecuenciaFinal[1,1]
        # DataMaestra[i,"j_R"]=j
        # DataMaestra[i,"PorcentajeR"]=FrecuenciaFinal[1,1]/nrow(DataMaestra)
        # prop<-FrecuenciaFinal[1,1]/nrow(DataMaestra)
      }
    }
    DataMaestra[i,"Categoria_R1"]=keys_revista[1]
    DataMaestra[i,"Categoria_R2"]=keys_revista[2]    
  }
  # 
  # print(j)
  # print(prop)
  # print(palabra_encontrada)
  # j=j+1
  # Aux<-filter(DataMaestra,Categoria_R=="")
  # print(nrow(Aux))
  # if(nrow(Aux)==0 || prop<0.005)
  # {
  #   Terminar=TRUE
  # }
  
# }

DataMaestra$Categoria_R<-NULL



#########################################################################
#################### Guardar el dominio #################################
#########################################################################

DataMaestra%>%summary()
DataMaestra$Disciplina<-as.factor(DataMaestra$Disciplina)
DataMaestra$Categoria_1<-as.factor(DataMaestra$Categoria_1)
DataMaestra$Cluster_15<-as.factor(DataMaestra$Cluster_15)
DataMaestra$Cluster_8<-as.factor(DataMaestra$Cluster_8)
DataMaestra$Cluster_20<-as.factor(DataMaestra$Cluster_20)
DataMaestra$lda_10<-as.factor(DataMaestra$lda_10)
DataMaestra$lda_30<-as.factor(DataMaestra$lda_30)
DataMaestra$lda_45<-as.factor(DataMaestra$lda_45)
DataMaestra$Titulo_Revista<-as.factor(DataMaestra$Titulo_Revista)
DataMaestra$Categoria_R1<-as.factor(DataMaestra$Categoria_R1)
DataMaestra$Categoria_R2<-as.factor(DataMaestra$Categoria_R2)

addNoAnswer <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "No Answer")))
  return(x)
}

DataMaestra <- as.data.frame(lapply(DataMaestra, addNoAnswer))


l<-list()
l[["Disciplina"]]<-levels(DataMaestra$Disciplina)
l[["Categoria_1"]]<-levels(DataMaestra$Categoria_1)
l[["Cluster_15"]]<-levels(DataMaestra$Cluster_15)
l[["Cluster_8"]]<-levels(DataMaestra$Cluster_8)
l[["Cluster_20"]]<-levels(DataMaestra$Cluster_20)
l[["lda_10"]]<-levels(DataMaestra$lda_10)
l[["lda_30"]]<-levels(DataMaestra$lda_30)
l[["lda_45"]]<-levels(DataMaestra$lda_45)
l[["Titulo_Revista"]]<-levels(DataMaestra$Titulo_Revista)
l[["Categoria_R1"]]<-levels(as.factor(DataMaestra$Categoria_R1))
l[["Categoria_R2"]]<-levels(as.factor(DataMaestra$Categoria_R2))


#########################################################################
##################### Entrenamiento SVM #################################
#########################################################################

library(dplyr)
UniversoSupervisados<-filter(DataMaestra,!is.na(Disciplina));View(UniversoSupervisados)
#install.packages("sampling")
library(sampling)

clasificados<-stratified(as.data.frame(UniversoSupervisados),7,.45)


#names(clasificados)

Abstract_Catalogados=mutate(clasificados, AbstractYRevista= paste(Abstract,keyR1,keyR2,keyR3,keyR4,keyR5,sep=" ")) #paste(Abstract,Titulo_Revista,sep=" "))

Abstract_Catalogados=filter(Abstract_Catalogados,Disciplina!="")

source("Entrenamiento_Svm_1.R")

levels(Abstract_Catalogados$Disciplina)<-l$Disciplina
Abstract_Catalogados%>%summary()

EntrenarSVMTexto(Abstract_Catalogados$Disciplina,Abstract_Catalogados$AbstractYRevista)

modelo<-readRDS("ModeloEntrenado.rds")


#Se saca el 55% restante para evaluar
x <- rownames(UniversoSupervisados)
y <- row.names(clasificados)
res <- x[is.na(pmatch(x,y))]

DataAEvaluar<-UniversoSupervisados[as.integer(res),]
#names(DataAEvaluar)

DataAEvaluar=mutate(DataAEvaluar, AbstractYRevista= paste(Abstract,keyR1,keyR2,keyR3,keyR4,keyR5,sep=" "))

MATRIZ_CUERPO=modelo$MATRIZ

PREDICCION_CUERPO <- crear_matriz(DataAEvaluar$AbstractYRevista
                                  ,language='english'
                                  ,removeNumbers=TRUE
                                  ,stemWords=TRUE
                                  ,toLower = TRUE
                                  ,stripWhitespace = TRUE
                                  ,removeStopwords = TRUE
                                  ,originalMatrix=MATRIZ_CUERPO)



largo_prediccion_CUERPO=nrow(PREDICCION_CUERPO);

predictionContainer_CUERPO<- create_container(PREDICCION_CUERPO, 
                                              labels=rep(0,largo_prediccion_CUERPO)
                                              ,testSize=1:largo_prediccion_CUERPO, virgin=FALSE)



modelo_entrenado=modelo$MODELO
scores <- classify_model(predictionContainer_CUERPO,modelo_entrenado)

DataAEvaluar$Etiqueta<-scores$SVM_LABEL
DataAEvaluar$Probabilidad<-scores$SVM_PROB
View(DataAEvaluar)

table(DataAEvaluar$Disciplina,DataAEvaluar$Etiqueta)%>%as.data.frame()%>%cast(Var1~Var2)%>%View()

levels(DataAEvaluar$Etiqueta)<-levels(DataAEvaluar$Disciplina)

##### Evaluacion ######
CM_SVM<-confusionMatrix(DataAEvaluar$Disciplina,DataAEvaluar$Etiqueta)
write.csv2(CM_SVM$table,"CF_SVM.csv")
write.csv2(CM_SVM$overall,"Accuracy_SVM.csv")
write.csv2(CM_SVM$byClass,"CF_By_Class.csv")



saveRDS(DataAEvaluar,"SVMEvaluado.rds")


svm_agrupado <- group_by(DataAEvaluar, Ano,Etiqueta)
svm_agrupado<-summarise(svm_agrupado, Total = n(),Probabilidad=mean(Probabilidad, na.rm = T) )

View(svm_agrupado)
write.csv(svm_agrupado,"resultados_svm_agrupados_11_9_17.csv")

library(ggplot2)


p <- ggplot(svm_agrupado, aes(Ano, log(Total)))
p + geom_point(aes(size=Probabilidad,color=factor(Etiqueta)))


archivo<-paste("svm.png")
png(filename=archivo)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, paste(NivelesCategorias[i]," svm_10_9_2017"))
p <- ggplot(svm_agrupado, aes(Ano, log(Total)))
p + geom_point(aes(size=Probabilidad,color=factor(Etiqueta)))

dev.off()


NivelesCategorias=levels(as.factor(DataAEvaluar$Etiqueta))

for (i in 1:length(NivelesCategorias))
{
  #DataMaestra[,NivelesCategorias[i]]
  Words<-filter(DataAEvaluar,Etiqueta==NivelesCategorias[i]) 
  print(i)
  FrecuenciaFinal<-Keyword(Words$Abstract)
  archivo<-paste(NivelesCategorias[i],"_wordcloud_svm_11_10_2017.png")
  png(filename=archivo)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, paste(NivelesCategorias[i]," Wordcloud"))
  
  wordcloud(words = rownames(FrecuenciaFinal), freq = FrecuenciaFinal$freq.ord., min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"),main="Title")
  dev.off()
}




########################## Logic Boost Sobre todo lo que queda ##############################


names(DataAEvaluar)


DataAEvaluar$Disciplina<-as.factor(DataAEvaluar$Disciplina)

DataAEvaluar%>%mutate(Categoria_R1=ifelse(""==Categoria_R1,"Sin Categoria",Categoria_R1))->DataAEvaluar

DataAEvaluar%>%mutate(Categoria_R2=ifelse(""==Categoria_R2,"Sin Categoria",Categoria_R2))->DataAEvaluar

View(DataAEvaluar)
DataAEvaluar$Disciplina<-droplevels(DataAEvaluar$Disciplina)

Data_Para_logistica<-stratified(DataAEvaluar,7,.82)


levels(Data_Para_logistica$Disciplina)<-l$Disciplina
levels(Data_Para_logistica$Categoria_1)<-l$Categoria_1
levels(Data_Para_logistica$Categoria_R1)<-l$Categoria_R1
levels(Data_Para_logistica$Categoria_R2)<-l$Categoria_R2
levels(Data_Para_logistica$Cluster_15)<-l$Cluster_15
levels(Data_Para_logistica$lda_30)<-l$lda_30
levels(Data_Para_logistica$Etiqueta)<-l$Disciplina

View(Data_Para_logistica)

droplevels(Data_Para_logistica)->Data_Para_logistica

glm<-train(Disciplina ~ Categoria_R1+Categoria_R2+ Categoria_1+Cluster_15+lda_30+Etiqueta, data = Data_Para_logistica, 
      method = "LogitBoost")


x <- rownames(DataAEvaluar)
y <- row.names(Data_Para_logistica)
res <- x[is.na(pmatch(x,y))]

EvaluacionBosque<-DataAEvaluar[as.integer(res),]

#levels(as.factor(EvaluacionBosque$Categoria_R1))%in%levels(as.factor(Data_Para_logistica$Categoria_R1))

EvaluacionBosque%>%mutate(Categoria_R1=ifelse(Categoria_R1%in%levels(as.factor(Data_Para_logistica$Categoria_R1)),Categoria_R1,"Sin Categoria"))->EvaluacionBosque

EvaluacionBosque%>%mutate(Categoria_R2=ifelse(Categoria_R2%in%levels(as.factor(Data_Para_logistica$Categoria_R2)),Categoria_R2,"Sin Categoria"))->EvaluacionBosque

#levels(as.factor(Aux$Categoria_R1))%in%levels(as.factor(Data_Para_logistica$Categoria_R1))


predict(glm,as.data.frame(EvaluacionBosque))->p

table(EvaluacionBosque$Disciplina,p)

levels(p)<-levels(as.factor(c(levels(p),levels(EvaluacionBosque$Disciplina))))
levels(EvaluacionBosque$Disciplina)<-levels(p)

CMlb<-confusionMatrix(EvaluacionBosque$Disciplina,p)



##### Evaluacion ######
write.csv2(CMlb$table,"CM_lb.csv")
write.csv2(CMlb$overall,"Accuracy_lb.csv")
write.csv2(CMlb$byClass,"CM_By_Class_lb.csv")



########################## Bosque Sobre todo lo que queda ##############################


RForest<-train(Disciplina ~ Categoria_R1+Categoria_R2+ Categoria_1+Cluster_15+lda_30+Etiqueta, data = Data_Para_logistica, 
           method = "ranger", importance="impurity")

#Escoger modelos
#https://topepo.github.io/caret/available-models.html

x <- rownames(DataAEvaluar)
y <- row.names(Data_Para_logistica)
res <- x[is.na(pmatch(x,y))]

EvaluacionBosque<-DataAEvaluar[as.integer(res),]

EvaluacionBosque%>%mutate(Categoria_R1=ifelse(Categoria_R1%in%levels(as.factor(Data_Para_logistica$Categoria_R1)),Categoria_R1,"Sin Categoria"))->EvaluacionBosque
EvaluacionBosque%>%mutate(Categoria_R2=ifelse(Categoria_R2%in%levels(as.factor(Data_Para_logistica$Categoria_R2)),Categoria_R2,"Sin Categoria"))->EvaluacionBosque
predict(glm,as.data.frame(EvaluacionBosque))->p
table(EvaluacionBosque$Disciplina,p)

levels(p)<-levels(as.factor(c(levels(p),levels(EvaluacionBosque$Disciplina))))
levels(EvaluacionBosque$Disciplina)<-levels(p)
CMbosque<-confusionMatrix(EvaluacionBosque$Disciplina,p)

##### Evaluacion ######
write.csv2(CMbosque$table,"CM_Bosque.csv")
write.csv2(CMbosque$overall,"Accuracy_Bosque.csv")
write.csv2(CMbosque$byClass,"CM_By_Class_Bosque.csv")


#Importancia de las variables con el indice de Gini
#install.packages("randomForest")
ranger::importance(RForest$finalModel)%>%as.data.frame()%>%View()



####################################################################################################

UniversoSupervisados$Disciplina<-as.factor(UniversoSupervisados$Disciplina)

UniversoSupervisados%>%mutate(Categoria_1=ifelse(is.na(Categoria_1),"Sin Categoria",Categoria_1))->DataAEvaluar
UniversoSupervisados$Categoria_1<-as.factor(UniversoSupervisados$Categoria_1)
UniversoSupervisados$Titulo_Revista<-as.factor(UniversoSupervisados$Titulo_Revista)

UniversoSupervisados$Cluster_10<-as.factor(UniversoSupervisados$Cluster_10)
UniversoSupervisados$lda_30<-as.factor(UniversoSupervisados$lda_30)

str(UniversoSupervisados)

Evaluacion90<-stratified(as.data.frame(UniversoSupervisados),7,.9)
View(clasificados);View(UniversoSupervisados)


x <- rownames(UniversoSupervisados)
y <- row.names(Evaluacion90)
res <- x[is.na(pmatch(x,y))]

Evaluacion10<-UniversoSupervisados[as.integer(res),]


glm<-train(Disciplina ~ Titulo_Revista+ Categoria_1+Cluster_10+lda_30, data = Evaluacion90, 
           method = "LogitBoost")



predict(glm,as.data.frame(Evaluacion10))->p

CMlb<-confusionMatrix(Evaluacion10$Disciplina,p)

CMlb$overall


##########################################De aquí en adelante no cacho qué es. Me tinca que son sobras del SVM.

#names(clasificados)

Abstract_Catalogados=mutate(Evaluacion90, AbstractYRevista= paste(Abstract,Titulo_Revista,sep=" "))

Abstract_Catalogados=filter(Abstract_Catalogados,Disciplina!="")

source("Entrenamiento_Svm_1.R")

EntrenarSVMTexto(Abstract_Catalogados$Disciplina,Abstract_Catalogados$AbstractYRevista)

modelo<-readRDS("ModeloEntrenado.rds")

DataAEvaluar<-Evaluacion10
#names(DataAEvaluar)

DataAEvaluar=mutate(DataAEvaluar, AbstractYRevista= paste(Abstract,Titulo_Revista,sep=" "))

MATRIZ_CUERPO=modelo$MATRIZ

PREDICCION_CUERPO <- crear_matriz(DataAEvaluar$AbstractYRevista
                                  ,language='english'
                                  ,removeNumbers=TRUE
                                  ,stemWords=TRUE
                                  ,toLower = TRUE
                                  ,stripWhitespace = TRUE
                                  ,removeStopwords = TRUE
                                  ,originalMatrix=MATRIZ_CUERPO)



largo_prediccion_CUERPO=nrow(PREDICCION_CUERPO);

predictionContainer_CUERPO<- create_container(PREDICCION_CUERPO, 
                                              labels=rep(0,largo_prediccion_CUERPO)
                                              ,testSize=1:largo_prediccion_CUERPO, virgin=FALSE)



modelo_entrenado=modelo$MODELO
scores <- classify_model(predictionContainer_CUERPO,modelo_entrenado)

DataAEvaluar$Etiqueta<-scores$SVM_LABEL
DataAEvaluar$Probabilidad<-scores$SVM_PROB
View(DataAEvaluar)

table(DataAEvaluar$Disciplina,DataAEvaluar$Etiqueta)%>%as.data.frame()%>%cast(Var1~Var2)

confusionMatrix(DataAEvaluar$Disciplina,DataAEvaluar$Etiqueta)





