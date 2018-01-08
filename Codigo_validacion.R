library(readxl)
path<-"C:/Users/César/Desktop/Tesis/LDA/Tesis Abstracts/Data.xlsx"
xlsx_excel<-read_excel(path)
excel_sheets(path)
DataMaestra<-read_excel(path, sheet = "Hoja4")

View(DataMaestra)

Test<-filter(DataMaestra,Test==1);View(Test)

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
  genericterms <- readLines("C:/Users/César/Desktop/Tesis/LDA/Tesis Abstracts/genericterms.txt")
  
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
  genericterms <- readLines("C:/Users/César/Desktop/Tesis/LDA/Tesis Abstracts/genericterms.txt")
  
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



library("wordcloud")
library("RColorBrewer")
library("dplyr")

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


#######################################################################
########### Uso de las Clasificaciones Heurística #####################
#######################################################################


######### Reemplazar por el archivo que sale del entrenamiento ########

path<-"C:/Users/César/Desktop/Tesis/LDA/Tesis Abstracts/resultados_entrenamiento_11_9_17.csv"
library(readr)
Training <- read_csv(path)
View(Training);names(Training)
niveles<-levels(factor(Training$Categoria_1))

############## Catalogar #####################################


#Se inicializa la primera palabra
palabra_encontrada=""

#Se inicializan las categorías con las palabras claves del texto. Se le añade la frecuencia, categoría, y la iteración.
Test=mutate(Test,key1="",key2="",key3="",key4="",key5="",
                   Categoria_1=""
)


#Todavía no ha terminado, y la iteración es cero
Terminar=FALSE
j=0

Aux<-Test

#Todo el corpus como un vector
AbstractVector=as.vector(Aux$Abstract)

#Vector vacío
TotalKeyWords=vector(mode = "character",length=length(AbstractVector))


for(i in 1:nrow(Test)){
  
  #Cada abstract como un vector
  NewAbstractVector=as.character(Test[i,"Abstract"])
  
  KeyWordsAbsi=Keyword(NewAbstractVector)
  rownamesFreq=rownames(KeyWordsAbsi)
  Test[i,"key1"]=rownamesFreq[1]
  Test[i,"key2"]=rownamesFreq[2]
  Test[i,"key3"]=rownamesFreq[3]
  Test[i,"key4"]=rownamesFreq[4]
  Test[i,"key5"]=rownamesFreq[5]
}

Aux<-Test



for (i in niveles)
{
  for(j in 1:nrow(Test))
  {
    if(existe_palabra(paste(Test[j,"key1"],Test[j,"key2"],Test[j,"key3"],Test[j,"key4"],Test[j,"key5"],sep=" "),i) && Test[j,"Categoria_1"]=="")
    {
      Test[j,"Categoria_1"]=i
    }
  }
}
View(Test)

################################ kmeans Predict ######################################


####Document Term Matrix Sobre el Corpus
NewAbstractVector=unlist(as.vector(Test[,"Abstract"]))
mat<-Doc_term_Matrix(NewAbstractVector);mat


#Term Frequency Inverse Term
mat4 <- weightTfIdf(mat)
mat4 <- as.matrix(mat4)

#Normalizar los Scores por distancia Euclidiana
norm_eucl <- function(m)
  m/apply(m,1,function(x) sum(x^2)^.5)
mat_norm <- norm_eucl(mat4)

kmeansResult<-readRDS("kmeans.rds")

clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}

Test$kmeans10<-clusters(mat_norm,kmeansResult$centers)

lda<-readRDS("lda.rds")
predict(lda,mat)

test.topics <- posterior(lda,mat)
(test.topics <- apply(test.topics$topics, 1, which.max))

Test$lda_10<-test.topics

#########################################################################
##################### SVM ###############################################
#########################################################################


modelo<-readRDS("ModeloEntrenado.rds")


DataAEvaluar<-Test
names(DataAEvaluar)

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

#######################################################################
#################     Random Forest    ################################
#######################################################################




