library(readr)
Abstract_Catalogados <- read_csv("Abstract Catalogados.csv")

View(Abstract_Catalogados)

library(dplyr)

names(Abstract_Catalogados)

Abstract_Catalogados=mutate(Abstract_Catalogados, AbstractYRevista= paste(Abstract,Titulo_Revista,sep=" "))

Abstract_Catalogados=filter(Abstract_Catalogados,Disciplina!="")

source("Entrenamiento_Svm_1.R")



EntrenarSVMTexto(Abstract_Catalogados$Disciplina,Abstract_Catalogados$AbstractYRevista)


modelo<-readRDS("ModeloEntrenado.rds")

Abstract_Catalogados <- read_csv("Testeo Tesis Experimento 3.csv")

Abstract_Catalogados=mutate(Abstract_Catalogados, AbstractYRevista= paste(Abstract,Titulo_Revista,sep=" "))

Abstract_Catalogados=filter(Abstract_Catalogados,Abstract!="")


MATRIZ_CUERPO=modelo$MATRIZ

PREDICCION_CUERPO <- crear_matriz(Abstract_Catalogados$AbstractYRevista
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


Abstract_Catalogados$Etiqueta<-scores$SVM_LABEL
Abstract_Catalogados$Probabilidad<-scores$SVM_PROB

View(Abstract_Catalogados)

write.csv(Abstract_Catalogados,"Resultados_SVM_1.csv")

Resultados_Grafico <- read_csv("Resultados_Grafico.csv")


library(ggplot2)
p <- ggplot(Resultados_Grafico, aes(Ano, Cantidad))
p + geom_point(aes(colour = factor(Etiqueta)))

ggplot(Resultados_Grafico, aes(Ano, Cantidad)) +
  geom_point() +
  facet_grid(Etiqueta ~ ., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0))