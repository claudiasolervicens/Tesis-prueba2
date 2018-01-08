


ScoreSVMTexto<-function(MODELO, MATRIZ, MatrizAEvaluar,ColumnaDeInformacion)
{
  
  MATRIZ_CUERPO=MATRIZ
  PREDICCION_CUERPO <- crear_matriz(test$ComoyDiagnosticos
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
  
  
  scores <- classify_model(predictionContainer_CUERPO,modelo_entrenado)
}


