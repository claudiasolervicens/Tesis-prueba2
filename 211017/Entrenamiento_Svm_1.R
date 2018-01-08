if (!require("pacman")) install.packages("pacman")
pacman::p_load(NLP, tm, SnowballC,caret, RTextTools, dplyr)


EntrenarSVMTexto<-function(ColumnaEtiqueta, ColumnaTexto)
{
  crear_matriz<-function (textColumns, language = "english", minDocFreq = 1, 
                          maxDocFreq = Inf, minWordLength = 3, maxWordLength = Inf, 
                          ngramLength = 1, originalMatrix = NULL, removeNumbers = FALSE, 
                          removePunctuation = TRUE, removeSparseTerms = 0, removeStopwords = TRUE, 
                          stemWords = FALSE, stripWhitespace = TRUE, toLower = TRUE, 
                          weighting = weightTf) 
  {
    stem_words <- function(x) {
      split <- strsplit(x, " ")
      return(wordStem(unlist(split), language = language))
    }
    tokenize_ngrams <- function(x, n = ngramLength) return(rownames(as.data.frame(unclass(textcnt(x, 
                                                                                                  method = "string", n = n)))))
    control <- list(bounds = list(local = c(minDocFreq, maxDocFreq)), 
                    language = language, tolower = toLower, removeNumbers = removeNumbers, 
                    removePunctuation = removePunctuation, stopwords = removeStopwords, 
                    stripWhitespace = stripWhitespace, wordLengths = c(minWordLength, 
                                                                       maxWordLength), weighting = weighting)
    if (ngramLength > 1) {
      control <- append(control, list(tokenize = tokenize_ngrams), 
                        after = 7)
    }
    else {
      control <- append(control, list(tokenize = scan_tokenizer), 
                        after = 4)
    }
    if (stemWords == TRUE && ngramLength == 1) 
      control <- append(control, list(stemming = stem_words), 
                        after = 7)
    trainingColumn <- apply(as.matrix(textColumns), 1, paste, 
                            collapse = " ")
    trainingColumn <- sapply(as.vector(trainingColumn, mode = "character"), 
                             iconv, to = "UTF8", sub = "byte")
    corpus <- Corpus(VectorSource(trainingColumn), readerControl = list(language = language))
    matrix <- DocumentTermMatrix(corpus, control = control)
    if (removeSparseTerms > 0) 
      matrix <- removeSparseTerms(matrix, removeSparseTerms)
    if (!is.null(originalMatrix)) {
      terms <- colnames(originalMatrix[, which(!colnames(originalMatrix) %in% 
                                                 colnames(matrix))])
      weight <- 0
      if (attr(weighting, "acronym") == "tf-idf") 
        weight <- 1e-09
      amat <- matrix(weight, nrow = nrow(matrix), ncol = length(terms))
      colnames(amat) <- terms
      rownames(amat) <- rownames(matrix)
      fixed <- as.DocumentTermMatrix(cbind(matrix[, which(colnames(matrix) %in% 
                                                            colnames(originalMatrix))], amat), weighting = weighting)
      matrix <- fixed
    }
    matrix <- matrix[, sort(colnames(matrix))]
    gc()
    return(matrix)
  }
  
  remove_my_stopwords<-function(own_stw, dtm){
    ind<-sapply(own_stw, function(x, words){
      if(any(x==words)) return(which(x==words)) else return(NA)
    }, words=colnames(dtm))
    return(dtm[ ,-c(na.omit(ind))])  
  }  
  
  
  MATRIZ_A_ENTRENAR <- crear_matriz(ColumnaTexto
                                ,language='english'
                                ,removeNumbers = TRUE
                                ,stemWords=TRUE
                                ,toLower = TRUE
                                ,stripWhitespace = TRUE
                                ,removeStopwords = TRUE)
  
  dtm2=as.matrix(MATRIZ_A_ENTRENAR)
  frequency=colSums(dtm2)
  frequency=sort(frequency, decreasing=TRUE)
  ELIMINAR=names(frequency[frequency<5])
  
  #MATRIZ_A_ENTRENAR_2<-remove_my_stopwords(own_stw=ELIMINAR, dtm=MATRIZ_A_ENTRENAR)
  
  MATRIZ_A_ENTRENAR_2<-MATRIZ_A_ENTRENAR
  
  container_AGENTE <- create_container(MATRIZ_A_ENTRENAR_2,
                                       ColumnaEtiqueta, trainSize=1:length(ColumnaEtiqueta),virgin=FALSE)
  ####################################SVM_AGENTE#################################
  
  modelo_entrenado <- train_model(container_AGENTE,"SVM")
  
  model<-list("MODELO"=modelo_entrenado,"MATRIZ"=MATRIZ_A_ENTRENAR_2)
  
  saveRDS(model,"ModeloEntrenado.rds")  
    
}

crear_matriz<-function (textColumns, language = "english", minDocFreq = 1, 
                        maxDocFreq = Inf, minWordLength = 3, maxWordLength = Inf, 
                        ngramLength = 1, originalMatrix = NULL, removeNumbers = FALSE, 
                        removePunctuation = TRUE, removeSparseTerms = 0, removeStopwords = TRUE, 
                        stemWords = FALSE, stripWhitespace = TRUE, toLower = TRUE, 
                        weighting = weightTf) 
{
  stem_words <- function(x) {
    split <- strsplit(x, " ")
    return(wordStem(unlist(split), language = language))
  }
  tokenize_ngrams <- function(x, n = ngramLength) return(rownames(as.data.frame(unclass(textcnt(x, 
                                                                                                method = "string", n = n)))))
  control <- list(bounds = list(local = c(minDocFreq, maxDocFreq)), 
                  language = language, tolower = toLower, removeNumbers = removeNumbers, 
                  removePunctuation = removePunctuation, stopwords = removeStopwords, 
                  stripWhitespace = stripWhitespace, wordLengths = c(minWordLength, 
                                                                     maxWordLength), weighting = weighting)
  if (ngramLength > 1) {
    control <- append(control, list(tokenize = tokenize_ngrams), 
                      after = 7)
  }
  else {
    control <- append(control, list(tokenize = scan_tokenizer), 
                      after = 4)
  }
  if (stemWords == TRUE && ngramLength == 1) 
    control <- append(control, list(stemming = stem_words), 
                      after = 7)
  trainingColumn <- apply(as.matrix(textColumns), 1, paste, 
                          collapse = " ")
  trainingColumn <- sapply(as.vector(trainingColumn, mode = "character"), 
                           iconv, to = "UTF8", sub = "byte")
  corpus <- Corpus(VectorSource(trainingColumn), readerControl = list(language = language))
  matrix <- DocumentTermMatrix(corpus, control = control)
  if (removeSparseTerms > 0) 
    matrix <- removeSparseTerms(matrix, removeSparseTerms)
  if (!is.null(originalMatrix)) {
    terms <- colnames(originalMatrix[, which(!colnames(originalMatrix) %in% 
                                               colnames(matrix))])
    weight <- 0
    if (attr(weighting, "acronym") == "tf-idf") 
      weight <- 1e-09
    amat <- matrix(weight, nrow = nrow(matrix), ncol = length(terms))
    colnames(amat) <- terms
    rownames(amat) <- rownames(matrix)
    fixed <- as.DocumentTermMatrix(cbind(matrix[, which(colnames(matrix) %in% 
                                                          colnames(originalMatrix))], amat), weighting = weighting)
    matrix <- fixed
  }
  matrix <- matrix[, sort(colnames(matrix))]
  gc()
  return(matrix)
}

remove_my_stopwords<-function(own_stw, dtm){
  ind<-sapply(own_stw, function(x, words){
    if(any(x==words)) return(which(x==words)) else return(NA)
  }, words=colnames(dtm))
  return(dtm[ ,-c(na.omit(ind))])  
}  
