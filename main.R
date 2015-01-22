################################################################################
## Projekt MOW 14Z
##
## Marcin Janicki
## Kamil Gołofit
## 
##

## przykladowe wywolanie (dla Iris)
example <- function()
{
  library(ada)
  library(caret)
  
  print("========================================")
  print("Test w oparciu o standardowy zbior IRIS:")
  print("========================================")
  print(iris)
  print("========================================")
  
  X <- iris[,-5]
  Y <- iris[,5]
  myModels <- oneVsAll(X, Y, ada)
  preds <- predict(myModels, X, type='probs')
  
  print("Ogólna jakość predykcji wyniosła:")
  
  countQuality(preds, Y)
}


oneVsAll <- function(X,Y,FUN,n=FALSE,...)
{
  #Macierz ECOC dla zadanego zbioru testowego
  ecocMatrix <- makeMatrix(length(unique(Y)), n)
  classes <- unique(Y)
  
  models <- apply(ecocMatrix, 2, function(x)
  {
    # klasy dla ktorych mamy '1' w kolumnie. x - aktualna kolumna
    goodClasses <- classes[x == 1]
    
    # mapa 1 i 0 dla przykladow. 1 dla 'dobrych' dla kolumny klas, 0 w p.p.
    .Target <- factor(ifelse(Y %in% goodClasses,1,0), levels=c(1,0))
    
    # dodanie do mapy wartosci atrybutow
    dat <- data.frame(.Target, X)
    
    # tworzenie modelu (klasyfikatora binarnego dla zadanej kolumny x)
    model <- FUN(.Target~., data=dat, ...)
    return(model)
  })
  
  # nadanie nazw klasyfikatorom kolejnych kolumn
    names(models) <- 1:sqrt(length(ecocMatrix))
    info <- list(X=X, Y=Y, classes=unique(Y))
    out <- list(models=models, info=info, ecoc=ecocMatrix)
    class(out) <- 'oneVsAll'
    return(out)
}

predict.oneVsAll <- function(object, newX=object$info$X, ...) 
{
    stopifnot(class(object)=='oneVsAll')
  
    # mozliwe do przewidzenia klasy
    classes <- object$info$classes
    ecocMatrix <- object$ecoc
  
    # zebranie predictions dla kazdego z modeli (dla kazdej kolumny macierzy ECOC)
    predictions <- lapply(object$models, function(x)
    {
      predict(x, newX, ...)
    })
    
    # stworzenie macierzy ciagu bitow (kodow korekcyjnych) dla kazdego z wierszy (przykladu)
    ecocPreds <- round(do.call(cbind, predictions))
    ecocPreds <- ecocPreds[,seq(2,dim(ecocPreds)[2],2)]
    
    # dekodowanie slow kodowych powstalych z predykcji przykladow przez modele
    decoded <- apply(ecocPreds, 1, function(x)
    {
      decodeClass(ecocMatrix, x)
    })
    
    # zmapowanie numerow klas na nazwy uzyte w datasecie
    decisions <- classes[decoded]
    
    return(decisions)
}

# podaje w % ile przykladow jest poprawnie sklasyfikowanych
countQuality <- function(predicted, original)
{
  booleans <- predicted == original
  tab <- table(booleans)
  
  tab / length(booleans)
}

# classify <- function(dat) 
# {
# # To chyba u nas niepotrzebne (?) 
#   
#     out <- dat/rowSums(dat)
#     out$Class <- apply(dat, 1, function(x) names(dat)[which.max(x)])
#     out
# }

####################################################################

makeMatrix <- function(classNo = 3, naive = FALSE)
{
  if(classNo < 3)
  {
    return()
  }
  
  if(naive == TRUE)
  {
    retMatrix <- naiveCodes(classNo)
  }
  else
  {
    if(classNo >= 3 && classNo <= 7)
    {
      retMatrix <- exhaustiveCodes(classNo)
    }
  }
  
  retMatrix
}

naiveCodes <- function(classNo)
{
  #create 0-filled matrix
  retMatrix <- matrix(0, classNo, classNo)
  
  #add 1 to diagonal
  diag(retMatrix) <- diag(retMatrix) + 1
  
  #return matrix
  retMatrix
}

exhaustiveCodes <- function(classNo)
{
  #specify rows length
  rowLength <- 2^(classNo-1) - 1
  
  #sequence length is 2^classNo so it's easier to prepare rows
  seqLength <- rowLength + 1
  
  #initialize a matrix to work with
  retMatrix <- matrix(0, classNo, rowLength)
  
  #prepare each row and fill a matrix with it
  for(i in 1:classNo)
  {
    retMatrix[i,] <- prepareRow(rowLength, seqLength)
    #cut seqLength in half
    seqLength <- seqLength / 2
  }
  
  #return matrix for ECOC
  retMatrix
}

prepareRow <- function(rowLength, seqLength)
{
  #initialize a row vector
  row <- 1:rowLength
  
  if(rowLength <= seqLength)
  {
    for(i in 1:rowLength)
    {
      row[i] <- 1
    }
  }
  else
  {
    #if there are some zero's we start with them
    bit <- 0
    
    for(i in 1:rowLength)
    {
      row[i] <- bit
      
      if(i %% seqLength == 0)
      {
        #thanks to that we don't have new ifs
        bit <- abs(bit - 1)
      }
    }
  }
  
  row
}

#function counts Hamming distance beetwen two words (we expect words to have equal length)
countHamming <- function(wordA, wordB)
{
  #perform a xor on two words
  xorWord <- abs(wordA - wordB)
  
  #sum elements (different positions)
  distance <- sum(xorWord)
  
  #return the distance
  distance
}

#return index of class represented by codeword
decodeClass <- function(ecocMatrix, classWord)
{
  #initialize variables
  minDistance <- length(classWord)
  retClass <- NA
  
  #find closest one in a loop
  for(i in 1:length(ecocMatrix[ ,1]))
  {
    distance <- countHamming(ecocMatrix[i, ], classWord)
    #if we've found closer match - save it
    if(distance < minDistance)
    {
      minDistance <- distance
      retClass <- i
    }
  }
  
  #return class
  retClass
}

#function returns minimum Hamming distance between any two words in a given matrix
minimalHamming <- function(ecocMatrix)
{
  classNo <- length(ecocMatrix[, 1])
  
  #there will be no greater distance than length of the word
  minDistance <- length(ecocMatrix[1, ])
  
  for(i in 1:(classNo - 1))
  {
    for(j in (i+1):classNo)
    {
      print(j)
      distance <- countHamming(ecocMatrix[i, ], ecocMatrix[j, ])
      if(distance < minDistance)
      {
        minDistance <- distance
      }
    }
  }
  
  #return minDistance
  minDistance
}