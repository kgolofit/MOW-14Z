################################################################################
## Projekt MOW 14Z
##
## Marcin Janicki
## Kamil Gołofit
## 

## wczytywanie pliku poprzez read.csv("nazwa_pliku")
## zapis do pliku poprzez write.csv(obiekt, "nazwa_pliku")

## Opakowanie do przeprowadzenia testów
doAllTests <- function(fac=0.8)
{
  library(ada)
  library(caret)
  library(party)
  library(tree)
  
  pen <- read.csv("pendigits.csv")
  wine_white <- read.csv("winequality-white.csv")
  wine_red <- read.csv("winequality-red.csv")
  letter <- read.csv("letter.csv")
  
  penTr <- pen[1:(fac * dim(pen)[1]),]
  penTe <- pen[(fac * dim(pen)[1]):dim(pen)[1],]
  
  wwTr <- wine_white[1:(fac * dim(wine_white)[1]),]
  wwTe <- wine_white[(fac * dim(wine_white)[1]):dim(wine_white)[1],]
  
  wrTr <- wine_red[1:(fac * dim(wine_red)[1]),]
  wrTe <- wine_red[(fac * dim(wine_red)[1]):dim(wine_red)[1],]
  
  letTr <- letter[1:(fac * dim(letter)[1]),]
  letTe <- letter[(fac * dim(letter)[1]):dim(letter)[1],]
  
  print("Czas: ")
  print(Sys.time())
  print("============================================")
  print("============================================")
  print("Testy dla zbioru cyfr pisanych recznie:")
  print("============================================")
  print("RPART:")
  print("============================================")
  doSetOfTests(penTr, penTe, rpart, 'prob')
  print("============================================")
  print("TREE:")
  doSetOfTests(penTr, penTe, tree, 'vector')
  
  print("Czas: ")
  print(Sys.time())
  print("============================================")
  print("============================================")
  print("Testy dla zbioru oceny wina bialego:")
  print("============================================")
  print("RPART:")
  print("============================================")
  doSetOfTests(wwTr, wwTe, rpart, 'prob')
  print("============================================")
  print("TREE:")
  doSetOfTests(wwTr, wwTe, tree, 'vector')
  
  print("Czas: ")
  print(Sys.time())
  print("============================================")
  print("============================================")
  print("Testy dla zbioru oceny wina czerwonego:")
  print("============================================")
  print("RPART:")
  print("============================================")
  doSetOfTests(wrTr, wrTe, rpart, 'prob')
  print("============================================")
  print("TREE:")
  doSetOfTests(wrTr, wrTe, tree, 'vector')
  
  print("Czas: ")
  print(Sys.time())
  print("============================================")
  print("============================================")
  print("Testy dla zbioru rozpoznawania liter:")
  print("============================================")
  print("RPART:")
  doSetOfTests(letTr, letTe, rpart, 'prob')
  print("============================================")
  print("TREE:")
  doSetOfTests(letTr, letTe, tree, 'vector')
}

## wykonanie zbioru testów
doSetOfTests <- function(train, test, FUN, type='prob', naive=FALSE)
{
  print("Czas rozpoczecia: ")
  print(Sys.time())
  print("============================================")
  print("Test w oparciu o zbior (3 pierwsze wiersze):")
  print(train[1:3,])
  print("============================================")
  
  XTrain <- train[,-dim(train)[2]]
  YTrain <- train[,dim(train)[2]]
  
  XTest <- test[,-dim(test)[2]]
  YTest <- test[,dim(test)[2]]
  
  print("Dane przygotowane. Czas:")
  print(Sys.time())
  print("============================================")
  
  # liczenie dla modelu ADA
  
  adaModels <- mccubc(XTrain, YTrain, ada, naive)
  adaPreds <- predict(adaModels, XTest, type='prob')
  
  print("Predykcja dla modelu ADA zakonczona. Czas:")
  print(Sys.time())
  print("============================================")
  print("Jakosc predykcji z wykorzystaniem kodow korekcyjnych i modelu ADA:")
  print(countQuality(adaPreds, YTest))
  
  # liczenie dla zadanego modelu z wykorzystaniem kodow korekcyjnych
  
  myModels <- mccubc(XTrain, YTrain, FUN, naive)
  myPreds <- predict(myModels, XTest, type)
  
  print("Predykcja dla zadanego modelu z kodami korekcyjnymi zakonczona. Czas:")
  print(Sys.time())
  print("============================================")
  print("Jakosc predykcji dla zadanej funkcji z wykorzystaniem kodow korekcyjnych:")
  print(countQuality(myPreds, YTest))
  
  # liczenie dla zadanego modelu (multiclass)
  
  multiModel <- FUN(class~., data=train)
  multiPredict <- predict(multiModel, XTest)
  
  print("============================================")
  print("Jakosc predykcji dla zadanej funkcji bez kodow (multiclass):")
  if(!is.null(dim(multiPredict)))
  {
    print(countQualityForTree(multiPredict, YTest))
  } else 
  {
      print(countQuality(round(multiPredict), YTest))
  }
}

## ######################################################################################## ##
## nasza klasa modelu predykcji opartego na klasyfikatorach binarnych i kodach korekcyjnych ##
## ######################################################################################## ##
##              Multi class classification using binary classificators - MCCUBC             ##
## ######################################################################################## ##

mccubc <- function(X,Y,FUN,naive,...)
{
  #Macierz ECOC dla zadanego zbioru testowego
  classes <- unique(Y)
  ecocMatrix <- makeMatrix(length(classes), length(classes), naive, removeColumns=TRUE)
  cat("Wymiary macierzy ECOC: ", dim(ecocMatrix), "\n")
  
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
  class(out) <- 'mccubc'
  return(out)
}

## metoda predict dla naszego modelu mccubc
predict.mccubc <- function(object, newX=object$info$X, ...) 
{
  stopifnot(class(object)=='mccubc')
  
  # mozliwe do przewidzenia klasy
  classes <- object$info$classes
  ecocMatrix <- object$ecoc
  
  # zebranie predictions dla kazdego z modeli (dla kazdej kolumny macierzy ECOC)
  predictions <- lapply(object$models, function(x)
  {
    predict(x, newX, ...)
  })
  
  if(class(object$models$'1') == "rpart") #rpart
  {
    # dla rpart mamy inaczej zwracane pnstwo (odwrotnie dla 0 i 1)
    
    ecocPreds <- round(do.call(cbind, predictions))
    ecocPreds <- ecocPreds[,seq(1,dim(ecocPreds)[2],2)]
  } else if(class(object$models$'1') == "tree") #tree
  {
    # dla rpart mamy inaczej zwracane pnstwo (odwrotnie dla 0 i 1)
    ecocPreds <- round(do.call(cbind, predictions))
    ecocPreds <- ecocPreds[,seq(1,dim(ecocPreds)[2],2)]
  } else #ada
  {
    # stworzenie macierzy ciagu bitow (kodow korekcyjnych) dla kazdego z wierszy (przykladu)
    ecocPreds <- round(do.call(cbind, predictions))
    ecocPreds <- ecocPreds[,seq(2,dim(ecocPreds)[2],2)]
  }
  
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
  
  quality <- tab / length(booleans)
  return(quality)
}

# dla tree w predictedTree zwracane sa prawdopodobienstwa dla kazdej z klas
countQualityForTree <- function(predictedTree, original)
{
  
  predictedFinal <- apply(predictedTree, 1, function(x)
  {
    return(colnames(predictedTree)[which(x == max(x))])
  })
  
  booleans <- predictedFinal == original
  tab <- table(booleans)
  
  quality <- tab / length(booleans)
  return(quality)
}

## ################################################################### ##
## Funkcje dotyczace tworzenia kodow korekcji bledow i odkodowania ich ##
## ################################################################### ##

makeMatrix <- function(classNo = 3, columnNo = 0, naive = FALSE, removeColumns = FALSE)
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
    #create ecoc matrix
    if(classNo >= 3 && classNo <= 11)
    {
      retMatrix <- exhaustiveCodes(classNo)
      
      #remove columns if it's nessecary
      if(removeColumns == TRUE)
      {
        retMatrix <- removeColumns(retMatrix, columnNo)
      }
      
    }
    else
    {
      retMatrix <- randomWalk(classNo, columnNo)
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

#function removes colums from ECOC matrix until Hamming Distance is 3 (minimum for ECOC codes)
#or until columnNo columns are left
removeColumns <- function(ecocMatrix, columnNo)
{
  #iteration variable
  i <- 1
  
  #iterate while index is in bounds
  while(i <= length(ecocMatrix[1,]) && length(ecocMatrix[1,]) > columnNo)
  {
    tmpMatrix <- ecocMatrix[,-i]
    
    #if ecoc Matrix is still ok remove column
    if(minimalHamming(tmpMatrix) >= 3)
    {
      ecocMatrix <- tmpMatrix
    }
    else #check next column
    {
      i <- i + 1
    }
  }
  
  #return column with some columns removed
  ecocMatrix  
}

#function generates ecoc matrix using random walk algorithm
randomWalk <- function(classNo, columnNo)
{
  #generate vector to initialize a matrix
  iniVector <- sample(c(0, 1), classNo * columnNo, replace = TRUE)
  
  #create matrix
  retMatrix <- matrix(iniVector, classNo, columnNo)
  
  minimalHD <- minimalHammingDetails(retMatrix)
  
  #improve matrix
  while(minimalHD[3] < 3)
  {
    #find the columns that are identical
    columns <- findColumns(retMatrix[minimalHD[1], ], retMatrix[minimalHD[2], ])
    
    #negate bits to enlarge the Hamming distance
    retMatrix[minimalHD[1], columns[1]] <- abs(retMatrix[minimalHD[1], columns[1]] - 1)
    retMatrix[minimalHD[2], columns[2]] <- abs(retMatrix[minimalHD[2], columns[2]] - 1)
    
    print(retMatrix)
    minimalHD <- minimalHammingDetails(retMatrix)
  }
  
  retMatrix
}

findColumns <- function(wordA, wordB)
{
  column1 <- 0
  column2 <- 0
  i <- 1
  
  #iterate through columns to find same-value ones in both rows
  while(i < length(wordA) && (column1 == 0 || column2 == 0))
  {
    if(wordA[i] == wordB[i])
    {
      if(column1 == 0)
      {
        column1 <- i
      }
      else
      {
        column2 <- i
      }
      i <- i + 1
    }
  }
  
  #return the columns
  ret <- c(column1, column2)
  
  ret
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
  retHamming <- minimalHammingDetails(ecocMatrix)
  
  retHamming[3]
}

#function returns minimum Hamming distance between any two words in a given matrix
#additional return values are rows numbers
minimalHammingDetails <- function(ecocMatrix)
{
  classNo <- length(ecocMatrix[, 1])
  
  #there will be no greater distance than length of the word
  minDistance <- length(ecocMatrix[1, ])
  
  #rows to return
  row1 <- 0
  row2 <- 0
  
  for(i in 1:(classNo - 1))
  {
    for(j in (i+1):classNo)
    {
      distance <- countHamming(ecocMatrix[i, ], ecocMatrix[j, ])
      if(distance < minDistance)
      {
        minDistance <- distance
        row1 <- i
        row2 <- j
      }
    }
  }
  
  #return minDistance
  c(row1, row2, minDistance)
}