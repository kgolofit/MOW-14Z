# First file
oneVsAll <- function(X,Y,FUN,...)
{
# Tu bedziemy tworzyc modele - tu stworzy sie macierz i klasyfikatory binarne.
# Tu tez chyba bedzie ich trenowanie
  
  
#   models <- lapply(unique(Y), function(x)
#   {
#     name <- as.character(x)
#     .Target <- factor(ifelse(Y==name,name,'other'), levels=c(name, 'other'))
#     dat <- data.frame(.Target, X)
#     model <- FUN(.Target~., data=dat, ...)
#     return(model)
#   })
#   names(models) <- unique(Y)
#   info <- list(X=X, Y=Y, classes=unique(Y))
#   out <- list(models=models, info=info)
#   class(out) <- 'oneVsAll'
#   return(out)
}

predict.oneVsAll <- function(object, newX=object$info$X, ...) 
{
# Tutaj podobnie - predict dla kazdego z binarnych klasyfikatorow i dla wyjscia odkodowac decyzje. 
  
#   stopifnot(class(object)=='oneVsAll')
#   lapply(object$models, function(x)
#   {
#     predict(x, newX, ...)
#   })
}

classify <- function(dat) 
{
# To chyba u nas niepotrzebne (?) 
  
#   out <- dat/rowSums(dat)
#   out$Class <- apply(dat, 1, function(x) names(dat)[which.max(x)])
#   out
}

####################################################################

makeMatrix <- function(classNo = 2)
{
  if(classNo < 3)
  {
    return()
  }
  
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