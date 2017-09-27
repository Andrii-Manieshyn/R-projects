splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/5))
  d_testset <- dataframe[trainindex, ]
  d_trainset <- dataframe[-trainindex, ]
  list(trainset=d_trainset,testset=d_testset)
}