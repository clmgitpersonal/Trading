
## To copy HTML table from Mac clipboard 
data <- read.table(pipe("pbpaste"), sep="\t", header=T)
readLines(pipe("pbpaste"))

x <- read.table(pipe("pbpaste"))

x <- read.table(pipe("pbpaste"), header=TRUE, sep="\t")


increment <- function(x) x + 1
some.funs <-  list(sum, increment)
apply(iris[,1:4], 2, mean)


aggregate_iris<-function(data, statistic) {
  result<-c()
  for (i in 1:4) {
    result<-c(result, statistic$execute(data[,i]))
  }
  names(result)<-colnames(data)[1:4]
  result }


AbstractStatistic<-setRefClass(
  Class="AbstractStatistic",
  fields=list(),
  methods=list(
    initialize=function(...) { callSuper(...) },
    execute=function(data, ...) {
      stop("Interface should not be called directly")
    }
  ) )
Mean<-setRefClass(
Class="Mean",
fields=list(),
contains="AbstractStatistic",
methods=list(
  initialize=function(...) { callSuper(...) },
  execute=function(data, ...) { mean(data, ...) }
)
)

m<-Mean$new()
aggregate_iris(iris, m)


library(randomForest)
classify_iris<-function(x)  {
  x$Sepal.LW<-x$Sepal.Length/x$Sepal.Width
  x$Petal.LW<-x$Petal.Length/x$Petal.Width
  x$SP.Length<-x$Sepal.Length/x$Petal.Length
  x$SP.Width<-x$Sepal.Width/x$Petal.Width
  randomForest(Species ~ ., x)
}

classify_iris(iris)


classify_iris<-function(x, na.fn, model=randomForest) {
  cols<- c("Sepal.Length", "Sepal.Width","Petal.Length", "Petal.Width")
  x[,cols]<- apply(x[,cols], 2,function(z) ifelse(is.na(z), na.fn(z), z))
  
  x$Sepal.LW<-x$Sepal.Length/x$Sepal.Width
  x$Petal.LW<-x$Petal.Length/x$Petal.Width
  x$SP.Length<-x$Sepal.Length/x$Petal.Length
  x$SP.Width<-x$Sepal.Width/x$Petal.Width
  model(Species ~ ., x)
}

iris1<-iris
iris1[,1:4]<-apply(iris1[,1:4], 2, function(x) { 
                x[sample(length(x),10)]<-NA
                x
              })

classify_iris(iris1, function(x) mean(x, na.rm=TRUE), ksvm)


setup_svm<-function(...) {
  function(formula, data) {
    ksvm(formula, data, ...)
  }
}