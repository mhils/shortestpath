\name{r}
\alias{r}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("r")}
\format{
  The format is:
List of 10
 $ learner.id    : chr "classif.lda"
 $ task.id       : chr "tutorial"
 $ measures.train:'data.frame':	10 obs. of  2 variables:
  ..$ iter: int [1:10] 1 2 3 4 5 6 7 8 9 10
  ..$ mmce: logi [1:10] NA NA NA NA NA NA ...
 $ measures.test :'data.frame':	10 obs. of  2 variables:
  ..$ iter: int [1:10] 1 2 3 4 5 6 7 8 9 10
  ..$ mmce: num [1:10] 0 0 0 0.0667 0 ...
 $ aggr          : Named num 0.02
  ..- attr(*, "names")= chr "mmce.test.mean"
 $ pred          :List of 6
  ..$ instance    :List of 5
  .. ..$ desc      :List of 4
  .. .. ..$ id      : chr "cross-validation"
  .. .. ..$ iters   : int 10
  .. .. ..$ predict : chr "test"
  .. .. ..$ stratify: logi TRUE
  .. .. ..- attr(*, "class")= chr [1:2] "CVDesc" "ResampleDesc"
  .. ..$ size      : int 150
  .. ..$ train.inds:List of 10
  .. .. ..$ : int [1:135] 11 3 8 25 50 18 28 16 4 41 ...
  .. .. ..$ : int [1:135] 30 11 36 8 25 18 28 16 4 41 ...
  .. .. ..$ : int [1:135] 30 11 3 36 8 25 50 18 28 16 ...
  .. .. ..$ : int [1:135] 30 11 3 36 8 25 50 18 28 16 ...
  .. .. ..$ : int [1:135] 30 11 3 36 8 25 50 16 4 41 ...
  .. .. ..$ : int [1:135] 30 11 3 36 8 25 50 18 28 16 ...
  .. .. ..$ : int [1:135] 30 11 3 36 50 18 28 4 41 31 ...
  .. .. ..$ : int [1:135] 30 11 3 36 8 25 50 18 28 16 ...
  .. .. ..$ : int [1:135] 30 11 3 36 8 25 50 18 28 16 ...
  .. .. ..$ : int [1:135] 30 3 36 8 25 50 18 28 16 4 ...
  .. ..$ test.inds :List of 10
  .. .. ..$ : int [1:15] 14 21 22 30 36 57 58 62 78 92 ...
  .. .. ..$ : int [1:15] 1 2 3 10 50 66 88 91 94 97 ...
  .. .. ..$ : int [1:15] 5 20 23 27 41 63 67 80 85 96 ...
  .. .. ..$ : int [1:15] 4 13 29 39 44 53 68 69 76 87 ...
  .. .. ..$ : int [1:15] 18 28 40 45 49 51 70 82 90 100 ...
  .. .. ..$ : int [1:15] 7 9 12 38 43 54 74 77 79 83 ...
  .. .. ..$ : int [1:15] 8 16 25 26 46 61 72 75 81 99 ...
  .. .. ..$ : int [1:15] 17 31 33 34 35 52 55 56 60 73 ...
  .. .. ..$ : int [1:15] 6 24 37 42 48 64 71 84 86 98 ...
  .. .. ..$ : int [1:15] 11 15 19 32 47 59 65 89 93 95 ...
  .. ..$ group     : Factor w/ 0 levels: 
  .. ..- attr(*, "class")= chr "ResampleInstance"
  ..$ predict.type: chr "response"
  ..$ data        :'data.frame':	150 obs. of  5 variables:
  .. ..$ id      : int [1:150] 14 21 22 30 36 57 58 62 78 92 ...
  .. ..$ truth   : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 2 2 2 2 2 ...
  .. ..$ response: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 2 2 2 2 2 ...
  .. ..$ iter    : int [1:150] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..$ set     : Factor w/ 1 level "test": 1 1 1 1 1 1 1 1 1 1 ...
  ..$ threshold   : num NA
  ..$ task.desc   :List of 11
  .. ..$ id          : chr "tutorial"
  .. ..$ type        : chr "classif"
  .. ..$ target      : chr "Species"
  .. ..$ size        : int 150
  .. ..$ n.feat      : Named int [1:3] 4 0 0
  .. .. ..- attr(*, "names")= chr [1:3] "numerics" "factors" "ordered"
  .. ..$ has.missings: logi FALSE
  .. ..$ has.weights : logi FALSE
  .. ..$ has.blocking: logi FALSE
  .. ..$ class.levels: chr [1:3] "setosa" "versicolor" "virginica"
  .. ..$ positive    : chr NA
  .. ..$ negative    : chr NA
  .. ..- attr(*, "class")= chr [1:2] "TaskDescClassif" "TaskDesc"
  ..$ time        : num [1:10] 0 0 0 0 0.02 ...
  ..- attr(*, "class")= chr [1:2] "ResamplePrediction" "Prediction"
 $ models        : NULL
 $ err.msgs      :'data.frame':	10 obs. of  3 variables:
  ..$ iter   : int [1:10] 1 2 3 4 5 6 7 8 9 10
  ..$ train  : Factor w/ 0 levels: NA NA NA NA NA NA NA NA NA NA
  ..$ predict: Factor w/ 0 levels: NA NA NA NA NA NA NA NA NA NA
 $ extract       :List of 10
  ..$ : NULL
  ..$ : NULL
  ..$ : NULL
  ..$ : NULL
  ..$ : NULL
  ..$ : NULL
  ..$ : NULL
  ..$ : NULL
  ..$ : NULL
  ..$ : NULL
 $ runtime       : num 0.137
 - attr(*, "class")= chr [1:2] "ResampleResult" "list"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(r)
## maybe str(r) ; plot(r) ...
}
\keyword{datasets}
