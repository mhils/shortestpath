\name{task}
\alias{task}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("task")}
\format{
  The format is:
List of 4
 $ env      :<environment: 0x000000000a7749b8> 
 $ weights  : NULL
 $ blocking : NULL
 $ task.desc:List of 11
  ..$ id          : chr "tutorial"
  ..$ type        : chr "classif"
  ..$ target      : chr "Species"
  ..$ size        : int 150
  ..$ n.feat      : Named int [1:3] 4 0 0
  .. ..- attr(*, "names")= chr [1:3] "numerics" "factors" "ordered"
  ..$ has.missings: logi FALSE
  ..$ has.weights : logi FALSE
  ..$ has.blocking: logi FALSE
  ..$ class.levels: chr [1:3] "setosa" "versicolor" "virginica"
  ..$ positive    : chr NA
  ..$ negative    : chr NA
  ..- attr(*, "class")= chr [1:2] "TaskDescClassif" "TaskDesc"
 - attr(*, "class")= chr [1:3] "ClassifTask" "SupervisedTask" "Task"
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
data(task)
## maybe str(task) ; plot(task) ...
}
\keyword{datasets}
