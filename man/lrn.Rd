\name{lrn}
\alias{lrn}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("lrn")}
\format{
  The format is:
List of 12
 $ id                    : chr "classif.lda"
 $ type                  : chr "classif"
 $ package               : chr "MASS"
 $ properties            : chr [1:5] "twoclass" "multiclass" "numerics" "factors" ...
 $ par.set               :List of 2
  ..$ pars     :List of 4
  .. ..$ method        :List of 12
  .. .. ..$ id         : chr "method"
  .. .. ..$ type       : chr "discrete"
  .. .. ..$ len        : int 1
  .. .. ..$ lower      : NULL
  .. .. ..$ upper      : NULL
  .. .. ..$ values     :List of 4
  .. .. .. ..$ moment: chr "moment"
  .. .. .. ..$ mle   : chr "mle"
  .. .. .. ..$ mve   : chr "mve"
  .. .. .. ..$ t     : chr "t"
  .. .. ..$ cnames     : NULL
  .. .. ..$ has.default: logi TRUE
  .. .. ..$ default    : chr "moment"
  .. .. ..$ trafo      : NULL
  .. .. ..$ requires   : NULL
  .. .. ..$ when       : chr "train"
  .. .. ..- attr(*, "class")= chr [1:2] "LearnerParam" "Param"
  .. ..$ nu            :List of 12
  .. .. ..$ id         : chr "nu"
  .. .. ..$ type       : chr "numeric"
  .. .. ..$ len        : int 1
  .. .. ..$ lower      : num 2
  .. .. ..$ upper      : num Inf
  .. .. ..$ values     : NULL
  .. .. ..$ cnames     : NULL
  .. .. ..$ has.default: logi FALSE
  .. .. ..$ default    : NULL
  .. .. ..$ trafo      : NULL
  .. .. ..$ requires   :  expression(method == "t")
  .. .. ..$ when       : chr "train"
  .. .. ..- attr(*, "class")= chr [1:2] "LearnerParam" "Param"
  .. ..$ tol           :List of 12
  .. .. ..$ id         : chr "tol"
  .. .. ..$ type       : chr "numeric"
  .. .. ..$ len        : int 1
  .. .. ..$ lower      : num 0
  .. .. ..$ upper      : num Inf
  .. .. ..$ values     : NULL
  .. .. ..$ cnames     : NULL
  .. .. ..$ has.default: logi TRUE
  .. .. ..$ default    : num 1e-04
  .. .. ..$ trafo      : NULL
  .. .. ..$ requires   : NULL
  .. .. ..$ when       : chr "train"
  .. .. ..- attr(*, "class")= chr [1:2] "LearnerParam" "Param"
  .. ..$ predict.method:List of 12
  .. .. ..$ id         : chr "predict.method"
  .. .. ..$ type       : chr "discrete"
  .. .. ..$ len        : int 1
  .. .. ..$ lower      : NULL
  .. .. ..$ upper      : NULL
  .. .. ..$ values     :List of 3
  .. .. .. ..$ plug-in   : chr "plug-in"
  .. .. .. ..$ predictive: chr "predictive"
  .. .. .. ..$ debiased  : chr "debiased"
  .. .. ..$ cnames     : NULL
  .. .. ..$ has.default: logi TRUE
  .. .. ..$ default    : chr "plug-in"
  .. .. ..$ trafo      : NULL
  .. .. ..$ requires   : NULL
  .. .. ..$ when       : chr "predict"
  .. .. ..- attr(*, "class")= chr [1:2] "LearnerParam" "Param"
  ..$ forbidden: NULL
  ..- attr(*, "class")= chr "ParamSet"
 $ par.vals              : list()
 $ predict.type          : chr "response"
 $ name                  : chr "Linear Discriminant Analysis"
 $ short.name            : chr "lda"
 $ note                  : chr "Learner param 'predict.method' maps to 'method' in predict.lda."
 $ fix.factors.prediction: logi FALSE
 $ config                : list()
 - attr(*, "class")= chr [1:4] "classif.lda" "RLearnerClassif" "RLearner" "Learner"
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
data(lrn)
## maybe str(lrn) ; plot(lrn) ...
}
\keyword{datasets}
