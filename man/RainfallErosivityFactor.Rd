\name{RainfallErosivityFactor-package}
\alias{RainfallErosivityFactor-package}
\title{The Rainfall-Runoff Erosivity Factor}
\description{
  Determination of rainfall-runoff erosivity factor
}
\author{
  Dione Pereira Cardoso \email{cardoso.dione@gmail.com}
  Edilson Marcelino Silva \email{edilsonmg3@hotmail.com}
  Paulo Cesar Ossani \email{ossanipc@hotmail.com}
  Marx Leandro Naves Silva \email{marx@dcs.ufla.br}
  Joel Augusto Muniz \email{joamuniz@des.ufla.br}
  Daniel Furtado Ferreira \email{danielff@des.ufla.br}
  Junior Cesar Avanzi \email{junior.avanzi@dcs.ufla.br}
}
\details{
  \tabular{ll}{
   Package: \tab RainfallErosivityFactor\cr
   Type: \tab Package\cr
   Version: \tab 1.0.0\cr
   Date: \tab 2024-04-22\cr
   License: \tab GPL (>= 2)\cr\cr
  }
}
\keyword{
  Erosivity index; R-Factor; Universal Soil Loss Equation-USLE
}
\references{
  Wischmeier, W. & Smith, D. (1978). Predicting rainfall erosion losses: a guide to conservation planning. Agricultural Handbook No. 537. U.S. Department of Agriculture,Washington DC, USA.
}
\examples{
  # Example
  library(RainfallErosivityFactor)
  data(RainFallExample, package="RainfallErosivityFactor")
  RFactor(RainFallExample,1,24,10,2)
}
