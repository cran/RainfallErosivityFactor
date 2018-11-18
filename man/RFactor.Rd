\name{RFactor}
\alias{RFactor}
\title{The Rainfall RFactor function- Computes Erosivity Factor}
\description{
  Function for determination of rainfall runoff erosivity factor.
}
\details{
follow the steps: \itemize{
  \item 1st Step: Read the bank. This dataset has three columns, the first one is Date (dd/mm/yyyy),  the second column is   Time (hh:mm) and the third one is Rainfall (mm), in which each word is capitalized.
  \item 2nd Step: The break down of a long rainfall event.  When accumulated  precipitation was less than 1 mm in over 6   hours, the rainfall event is divided in two rainfall events.
  \item Note: Note for step 3, the calculations are performed for each rainfall event separately.
  \item 3rd Step: To obtain the I30 component, the sum of three consecutive rainfall were considered, with interval of 10
  minutes. Subsequently, this value was expressed as rain intensity in millimeters per hour, referent to the interval of 30
  minutes.
  \item 4th Step: I10 determination: recording-rain were in 10 minutes intervals. Then, the values were expressed as   intensity
  of this interval in millimeters per hour.
  \item 5th Step: The unitary kinetic energy was calculated according to the equation developed by Wischmeier & Smith   (1978), were E = 0.119 + 0.0873*Log I10 (MJ ha-1 mm-1).
  \item 6th Step: Multiplied the unitary kinetic energy (MJ ha^(-1) mm^(-1)) by its respective precipitation (mm) of the   interval. Thus, the kinetic energy of the interval (MJ ha^(-1)) was determined.
  \item 7th Step: The sum of this kinetic energy of the interval was realized.
  \item 8th Step: Maximum value of I30 in the step 3rd was identified.
  \item 9th Step: EI30 determination was obtained by multiplication of 7th and 8th steps.
  \item 10th Step: Erosive rainfall must meet at least one of the three conditions: first, precipitation heigth greater   than 10 mm; second, maximum I30 at least 24mm of rainfall fell in 15 minutes; and third, total kinetic energy greater   than 3.6 MJ.
  \item 11th Step: In this package, was considered only the first condition.
  \item 12th Step: EI30 was computed only from erosive rainfall.
  \item 13th Step: Sum of erosive events in the specific month, was the monthly rainfall erosivity of such month.
  \item 14th Step: Sum of EI30 of each month, was the annual rainfall erosivity.
  \item 15th Step: Finally, the anual average rainfall erosivity was obtained for the period evaluated performing the sum   of annual rainfall erosivity divided by number of years.
  \item Note: The sum of the EI30 values corresponds to rainfall erosivity, also called the R-Factor of the Universal Soil   Loss Equation-USLE.  
}
}

\usage{
  RFactor(Data,initialmonth,months,registration,nyear)
}
\arguments{
  \item{Data}{Example data set}
  \item{initialmonth}{First month, data set}
  \item{months}{Total number of months of the data set}
  \item{registration}{Record of rainfall relative to 5 or 10 or 15 minute}
  \item{nyear}{Number Years relative to the size of the historical series}
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
