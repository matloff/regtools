#  Clearing the Confusion: Poisson regression

One of the most commonly used statistical methods is the *general linear
model*, implemented in R as the **glm()** function.  The most common
usage of that function is for *logistic regression*, but it's also
popular for *Poisson regression* (PR), the subject of this entry in our
Clearing the Confusion series.  PR is often used as a regression model
in which the response variable Y consists of counts, typically in the
one- or two-digit range.

This is not a tutorial on Poisson regression (PR).  It is assumed that
the reader already has some familiarity with the model, and the
treatment here is somewhat critical.  There are two main themes:

* Unlike the logistic and linear cases, there is no theoretical or
  modeling justification for PR.

* Though many analysts exponentiate the regression coefficients returned
  by **glm()** in the Poisson case, i.e. run them through the **exp()**
function, there are reasons arguing against this practice.

(WORK IN PROGRESS)

