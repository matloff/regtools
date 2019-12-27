# Clearing the Confusion on Unbalanced Class Data 

In discussions of machine learning (ML) classification problems, 
the issue of *unbalanced data* -- having an unequal number of cases in
each class -- arises frequently.  Illustration of the problem and
offered remedies appear in numerous parts of the ML literature, ranging
from [Web tutorials](https://www.datacamp.com/community/tutorials/diving-deep-imbalanced-data)
to [the research literature](https://link.springer.com/article/10.1186/s40537-018-0151-6#Sec2).  Major packages, such as
[caret](https://cran.r-project.org/package=caret) and
[mlr3](https://cran.r-project.org/package=mlr3) also offer remedies.
In almost all cases, the prescribed antidote involves
involving changing the data via resampling, so that the new
data is balanced. 

Upon closer inspection, though, reveals that **use of resampling methods
is generally inadvisable, indeed harmful, for several reasons:**

* Undersampling is clearly problematic:  Why throw away data?  Unless
  the number of cases is much larger than the number of features,
**discarding data weakens our ability to predict new cases.**

* The data may be unbalanced *for a reason*.  Thus the imbalance itself
  is useful information, again resulting in reduced predictive power if
it is ignored.

* For most ML methods, **a principled alternative to resampling is
  available,** to be presented here.  (See also
[my regression and classification
book](https://books.google.com/books?id=IHs2DwAAQBAJ&printsec=frontcover&dq=matloff&hl=en&newbks=1&newbks_redir=0&sa=X&ved=2ahUKEwje9LbA5dLmAhVJsZ4KHTvdADIQ6AEwAHoECAQQAg#v=onepage&q=matloff&f=false).)

In other words, resampling methods **are both harmful and unnecessay**.

## Motivating example:  Missed appointments

## Key issue:  How were the data generated?

(under construction)

