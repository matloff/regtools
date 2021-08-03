
#  Clearing the Confusion: the Case Against Significance Tests

In March 2016, the august American Statistical Association (ASA) issued
the first [position
paper](https://www.amstat.org/asa/files/pdfs/p-valuestatement.pdf) in
its 177-year history.  It called into question one of core concepts in
statistical methodology, *significance testing* (ST):

> “The p-value was never intended to be a substitute for scientific
> reasoning,” said Ron Wasserstein, the ASA’s executive director.
> “Well-reasoned statistical arguments contain much more than the value
> of a single number and whether that number exceeds an arbitrary
> threshold.  The ASA statement is intended to steer research into a
> ‘post p<0.05 era.’”  

To be sure, though "post p<0.05 era" sounds like a ban on ST,
the position paper did not quite call for a ban.  It was, in many
ways, the proverbial "camel created by a committee"; as some on the
committee still believe in ST, the paper stopped short of a ban.  Yet,
the paper offered no use case in which ST *should* be used; their
silence on that may be considered "significant" (pun intended).

As the paper points out, these concerns about ST were not new.  They
were, for instance, explained in 
[a popular elementary stat text](https://wwnorton.com/books/9780393929720) and 
even 
[entire books](https://www.press.umich.edu/186351/cult_of_statistical_significance).  (See also 
[my paper](https://academic.oup.com/ee/article/20/5/1246/2480617).)

Indeed, most statisticians had been aware of the flaws of ST, but had
continued to use them and teach them.  Why did they proceed in spite of
knowing the dangers of STs?  For some it was out of habit, for others it
was an appreciation of the elegance of the theory.  In many cases, it
was  just plain convenience: An ST will at least give the analyst "an
answer," and the analyst may thus be willing to ignore the potential
problems with that answer.

Then what changed?  Among other things, 
[a paper by John Ionnidis](https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.0020124), 
provcatively titled, "Why Most Published Research Findings Are 
False," shook things up . Though it was mainly on the related issue of 
*p-hacking* rather on the core of ST, it caused the statistical
community to take a long-overdue look at statistical practice.

## Running example class of STs

Say we have an asymptotically normal estimator T of some population
value &theta;, with standard error s.e.(T).  For a test of approximate
Type I error &alpha; of H<sub>0</sub>:   &theta; =  &theta;<sub>0</sub>,
reject H<sub>0</sub> if 

Z = |(T-&theta;<sub>0</sub></sub>) / s.e.(T)| &gt; 1 - q<sub>&alpha; / 2</sub>

where q() is the quantile function for N(0,1).

If we reject, we say the result is "significant," that "&theta; is
significantly different from &theta;<sub>0</sub>," one of the greatest
abuses of the English language in all of science.

The p-value is the most stringent &alpha; at which we would still
reject.  For researchers, typically the smaller the p-value, the more
"significant" the finding, and the more elated the researchers are.
"We not only rejected at &alpha; = 0.05 but even at &alpha; = 0.002!"
Journals -- and even R output -- then report the result as "significant"
(&alpha; below 0.05), "highly significant" (below 0.01), or "very highly
significant" (below 0.001).  Given that the word *significant*
ordinarily means important, a very highly significant research finding
is a cause for enormous joy.  But it may be unwarranted joy, as we will
discuss here.

## Confidence intervals as the preferred alternative to STs

The familiar confidence interval (CI) based on T is

T &pm; q<sub>&alpha; / 2</sub> s.e.(T)

Some defenders of ST point out, correctly, that there is a mathematical
relation between STs and CIs.  For instance, we can create a CI by
*inverting* a test. (See for example [this
presentation](https://www.stat.purdue.edu/~fmliang/STAT611/st611lect9.pdf).)

Conversely, if one has a CI for the above H<sub>0</sub>, we can perform
an &alpha; level test by simply observing whether &theta;<sub>0</sub> is
in the CI.  **BUT THIS WOULD BE UNDESIRABLE, BECAUSE THE CI IS MORE
INFORMATIVE.**  Using a CI to perform a test throws away valuable
information:

1. The center of the CI gives us some indication of how close the true
 &theta; is to  &theta;<sub>0</sub>.

2. The width of the CI gives us an idea as to how accurate the
   indication in (1) is above.

### Example:  Opinion poll in an election

Say you are the campaign manager for Candidate X, and conduct a poll of
the electorate.  You form a CI for p, the population proportion of
voters who favor X, which turns out to be (0.505,0.518).  You have two
options in terms of reporting the results to X:

* Tell X, "Your support is significantly higher than 50%."

* Tell X, "You seem to be slightly ahead, with a CI for p being (0.505,0.518)."

Of course, the second choice is far more informative.  Not only is the
CI close to 0.50, indicating that X is in a tight race, but also the
narrowness of the interval says that the estimate of p here is likely
very accurate.

Or, say the CI is (0.492,0.551).  While we should not give the
impression that all the points in the CI are "equally likely," it will
give Candidate X cause for optimism, as opposed to simply telling X,
"Your support is not significantly different from 50%."  At any rate,
most important, X sees from the large width of the CI that a much larger
poll is needed to get accurate results.

### Another advantage of CIs over ST

The opinion poll example illustrates the fact that, in addition to being
more informative, another advantage of CIs is their Intuitive
interpretability.  Every poll is reported with a *margin of error*.
This is actually the radius of a 95% CI for the population proportion
under discussiuon.  The general public is quite comfortable with margins
of error, while by contrast it is exceedingly difficult to explain
p-values to them.

## H<sub>0</sub> is always false/meaningless

One of the central problems with STs is that we always know *a priori*
that H<sub>0</sub> is false, or worse, meaningless.  

For a toy example, say we have some coin with unknown probability p
of heads, and wish to test 
H<sub>0</sub>: p = 0.5000000000000000000000..., infinitely many 0s.
That's impossible.  We know *a priori* that the coin is weighted
slightly differently on one side than the other.

Or say we have two prescription drugs, A and B, for alleviating
hypertension, with &mu;<sub>A</sub> and &mu;<sub>B</sub> denoting the
population mean reductions in blood pressure.  We know those two means
cannot be equal to infinitely many decimal places.

But even worse, the very notion of an "exact" comparison of
&mu;<sub>A</sub> and &mu;<sub>B</sub> is unwarranted.  To begin with,
our blood pressure measuring  instruments don't have infinite precision,
so it makes no sense to ask whether the two drugs have exactly the same
mean, to infinitely many decimal places.

## Implication 1

In our example with T and &theta; above, as our sample size n grows,
s.e.(T) will converge to 0, while the numerator in Z converges to
&theta;<sub>true</sub> - &theta;<sub>0</sub>, the difference between the
true population value of &theta; and the hypothesized value.  In other
words, Z will go to &pm;&infin;.  The p-value will go to 0, and we will
reject H<sub>0</sub> -- which we already knew was false anyway.

Our 
[Bay Area R Users Group](https://www.meetup.com/R-Users/)
once had a speaker from LinkedIN, who discussed his analyses of large
LinkedIn datasets.  At one point in the talk, he mentioned that he had
been startled by the results at first, because "Everything came out
significant!"  Of course, he shouldn't have been surprised.

One might argue that these issues could be addressed by paying attention to the power of the test.
But it is not very meaningful to ask the probability of rejecting a hypothesis that we already know 
is false.

## Implication 2

It makes no sense to ask a question that we already know to be
false/meaingless.  **THE ST IS SIMPLY ASKING THE WRONG QUESTION.**

In the hypertension drugs example, as mentioned, as a practical matter
it is meaningless to ask if the two population means are exactly equal,
and anyway what we actually care about is whether one is subtantially
better than the other.  Thus the proper approach is to form a CI for the
difference between the two means, and then factor in comparisons of
costs, side effects and so on to our decision as to which one to use.

The situation for goodness-of-fit tests is similar.  Say we are
constructing a mixed effects variance components model, which assumes
normal distributions.  Suppose we have a procedure to test for
normality; should we use it?  No!  We know *a priori* that the effects
can't be exactly normal -- e.g.  no one is 20 feet tall nor has a
negative height, yet normality would imply both -- so again, the ST, by
asking whether the distributions are exactly normal, is asking the wrong
question.  CI approaches may not be practical either (possibly the
bootstrap could be made to work), so we may wish to continue a Method of
Moments approach to estimating the variance components.

## But what if we just want to know whether there is *some* difference, even if small?

Say we are investigating the validity of some scientific theory, say the
existence of gravity waves.  (A statistical analysis was actually done
for the latter, with problems, but the details would be a distraction
here.)  We might have some H<sub>0</sub> for which rejection means
confirmation of the validity of the theory; even a tiny but nonzero
effect might be of interest.

Here again we have the problem in which exact specification of the null
hypothesis is meaningless, due to issues with our measuring instruments
and so on, which make our view "We even care about a tiny effect"
untenable.

In the hypertension drugs example, one might consider

H<sub>0</sub>: &mu;<sub>B</sub> &le; &mu;<sub>A</sub> + c

vs.

H<sub>A</sub>: &mu;<sub>B</sub> &gt; &mu;<sub>A</sub> + c

What if the new drug, B, falls just short of improving over A by an
amount c?  Should we just scrap the drug, after all the expense of
developing it?  Of course not.  So again the hypothesis should not be
taken so literally, which unfortunately the test does.  A CI is still
the proper analysis.

## p-hacking

The term *p-hacking* refers to analyses of large studies with the
following pitfall.  

Say one is studying genetic impacts on some outcome, with very large
numbers of genes involved.  Even if no gene has a real impact, due to
sampling variation one of them will likely appear to have a
``significant'' impact, just by accident.  Note that this is a problem
for CIs too, not just STs.

One way to deal with this is to use the *Bonferroni-Dunn Inequality*,
which in this context means that if you wish to analyze k different
aspects of a setting, divide your chosen &alpha; value by k.  This is
the simplest of a class of methods known as *simultaneous inference
techniques* or *multiple comparison techniques*.  See for instance the 
[book by Jason Hsu](https://www.taylorfrancis.com/chapters/introduction-simultaneous-statistical-inference-jason-hsu/10.1201/b15074-6).


