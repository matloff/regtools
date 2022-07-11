#  Clearing the Confusion:  R and Python as Interpreted Languages, and the Roles of Vectorization and Pointers

One often hears statements like "R is an interpreted language, and thus
is slow."  The same is true for Python and Java.  But what does that
really mean?

## Example

Consider this code snippet:

``` r
# x, creating previously, is a vector of 100 elements
tot <- 0
for (i in 1:100)
   tot <- tot + x[i]
```

The *vectorized* alternative is

``` r
tot <- sum(x)
```

The second version will indeed be faster than the first (though with a
small vector like this, both will execute essentially instantaneously).
But why?

## Interpreted languages

R does not run directly on your machine.  What runs directly is another
program, the *interpreter*.  Like any program running on your machine,
it is written in machine language, patterns of 0s and 1s that code
primitive operations, e.g. ``Add these 2 numbers.''  In most cases, that
machine language was generated from C/C++ source, translated to machine
code by a *compiler*.  The interpreter in essence simulates a fake
machine, on which your R code "runs."  The details are not important for
us.

The reason that first code snippet above will run slowly is that the
compiler will have to do a lot of repetitive work.  For instance, it
will have to look up where in memory **tot** and **x** are--100 times!

By contrast, in that second code snippet, the **sum()** function is
actual machine code (again, generated from C by the compiler).  The
location of **x** is set just once, and the code marches through the
vector without any lookup.  That code will still repeat a summation
operation 100 times, but without the time-consuming lookup.  The
situation is similar for **tot**.

## Vectorized code

We say that the second snippet is *vectorized* code, because the entire
vector is processed directly by machine code.  As noted, that code will
still loop around 100 times, so it is not the case that the code
operates on the entire vector at once (there are special types 
of hardware, e.g. GPUs, that *can* do this), but it will be faster for
the reasons cited above.  Some R code is vectorizable, some not.

## Pointers

A computer's memory is broken down into *bytes*, with a certain number
of bytes forming a *word*.  On today's machines, word size is typically
8 bytes.  So, if your machine has, say 4 Gb of RAM, then it has 0.5
billion words.  Your code's numbers are typically stored one to a word,
e.g. 100 consecutive words in the above code, while your text data is
stored one to a byte.

Each byte, and each word, has an ID, called an *address*.  When your
code refers to a vector, say **x** above, internally it refers to the
address in memory at which the vector starts.  Internally, that starting
address is kept in a variable called a *pointer*.

Now, here is a key point:  Say the vector **u** is rather long, say 100
million elements, and we execute

``` r
v <- u
```

Will that operation be slow, due to the need to do all that copy of one
vector to another?  No!  Since **u** and **v** are referenced by
pointers, executing the above line of code merely means copying one
pointer to another; they both point to the same place.

But...what if we then execute

``` r
v[88] <-3
```

We want **v** to change but **u** to NOT change.  Now the interpreter
must do some work.  In preparing to make **v** separate from **u**,
the interpreter must (a) find some unused part of memory at which to
create the new vector, (b) copy all of **u** to that space, (c) point
**v**'s pointer to that space, and (d) set the third word in that space
to 3.  

So, here we see two lines of code, the first appearing to be slow but
actually not slow, and the second looking innocuous and fast but
actually slow.  Writing fast R code does take some sophistication.

## Every operation is a function

This is not directly related to the above material, but worth mentioning
in this context.

You are probably well familiar with functions, e.g. **sum()** above, but
may think of something like

``` r
3 + 8
```

as a different animal.  Actually, the latter is also a function call!
The name of the function is **`+`**!  (Note the backticks.)   For
example:

``` r
> 3 + 8
[1] 11
> `+`(3,8)
[1] 11
```

So, addition is done via the **`+`()** function, with the addends as
arguments.  The R interpreter converts that first form to the second.

Similarly, we have a function **`[`** for vector element access,
a function **`$`** for list element access, and so on.

