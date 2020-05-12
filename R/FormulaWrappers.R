
# basic idea:  some popular packages for regression, classification and
# machine learning do not acccommodate specifying Y and X vi an R
# formula, e.g. weight ~ height+age; this file contains wrappers to
# allow this

# also allowed will be factor-valued X

# note that the generic predict() functions must also be wrappers

# the suffix 'W' will be used to indicate "wrapper"

