
# recommender systems tools; partially adapted from rectools pkg


##################  ops to group recsys data  ############################

# utility to in raw data in standard format,

#    (user ID, item ID, rating)

# and form an R list for user data, with class 'usrData'; each element
# of the list will be of class 'usrDatum', representing one user's data,
# and will have components as seen in 'value' below 

# arguments:

#    ratingsIn: input data, whose first 3 cols are user ID, item ID
#               and rating 

# value:

#    object of class 'usrData': an R list with one element per user;
#    each such element is itself an R list, an object of class
#    'usrDatum', with these components:
#
#       userID:  the ID of this user
#       ratings:  vector of ratings made by this user
#       itms:  IDs of items rated by this user

groupUserData <- function(ratingsIn) 
{

   if (ncol(ratingsIn) > 3)
      stop('ratingsIn more than 3 columns')

   # IMPORTANT NOTE: in order to work in cross-validation, etc. we need
   # to abandon the idea of having the user IDs start at 1 and be
   # consecutive; instead, we will just use the ID numbers as list
   # indices; e.g. if we have users numbered 2,8,85 then retval below
   # will consist of retval[[2]], retval[[8]] and retval[[85]]

   # rownums[[i]] will be the row numbers in ratingsIn belonging to user i
   rownums <- split(1:nrow(ratingsIn),ratingsIn[,1])
   nusers <- length(rownums)
   nitems <- length(unique(ratingsIn[,2]))

   # retval will ultimately be the return value, a list of lists as
   # described above.
   retval <- list()

   for (i in 1:nusers) {
      whichrows <- rownums[[i]]  # row nums in ratingsIn for user i
      userID <- as.character(ratingsIn[whichrows[1],1])
      # start building usrDatum object for this user
      retval[[userID]] <- list()
      retval[[userID]]$userID <- userID
      retval[[userID]]$itms <- ratingsIn[whichrows,2]
      retval[[userID]]$ratings <- ratingsIn[whichrows,3]
      names(retval[[userID]]$ratings) <- as.character(retval[[userID]]$itms) 

      class(retval[[userID]]) <- 'usrDatum'
   }
   class(retval) <- 'usrData'
   retval
}

# analog of groupUserData() for items

groupItemData <- function(ratingsIn) 
{

   if (ncol(ratingsIn) > 3)
      stop('ratingsIn more than 3 columns')

   # IMPORTANT NOTE: in order to work in cross-validation, etc. we need
   # to abandon the idea of having the item IDs start at 1 and be
   # consecutive; instead, we will just use the ID numbers as list
   # indices; e.g. if we have items numbered 2,8,85 then retval below
   # will consist of retval[[2]], retval[[8]] and retval[[85]]

   # rownums[[i]] will be the row numbers in ratingsIn belonging to item i
   rownums <- split(1:nrow(ratingsIn),ratingsIn[,2])
   nitems <- length(rownums)
   nusers <- length(unique(ratingsIn[,1]))

   # retval will ultimately be the return value, a list of lists as
   # described above.
   retval <- list()

   for (i in 1:nitems) {
      whichrows <- rownums[[i]]  # row nums in ratingsIn for item i
      itemID <- as.character(ratingsIn[whichrows[1],2])
      # start building itmDatum object for this user
      retval[[itemID]] <- list()
      retval[[itemID]]$itemID <- itemID
      retval[[itemID]]$usrs <- ratingsIn[whichrows,1]
      retval[[itemID]]$ratings <- ratingsIn[whichrows,3]
      names(retval[[itemID]]$ratings) <- as.character(retval[[itemID]]$itms) 

      class(retval[[itemID]]) <- 'itmDatum'
   }
   class(retval) <- 'itmData'
   retval
}

# utility:  find input row for a given user, item
findInputRow <- function(ratingsIn,usrID,itmID) {
   ratingsIn[ratingsIn[,1]==usrID & ratingsIn[,2]==itmID,]
}

