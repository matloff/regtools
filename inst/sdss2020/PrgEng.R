
prgengExpVars <- function() 
{
   data(peDumms) 
   ped <- peDumms[,c(1,20,22,24:29,31,32)] 
   x <- ped[,-10] 
   y <- ped[,10] 
   plotExpVars(x,y,x,y,25,c(1,2,8,9,10),1.5,lossFtn='MAPE',ylim=c(23500,25000)) 
}

