###############################################################################
#                                                                             #
#   Author  freya qv                                                          #
#   Audit Trail                                                               #
#    2014-12-15    Creation                                                   #
#    2014-12-29    Add to Freya Git tutorial                                  #
#    2015-01-01    Fetch to archimedes Git tutorial via sparc2                #
#                  added function setPathsWindowsGit - now we have,           #
#                  at least on Archimedes_PC, the following subdirectories    #
#                  in ..Rgit/tutorial     ie tutorialdata and graphs          #
#   2015-01-02     Mod on freya, Kubuntu at Sris, only for linux/windows      #
#                                                                             #
#
#   https://confluence.atlassian.com/display/BITBUCKET/Using+Git+branches
#
#  https://www.atlassian.com/git/tutorials/making-a-pull-request
#
#                                                                             #
#  TODO - need to test intraday stuff                                         #
###############################################################################

library(its)
#graphPath = "c:\\projects\\R\\its\\graphs\\"
dataPath = "tutorial/tutorialdata/"
#filename = paste(graphPath, "testZoo3.csv", sep="")
filename = paste(dataPath, "testZoo1.csv", sep="")

getData1 <- function(){
  start = "2014-01-01"
  end = "2014-11-12"
  mydata = priceIts(instruments = "^gdaxi", start, end, quote = c("Open","High", "Low", "Close"), provider = "yahoo", method = "auto", origin = "1899-12-30", compression = "d", quiet=TRUE)
  print(head(mydata))
  plot(mydata)
}

getData2 <- function(){
  ## Not run: 
  x1 <- priceIts(instrument = c("^ftse"), start = "1998-01-01", quote = "Close") 
  x2 <- priceIts(instrument = c("^gdaxi"), start = "1998-01-01", quote = "Close") 
  x <- union(x1,x2) 
  names(x) <- c("FTSE","DAX") 
  plot(x,lab=TRUE, xlab="dates label", ylab="prices", main="main title")
  ## End(Not run)
  x
}

#
#   function to set paths and key filenames
#
getPathsWindows <- function(){
   cat("current working dir  getwd() = ", getwd(), "\n")
   basePath = "c:\\projects\\R\\its\\"
   graphPath <<- paste(basePath, "graphs\\", sep="")
   dataPath  <<- paste(basePath, "data\\", sep="")
   datafile  <<- paste(dataPath, "test1.csv", sep="")
   cat("\n datafile = ", datafile, "\n")
}
getPathsWindowsGit <- function(){
  cat("current working dir  getwd() = ", getwd(), "\n")
  basePath = "c:\\projects\\Rgit\\tutorial\\"
  graphPath <<- paste(basePath, "graphs\\", sep="")
  dataPath  <<- paste(basePath, "tutorialdata\\", sep="")
  datafile  <<- paste(dataPath, "test1.csv", sep="")
  cat("\n datafile = ", datafile, "\n")
}
getPathsKubuntuFreya <- function(){
  cat("current working dir  getwd() = ", getwd(), "\n")
  basePath = "tutorial/"
  #graphPath <<- paste(basePath, "graphs\\", sep="")
  dataPath  <<- paste(basePath, "tutorialdata/", sep="")
  datafile  <<- paste(dataPath, "test1.csv", sep="")
  cat("\n datafile = ", datafile, "\n")
}
#readcsvIts(filename,informat=its.format(),outformat=its.format(),tz="",usetz = FALSE,header=TRUE,...) 
#writecsvIts(x,filename,format=its.format(),tz="",usetz = FALSE,col.names=NA, sep=",",split=FALSE,...)

#
#   function writeDatafile()
#
writeDatafile <- function(x){
  writecsvIts(x,datafile,format=its.format(),tz="",usetz = FALSE,col.names=NA, sep=",",split=FALSE)
  
}

#
#   readDatafile function
#
readDatafile <- function(){
 myf = readcsvIts(datafile,informat=its.format(),outformat=its.format(),tz="",usetz = FALSE,header=TRUE) 
 cat("\n readDatafile:   class(myf) = ", class(myf), "\n")
 print(head(myf))      
 
 # plot(myf,lab=TRUE, xlab="dates label", ylab="prices", main="main title") # lab wrong length
 plot(myf, xlab="dates label", ylab="prices", main="main title", type="l") 
 
 #  DUD PLOT SOLVED BELOW - by just convert matrix to its
 # so here, what to do about the dud plot; its because myf here is a matrix
 # and not an its object
 # so, (a) Convert to its;
 #     (b) plot columns from matrix (remember that times / dates are row lables
 #
 #  page 5 of manual has lengthy eg of this
 qq = its(myf, dates=as.POSIXct(x=strptime(dimnames(myf)[[1]],format=its.format())), names=dimnames(myf)[[2]], format=its.format())
 
 plot(qq, xlab="dates label", ylab="prices", main="Super its conversion - main title", type="l") 
 # and the conversion to its worked!
 
 cat("\n dim(myf)[1] = ", dim(myf)[1])
 cat("\n dim(myf)[2] = ", dim(myf)[2])
 qq
}    # end function readDatafile()

#
#   test intraday its function
#
getIntradayIts <- function(filename1){
  cat("\n --- --- getIntradayIts --- --- \n")
  tmp2 <- read.table(filename1, sep = ",", header=TRUE) # this header = true has stopped this working
  cat("\n ---  --- class(tmp2) = ", class(tmp2), "\n")
  print(head(tmp2))
  its.format("%Y-%m-%d %H:%M:%S")
  #z = zoo(tmp[,2:6], as.Date(as.character(tmp[,1]), format="%Y-%m-%d %H:%M:%S")) # ?? error here
  #z9 = zoo(tmp2[,2:6], as.POSIXct(as.character(tmp2[,1]), format="%Y-%m-%d %H:%M:%S")) # ?? error here

#  z9 = its(tmp2, dates=as.POSIXct(as.character(tmp2[,1]), format="%Y-%m-%d %H:%M:%S"))
  ##### the above is a read in in the correct format from file;
  #     next see if i can do a window on this data
z9 = its(tmp2[,2:6], dates=as.POSIXct(tmp2[,1]) )
#z9 = its(tmp2[,2:6], dates=as.POSIXct(tmp2[,1], format=its.format()))
cat("\n ---  --- class(z9) = ", class(z9), "\n")
  print(head(z9))
  
}
#
#   placeholder function
#
getData3 <- function(){
  
}

###############################################################################
#                                                                             #
#                                     main                                    #
#                                                                             #
###############################################################################

#myits = getData2()
#cat("\n  myits object  is of class", class(myits), "\n")
#print(head(myits))
#  next try some routine its commands to see their effect
#
####getPaths()
# writeDatafile(myits)
####readDatafile()

#
#   the above tests worked well; so next try a intraday version of its
#
#getPathsKubuntuFreya()
getPathsWindowsGit()
obj = readDatafile()
writeDatafile(obj)

 
# TODO
#graphPath = "c:\\projects\\R\\test01\\zoo\\"
#dataPath1 = paste(graphPath, "", sep="")
#filename1 = paste(dataPath1, "testZoo1.csv", sep="")
#getIntradayIts(filename1)
