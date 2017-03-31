#Useful string processing patterns
# 1. YOu can read data into a long string- perhaps from a file
# using readLines(), you can then call str_trim to trim leading and trailing whitespace,
# and then call str_split passing "\\W" in for the split param

##START PROCESSING OF BALTIMORE SPEED CAMERA FILE

if(!file.exists("data")) {
  dir.create("data")
}
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
if(!file.exists("./data/cameras.csv")) {
  download.file(fileUrl, destfile="./data/cameras.csv")
}

if(!file.exists("./data/secondCameras.csv")) {
  download.file(fileUrl,destfile="./data/secondCameras.csv",mode="wb") #just to see if cameras.csv 
  #and secondCameras.csv are identical
}


if(!exists("cameraData")){
  cameraData <- read.csv("./data/cameras.csv")
  names(cameraData)<-sapply(strsplit(names(cameraData), split = "\\."),function(x)x[[1]])  
}

#find intersections with more than a single observation
intersectionTable<-table(cameraData$intersection) #note: a single variable table is just a vector
#and can be manipulated as such
intersectionsWithMoreThanOneObs<-intersectionTable[intersectionTable>1]
cat("Intersections with more than one observation along with their respective numbers of observations","\n")
print(intersectionsWithMoreThanOneObs)


cat("record numbers of intersections containing Alameda","\n",sep="")
grep(pattern = "Alameda", x = cameraData$intersection)%>%print  # Find observation numbers of intersections that contain 'Alameda'

cat("Intersections containing Alameda","\n",sep="")
grep(pattern = "Alameda", x = cameraData$intersection,value=T)%>%print  # Find intersections that contain 'Alameda'


table(grepl(pattern = "Alameda", x = cameraData$intersection))%>%print  # build a table of intersections that do, and do not, contain 'Alameda'
##END PROCESSING OF BALTIMORE SPEED CAMERA FILE

##START PROCESSING OF SAT PEER REVIEW FILE
if (!file.exists("./data")) {
  dir.create("./data")
}
fileUrl1 = "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 = "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
if (!file.exists("./data/reviews.csv")) {
  download.file(fileUrl1, destfile = "./data/reviews.csv")
}
if (!file.exists("./data/solutions.csv")) {
  download.file(fileUrl2, destfile = "./data/solutions.csv")
}
reviews = read.csv("./data/reviews.csv")
solutions = read.csv("./data/solutions.csv")

cat("names(reviews) with any underscores removed","\n")
print(sub(pattern = "_", replacement = "", x = names(reviews)))

##END PROCESSING OF SAT PEER REVIEW FILE



#TRIM LEADING SPACES USING SUB FN
sub("\\s+","","  hinges are fun")
#[1] "hinges are fun"

#sentence to vector of words archetype
words<-base::strsplit("Oh Eumaus, eumaus, eumaus,you are : my      swinherd.","\\W+")
words%>%unlist()
#[1] "Oh"       "Eumaus"   "eumaus"   "eumaus"   "you"      "are"      "my"      
#[8] "swinherd"


#two or more contiguous "a"s
grepl("a{2,}",c("Maryland","coolaagabrielle","lulemaaan"))
#[1] FALSE  TRUE  TRUE


grepl("(s){2}","Misissippi")
#[1] TRUE

grepl("(ss){2}","Mississippi")#this looks for four (i.e. two consecutive 
#sequences of two consective s characters) consecutive s characters
#[1] FALSE

#Metacharacters
# \\w (a word character -i.e. any letter,digit or underscore)
# \\W (not a word character)
# \\s  (whitespace character- i.e. line breaks, tabs or spaces)
# \\S   (not a whitespace character)
# \\d (a digit)
# \\D  (not a digit)
# .  (any character)
# *   (any number of times)
# ^ beginning of string
# $ end of string
#Reserved characters ("+",".","*" and probably more)
# \\+
# \\.
# \\*


#strsplit revisited (string containing numbers to integer vector of numbers)
strsplit("13 234 9281        93 2",split="\\D+")%>%unlist()%>%as.integer
#[1]   13  234 9281   93    2

grepl("a|b", c("abc", "bcd", "cde"))
#[1]  TRUE  TRUE FALSE

#state.name {datasets} contains names of states
state.name%>%head
#[1] "Alabama"    "Alaska"     "Arizona"    "Arkansas"   "California" "Colorado"

#below cmd lists the states whose names both start and end w/ vowel
grep("^[AEIOU].*[aeiou]$",state.name,value=T)
#[1] "Alabama"  "Alaska"   "Arizona"  "Idaho"    "Indiana"  "Iowa"     "Ohio"    
#[8] "Oklahoma"



bushes<-str_trim(" bush BUSH busH   bUsh   ")%>%strsplit("\\W+")%>%unlist
bushes
#[1] "bush" "BUSH" "busH" "bUsh"
grepl("[bB][uU][sS][hH]",bushes)
#[1] TRUE TRUE TRUE TRUE

#reading from sentences1.txt
cat("\n")
cat("Reading from sentences1.txt","\n",sep="")
con<-file("sentences1.txt","r")
lines<-readLines(con)
close(con)
print(lines)
cat("lines ending in either a period or question mark.","\n",sep="")
grep("[\\.\\?]$",lines,value=T)%>%print
cat("\n")
cat("lines NOT ending in either a period or question mark","\n",sep="")
grep("[^\\.\\?]$",lines,value=T)%>%print


# When used at the beginning of a character class, the ^ is also a metacharacter and indicates characters that should not be matched.
# 
# For example this:
#   
#   [^?.]$
#   will match any lines that do not end in either a question mark or a period.
# 
# Also note that ^ has this functionality only at the the beginning of a character class. This:
#   
#   [a-z^]
# matches any lower case letter or the ^ symbol.


#Some more complicated regex
cat("\n","Reading from sentences2.txt","\n",sep="")
pattern<-"^[Gg]ood|[Bb]ad" #starts w/ [Gg]ood or has [Bb]ad somewhere
con<-file("sentences2.txt","r")
lines<-readLines(con)
close(con)

cat("All lines","\n")
print(lines)
cat("Only those lines either starting with [Gg]ood or having [Bb]ad somewhere in the line","\n",sep="")
grep(pattern,lines,value=T)%>%print

cat("\n","Only lines starting w/ [Gg]ood or starting w/ [Bb]ad","\n",sep="")
pattern<-"^([Gg]ood|[Bb]ad)"
grep(pattern,lines,value=T)%>%print

cat("\n","\n","Reading from sentences3.txt","\n",sep="")
con<-file("sentences3.txt","r")
lines<-readLines(con)
close(con)
cat("Printing all lines")
lines%>%print 
exp<-"[Gg]eorge( [Ww]\\.)? [Bb]ush"
cat("Target expression: ",exp,"\n",sep="")
cat("Prining only those lines that match the expression","\n","Notice that W- although optional- if present, must be preceded by a space","and succeeded by a period","\n",sep="")
grep(exp,lines,value=T) %>%print

writeLines("Reading from parens.txt")
exp<-"\\(.*\\)"
con<-file("parens.txt","r")
lines<-readLines(con)
writeLines("original lines")
print(lines)
close(con)
cat("Target expression:",exp,"\n",sep="")
cat("Only those lines matching target expression:","\n",sep="")
grep(exp,lines,value=T)%>%print
