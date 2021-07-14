install.packages(c("readxl", "magrittr", "tidyverse", "dplyr"))
install.packages("xtable")
install.packages("rapportools")

library(readxl) #import data from .xls files
library(tidyverse) #Data analysis
library(magrittr) # gives us the %>% function
library(dplyr) #Gives us the mutate function
library(ggplot2)
library(xtable) #outputs tables as LaTeX code
library(rapportools)


#------------------------------------------------------------------------------
# CONFIDENCE PLOTTING
# PER PARTICIPANT
meanConfidencePerParticipant <- answers %>% 
  select(C1, C2, C3, C4, C5, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22, C23) %>% 
  mutate(Confidence = (C1 + C2 + C3 + C4 + C5 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 +C15 + C16 + C17 + C18 + C19 + C20 + C21 + C22 + C23) / 23) %>% 
  mutate(Participant = 1:n()) %>% select(Participant, everything()) %>% 
  select(Participant, Confidence) 

ggplot(data = meanConfidencePerParticipant) + geom_point(mapping = aes(Participant, Confidence))

# PER TASK
meanConfidencePerTask <- answers %>% 
  select(C1, C2, C3, C4, C5, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22, C23)  
meanConfidencePerTask <- colMeans(meanConfidencePerTask, na.rm = FALSE, dims =1) 
meanConfidencePerTask <- meanConfidencePerTask %>% 
  
  barplot(meanConfidencePerTask)

standardDeviationPerTask <- answers %>% 
  select(C1, C2, C3, C4, C5, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22, C23)  
apply(standardDeviationPerTask,2,sd)

#------------------------------------------------------------------------------
# DELTA P CALCULATIONS PER TASK
#------------------------------------------------------------------------------
# EXAMPLE TASK WITH (MOSTLY) OLD CODE
# Delta p: p(white|circle)-p(white|not-circle)
# 4 Visible sides (2 non-visible sides):
# circle black, circle black, circle white, triangle black, ? , ?
# Conditional: if circle, then white

constituents <- rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1)))  
constituents
c1<-constituents[,1]
c2<-constituents[,2]
w1<-constituents[,3]
w2<-constituents[,4]
constituents

#W1= the first extra face is white
#W2= the second extra face is white
#C1= the first extra face is Circle
#C2= the second extra face is Circle

C<-3+c1+c2# the total number of circle
W<-1+w1+w2# the total number of w
nC<-1+(1-c1)+(1-c2)
WC<-1+c1*w1+c2*w2
WnC<-0+w1*(1-c1)+w2*(1-c2)
pWgC<-WC/C
pWgnC<-WnC/nC
pWgCmpWgnC<-pWgC-pWgnC
output<-cbind(c1,c2,w1,w2,WC,C,WnC,nC,pWgC,pWgnC,pWgCmpWgnC)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 
#------------------------------------------------------------------------------
# TASK 1
# 6 visible sides: 1 white circle, 2 Black Squares, 3 white squares
# square --> black (so square is A, black is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

A<-5# the total number of squares
C<-2# the total number of black
nA<-6-A # the total number of circles
AC<-2 # the total number of black squares
CnA<-0 # the total of black circles
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 

#------------------------------------------------------------------------------
# TASK 2
# 6 visible sides: 1 white triangle, 3 Black Squares, 2 white squares
# white --> triangle (so white is A, triangle is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

A<-3# the total number of A
C<-1# the total number of C
nA<-6-A # the total number of not-A
AC<-1 # the total number of A and C
CnA<-0 # the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 

#------------------------------------------------------------------------------
# TASK 3
# 6 visible sides: 1 black triangle, 3 black circles, 2 white triangles
# black --> circle (so black is A, circle is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

A<-4# the total number of A
C<-3# the total number of C
nA<-6-A # the total number of not-A
AC<-3 # the total number of A and C
CnA<-0 # the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 


#------------------------------------------------------------------------------
# TASK 4
# one ?, 5 visible sides: 2 black squares, 1 black circle, 2 white squares 
# circle --> black (so circle is A, black is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

constituents <- rev(expand.grid(c(0,1),c(0,1))) #one ? side
constituents
a1<-constituents[,1]
c1<-constituents[,2]


A<-1 + a1  # the total number of A
C<-3 + c1 # the total number of C
nA<- 4 + (1-a1) # the total number of not-A
AC<-1 + a1*c1 # the total number of A and C
CnA<- 2 + c1*(1-a1)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex")

#------------------------------------------------------------------------------
# TASK 5
# two ?, 4 visible sides: 1 black square, 2 white triangles, 1 white square
# white --> square (so white is A, square is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

constituents <- rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1))) #two ? sides
constituents
a1<-constituents[,1]
a2<-constituents[,2]
c1<-constituents[,3]
c2<-constituents[,4]


A<-3 + a1 + a2  # the total number of A
C<-2 + c1 + c2# the total number of C
nA<- 1 + (1-a1) + (1-a2)# the total number of not-A
AC<-1 + a1*c1 + a2*c2 # the total number of A and C
CnA<- 1 + c1*(1-a1) + c2*(1-a2)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 

#------------------------------------------------------------------------------
# TASK 6
# three ?, 3 visible sides: 1 black circle, 1 white circle, 1 white square
# circle --> white (so circle is A, white is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

constituents <- rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1))) 
#three ? sides
constituents
a1<-constituents[,1]
a2<-constituents[,2]
a3<-constituents[,3]
c1<-constituents[,4]
c2<-constituents[,5]
c3<-constituents[,6]

A<-2 + a1 + a2 + a3 # the total number of A
C<-2 + c1 + c2 + c3 # the total number of C
nA<- 1 + (1-a1) + (1-a2) + (1-a3)# the total number of not-A
AC<-1 + a1*c1 + a2*c2 + a3*c3 # the total number of A and C
CnA<- 1 + c1*(1-a1) + c2*(1-a2) + c3*(1-a3)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 

#------------------------------------------------------------------------------
# TASK 7
# two ?, 4 visible sides: 2 black squares, 2 white squares
# white --> square (so white is A, square is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

constituents <- rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1))) #two ? sides
constituents
a1<-constituents[,1]
a2<-constituents[,2]
c1<-constituents[,3]
c2<-constituents[,4]


A<-2 + a1 + a2  # the total number of A
C<-4 + c1 + c2# the total number of C
nA<- 2 + (1-a1) + (1-a2)# the total number of not-A
AC<-2 + a1*c1 + a2*c2 # the total number of A and C
CnA<- 2 + c1*(1-a1) + c2*(1-a2)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 

#------------------------------------------------------------------------------
# TASK 8
# one ?, 5 visible sides: 1 black square, 3 white circles, 1 white square
# square --> black (so square is A, black is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 
constituents <- rev(expand.grid(c(0,1),c(0,1))) #one ? side
constituents
a1<-constituents[,1]
c1<-constituents[,2]
A<-2 + a1  # the total number of A
C<-1 + c1 # the total number of C
nA<- 3 + (1-a1) # the total number of not-A
AC<-1 + a1*c1 # the total number of A and C
CnA<- 0 + c1*(1-a1)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex")

#------------------------------------------------------------------------------
# TASK 9
# two ?, 4 visible sides: 1 black triangle, 2 white triangles, 1 black circle
# black --> circle (so black is A, circle is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

constituents <- rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1))) #two ? sides
constituents
a1<-constituents[,1]
a2<-constituents[,2]
c1<-constituents[,3]
c2<-constituents[,4]


A<-2 + a1 + a2  # the total number of A
C<-1 + c1 + c2# the total number of C
nA<- 2 + (1-a1) + (1-a2)# the total number of not-A
AC<-1 + a1*c1 + a2*c2 # the total number of A and C
CnA<- 0 + c1*(1-a1) + c2*(1-a2)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 

#------------------------------------------------------------------------------
# TASK 10
# one ?, 5 visible sides: 1 black square, 2 white triangles, 2 white squares
# triangle --> white (so triangle is A, white is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 
constituents <- rev(expand.grid(c(0,1),c(0,1))) #one ? side
constituents
a1<-constituents[,1]
c1<-constituents[,2]
A<-2 + a1  # the total number of A
C<-4 + c1 # the total number of C
nA<- 3 + (1-a1) # the total number of not-A
AC<-2 + a1*c1 # the total number of A and C
CnA<- 2 + c1*(1-a1)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex")

#------------------------------------------------------------------------------
# TASK 11
# three ?, 3 visible sides: 1 black square, 2 black circles
# square --> white (so square is A, white is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

constituents <- rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1))) 
#three ? sides
constituents
a1<-constituents[,1]
a2<-constituents[,2]
a3<-constituents[,3]
c1<-constituents[,4]
c2<-constituents[,5]
c3<-constituents[,6]

A<-1 + a1 + a2 + a3 # the total number of A
C<-0 + c1 + c2 + c3 # the total number of C
nA<- 2 + (1-a1) + (1-a2) + (1-a3)# the total number of not-A
AC<-0 + a1*c1 + a2*c2 + a3*c3 # the total number of A and C
CnA<- 0 + c1*(1-a1) + c2*(1-a2) + c3*(1-a3)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 

#------------------------------------------------------------------------------
# TASK 12
# 6 visible sides: 4 black triangles, 1 Black Square, 1 white square
# square --> black (so square is A, black is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

A<-2# the total number of squares
C<-5# the total number of black
nA<-6-A # the total number of circles
AC<-1 # the total number of black squares
CnA<-4 # the total of black circles
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 

#------------------------------------------------------------------------------
# TASK 13
# two ?, 4 visible sides: 1 black triangle, 1 white circle, 2 black circles
# circle --> black (so circle is A, black is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

constituents <- rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1))) #two ? sides
constituents
a1<-constituents[,1]
a2<-constituents[,2]
c1<-constituents[,3]
c2<-constituents[,4]


A<-3 + a1 + a2  # the total number of A
C<-3 + c1 + c2# the total number of C
nA<- 1 + (1-a1) + (1-a2)# the total number of not-A
AC<-2 + a1*c1 + a2*c2 # the total number of A and C
CnA<- 1 + c1*(1-a1) + c2*(1-a2)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 

#------------------------------------------------------------------------------
# TASK 14
# three ?, 3 visible sides: 1 white square, 1 black triangle, 1 white triangle
# black --> triangle (so black is A, triangle is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

constituents <- rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1))) 
#three ? sides
constituents
a1<-constituents[,1]
a2<-constituents[,2]
a3<-constituents[,3]
c1<-constituents[,4]
c2<-constituents[,5]
c3<-constituents[,6]

A<-1 + a1 + a2 + a3 # the total number of A
C<-2 + c1 + c2 + c3 # the total number of C
nA<- 2 + (1-a1) + (1-a2) + (1-a3)# the total number of not-A
AC<-1 + a1*c1 + a2*c2 + a3*c3 # the total number of A and C
CnA<- 1 + c1*(1-a1) + c2*(1-a2) + c3*(1-a3)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex")

#------------------------------------------------------------------------------
# TASK 15
# one ?, 5 visible sides: 1 white circle, 2 white triangles, 2 black triangles
# white --> triangle (so white is A, triangle is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 
constituents <- rev(expand.grid(c(0,1),c(0,1))) #one ? side
constituents
a1<-constituents[,1]
c1<-constituents[,2]
A<-3 + a1  # the total number of A
C<-4 + c1 # the total number of C
nA<- 2 + (1-a1) # the total number of not-A
AC<-2 + a1*c1 # the total number of A and C
CnA<- 2 + c1*(1-a1)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex")

#------------------------------------------------------------------------------
# TASK 16
# three ?, 3 visible sides: 1 black triangle, 2 black circles
# circle --> white (so circle is A, white is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

constituents <- rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1))) 
#three ? sides
constituents
a1<-constituents[,1]
a2<-constituents[,2]
a3<-constituents[,3]
c1<-constituents[,4]
c2<-constituents[,5]
c3<-constituents[,6]

A<-2 + a1 + a2 + a3 # the total number of A
C<-0 + c1 + c2 + c3 # the total number of C
nA<- 1 + (1-a1) + (1-a2) + (1-a3)# the total number of not-A
AC<-0 + a1*c1 + a2*c2 + a3*c3 # the total number of A and C
CnA<- 0 + c1*(1-a1) + c2*(1-a2) + c3*(1-a3)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex")

#------------------------------------------------------------------------------
# TASK 17
# two ?, 4 visible sides: 3 white circles, 1 black circle
# black --> square (so black is A, square is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

constituents <- rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1))) #two ? sides
constituents
a1<-constituents[,1]
a2<-constituents[,2]
c1<-constituents[,3]
c2<-constituents[,4]


A<-1 + a1 + a2  # the total number of A
C<-0 + c1 + c2# the total number of C
nA<- 3 + (1-a1) + (1-a2)# the total number of not-A
AC<-0 + a1*c1 + a2*c2 # the total number of A and C
CnA<- 0 + c1*(1-a1) + c2*(1-a2)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 

#------------------------------------------------------------------------------
# TASK 18
# one ?, 5 visible sides: 1 white circle, 4 black circles
# white --> circle (so white is A, circle is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 
constituents <- rev(expand.grid(c(0,1),c(0,1))) #one ? side
constituents
a1<-constituents[,1]
c1<-constituents[,2]
A<-1 + a1  # the total number of A
C<-5 + c1 # the total number of C
nA<- 4 + (1-a1) # the total number of not-A
AC<-1 + a1*c1 # the total number of A and C
CnA<- 4 + c1*(1-a1)# the total number of not-A and C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex")

#------------------------------------------------------------------------------
# TASK 19
# 6 visible sides: 1 white circle, 2 Black circles, 3 white triangles
# white --> triangle (so white is A, triangle is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

A<-4# the total number of A
C<-3# the total number of C
nA<-6-A # the total number of not-A
AC<-3 # the total number of A and C 
CnA<-0 # the total of black not-A C
pCgA<-AC/A
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 

#------------------------------------------------------------------------------
# TASK 20
# three ?, 3 visible sides: 3 white circles
# square --> black (so square is A, black is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

constituents <- rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1))) 
#three ? sides
constituents
a1<-constituents[,1]
a2<-constituents[,2]
a3<-constituents[,3]
c1<-constituents[,4]
c2<-constituents[,5]
c3<-constituents[,6]

A<-0 + a1 + a2 + a3 # the total number of A
C<-0 + c1 + c2 + c3 # the total number of C
nA<- 3 + (1-a1) + (1-a2) + (1-a3)# the total number of not-A
AC<-0 + a1*c1 + a2*c2 + a3*c3 # the total number of A and C
CnA<- 0 + c1*(1-a1) + c2*(1-a2) + c3*(1-a3)# the total number of not-A and C
pCgA<-AC/A
pCgA[1:8]<-c(0,0,0,0,0,0,0,0) #replaces NA values with 0 (division not defined)
pCgnA<-CnA/nA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex")

#------------------------------------------------------------------------------
# TASK 21
# 6 visible sides: 6 white circles
# square --> black (so square is A, black is C)
rm(A, C, nA, AC, CnA, pCgA, pCgnA, deltaP, output, summary, a1, a2, 
   a3, a4, a5, a6, c1, c2, c3, c4, c5, c6, constituents) #clears values 

A<-0# the total number of A
C<-0# the total number of C
nA<-6-A # the total number of non-A
AC<-0 # the total number of A and C
CnA<-0 # the total of black C and non-A
pCgA<-AC/A
pCgA[1]<-c(0)
pCgnA<-CnA/nA
pCgnA
deltaP<-pCgA-pCgnA
output<-cbind(AC,A,CnA,nA,pCgA,pCgnA,deltaP)
summary <- summary(cbind(deltaP))
summary
xtable(summary, type = "latex")
xtable(output,type="latex") 


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# TRYING MY HANDS AT A SUBROUTINE FOR DELTA-P
calculateDeltaP <- function(vecA, vecC) {
  #Expects as input 2 equal length vectors containing 6 or fewer boolean values 
  #each. VecA should contain whether the side in question makes the 
  #antecedent of the conditional true, VecC the conditional. 
  
  if (!is.vector(vecA) |!is.vector(vecC)){
    stop('Invalid input: Expected 2 vectors')
  }
  if (length(vecA)!= length(vecC)){
    stop('Invalid input: Unequal vector length.')
  }
  if (!all(is.logical(vecA))) {
    stop('Invalid input: Vector A contains non-boolean value.')
  }
  if (!all(is.logical(vecC))){
    stop('Invalid input: Vector C contains non-boolean value.')
  }
  if (length(vecA)>6){
    stop('Invalid input: Vectors contain too many values.')
  }
  
  #-----------------------------------------------------------------------------
  #Vectors shorter than 6 values are taken to be uncertainty tasks with an 
  #appropriate amount of blank sides.
  aBlanks = list(c(0), c(0), c(0), c(0), c(0), c(0))
  cBlanks = list(c(0), c(0), c(0), c(0), c(0), c(0))#eventuell NULL setzen
  consituents <- vector(mode = "list", length = 0)
  constituents <<- NULL
  numberOfConceiledSides <- (6 - length(vecA))
  
  if (numberOfConceiledSides > 0){ 
    for (i in 1 :numberOfConceiledSides ){ #preparation to expand the grid
      additionalVariable <- list(c(0,1),c(0,1))
      constituents <- c(constituents, additionalVariable)
    }
    constituents <- rev(expand.grid(constituents)) #grid expansion
    for (i in 1 : numberOfConceiledSides){ #grid to vectors
      aBlanks[[i]]<-constituents[,i]
      cBlanks[[i]]<-constituents[,(i+numberOfConceiledSides)]
    }
  }
  #-----------------------------------------------------------------------------
  #Reads out the inputs and (if applicable) the arrays containing the variables
  #for blank sides. 
  A <- (sum(vecA) + aBlanks[[1]] + aBlanks[[2]] + aBlanks[[3]] + aBlanks[[4]] 
        + aBlanks[[5]] + aBlanks[[6]]) #irrelevant values equal 0
  C <- (sum(vecC) + cBlanks[[1]] + cBlanks[[2]] + cBlanks[[3]] + cBlanks[[4]] 
        + cBlanks[[5]] + cBlanks[[6]]) #irrelevant values equal 0
  nA<- 6 - A
  tempAC <- 0
  if (length(vecA)>0){
  for (i in 1 : length(vecA)){
    if (vecA[i] & vecC[i]){
      tempAC <- tempAC + 1
    }
  }}
  AC<- (tempAC + aBlanks[[1]]*cBlanks[[1]] + aBlanks[[2]]*cBlanks[[2]]
        + aBlanks[[3]]*cBlanks[[3]] + aBlanks[[4]]*cBlanks[[4]] 
        + aBlanks[[5]]*cBlanks[[5]]  + aBlanks[[6]]*cBlanks[[6]])
  
  CnA <- C - AC
  #-----------------------------------------------------------------------------
  #Calculates delta P, substituting 0 for undefined values resulting from 
  #division by 0
  pCgA<-AC/A
  for (i in 1 : length(pCgA)){
    if (is.na(pCgA[i])){
      pCgA[i]<-c(0)
    }
  }
  pCgnA<-CnA/nA
  for (i in 1 : length(pCgnA)){
    if (is.na(pCgnA[i])){
      pCgnA[i]<-c(0)
    }
  }
  deltaP<-pCgA-pCgnA
  print(length(deltaP))
  return(summary(cbind(deltaP))) #returns the summary of the delta P column
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# TRYING MY HANDS AT A SUBROUTINE FOR THE INTERPRETATIONS

calculateInterpretations <- function(vecA, vecC) {
  #Expects as input 2 equal length vectors containing 6 or fewer boolean values 
  #each. VecA should contain whether the side in question makes the 
  #antecedent of the conditional true, VecC the conditional. 
  
  if (!is.vector(vecA) |!is.vector(vecC)){
    stop('Invalid input: Expected 2 vectors')
  }
  if (length(vecA)!= length(vecC)){
    stop('Invalid input: Unequal vector length.')
  }
  if (!all(is.logical(vecA))) {
    stop('Invalid input: Vector A contains non-boolean value.')
  }
  if (!all(is.logical(vecC))){
    stop('Invalid input: Vector C contains non-boolean value.')
  }
  if (length(vecA)>6){
    stop('Invalid input: Vectors contain too many values.')
  }
  
  #-----------------------------------------------------------------------------
  #Vectors shorter than 6 values are taken to be uncertainty tasks with an 
  #appropriate amount of blank sides.
  aBlanks = list(c(0), c(0), c(0), c(0), c(0), c(0))
  cBlanks = list(c(0), c(0), c(0), c(0), c(0), c(0))#eventuell NULL setzen
  consituents <- vector(mode = "list", length = 0)
  constituents <<- NULL
  numberOfConceiledSides <- (6 - length(vecA))
  
  if (numberOfConceiledSides > 0){ 
    for (i in 1 :numberOfConceiledSides ){ #preparation to expand the grid
      additionalVariable <- list(c(0,1),c(0,1))
      constituents <- c(constituents, additionalVariable)
    }
    constituents <- rev(expand.grid(constituents)) #grid expansion
    for (i in 1 : numberOfConceiledSides){ #grid to vectors
      aBlanks[[i]]<-constituents[,i]
      cBlanks[[i]]<-constituents[,(i+numberOfConceiledSides)]
    }
  }
  #-----------------------------------------------------------------------------
  #Reads out the inputs and (if applicable) the arrays containing the variables
  #for blank sides. 
  A <- (sum(vecA) + aBlanks[[1]] + aBlanks[[2]] + aBlanks[[3]] + aBlanks[[4]] 
        + aBlanks[[5]] + aBlanks[[6]]) #irrelevant values equal 0
  C <- (sum(vecC) + cBlanks[[1]] + cBlanks[[2]] + cBlanks[[3]] + cBlanks[[4]] 
        + cBlanks[[5]] + cBlanks[[6]]) #irrelevant values equal 0
  nA<- 6 - A
  nC<- 6 - C
  tempAC <- 0
  if (length(vecA)>0){
  for (i in 1 : length(vecA)){
    if (vecA[i] & vecC[i]){
      tempAC <- tempAC + 1
    }
  }
  }
  AC<- (tempAC + aBlanks[[1]]*cBlanks[[1]] + (1-aBlanks[[2]])*(1-cBlanks[[2]])
        + (1-aBlanks[[3]])*(1 - cBlanks[[3]]) + (1-aBlanks[[4]])*(1-cBlanks[[4]]) 
        + (1-aBlanks[[5]])*(1-cBlanks[[5]])  + (1-aBlanks[[6]])*(1-cBlanks[[6]]))
  tempnAnC <- 0
  if(length(vecA)>0){
    for (i in 1 : length(vecA)){
      if (!vecA[i] & !vecC[i]){
      tempnAnC <- tempnAnC + 1
      }
    }
  }
  nAnC <- (tempnAnC + (1-aBlanks[[1]])*(1-cBlanks[[1]]) + aBlanks[[2]]*cBlanks[[2]]
           + aBlanks[[3]]*cBlanks[[3]] + aBlanks[[4]]*cBlanks[[4]] 
           + aBlanks[[5]]*cBlanks[[5]]  + aBlanks[[6]]*cBlanks[[6]])
  
  CnA <- C - AC
  AnC <- A - AC
  #-----------------------------------------------------------------------------
  #Calculates delta P, substituting 0 for undefined values resulting from 
  #division by 0
  materialConditional <- ((AC + CnA + nAnC)/6) #continue later
  print(summary(cbind(materialConditional)))
  equivalent <- ((AC + nAnC)/6)
  print(summary(cbind(equivalent)))
  conjunction <- (AC/6)
  print(summary(cbind(conjunction)))
  
  pCgA<-AC/A
  for (i in 1 : length(pCgA)){
    if (is.na(pCgA[i])){
      pCgA[i]<-c(0)
    }
  }
  pCgnA<-CnA/nA
  for (i in 1 : length(pCgnA)){
    if (is.na(pCgnA[i])){
      pCgnA[i]<-c(0)
    }
  }
  deltaP<-pCgA-pCgnA
  
  #-----------------------------------------------------------------------------
  #print and return our results
  print(summary(cbind(materialConditional)))
  print(summary(cbind(equivalent)))
  print(summary(cbind(conjunction)))
  print(length(deltaP))
  
  
  
  return(summary(cbind(deltaP))) #returns the summary of the delta P column
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# TESTING THE SUBROUTINES

vectorA <- logical() 
vectorC <- logical() 
calculateDeltaP(vectorA, vectorC)
calculateInterpretations(vectorA, vectorC)


