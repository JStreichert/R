A2ptAtt <- 0
A2ptMak <- 0
ANC3Att <- 0
ANC3Mak <- 0
AC3Att <- 0
AC3Mak <- 0
ATotAtt <- 0
B2ptAtt <- 0
B2ptMak <- 0
BNC3Att <- 0
BNC3Mak <- 0
BC3Att <- 0
BC3Mak <- 0
BTotAtt <- 0
shotDist <- function(a,b) {
  (a/b)*100
}
eFG <- function(a,b,c) {
  ((a+(0.5*b))/c)*100
}
inc <- function(x)
{
  eval.parent(substitute(x <- x + 1))
}
euclidean <- function(a, b) if(a<47) {sqrt(sum(((a-5.25) - (b-25))^2))} else {sqrt(sum(((88.75-a) - (b-25))^2))}
# Reads in data
Shots_Data <- read.csv(file.choose())
Shots_Data[ , 'Shot Zone'] <- NA
# Nested For loops read through dataframe and pick zones for every shot
for (row in 1:nrow(Shots_Data)) { 
  for (col in 1:ncol(Shots_Data)) {
    if(((Shots_Data[row,2] > 22)||(Shots_Data[row,2] < -22))&&(Shots_Data[row,3]<=7.8)){
      Shots_Data[row,5] <- "C3"
    } else if (euclidean(Shots_Data[row,2],Shots_Data[row,3]) > 23.75){
      Shots_Data[row,5] <- "NC3"
    } else {
      Shots_Data[row,5] <- "2PT"
    }
  }
}
print(Shots_Data[row,col])
for (row in 1:nrow(Shots_Data)) { 
  for (col in 1:ncol(Shots_Data)) {
    if (grepl("A", Shots_Data[row,1], fixed = TRUE)) {
      if (grepl("2PT", Shots_Data[row,5], fixed = TRUE)) {
        if (1 == Shots_Data[row,4]) {
          inc(A2ptAtt)
          inc(A2ptMak)
          inc(ATotAtt)
          break
        } else {
          inc(A2ptAtt)
          inc(ATotAtt)
          break
        }
      } else if (grepl("NC3", Shots_Data[row,5], fixed = TRUE)) {
        if (1 == Shots_Data[row,4]) {
          inc(ANC3Att)
          inc(ANC3Mak)
          inc(ATotAtt)
          break
        } else {
          inc(ANC3Att)
          inc(ATotAtt)
          break
        }
      } else if (grepl("C3", Shots_Data[row,5], fixed = TRUE)) {
        if (1 == Shots_Data[row,4]) {
          inc(AC3Att)
          inc(AC3Mak)
          inc(ATotAtt)
          break
        } else {
          inc(AC3Att)
          inc(ATotAtt)
          break
        }
      }
    } else if (grepl("B", Shots_Data[row,1], fixed = TRUE)) {
      if (grepl("2PT", Shots_Data[row,5], fixed = TRUE)) {
        if (1 == Shots_Data[row,4]) {
          inc(B2ptAtt)
          inc(B2ptMak)
          inc(BTotAtt)
          break
        } else {
          inc(B2ptAtt)
          inc(BTotAtt)
          break
        }
      } else if (grepl("NC3", Shots_Data[row,5], fixed = TRUE)) {
        if (1 == Shots_Data[row,4]) {
          inc(BNC3Att)
          inc(BNC3Mak)
          inc(BTotAtt)
          break
        } else {
          inc(BNC3Att)
          inc(BTotAtt)
          break
        }
      } else if (grepl("C3", Shots_Data[row,5], fixed = TRUE)) {
        if (1 == Shots_Data[row,4]) {
          inc(BC3Att)
          inc(BC3Mak)
          inc(BTotAtt)
          break
        } else {
          inc(BC3Att)
          inc(BTotAtt)
          break
        }
      }
    } 
  }
}
# Team A Stats
print(A2ptMak)
print(A2ptAtt)
print(ANC3Mak)
print(ANC3Att)
print(AC3Mak)
print(AC3Att)
print(ATotAtt)
# Team A Shot distribution and effective FG %
shotDist(A2ptAtt,ATotAtt)
shotDist(ANC3Att,ATotAtt)
shotDist(AC3Att,ATotAtt)
eFG(A2ptMak,0,A2ptAtt)
eFG(ANC3Mak,ANC3Mak,ANC3Att)
eFG(AC3Mak,AC3Mak,AC3Att)

# Team B Stats
print(B2ptMak)
print(B2ptAtt)
print(BNC3Mak)
print(BNC3Att)
print(BC3Mak)
print(BC3Att)
print(BTotAtt)
# Team B Shot distribution and effective FG %
shotDist(B2ptAtt,BTotAtt)
shotDist(BNC3Att,BTotAtt)
shotDist(BC3Att,BTotAtt)
eFG(B2ptMak,0,B2ptAtt)
eFG(BNC3Mak,BNC3Mak,BNC3Att)
eFG(BC3Mak,BC3Mak,BC3Att)