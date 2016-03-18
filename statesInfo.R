setwd('C:/Users/CMD JMRC/Downloads')

statesInfo <- read.csv('stateData.csv')
str(statesInfo)
stategrad <- subset(statesInfo, highSchoolGrad > 50.0)
stategrad

stateilletricy <- statesInfo[statesInfo$illiteracy > 1.0, ]
str(stateilletricy)
dim(stateilletricy)
getwd()


