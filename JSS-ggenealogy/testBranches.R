rm(list=ls())
library("devtools")
load_all("SourceCode")
library("ggenealogy")
data("sbGeneal")
data("statGeneal")
library(stringr) # for str_count

v1="Tokyo";geneal=sbGeneal;colName="devYear";gen=15
v1="David Cox";geneal=statGeneal;colName="gradYear";gen=15

getBranchStatQuant = function(v1, geneal, colName, gen=3){
  id.offset <- NULL
  if (is.null(getChild(v1, geneal))){
    return(data.frame())
  }
  childList <- getChild(v1,geneal)
  datRet <- data.frame()
  for (i in 1:length(childList)){
    dat <- getDescendants(childList[i], geneal, gen)
    dat[[colName]] <- geneal[match(dat$label, geneal$child),][[colName]]
    Labels <- dat$label
    Count <- length(Labels)
    Mean <- mean(dat[[colName]], na.rm=TRUE)
    SD <- sd(dat[[colName]], na.rm = TRUE)
    NACount <- sum(is.na(dat[[colName]]))
    PercentNotNA <- round(100*(1-(NACount/Count)),digits=2)
    datRet <- rbind(datRet, data.frame(Name=childList[i], Mean=Mean, SD=SD, Count=Count, NACount=NACount, PercentNotNA=PercentNotNA, Labels=paste(Labels, collapse = ',')))
  }
  datRet <- datRet[order(-datRet$Mean),]
  return(datRet)
}

TokyoBranches <- getBranchStatQuant("Tokyo", sbGeneal, "devYear", 15)
AKBranches <- getBranchStatQuant("A.K.", sbGeneal, "yield", 15)
CoxBranches <- getBranchStatQuant("David Cox", statGeneal, "gradYear", 15)

##############################################################################
##############################################################################


v1="David Cox";geneal=statGeneal;colName="thesis";gen=15

rExpr = "grepl('Stochastic', geneal$colName)"

getBranchStatQual = function(v1, rExpr, gen=3){
  id.offset <- NULL
    if (is.null(getChild(v1, geneal))){
    return(data.frame())
  }
  childList <- getChild(v1,geneal)
  datRet <- data.frame()
  for (i in 1:length(childList)){
    dat <- getDescendants(childList[i], geneal, gen)
    dat[[colName]] <- geneal[match(dat$label, geneal$child),][[colName]]
    Labels <- dat$label
    Count <- length(Labels)
    
    rExpr = gsub("geneal[$]colName", "dat[[colName]]", rExpr)
    
    eval(parse(text=rExpr))
    
    Mean <- mean(eval(parse(text=rExpr)),na.rm=TRUE)
    SD <- sd(eval(parse(text=rExpr)),na.rm=TRUE)
    NACount <- sum(is.na(eval(parse(text=rExpr))))
    PercentNotNA <- round(100*(1-(NACount/Count)),digits=2)
    datRet <- rbind(datRet, data.frame(Name=childList[i], Mean=Mean, SD=SD, Count=Count, NACount=NACount, PercentNotNA=PercentNotNA, Labels=paste(Labels, collapse = ',')))
  }
  datRet <- datRet[order(-datRet$Mean),]
  return(datRet)
}

grepl("Stochastic", statGeneal$thesis) #150 \\147
rExpr = "grepl('Stochastic', statGeneal$thesis)" #works

grepl("(?i)Stochastic", statGeneal$thesis) #163 \\160
rExpr = "grepl('(?i)Stochastic', statGeneal$thesis)" #works

(statGeneal$thesis=="University of Leeds")
rExpr = "statGeneal$thesis=='University of Leeds'" #works

# Number, not just TRUE or FALSE
str_count(statGeneal$thesis, 'Stochastic')
rExpr = "str_count(statGeneal$thesis, 'Stochastic')" #works

str_count(statGeneal$thesis, '(?i)Stochastic')
rExpr = "str_count(statGeneal$thesis, '(?i)Stochastic')" #works


