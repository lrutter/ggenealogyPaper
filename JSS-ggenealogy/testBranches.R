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

getBranchStatQual = function(v1, geneal, colName, rExpr, gen=3){
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
    CountTrue <- sum(eval(parse(text=rExpr)),na.rm=TRUE)
    NACount <- sum(is.na(eval(parse(text=rExpr))))
    PercentNotNA <- round(100*(1-(NACount/Count)),digits=2)
    datRet <- rbind(datRet, data.frame(Name=childList[i], CountTrue = CountTrue, Count=Count, NACount=NACount, PercentNotNA=PercentNotNA, Labels=paste(Labels, collapse = ',')))
  }
  datRet <- datRet[order(-datRet$CountTrue),]
  return(datRet)
}

v1="David Cox";geneal=statGeneal;colName="thesis";gen=15
rExpr = "grepl('Stochastic', geneal$colName)"
DCBranches_Stochastic <- getBranchStatQual(v1, geneal, colName, rExpr, gen)

rExpr = "grepl('(?i)Stochastic', geneal$colName)"
DCBranches_StochasticCaps <- getBranchStatQual(v1, geneal, colName, rExpr, gen)

colName = "country"
rExpr = "geneal$colName=='UnitedKingdom'" #works
DCBranches_UK <- getBranchStatQual(v1, geneal, colName, rExpr, gen)

desDC <- getDescendants("David Cox", statGeneal,15)
sort(table(statGeneal[match(desDC$label,statGeneal$child),]$school))
sum(statGeneal[match(getChild("David Cox",statGeneal),statGeneal$child),]$school=="University of London")

colName = "school"
rExpr = "geneal$colName=='University of London'" #works
DCBranches_UL <- getBranchStatQual(v1, geneal, colName, rExpr, gen)

rExpr = "geneal$colName=='Universidade de São Paulo'"
DCBranches_USP <- getBranchStatQual(v1, geneal, colName, rExpr, gen)
