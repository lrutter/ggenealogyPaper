rm(list=ls())
library("devtools")
load_all("SourceCode")
library("ggenealogy")
data("sbGeneal")

getBranches = function(v1, geneal, ancGen = 3, desGen = 3){
  id.offset <- NULL
  if (!is.null(buildAncList(v1, geneal))){
    aDF = buildAncDesCoordDF(nodeToDF(buildAncList(v1, geneal)))
    subDF = aDF[aDF$gen <= ancGen & aDF$gen != 0,]
    keep = c("label","gen")
    subDF = subDF[keep]
    row.names(subDF) = NULL
    aDF <- subDF[order(subDF$gen,subDF$label),]
  }
  id.offset <- NULL
  if (!is.null(buildDesList(v1, geneal))){
    dDF = buildAncDesCoordDF(nodeToDF(buildDesList(v1, geneal)))
    subDF = dDF[dDF$gen <= desGen & dDF$gen != 0,]
    keep = c("label","gen")
    subDF = subDF[keep]
    row.names(subDF) = NULL
    dDF <- subDF[order(subDF$gen,subDF$label),]
  }
  allDF <- rbind(aDF,dDF)
  if (is.null(allDF)){
    return(data.frame())
  }
  return(allDF)
}



sbFilt <- sbGeneal[complete.cases(sbGeneal[1:3]),]

getBranchStatistics = function(v1, geneal, colName, gen=3){
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
    Mean <- mean(dat[[colName]])
    SD <- sd(dat[[colName]])
    datRet <- rbind(datRet, data.frame(Name=childList[i],Count=Count, Mean=Mean, SD=SD, Labels=paste(Labels, collapse = ',')))
  }
  return(datRet) 
}

