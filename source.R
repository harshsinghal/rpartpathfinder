require(mvpart)
require(plyr)


rpartPathFinder <- function(rpartmodel)
{	
	if(!inherits(rpartmodel,"rpart"))
	{
		stop("Not an rpart object")	
	}
	
	if(inherits(rpartmodel,"rpart"))
	{
		f <- rpartmodel$frame
		if(nrow(f)>1)
		{
			f$start <- 0
			f$end <- 0
			start <- 1
			for(i in 1:nrow(f))
			{
				if(as.character(f$var[i])!="<leaf>")
				{
					f$start[i] <- start
					f$end[i] <- f$start[i] + f$ncompete[i] + f$nsurrogate[i]
					start <- f$end[i] + 1
				}
			}
			
			f$nodenumber <- as.numeric(rownames(f))-1 
			f$previousnode <- (f$nodenumber - 1)/2
			f$previousnode <- floor(f$previousnode)
			
			f$nodenumber <- f$nodenumber + 1 
			f <- f[,c('var','nodenumber')]
			rpartPaths <- path.rpart(rpartmodel,f$nodenumber,print.it=FALSE)
			f$rpartpaths <- do.call("c",lapply(rpartPaths,function(x)paste(x,collapse = " -AND- ")))
			f<- as.data.frame(f,stringsAsFactors=FALSE)
			return(f)
		}
		if(nrow(f)<2){
			stop("Tree has only root node")
		}
	}
}	



keepDrop <- function(d,treeObj)
{
	if(unique(d$compareOp) == '<'){
		d$varValue <- as.numeric(d$varValue)
		d<- d[order(d$varValue,decreasing=TRUE),]
		ruleString <- rownames(d)[1]
	} 
	if(unique(d$compareOp) == '>='){
		d$varValue <- as.numeric(d$varValue)
		d<- d[order(d$varValue,decreasing=FALSE),]
		ruleString <- rownames(d)[1]
	} 
	if(unique(d$compareOp) == '='){
		
		facList <- attr(treeObj,"xlevels")
		levelsList <- facList[unique(d$varName)][[1]]
		d$levelsList <- sapply(d$varValue,function(x,y=levelsList)
				{   lvs <- NULL
					for(l in levelsList)
					{
						if(grepl(l,x,fixed=T))
						{
							lvs <- c(lvs,l)
						}
					}
					return(paste(lvs,collapse="-AND-"))
				})
		
		d$levelCt <- sapply(d$levelsList,function(x)length(strsplit(x,"-AND-")[[1]]))
		d<-d[order(d$levelCt,decreasing=T),]
		d$keepNoKeep <- 1:nrow(d)
		d$keepNoKeep[1] <- "KEEP"
		superSet <- strsplit(d$varValue[1],",")[[1]]
		if(nrow(d)>1){
			for(r in 2:nrow(d)){
				subSetVals <- strsplit(d$varValue[r],",")[[1]]
				if(sum(subSetVals %in% superSet) == length(subSetVals))
				{
					d$keepNoKeep[r] <- "DROP"
				}
				if(sum(subSetVals %in% superSet) != length(subSetVals))
				{
					d$keepNoKeep[r] <- "KEEP"
				}
			} # for loop
			d<-d[d$keepNoKeep == 'KEEP',]
			ruleString <- paste(rownames(d),collapse = " -AND- ")
		} # if nrow(d)
		if(nrow(d)==1){
			ruleString <- rownames(d)[1]	
		}
	} #if = operator (categoricals only) 
	
	
	#reducedRule <- rownames(d)[1]
	return(ruleString)
}

reduceRules <- function(df,treeObj)
{
	df$simplePath <- 1:nrow(df)
	for (i in 1:nrow(df))
	{
		atomicRules <- strsplit(df$rpartpaths[i],"\\s+-AND-\\s+")[[1]]
		atomicRules <- atomicRules[atomicRules != "root"]
		if(length(atomicRules) != 0)
		{
			rulesMat <- t(sapply(atomicRules,function(x)strsplit(x,"<|>=|=")[[1]]))
			rulesDf <-as.data.frame(rulesMat,stringsAsFactors=FALSE)
			names(rulesDf) <- c("varName","varValue")
			#rulesDf$varValue <- as.numeric(rulesDf$varValue)
			rulesDf$compareOp <- gsub("[a-z]{1,}|[0-9]|\\.|\\s+|,|\\]|\\[|\\(|\\)","",tolower(rownames(rulesDf)))
			rulesDf$varNmCompOp <- paste(rulesDf$varName,rulesDf$compareOp,sep="")	
			simpleRules <- ddply(rulesDf,.(varNmCompOp),keepDrop,treeObj)
			df$simplePath[i] <- paste(c('root',simpleRules[,2]),collapse=' -AND- ')
		}
		if(length(atomicRules) == 0)
		{
			df$simplePath[i] <- 'root'
		}
	}
	return(df)
}




