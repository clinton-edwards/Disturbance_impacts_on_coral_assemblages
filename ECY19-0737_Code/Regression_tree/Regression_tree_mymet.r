

y=0
moy=0
risk=0
######################## mymet file ###########################################
mymet = list()
#mymet is a list of length 4 and allows to calculate the criterion at each step
#of the splitting process using the RPART R package.
#See EOF example to call the method with RPART.
#Here, Euclidean distances between functional observations are used but the functions
#can easily be modified to compute other distances
# An error can occur when compiling this function, but it has no matter...


mymet$eval = function (y, w, p) 
{
    if (is.null(nrow(y))) 
        risk = 0
    else {
        moy = colMeans(y)
        risk = sweep(y, 2, moy, "-")
        risk = sum(risk^2)
    }
    list(label = moy, deviance = risk)
}

mymet$split = function (Y, w, X, p, continuous=T) {
	crit = function(xx)
	{
	    if (is.null(nrow(xx))) yy = 0
	    else
	    {
	        moy = colMeans(xx)
	        yy = sweep(xx,2,moy,"-")
        	yy = sum(yy^2)
	    }
	    yy
	}
	N = length(X)
	res = numeric()
	seuils = numeric()
	ldev = numeric()
        val0 = crit(Y)
        for (i in 1:(N - 1)) {
            ind = 1:i
            res[i] = crit(Y[ind, ]) + crit(Y[-ind, ])
        }
	res = val0-res
	cat("resmin=",min(res),fill=T)
	dir= rep(1,length(res))
	if(continuous) res0=list(goodness = res, direction=dir)
	else res0=list(goodness =, direction=)
	res0
 }

mymet$init = function (y, offset, wt) 
{
	resume = function(){"test"	}
	list(y = y, numy = ncol(y), numresp = ncol(y), parms = NULL,summary=resume)
}

mymet$numy = ncol(y)

#Example using a 3 dimensional response variable (datay) and 5 colums of the predictor matrix (datax)
#to build a functional regression tree 
#Argument values in function RPART are available with help files of package RPART
# res.tree = rpart(as.formula(paste("as.matrix(datay[,1:3]) ~ 
# ",paste("datax[,",1:5,"]",collapse="+"))),method=mymet,
# control=rpart.control(cp=.0002,xval=0,minbucket=3,maxsurrogate=0,minsplit=2))

     







   # }
    list(label = moy, deviance = risk)
#}

mymet$split = function (Y, w, X, p, continuous=T) {
	crit = function(xx)
	{
	    if (is.null(nrow(xx))) yy = 0
	    else
	    {
	        moy = colMeans(xx)
	        yy = sweep(xx,2,moy,"-")
        	yy = sum(yy^2)
	    }
	    yy
	}
	N = length(X)
	res = numeric()
	seuils = numeric()
	ldev = numeric()
        val0 = crit(Y)
        for (i in 1:(N - 1)) {
            ind = 1:i
            res[i] = crit(Y[ind, ]) + crit(Y[-ind, ])
        }
	res = val0-res
	cat("resmin=",min(res),fill=T)
	dir= rep(1,length(res))
	if(continuous) res0=list(goodness = res, direction=dir)
	else res0=list(goodness =, direction=)
	res0
}

mymet$init = function (y, offset, wt) 
{
	resume = function(){"test"	}
	list(y = y, numy = ncol(y), numresp = ncol(y), parms = NULL,summary=resume)
}

mymet$numy = ncol(y)

#Example using a 3 dimensional response variable (datay) and 5 colums of the predictor matrix (datax)
#to build a functional regression tree 
#Argument values in function RPART are available with help files of package RPART
# res.tree = rpart(as.formula(paste("as.matrix(datay[,1:3]) ~ 
# ",paste("datax[,",1:5,"]",collapse="+"))),method=mymet,
# control=rpart.control(cp=.0002,xval=0,minbucket=3,maxsurrogate=0,minsplit=2))

     




     