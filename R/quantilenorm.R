quantilenorm <- function(x, method="median", quantprob=0.5, refquant = NA) {
	# x = n-by-p matrix, with n samples in rows and p variables in columns
	# method = {"mean","median","quant"}
	# quant = (0,1), applicable only if "quantile" chosen as method
	
	quantfunc <- function(xx, quanttype) {
		switch(quanttype,
			mean = mean(xx,na.rm = TRUE),
			median = median(xx,na.rm = TRUE),
			quant = quantile(xx, probs = quantprob),na.rm = TRUE)
	}
	
	xsort = matrix(0,nrow=nrow(x),ncol=ncol(x))
	xi = matrix(0,nrow=nrow(x),ncol=ncol(x))
	xout = matrix(0,nrow=nrow(x),ncol=ncol(x))
	# sort x
	for(ii in 1:nrow(x)){
		output=sort(x[ii,], index.return = TRUE)
		xsort[ii,]=output$x
		xi[ii,]=output$ix
	}
	# estimate quantiles
	if(is.na(refquant[1])){
		quantiles=apply(X=xsort,MARGIN=2,FUN=quantfunc,quanttype=method)
	}
	else{
		quantiles=refquant
		if(length(as.vector(quantiles))!=ncol(x)) stop("Reference quantile does not match data dimensionality!")
	}

	# put quantiles back into the data
	
	for(ii in 1:nrow(x)){
		xout[ii,xi[ii,]]=quantiles
	}
	result <- list(quantiles=quantiles,xout=xout)
	return(result)
}

