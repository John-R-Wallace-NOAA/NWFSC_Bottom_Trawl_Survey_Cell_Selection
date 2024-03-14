
'  # contour() function ported from Splus - R's version does not allow the saving of the contour levels  '

contour.splus <- function(x, y, z, nint = 5, nlevels = nint, v = signif(pretty(z, nlevels), 3), levels = v, add = F, plotit = !save, save = FALSE, ..., 
         labex = 1, xlim = range(x), ylim = range(y), main = "", sub = "", xlab, ylab, axes = T, triangles = F)
{
	if(length(list(...)) > 0) {
		if(plotit) {
			opar <- par(...)
			opar[["new"]] <- NULL
			on.exit(par(opar))
		}
		else {
			warning("plotting parameters ignored when plotit=F")
		}
	}
	if(is.list(x) && !is.null(x$x) && !is.null(x$y) && !is.null(x$z)) {
		if(missing(xlab))
			xlab <- paste(deparse(substitute(x)), "$x", sep = "")
		if(missing(ylab))
			ylab <- paste(deparse(substitute(x)), "$y", sep = "")
		z <- x$z
		y <- x$y
		x <- x$x
	}
	else if(is.matrix(x) && ncol(x) > 1 && nrow(x) > 1) {
		if(missing(xlab))
			xlab <- paste("1:nrow(", deparse(substitute(x)), ")", sep = "")
		if(missing(ylab))
			ylab <- paste("1:ncol(", deparse(substitute(x)), ")", sep = "")
		z <- x
		y <- 1:ncol(x)
		x <- 1:nrow(x)
	}
	else {
# x, y, z arguments given
		if(missing(xlab)) xlab <- deparse(substitute(x))
		if(missing(ylab))
			ylab <- deparse(substitute(y))
	}
	z <- as.matrix(z)
	nr <- nrow(z)
	nc <- ncol(z)
	if(nr != length(x))
		stop("x wrong length")
	if(nc != length(y))
		stop("y wrong length")
	if(any(is.na(x)))
		stop("missing values in x")
	if(any(is.na(y)))
		stop("missing values in y")
	z[!is.finite(z)] <- NA
	if(any(is.na(levels)))
		stop("missing values in levels")
	if(any(diff(x) <= 0))
		stop("x must be increasing")
	if(any(diff(y) <= 0))
		stop("y must be increasing")
	if(any(dim(z) < 2))
		stop("z must be at least 2x2 matrix")
	storage.mode(x) <- "single"
	storage.mode(y) <- "single"
	storage.mode(z) <- "single"
	storage.mode(levels) <- "single"
	if(plotit && !add) {
		plot(range(x), range(y), type = "n", err = -1, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main, sub = sub, axes = axes)
	}
	if(save) {
		files <- tempfile(paste(seq(along = levels), "c", sep = ""))
	}
	if(plotit && labex > 0)
		ocex <- par(cex = par("cex") * labex)
	if(save) on.exit(add = T, .C("S_savecont_done"))	# S_savecont_done is safe to call even if there is nothing to clean up
# It will close any open temp file.
	for(i in seq(along = levels)) {
		if(save)
			.C("S_savecont_init",
				levels[i],
				files[i],
				as.integer(1))
		.C("S_contour",
			x,
			y,
			z,
			nr,
			nc,
			levels[i],
			as.integer(1),
			as.integer(plotit),
			as.integer(labex > 0),
			as.integer(triangles),
			NAOK = T)
		if(save)
			.C("S_savecont_done")
	}
	if(plotit && labex > 0)
		par(ocex)
	if(save) {
		names(files) <- as.character(levels)
		lines <- lapply(files, scan, what = list(x = single(1000), y = single(1000)))
		unlink(files)
		return(invisible(lines))
	}
	else {
		return(invisible(levels))
	}
}
