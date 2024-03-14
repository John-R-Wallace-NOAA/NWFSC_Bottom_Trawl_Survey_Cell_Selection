Grid.Map <- function(Selected.Cells = Select.Cells.2006.688.Tows, Grid = Grid.G2.30.700, ves.colors = col.alpha(c("red", "blue", "green", "purple"), 0.5), longrange = NULL, latrange = NULL, by.vessel = TRUE, 
                     by.order = FALSE, geo.labels = FALSE, cell.labels = FALSE, cell.labels.all = FALSE, cex.label = 0.5, zoom = TRUE, new.graph = TRUE, Size.Graph = 1) {
                     
  	# Bathemetry is in fathoms
  	
  	# graphics.off()
  	# guiSetOption(option.name = "AutoAddPages", value.string = "Off")
  	
    # ***** look at lib(ContourFunctions) a little more ********

    require(JRWToolBox)
    require(Imap)
     
    pt.label <- function(x, y, label, offset = 0.15, y.offset = 0, cex = 0.5, ...) {
  	     points(x, y, lwd = 2, cex = cex, ...)
  	     text(x + offset, y + y.offset, label, cex = cex, adj = 0, ...)
    }
    
    CC <- function(Cells = Select.Cells.2006.688.Tows) {
  	     tmp <- Cells[, c("SW.LON", "NW.LON", "NE.LON", "SE.LON")]
  	     tmp$SW.LON.H <- tmp$SW.LON
  	     tmp$NAs <- rep(NA, nrow(tmp))
  	     x <- c(t(as.matrix(tmp)))
  	     
  	     tmp <- Cells[, c("SW.LAT", "NW.LAT", "NE.LAT", "SE.LAT")]
  	     tmp$SW.LAT.H <- tmp$SW.LAT
  	     tmp$NAs<- rep(NA, nrow(tmp))
  	     y <- c(t(as.matrix(tmp)))
  	     
  	     cbind(x, y)
    }

    # Comment out if warnings need to be seen
    optOld <- options(warn = -1)
    on.exit(options(optOld))

    # Start list with the coastline
	coast.bath <- list(wc.world = coast.line.100k.pt)
	line.col <- 1
	poly <- NA
	poly.angle <- NA
    	
	# Add selected cells to list for poly fill-in
	if(by.vessel) {
		if(by.order) {
			for(i in 1:4) {
				for(j in 1:3)
				  coast.bath[[length(coast.bath) + 1]] <- CC(Selected.Cells[Selected.Cells$Vessel == i & Selected.Cells$Order == j,  ])
			}
			line.col <- c(line.col, 2, 2, 2, 3, 3, 3, 4, 4, 4, 6, 6, 6)
			poly <- c(poly, rep(ves.colors[1], 3), rep(ves.colors[2], 3), rep(ves.colors[3], 3), rep(ves.colors[4], 3))
			poly.angle <- c(poly.angle, rep(c(NA, 45, 135), 4))
            # poly.angle <- c(poly.angle, rep(NA, 12))
		}
		else {
			for(i in 1:4) {
				coast.bath[[length(coast.bath) + 1]] <- CC(Selected.Cells[Selected.Cells$Vessel == i,  ])
			}
			line.col <- c(line.col, 2, 3, 4, 6)
			poly <- c(poly, ves.colors)
			poly.angle <- c(poly.angle, rep(NA, 4))
		}
	}
	else {
		coast.bath[[length(coast.bath) + 1]] <- CC(Selected.Cells)
		line.col <- c(line.col, 2)
		poly <- c(poly, T)
		poly.angle <- c(poly.angle, NA)
	}
    
    # This bathymetry (nw.cont) was imported from Splus since R's contour() function does not have a 'save' argument. (nw.cont is loaded at the top of the main script.)
	# Create contours and add to list  
	    # postscript("nul")
        # usa()
	    # nw.cont <- contour(unique(PacCoast.Grid[, 1]),  - unique(PacCoast.Grid[, 2]), matrix(PacCoast.Grid[, 3], nrow = 1216), levels =  - c(100, 300, 500, 700), save = TRUE)
        # dev.off()
    	
	for(i in 1:length(nw.cont))
		coast.bath[[length(coast.bath) + 1]] <- cbind(long = nw.cont[[i]]$x, lat =  - nw.cont[[i]]$y)
	line.col <- c(line.col, rep(8, 4))
	poly <- c(poly, rep(NA, 4))
	poly.angle <- c(poly.angle, rep(NA, 4))
	
    
	# Add base grid	
	coast.bath[[length(coast.bath) + 1]] <- CC(Grid)
	line.col <- c(line.col, 5)
	poly <- c(poly, NA)
	poly.angle <- c(poly.angle, NA)
	
	
	if(new.graph)
		# graphsheet(8.5 * Size.Graph, 11 * Size.Graph, color.scheme = "topographical", image.color.scheme = "topographical")
        dev.new(8.5 * Size.Graph, 11 * Size.Graph)
	
	if(is.null(longrange))
		imap(coast.bath, col = line.col, poly = poly, poly.angle = poly.angle, z = zoom)
	else 
        imap(coast.bath, longrange = longrange, latrange = latrange, col = line.col, poly = poly, poly.angle = poly.angle, z = zoom)
	
	
	if(cell.labels)
		text(Selected.Cells$Long, Selected.Cells$Lat, as.character(Selected.Cells$Cent.ID), cex = cex.label, crt = 45)
	if(cell.labels.all)
		text(Grid$Long, Grid$Lat, as.character(Grid$Cent.ID), cex = cex.label, crt = 45)
	
	if(geo.labels) {
		#   *** Border *** 
		lines(US.Can.Border, col = 15)
		
		#  *** Labels States and Geographical Features *** 
		text(c(-121, -121), c(46.9, 44.5), c("WA", "OR"), cex = 1)
		text(c(-124.71, -122.35, -124.3), c(48.378, 46.325, 42.84), c("Cape Flattery", "Columbia River", "Cape Blanco"), adj = 0, cex = 0.5, font = 2)
		lines(c(-124.1376, -122.1), c(42, 42))	#OR/CA State line
		text(-121, 39.5, "CA", cex = 1)
		text(c(-124.23, -122.11), c(40.45, 37.88), c("Cape Mendocino", "San Francisco\n     Bay"), adj = 0, cex = 0.5, font = 2)
		lines(c(-124.1376, -120.4), c(42, 42))
		
		#  *** City Labels *** 
		pt.label(-123.83472, 46.18975, "Astoria")
		pt.label(-124.05875, 44.63312, "Newport")
		pt.label(-124.22017, 43.36817, "Coos Bay")
		pt.label(-124.28964, 42.0557, "Brookings", y.offset = 0.08)
		pt.label(-124.21351, 41.7632, "Crescent City")
		pt.label(-124.15956, 40.80324, "Eureka")
		pt.label(-121.90472, 36.61076, "Monterey")
		pt.label(-120.85028, 35.36859, "Morro Bay")
	}
	invisible()
}

