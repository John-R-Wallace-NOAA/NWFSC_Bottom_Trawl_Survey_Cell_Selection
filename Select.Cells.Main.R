        
        setwd("W:/ALL_USR/JRW/FRAM.Station.Pick/2025") # Change path as needed
        
        Year <- 2025
        Number.of.Tows <- 752
        Number.of.Vessels <- 4  #  Mostly fewer vessels are dealt with by assuming 4 vessels and then deleting those that are unwanted. Except where 'Number.of.Vessels' is used below. It's all random and independent, so deleting any number of vessels is not an issue.
        Delta <- 0.1 # This delta is needed for bad luck rounding issues with multiple numbers near x.5 - change as needed.
        RNGkind("Super-Duper") # R's Super-Duper is not identical to the Super-Duper (32-bit) one in S-Plus, see the R help for 'RNGkind'.
        
        load('Survey.allocation.2004.RData')
        load('coast.line.100k.pt.RData')
        load('coast.line.100k.pt.RData')
        load('Grid.G14.Cent.ID.Dep.RData')
        load('Grid.G4.RData')
        load('nw.cont.RData') 
        load('US.Can.Border.RData')

        source('Grid.Map.R')   
        source("Alt.Cells.f.R")
        
        if (!any(installed.packages()[, 1] %in% "JRWToolBox")) {
            if (!any(installed.packages()[, 1] %in% "remotes"))  install.packages('remotes')  
            remotes::install_github("John-R-Wallace-NOAA/JRWToolBox")
        }     
        
        library(JRWToolBox)
        lib("John-R-Wallace-NOAA/Imap")
        lib(MASS)
        lib(lattice)
        lib(openxlsx) #  openxlsx::read.xlsx() needs '.xlsx', it does not handle '.xls'
      
        # Change the format from '.xls" to '.xlsx' in Excel, if needed.  The 2025 file name from the GIS specialist (Curt Whitmire) was: SelectionSet2025_forWallace_v20241221.0.xls
        Grid.Cent.ID.Dep <- openxlsx::read.xlsx(paste0('SelectionSet', Year, '_wCCA_forWallace.xlsx')) 
               
        Grid.Cent.ID.Dep <- Grid.Cent.ID.Dep[, c(2, 4, 3, 5)]
        names(Grid.Cent.ID.Dep) <- c("Cent.ID", "Depth.Range", "Lat.34.5", "Hectares")
		headTail(Grid.Cent.ID.Dep) 
        
        # Table(Grid.G4.Pool.Curt$Lat.34.5, Grid.G4.Pool.Curt$Depth.Range)
        Table(Grid.Cent.ID.Dep$Lat.34.5, Grid.Cent.ID.Dep$Depth.Range)
        
        Grid.Cent.ID.Dep$Lat <- trunc(trunc(Grid.Cent.ID.Dep$Cent.ID/10000)/100) + dec(trunc(Grid.Cent.ID.Dep$Cent.ID/10000)/100)/0.6
        Grid.Cent.ID.Dep$Long <-  - (trunc((10000 * round(dec(Grid.Cent.ID.Dep$Cent.ID/10000), 9))/100) + dec((10000 * dec(Grid.Cent.ID.Dep$Cent.ID/10000))/100)/0.6 + 100)
        
        # Seed set to the current year so that the seed will always be different for each survey year
        set.seed(Year)
        
        # Total number of tows by strata goal:
        (Strata.Num <- 4 * round(((Number.of.Tows * Survey.allocation.2004)/100)/4 + Delta)) 
        
        # Total number of tows check: 
        sum(Strata.Num)
        
        (AREAS <- sort(unique(Grid.Cent.ID.Dep$Lat.34.5)))
        (DEPTHS <- sort(unique(Grid.Cent.ID.Dep$Depth.Range))[c(2,1,3)])
        
        Primary.Cells <- NULL
        for(i in 1:2) {
          for(j in 1:3) {
               tmp <- Grid.Cent.ID.Dep[Grid.Cent.ID.Dep$Depth.Range == DEPTHS[j] & Grid.Cent.ID.Dep$Lat.34.5 == AREAS[i],  ]
               Primary.Cells <- rbind(Primary.Cells, renum(tmp[sample(1:nrow(tmp), Strata.Num[j, i]),  ]))
          }  
        }
        Primary.Cells$Vessel <- rep(1:4, len = Number.of.Tows)
        
        # Check Primary Cells
        head(Primary.Cells)
        dim(Primary.Cells)
        Table(Primary.Cells$Depth.Range, Primary.Cells$Lat.34.5)[c(2, 1, 3),  ]
        
        #   Check primary cell percentages against those in Survey.allocation.2004
        100 * round(Table(Primary.Cells$Depth.Range, Primary.Cells$Lat.34.5)[c(2, 1, 3),  ]/752, 4)
        Survey.allocation.2004
        
        # Add grid cell corners and plot primary selection
        Grid.Cent.ID.Dep <- match.f(Grid.Cent.ID.Dep, Grid.G4, "Cent.ID", "Cent.ID", c("SW.LON", "SW.LAT", "NW.LON", "NW.LAT", "NE.LON", "NE.LAT", "SE.LON", "SE.LAT"))
        Primary.Cells <- match.f(Primary.Cells, Grid.G4, "Cent.ID", "Cent.ID", c("SW.LON", "SW.LAT", "NW.LON", "NW.LAT", "NE.LON", "NE.LAT", "SE.LON", "SE.LAT"))
        
        Grid.Map(Primary.Cells, Grid.G14.Cent.ID.Dep)
        
        # Use below for printing
        # Grid.Map(Primary.Cells, Grid.G14.Cent.ID.Dep, geo.labels = TRUE, cell.labels = TRUE, cex.label = 0.15, Size = 5, zoom = FALSE)
        
        
        # Alternative Cell Selection
        
        # NOT using the current year's seed and trying multiple times - since it's random, change the value of the 'Density' factor for a First Alt. percent of  
        #          'positive distance above 2.25 (km)' of around 20-30%, and a Second Alt. 'percent of positive distance above 2.25 (km)' of around 55-75%.
        # Higher values of 'Density' give a higher percentage of alternative cells that are adjacent to the primary cell, and hence a lower percentage of alternative 
		#          cells that are not adjacent to the primary cell (i.e. positive distances above 2.25 km).
        # Higher value of  Density =>  lower percentage of alternative cells that are not adjacent to the primary cell.  
        # Lower value of  Density  => higher percentage of alternative cells that are not adjacent to the primary cell.   
        
        # Using the argument defaults in Alt.Cells.f() function to save verbiage below
        Cells_Current <- Primary.Cells
        Strata.Grid_Current <- Grid.Cent.ID.Dep
        
        Den.list <- list(N.30.100 = 5.2, N.100.300 = 4.5, N.300.700 = 5.8, S.30.100 = 6.3, S.100.300 = 5.4, S.300.700 = 18.0)
               
        Verb <- c(TRUE, FALSE)[1]  #  Use verbose = TRUE to start, so that numbers can be checked. Then switch to FALSE to check multiple random runs.
        for(i in if(Verb) 1 else 1:4) {    # Un-comment one strata at a time adjusting the Density num in 'Den.list' above as needed.
           set.seed(i + 2)
           # North
           test <- Alt.Cells.f(Lat.34.5. = "North", Depth = "30-100", Density = Den.list$N.30.100, verb = Verb)   
           # test <- Alt.Cells.f(Lat.34.5. = "North", Depth = "100-300", Density = Den.list$N.100.300, verb = Verb)
           # test <- Alt.Cells.f(Lat.34.5. = "North", Depth = "300-700", Density = Den.list$N.300.700, verb = Verb)
           
           # South
           # test <- Alt.Cells.f(Lat.34.5. = "South", Depth = "30-100", Density = Den.list$S.30.100, verb = Verb)
           # test <- Alt.Cells.f(Lat.34.5. = "South", Depth = "100-300", Density = Den.list$S.100.300, verb = Verb)  
           # test <- Alt.Cells.f(Lat.34.5. = "South", Depth = "300-700", Density = Den.list$S.300.700, verb = Verb)
        }
        
        # Run the final alternative cell selection with the densities found above and the random seed set to the current year.
        # Once the density factors are set above, reasonable misses from the ranges of 20-30% and 55-75% are ignored.  (Totally unreasonable misses should not happen and would be a reflection that something is wrong.)
        
        Verb <- c(TRUE, FALSE)[2] # Verbose or not
        # North
        set.seed(Year)
        Primary.Alt.Cells <- Alt.Cells.f(Lat.34.5. = "North", Depth = "30-100", Density = Den.list$N.30.100, verb = Verb)
        Primary.Alt.Cells <- rbind(Primary.Alt.Cells, Alt.Cells.f(Lat.34.5. = "North", Depth = "100-300", Density = Den.list$N.100.300, verb = Verb))
        Primary.Alt.Cells <- rbind(Primary.Alt.Cells, Alt.Cells.f(Lat.34.5. = "North", Depth = "300-700", Density = Den.list$N.300.700, verb = Verb))
        
        # South
        Primary.Alt.Cells <- rbind(Primary.Alt.Cells, Alt.Cells.f(Lat.34.5. = "South", Depth = "30-100", Density = Den.list$S.30.100, verb = Verb)) 
        Primary.Alt.Cells <- rbind(Primary.Alt.Cells, Alt.Cells.f(Lat.34.5. = "South", Depth = "100-300", Density = Den.list$S.100.300, verb = Verb)) 
        Primary.Alt.Cells <- rbind(Primary.Alt.Cells, Alt.Cells.f(Lat.34.5. = "South", Depth = "300-700", Density = Den.list$S.300.700, verb = Verb))
        
        Primary.Alt.Cells <- match.f(Primary.Alt.Cells, Primary.Cells, "Primary", "Cent.ID", "Vessel")
                  
        Primary.Alt.Cells <- sort.f(Primary.Alt.Cells, c('Vessel', 'Primary', 'Distance.nm'))  # Sort needs to happen before tow-within-vessel is created below
          
        # Note that R has a base::gl() function, but only returns factors.  I wrote gl.f() in S-plus before R was created.  [gl.f() and gl() were modeled after the GLIM 'gl' function.]  
        Primary.Alt.Cells$Tow.within.Vessel <- rep(gl.f(Number.of.Tows/4, 3), Number.of.Vessels)   
        Primary.Alt.Cells[560:570, ]  # Check correctness of tow within vessel (3 * 188 = 564, hence the first vessel ends between 560 to 570)
        
        # Map primary and alts 
        Primary.Alt.Cells.Grid.Cent.ID.Dep.Corners <- match.f(Primary.Alt.Cells, Grid.G4, "Cent.ID", "Cent.ID", c("Depth.Range", "SW.LON", "SW.LAT", "NW.LON", "NW.LAT", "NE.LON", "NE.LAT", "SE.LON", "SE.LAT"))
        Grid.Map(Primary.Alt.Cells.Grid.Cent.ID.Dep.Corners, Grid.G14.Cent.ID.Dep, by.order = TRUE) # Slashes for the alternatives not working correctly in the Imap package (worked in Splus under old imap functions)
          
               
        # Look to see if the results are kosher
        rev(sort(Primary.Alt.Cells$Distance.nm))[1:20]
        
        Primary.Alt.Cells[Primary.Alt.Cells$Distance.nm > 30, ]
        Primary.Alt.Cells[Primary.Alt.Cells$Distance.nm > 20, ]
        Primary.Alt.Cells[Primary.Alt.Cells$Distance.nm > 10, ]
        
        
        browsePlot('print(histogram(~Distance.nm | factor(Order), data = Primary.Alt.Cells[Primary.Alt.Cells$Order %in% 2:3,]))')     # All distances for Order '1' are zero
		       
		browsePlot('
           par(mfrow = c(2,1))
           MASS::truehist(Primary.Alt.Cells$Distance.nm[Primary.Alt.Cells$Order %in% 2], xlim = c(0, 30), xlab = "Distance Between Primary and 1st Alternative (nm)")
           MASS::truehist(Primary.Alt.Cells$Distance.nm[Primary.Alt.Cells$Order %in% 3], xlim = c(0, 30), xlab = "Distance Between Primary and 2nd Alternative (nm)")',
        file = 'Distance Between Primary and Alternatives.png')
        
        # Output final results without cell corners (the Excel file is emailed back to Curt and also to the lead of the Survey Team, Aimee Keller)
        (Primary.Alt.Cells <- Primary.Alt.Cells[, c('Vessel', 'Tow.within.Vessel', 'Primary', 'Cent.ID', 'Distance.nm', 'Order')])[1:10, ] # Reorder columns
        Primary.Alt.Cells[560:570, ]  # Check correctness of tow within vessel (3 * 188 = 564)
        dim(Primary.Alt.Cells) # 564 * 4 = 752 * 3 = 2,256 
        openxlsx::write.xlsx(Primary.Alt.Cells, paste0('Primary.Alt.Cells.', Year, '.xlsx'))
        
        # Output results with cell corners  
        Primary.Alt.Cells.Grid.Cent.ID.Dep.Corners <- Primary.Alt.Cells.Grid.Cent.ID.Dep.Corners[, c('Vessel', 'Tow.within.Vessel', 'Primary', 'Cent.ID', 
                'Long', 'Lat', 'Distance.nm', 'Order', 'Depth.Range', 'SW.LON', 'SW.LAT', 'NW.LON', 'NW.LAT', 'NE.LON', 'NE.LAT', 'SE.LON', 'SE.LAT')] # Reorder columns
        openxlsx::write.xlsx(Primary.Alt.Cells.Grid.Cent.ID.Dep.Corners, "Primary.Alt.Cells.Grid.Cent.ID.Dep.Corners.xlsx")
        
        
        