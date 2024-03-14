
Alt.Cells.f <- function(Lat.34.5. = "North", Depth = "30-100", Density = 3, Number.of.Tows = 752, Select.Cells = Cells_Current, 
                               Strata.Grid = Strata.Grid_Current, verbose = FALSE, Delta. = Delta)
{
    
  '  # Note, Delta arg is a "promise already under evaluation: recursive default argument reference or earlier problems?" issue. Another solution is: Delta = get("Delta", envir = globalenv())  '

    table.to.matrix <- function(table, row.name = "Rows", col.name = "Columns", data.name = "Data") {
        out <- cbind(expand.grid(list(dimnames(table)[[1]], dimnames(table)[[2]])), c(unlist(table)))
        dimnames(out)[[2]] <- c(row.name, col.name, data.name)
        out
    }
    
    nearest.dist.gcircle.f <- function(vec, mat, verbose = FALSE) {
        # assign("vec", vec, pos = 1)
        Distance.nm <- gdist(rep(vec[1], nrow(mat)), rep(vec[2], nrow(mat)), mat[, 2], mat[, 3], verbose = verbose)
        sort.f(cbind(mat, Distance.nm), 4)
    }

    Strata.Grid <- Strata.Grid[Strata.Grid$Lat.34.5 == Lat.34.5. & Strata.Grid$Depth.Range == Depth,  ]
    Strata.Cells <- Select.Cells[Select.Cells$Lat.34.5 == Lat.34.5. & Select.Cells$Depth.Range == Depth,  ]
    
    if(verbose) {
        cat("\n\nStrata Grid  for the", Lat.34.5., "area and", Depth, "depth before the current strata primary cells are removed:\n")
        print(dim(Strata.Grid))
        print(Strata.Grid[1:3,  ])
        
        cat("\n\nStrata cells for the", Lat.34.5., "area and", Depth, "depth strata:\n" )
        print(dim(Strata.Cells))
        print(Strata.Cells[1:3,  ])
    }

    # Remove the primary cells of the current strata from pool of cells to that the  alternative cells can be picked from
    Strata.Grid <- Strata.Grid[!(Strata.Grid$Cent.ID %in% Strata.Cells$Cent.ID),  ]
    if(verbose) {
        cat("\n\nStrata Grid after the strata primary cells are removed:\n")
        print(dim(Strata.Grid))
        print(Strata.Grid[1:3,  ])
    }

    Strata.Cells$Distance.nm <- rep(0, nrow(Strata.Cells))
    # print(Strata.Cells[1:3,  ]) 
    Delta <- Delta
    Strata.Num <- 4 * round(((Number.of.Tows * Survey.allocation.2004)/100)/4 + Delta.)
    Strata.Num.Matrix <- table.to.matrix(Strata.Num, "Depth.Range", "Lat.34.5", "Selected.Num")
    
    levels(Strata.Num.Matrix$Depth.Range) <- c("30-100", "100-300", "300-700")
    levels(Strata.Num.Matrix$Lat.34.5) <- c("North", "South")

    Strata.Num <- Strata.Num.Matrix[Strata.Num.Matrix$Lat.34.5 == Lat.34.5. & Strata.Num.Matrix$Depth.Range == Depth, "Selected.Num"]
    Strata.Samp <- Strata.Grid[sample(1:nrow(Strata.Grid), Density * Strata.Num), c("Cent.ID", "Long", "Lat")]
    # print(renum(Strata.Samp)) 
    
    if(verbose) 
        cat("\n\nBased on the current density factor of", Density, "the size of the remaining selection set of randomly picked cells within the current strata is", nrow(Strata.Samp), "\n\n")
    else 
        cat("\n\n")
        
    out <- NULL
    N <- nrow(Strata.Cells)
    for(i in 1:N) {
        bar(i, N)
        near.1 <- nearest.dist.gcircle.f(as.matrix(Strata.Cells[i, c("Long", "Lat")]), Strata.Samp)[1,  , drop = F]
        Strata.Samp <- Strata.Samp[!(Strata.Samp$Cent.ID %in% near.1$Cent.ID),  ]
        out <- rbind(out, cbind(Primary = rep(Strata.Cells[i, "Cent.ID"], 2), rbind(Strata.Cells[i, c("Cent.ID", "Long", "Lat", "Distance.nm")], near.1), Order = c(1, 2)))
    }
    if(verbose)
        cat("\n\nAfter the secondary cells are removed, the size of the remaining selection set is:", nrow(Strata.Samp), "\n\n")
       
    N <- nrow(Strata.Cells)
    for(i in 1:N) {
        bar(i, N)
        near.1 <- nearest.dist.gcircle.f(as.matrix(Strata.Cells[i, c("Long", "Lat")]), Strata.Samp)[1,  , drop = F]
        Strata.Samp <- Strata.Samp[!(Strata.Samp$Cent.ID %in% near.1$Cent.ID),  ]
        out <- rbind(out, cbind(Primary = rep(Strata.Cells[i, "Cent.ID"], 2), rbind(Strata.Cells[i, c("Cent.ID", "Long", "Lat", "Distance.nm")], near.1), Order = c(1, 3))[2,  , drop = F])
    }
    out <- sort.f(out)
    if(verbose) {
        cat("\n\nAfter the tertiary cells are removed, the size of the selection set is", nrow(Strata.Samp), "\n")
        cat("\n\nFirst 20 rows of the result:\n")
        print(out[1:20,  ])
    }
    
    out <- renum(out)
    cat("\nFirst Alt: percent of positive distance above 2.25 nm is", 100 * (sum(out$Distance.nm[out$Order == 2] > 2.25 & out$Distance.nm[out$Order == 2] > 0)/sum(out$Distance.nm[out$Order == 2] > 0)), "\n")
    cat("Second Alt: percent of positive distance above 2.25 nm is", 100 * (sum(out$Distance.nm[out$Order == 3] > 2.25 & out$Distance.nm[out$Order == 3] > 0)/sum(out$Distance.nm[out$Order == 3] > 0)), "\n\n")
    
    out.np <- out[out$Order != 1,  ]
    dist.2 <- NULL
    for(i in seq(1, nrow(out.np), 2)) {
        dist.2 <- c(dist.2, gdist(out.np$Long[i], out.np$Lat[i], out.np$Long[i + 1], out.np$Lat[i + 1]))
    }
    if(verbose) {
        cat("\n\nTen greatest distances of the alternatives from the primary (nm):\n")
        print(rev(sort(out$Distance.nm))[1:10])
        cat("\n\nTen greatest distances that the alternatives are from each other (nm):\n")
        print(rev(sort(dist.2))[1:10])
        dev.new()
        par(mfrow = c(2,1))
        plot(factor(out$Order), out$Distance.nm, ylab = "Distance from Primary (nm)", xlab = "Order Number")
        abline(h = 2.25)
        plot(dist.2, ylab = "Distance from Secondary to Tertiary (nm)", xlab = "Primary Cell Number")
       
    } 
    
    invisible(out)
}

