
#  Survey.allocation.2004
#                   North.Pt.Conception South.Pt.Conception 
#   "30+ thru  100"                  32                   5
#  "100+ thru  300 "                 24                   9
#   "300+ thru 700"                  24                   6
#  
#   
#  Survey.allocation.2004 <- read.clip()

Select.Cells.30.700.f <- 
function(Number.of.Tows = 752, Strata.Percent = Survey.allocation.2004, Grid = Grid.G4.Cent.ID.Dep, Delta = 0.1)
{
	Strata.Num <- 4 * round(((Number.of.Tows * Strata.Percent)/100)/4 + Delta)
	print(Strata.Num)
	cat("Total number of tows: ", sum(Strata.Num), "\n\n")
	""
	AREAS <- sort(unique(Grid$Lat.34.5))
	print(AREAS)
	""
	out <- NULL
	""
	for(i in 1:2) {
		tmp <- Grid[Grid$Depth.Range == "30-100" & Grid$Lat.34.5 == AREAS[i],  ]
		out <- rbind(out, renum(tmp[sample(1:nrow(tmp), Strata.Num[1, i]),  ]))
		""
		tmp <- Grid[Grid$Depth.Range == "100-300" & Grid$Lat.34.5 == AREAS[i],  ]
		out <- rbind(out, renum(tmp[sample(1:nrow(tmp), Strata.Num[2, i]),  ]))
		""
		tmp <- Grid[Grid$Depth.Range == "300-700" & Grid$Lat.34.5 == AREAS[i],  ]
		out <- rbind(out, renum(tmp[sample(1:nrow(tmp), Strata.Num[3, i]),  ]))
	}
	out$Vessel <- rep(1:4, len = Number.of.Tows)
	""
	out
}
