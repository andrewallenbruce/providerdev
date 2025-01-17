accrual_key <- c("
Term 	                ISO_8601
Decennial 	          R/P10Y
Quadrennial 	        R/P4Y
Annual 	              R/P1Y
Bimonthly 	          R/P2M or R/P0.5M
Semiweekly 	          R/P3.5D
Daily 	              R/P1D
Biweekly 	            R/P2W or R/P0.5W
Semiannual 	          R/P6M
Biennial 	            R/P2Y
Triennial 	          R/P3Y
Three times a week 	  R/P0.33W
Three times a month 	R/P0.33M
Continuously updated 	R/PT1S
Monthly             	R/P1M
Quarterly 	          R/P3M
Semimonthly 	        R/P0.5M
Three times a year 	  R/P4M
Weekly 	              R/P1W
Hourly 	              R/PT1H
")

accrual_key <- gsub(" \t", ", ", accrual_key)
accrual_key <- gsub("^\n|\n$", "", accrual_key)
accrual_key <- strsplit(accrual_key, "\n")[[1]]
accrual_key <- strsplit(accrual_key, ", ")
accrual_key <- trimws(unlist(accrual_key))
accrual_key <- accrual_key[c(-1, -2)]

accrual_key <- data.frame(
  term = accrual_key[c(TRUE, FALSE)],
  iso_8601 = accrual_key[c(FALSE, TRUE)])

accrual_key
