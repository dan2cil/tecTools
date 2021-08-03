#'	Composition of Thorntons Continental
#'
#'	A dataset containing the number of each praline present in the box
#'	the column name indicate the type of praline present in the box
#'	in the first row is inserted the numebr of that type present
#'	in the 'source.rda' file there are the weight of a sample of the
#'	same praline
#'
#'	@format A data frame 1 rows and 10 variables:
#'	\describe{
#'			\item{Alpini}{numbers of Alpine praline present in the box}
#'			\item{VanillaT}{numbers of Vanilla Truffle praline present in the box}
#'			\item{Sicilian}{numbers of Sicilian Lemon praline present in the box}
#'			\item{Apricot}{numbers of Apricot Danish praline present in the box}
#'			\item{Coffee}{numbers of Coffee praline present in the box}
#'			\item{Hslice}{numbers of Hazelnut Slice Lemon praline present in the box}
#'			\item{Milano}{numbers of Milano praline present in the box}
#'			\item{Mix}{numbers of Mix praline present in the box}
#'			\item{Orange}{numbers of Orange praline present in the box}
#'			\item{Salted}{numbers of Salted Caramel praline present in the box}
#'	}
#'
#'	@source	\url{http://www.diamondse.info/}
"composition"

#'	Weight of single praline in Thorntons Continental
#'
#'	A dataset containing the weight of each praline present in the box
#'	the column name indicate the type of praline, in the following row
#'	there is the weigth of a sample of 100 single praline
#'
#'	@format A data frame with 100 rows and 10 variables:
#'	\describe{
#'			\item{Alpini}{numbers of Alpine praline present in the box}
#'			\item{VanillaT}{numbers of Vanilla Truffle praline present in the box}
#'			\item{Sicilian}{numbers of Sicilian Lemon praline present in the box}
#'			\item{Apricot}{numbers of Apricot Danish praline present in the box}
#'			\item{Coffee}{numbers of Coffee praline present in the box}
#'			\item{Hslice}{numbers of Hazelnut Slice Lemon praline present in the box}
#'			\item{Milano}{numbers of Milano praline present in the box}
#'			\item{Mix}{numbers of Mix praline present in the box}
#'			\item{Orange}{numbers of Orange praline present in the box}
#'			\item{Salted}{numbers of Salted Caramel praline present in the box}
#'	}
#'
#'	@source	\url{http://www.diamondse.info/}
"source"

#'	Weight of single Rocher praline
#'
#'	A dataset containing the weight of the single Rocher praline
#'	produced the 11 November 2016 on Linea 1 of Alba
#'	The date of the 11th of November was the data where to Linea 1
#'	has had the minimun weigth of the year 12.695g, sd=0.287
#'
#'	@format A data frame with 939 rows and 1 variable:
#'	\describe{
#'			\item{RO}{weight of 939 Rocher}
#'	}
#'
#'	@source	\url{http://www.diamondse.info/}
"L1_min"

#'	Weight of single Rocher praline
#'
#'	A dataset containing the weight of the single Rocher praline
#'	produced the 23 September 2016 on Linea 4 of Alba
#'	The date of the 23rd of September was the data where to Linea 4
#'	has had the minimun weigth of the year 12.705g, sd=0.295
#'
#'	@format A data frame with 479 rows and 1 variable:
#'	\describe{
#'			\item{RO}{weight of 479 Rocher}
#'	}
#'
#'	@source	\url{http://www.diamondse.info/}
"L4_min"

#'	Weight of single Rocher praline
#'
#'	A dataset containing the weight of the single Rocher praline
#'	produced the 11 May 2016 on Linea 1 of Alba
#'	The date of the 11th of May was the data where to Linea 1
#'	has had the maximum weigth of the year 12.726g, sd=0.266
#'
#'	@format A data frame with 450 rows and 1 variable:
#'	\describe{
#'			\item{RO}{weight of 450 Rocher}
#'	}
#'
#'	@source	\url{http://www.diamondse.info/}
"L1_max"

#'	Weight of single Rocher praline
#'
#'	A dataset containing the weight of the single Rocher praline
#'	produced the 22 October 2016 on Linea 4 of Alba
#'	The date of the 22nd of October was the data where to Linea 4
#'	has had the maximum weigth of the year 12.840g, sd=0.292
#'
#'	@format A data frame with 310 rows and 1 variable:
#'	\describe{
#'			\item{RO}{weight of 310 Rocher}
#'	}
#'
#'	@source	\url{http://www.diamondse.info/}
"L4_max"

#'	Collapse Test on different chocolate
#'
#'	A dataset containing the results of a series of Collapse Test analysis
#'	on three different chocolate:
#'	Ferrero latte 2001
#'	Barry   Thorntons standard chocolate (containing 7% of crumb)
#'	Barry   Thorntons eritage chocolate (containing 24% of crumb)
#'
#'	@format A data frame with 240 rows and 3 variable:
#'	\describe{
#'			\item{Test}{factor, names of the three differen chocolate}
#'			\item{Temperature}{numeric, temperature of analysis}
#'			\item{Gap}{numeric, height of the sample during the analysis}
#'	}
"choc_ctest"

#'	Temperindex on Stadtallendorf test
#'
#'	A dataset containing the results of fat bloom analysis during 6 months
#'	with 2 different product cluster:
#'	TI5   Rocher produced with 2nd enrobing tempered with TI=5
#'	TI7   Rocher produced with 2nd enrobing tempered with TI=7
#'
#'	@format A data frame with 12 rows and 8 variable:
#'	\describe{
#'			\item{prodotto}{factor, name of ptest TI5=product with TI=5, TI7= product with TI=7}
#'			\item{mese}{factor, months of analysis after production}
#'			\item{L1}{numeric, number of product with level 1 of fat bloom}
#'			\item{L2}{numeric, number of product with level 2 of fat bloom}
#'			\item{L3}{numeric, number of product with level 3 of fat bloom}
#'			\item{L4}{numeric, number of product with level 4 of fat bloom}
#'			\item{L5}{numeric, number of product with level 5 of fat bloom}
#'			\item{L6}{numeric, number of product with level 6 of fat bloom}
#'	}
"RO_OHG"

#'	Temperindex analysis on stored Rocher of all plant
#'
#'	A dataset containing the results of fat bloom analysis during 6 months
#'	with Ferrero Rocher of all production plant
#'	Ferrero latte 2001
#'
#'	@format A data frame with 234 rows and 15 variable:
#'	\describe{
#'			\item{plant}{factor, name of ptest TI5=product with TI=5, TI7= product with TI=7}
#'			\item{lotto}{factor, }
#'			\item{data-produzione}{factor, }
#'			\item{data_reale}{factor, }
#'			\item{eta_reale}{int, }
#'			\item{mese}{factor, months of analysis after production}
#'			\item{confezione}{int, }
#'			\item{linea}{factor, }
#'			\item{L1}{numeric, number of product with level 1 of fat bloom}
#'			\item{L2}{numeric, number of product with level 2 of fat bloom}
#'			\item{L3}{numeric, number of product with level 3 of fat bloom}
#'			\item{L4}{numeric, number of product with level 4 of fat bloom}
#'			\item{L5}{numeric, number of product with level 5 of fat bloom}
#'			\item{L6}{numeric, number of product with level 6 of fat bloom}
#'	}
"RO_ALL"

#'	Temperindex analysis on stored Rocher of all plant (de-pivot data)
#'
#'	A dataset containing the results of fat bloom analysis during 6 months
#'	with Ferrero Rocher of all production plant
#'	Ferrero latte 2001
#'
#'	@format A data frame with 3694 rows and 9 variable:
#'	\describe{
#'			\item{plant}{factor, name of ptest TI5=product with TI=5, TI7= product with TI=7}
#'			\item{lotto}{factor, }
#'			\item{data-produzione}{factor, }
#'			\item{data_reale}{factor, }
#'			\item{eta_reale}{int, }
#'			\item{mese}{factor, months of analysis after production}
#'			\item{confezione}{int, }
#'			\item{linea}{factor, }
#'			\item{affioramento}{numeric, fat bloom level of the sample (single praline)}
#'	}
"RO_ALL_DE"
