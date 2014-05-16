bigdata_flightproj
==================

For files relating to the second group project in ST599 - Computing and Analysis of Big Data

### Final Products
*The Plight of Flight.pdf:* Deliverable Report

*Project2Pres.pdf:* Presentation

*README.md:* Readme / File Index

### Current Files
#### Code

*Code/edwardetFlightCode.R:**: Population code, along with some aggregation code (Dependent on kitadahSampling.R output!)

*kitadahFlightCode.R:* Precursor to sampling code; creates necessary files

*kitadahSampling.R:* Sampling code,along with some aggregation code (Dependent on edwardetFlightCode.R output!)

*AB_bias.R:* Bias histogram and visualization

*AB_popvis.R:* Final population/sample visualizations

#### Data

*Data/aggregate.csv:* Collection of population means/deviations and sample means/errors

*Data/mean_bias.csv:* Mean of the bias between sample and population means over 25 years

*Data/medianOutput.csv:* Median change in mean for each airline, along with an IQR for the .25th and .75th quantiles

*Data/popMean.csv:* Initial population mean data; contained in aggregate.csv, but still dependency for some code

*Data/popSd.csv:* Initial population standard deviation; contained in aggregate.csv, but still dependency for some code

*all_strat.csv:* Sample means/errors; contained in aggregate.csv, but still dependency for some code

*(carriercode).csv:* Outgoing flights by airport and year for (carriercode), used in creating (carriercode)_size.csv

*(carriercode)_size.csv:* Sorted airports into bins, small/medium/large; used in creating (carriercode)_strat.csv

*(carriercode)_strat.csv:* Sample means/errors for (carriercode); most data in all_strat.csv, but still dependency for some code

*nflights_all.csv:* Outgoing flights by airport and year combined, still dependency for some code

#### Documents
*Notes/:* Notes for all meetings, along with general group information and updated contract

*The Plight of Flight.docx:* .docx for project deliverable

*Project2Pres.tex:* .tex for project presentaiton

#### Graphs
*Graphs/meanDiff_(carriercode).pdf:* Mean difference from year to year for (carriercode), used in deliverable

*airtraffic.pdf:* Air traffic map, used in presentation

*Bias Histogram.pdf:* Histogram of our sampling method's bias, used in presentation

*summarySamp.pdf:* Sample mean difference from year to year for the airlines with 25 years active data, used in presentation

*noline.pdf:* Sample means and IQRs, with population means plotted in same color/axis for visualization, used in presentation

*popMean.pdf:* Population mean by year for the airlines with 25 years active data, used in presentation

*popPlot.pdf:* Population mean difference from year to year for the airlines with 25 years active data, used in presentation

*stratSampWithSE.pdf:* Sample means and IQRs, no population means, used in presentation

### Outdated Files
#### Code
*Code/allMeansGraph.R:* A brief visualization method we didn't end up using

*flight_pop.R:* Andrew's original exploratory code

*AB_flightcode.R:* Updated version of Andrew's flight code

#### Data
*carrierdelaySummaryTable.csv:* csv for carrierdelay (didn't use metric)

*carrierdelaySummaryTable.xls:* Formatted display table for carrierdelay (didn't use metric)

*meanDepDelaySummaryTable.csv:* csv for departure delay metric (outdated metric)

*meanDepDelaySummaryTable.xls:* Formatted display table for departure delay metric (outdated metric)

*meanTotalDelaySummaryTable.csv:* csv for "total" delay metric (outdated metric)

*meanTotalDelaySummaryTable.xls:* Formatted display table for "total" delay metric (outdated metric)

*delaySummaryTable.csv:* csv for secondary delay metric (outdated)

*delaySummaryTable.xls:* Formatted display table for secondary delay metric (personal display only)

*meanArrDelaySummaryTable.csv:* (outdated, better code written)

*meanArrDelaySummaryTable.xls:* Updated version of delaySummaryTable.xls (personal display only)

*nflights_all_withHeat.xls:* Heatmap of number of flights by carrier (early, personal use, outdated)

*ncarriers.csv:* Flights by carrier by year (early, outdated)

#### Documents
*TeamExpectations2.pdf:* Heather's example of Team Expectations

*TeamExpectations2.tex:* Raw TeX for above file
	

#### Graphs
*Graphs/10MeansPop.pdf:* overlaid means of all airlines with 10+ years data (didn't use)

*Graphs/25MeansPop.pdf:* overlaid means of all airlines with 25 years data (didn't use)

*bias.pdf:* Bias histogram (old version)

*coverage.pdf:* Sample means + IQR vs. population means (old version)

*Sample means.pdf*: Different method of displayin ghte information in stratSampeWithSE.pdf
