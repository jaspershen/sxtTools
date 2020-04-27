# Version 0.99.0
--------------
* First release version
* Fix bugs.

# Version 0.99.01
--------------
* Change some compounds whose formulas contain "X" or "R" in KEGG database (20170811).

# Version 0.99.02
--------------
* Change analysisReport function.
* Change the output files.
* Add use.default.md parameter in metABM function.
* Fix some bugs.

# Version 0.99.04
--------------
* Fix bugs in metModule and metModule2 when there are no mosules with p values less than 0.05.
* Fix bugs in analysisReport when there are no mosules with p values less than 0.05.

# Version 0.99.05
--------------
* New feature: Only use MRN based annotations for dysregulated network analysis.
* New feature: Use annotations of dysregulated peaks for pathway enrichment.
* Combine some functions as one R file.
* Fix bugs in findPathway and findPathway2.
* Fix bugs in tools.

# Version 0.99.06
--------------
* Fix some bugs in singleTransformation.
* Fix a bugs in analysisReport.
* Fix bugs in some functions which select some columns from a data frame or matrix.
* Fix bugs in heat map when module or pathway sample have less than two variables.

# Version 0.99.07
--------------
* Fix bug in metAnnotation: One peak should not be used to annotate itself.
* Fix bugs in ppm calculation: if m/z less than 400 Da, the difference of m/z should be divided by 400.
* New feature: Output MS/MS matching spectra between seed and neighbor.
* New feature: In MetDNA: If ms2.match.annotation.result.csv exist, set ms2.annotation as FALSE. If MRN.annotation.result.csv exist, set mrn.annotation as FALSE.

# Version 0.99.10
--------------
* Fix bug in ms2Annotation: Change the default molecular descriptors for RT prediction in RP model.
* Close the dysregulated pathway analysis in metPathway.
* Rearrange the output results of dysregulated network analysis.
* Combine metModule and metModule2 function. So positive, negative or both can be completed in one function.
* Fix the bugs when p value is NA and fold change is NA or infinite.
* Fix a bug in groupRT function.
* Fix a bug in metModule function.

# Version 0.99.11
--------------
* New feature: Use the annotations of dysregulated for pathway enrichment analysis. The users can also provide the markers which you select by your methods. The markers file should be named as "marker.csv" and placed in "POS" or "NEG" folder. The marker only contain two columns, first column is "peak.name" and second column is "KEGG.ID", and if one peak matches multiple annotations, the KEGG IDs should be seperated using semicolon ";".
* Dysregulated network is set FALSE by default.
* Pathway enrichment analysis is set TRUE by default if polarity is "both", and FALSE if polarity is "positive" or "negative".
* New feature: You can give remain.idx in the directory.
* Fix a fetal bug in readAnnotation.
* Fix a bug when msp data is from MetAnalyzer (20171029).
* Fix a bug in pathway quantitative analysis (20171107).
* New feature: metabolite quantitative, use all identification result; pathway quantitative, use the dysregulated metabolite.

# Version 0.99.12
--------------
* New feature: User can use MetDNA function to process positive and negative in one run.
* New feature: The run log is outputed as txt.
* Fix a bug in uniTest.
* Fix a bug in isotopeAnnotation when peak intensity is 0.
* New feature: Add two new arguments. 1. ms2.match.plot, default is TRUE, output MS/MS spectral match plot or not in MS2 match identification. 2. seed.neighbor.match.plot, default is FALSE, output MS/MS spectral match plot between seed and neighbor or not.
* Fix a bug when the sample name starts with a number.
* Fix a bug when the users change the data.csv to do pathway enrichment analysis.
* Fix a bug in analysisReport.
* New feature: The parameters can be direct from MetDNA.paramters.csv using the new argument: parameter (20171220).

# Version 0.99.13
--------------
* A new argument, instrument is added. It can be set as AgilentQTOF, SciexTripleTOF, other QTOF and ThermoOrbitrap.

# Version 0.99.14
--------------
* Fix a bug when there are only one sample in one group.

# Version 0.99.15
--------------
* Fix a bug in analysisGeneration.

# Version 0.99.16
--------------
* Add P-values to Quantitative.pathway.metabolite.result.csv.
* Re-orginize the analysis results.
* Remove the MS2_match_results.

# Version 1.0.0
--------------
* The First open release version.

# Version 1.0.1
--------------
* Fix a bugs: When number of dysregulated peaks is less than 10, give a warning.

# Version 1.0.2
--------------
* Revision for comments of reviewers from Nature methods.
* Pathway overview, change FDR to p value.
* A new paramter, candidate.num is added to control how many candidate for peaks are outputted. Default is 3.
* Fix a small bug in readAnnotation function.
* The weights have been optimized and changed for metabolite and adduct annotation.
* Change the score cutoff as 0.4 for identifications and identifications.
* Fix a small bug in metdnaFor2Mode.
* Fix a small bug in analysisGeneration.
* The name of package is temporarily changed to MetDNA102.
* The "Glycolysis / Gluconeogenesis" pathway in fruit fly is removed a metablites.

# Version 1.0.3
--------------
* Release version.
* Add README file.
* The default candidate number of peaks are used for anywhere, such as pathway enrichment, data output and report.

# Version 1.1
--------------
* Release version.
* Add README file.
* Fixed a small bug.
* Add a sample.name check in dataCheck function.

# Version 1.1.1
--------------
* Fixed a small bug, when all the intentsity are same in all samples.

# Version 1.1.2
--------------
* Fixed a small bug, when there is no marker with ID for pathway enrichment.
