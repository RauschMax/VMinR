# Conjoint R functions on Git Hub
The aim of `VMinR` is to provide a set of usfull R-functions to conjoint modellers.

## Installation
If you want to install the `VMinR` package, then you will need the following packages:

* devtools
* git2r

```
install.packages(c('devtools', 'git2r'))
library(devtools)
library(git2r)
creds <- cred_user_pass('<KT GIT USERNAME>', '<KT GIT PASSWORD>')
devtools::install_github("https://github.com/RauschMax/VMinR")
```

## Functionality
The package currently contains basic import and simulation functions for ValueDriver/ValuePricer dat/def-files. 
It also contains some basic import functions for the most often used Sawtooth files. 
Apart from that functions to create a heatmap for ValuePricer studies is included.

### ValueDriver
* get_DriverData
* VD.read_def
* VD.computeShares

### ValuePricer
* get_PricerData
* VP.read_def
* VP.computeShares

### read/write functions
* readCHO
* readDAT
* writeCHO

### Concept Optimization (ISBC)
* calibEXE
* summary_calibEXE
* prodAcceptance
* ConceptOpt_ISBC

### other functions
* covertSSItoDesign
* correlation_HeatMap
* hello

## Bug Report

If you find any bugs in the package, then please get back to me ([eMail](mailto:maximilian.rausch@tns-infratest.com)) giving as much detail as possible into what is breaking.
