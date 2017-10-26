# ValueManager R functions on Kantarware
The aim of `VMinR` is to provide a set of usfull R-functions to ValueManager modellers.

## Installation

```
library(devtools)

creds <- git2r::cred_user_pass("USER NAME", "PASSWORD")

devtools::install_git("https://kantarware.visualstudio.com/TNS-AnalyticsDE-VMTools/_git/VMinR_Test", 
                      credentials = creds)

library(VMinR)

hello()
```

## Functionality
The package currently contains basic import and simulation functions for ValueDriver/ValuePricer dat/def-files. 
It also contains some basic import functions for the most often used Sawtooth files. 
Apart from that functions to create a heatmap for ValuePricer studies is included.

For details please refer to 'VMinR.pdf' in  [VMinR_test/doc](https://kantarware.visualstudio.com/TNS-AnalyticsDE-VMTools/_git/VMinR_Test?path=%2Fdoc)

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

### other functions
* covertSSItoDesign
* correlation_HeatMap
* hello

## Bug Report

If you find any bugs in the package, then please get back to me ([eMail](mailto:maximilian.rausch@tns-infratest.com)) giving as much detail as possible into what is breaking.
