rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(data.table)
library(openxlsx)
library(VMinR)
library(VMOptimisation)

# layout function!------------------------------------------------------------------------------------------------------
color_from_middle <- function(data, color1, color2)
{
  max_val <- max(abs(data))
  JS(sprintf(paste0("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' +",
                    " (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,",
                    "transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + ",
                    "(50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'"),
             max_val, color1, max_val, color1, color2, color2, max_val, max_val))
}
# !---------------------------------------------------------------------------------------------------------------------
# !---------------------------------------------------------------------------------------------------------------------


# !---------------------------------------------------------------------------------------------------------------------
# READ DATA !-----------------------------------------------------------------------------------------------------------
# !---------------------------------------------------------------------------------------------------------------------

# stName <- "TEST_timtim_gew"
# popAtt <- 2
stName <- "Banking_Demo_ASD"
popAtt <- 1


# debugonce(VD.read_def)
defIN <- VD.read_def(paste0("testthat/data/", stName, ".def"))
# defIN
str(defIN)

datIN <- fread(paste0("testthat/data/", stName, ".dat"))
names(datIN) <- c("ID", "RLH", "nAtt", "nLev", "nSeg",
                  paste0("U", rep(seq_along(defIN$nlev), defIN$nlev),
                         "_", sequence(defIN$nlev)),
                  "NONE", "weight",
                  paste0("Seg", seq(defIN$nseg)))
datIN

# CBC_data <- get_DriverData(paste0("testthat/data/", stName, ".dat"),
#                            paste0("testthat/data/", stName, ".def"),
#                            defIN$nlev, TRUE)
# str(CBC_data)

# !---------------------------------------------------------------------------------------------------------------------

# EXTRACT INFO !--------------------------------------------------------------------------------------------------------
utils_mat <- as.matrix(datIN[, mget(c(grep("^U", names(datIN), value = TRUE), "NONE"))])
head(utils_mat)

nLev <- defIN$nlev
nAtt <- length(nLev)

price_Atts <- which(!sapply(sapply(defIN$num_att, is.na), all))
names(price_Atts) <- names(defIN$att_List)[price_Atts]
price_Atts

prices <- defIN$num_att[!sapply(sapply(defIN$num_att, is.na), all)]
prices

asd_rules <- list(
  list(baseAtt = 1,
       baseLev = 1,
       asdAtt = price_Atts[-1]),
  list(baseAtt = 1,
       baseLev = 2,
       asdAtt = price_Atts[-c(2, 3)]),
  list(baseAtt = 1,
       baseLev = 3,
       asdAtt = price_Atts[-c(1, 4)])
)
# asd_rules <- lapply(
#   seq_along(price_Atts),
#   function(i) {
#     list(baseAtt = 2,
#          baseLev = i,
#          asdAtt = price_Atts[-i])
#   })
asd_rules

# Scenario !------------------------------------------------------------------------------------------------------------ SCENARIO
attLev_List <- lapply(defIN$att_List,
                      function(att) {
                        # att <- defIN$att_List[[1]]
                        factor(seq_along(att),
                               levels = seq_along(att),
                               labels = gsub("[ ]+$", "", att))
                      })
attLev_List

defaultPrice <- 1
defaultLev <- 1
noneSwitch <- TRUE

scenarioPop <- Reduce(data.frame,
                      lapply(1:nAtt,
                             function(att) {
                               # att <- 1
                               if (att == popAtt) {
                                 out <- attLev_List[[att]]
                               } else if (att %in% price_Atts) {
                                 out <- rep(attLev_List[[att]][defaultPrice],
                                            nLev[popAtt])
                               } else {
                                 out <- rep(attLev_List[[att]][defaultLev],
                                            nLev[popAtt])
                               }
                               out
                             }))
names(scenarioPop) <- paste0("Att", 1:nAtt)
scenarioPop

for (asd in asd_rules) {
  # asd <- asd_rules[[1]]
  indHelp <- which(as.numeric(scenarioPop[, asd[["baseAtt"]]]) == asd[["baseLev"]])
  scenarioPop[indHelp, asd[["asdAtt"]]] <- NA
}
scenarioPop

scenario <- as.data.frame(sapply(scenarioPop,
                                 as.numeric))
scenario[is.na(scenario)] <- 0
if (noneSwitch) {
  scenario <- as.matrix(rbind(cbind(scenario, NONE = 0),
                              c(rep(0, ncol(scenario)), 1)), dimnames = NULL)

} else {
  scenario <- as.matrix(cbind(scenario, NONE = 0))

}
scenario

# Simulate Scenario !---------------------------------------------------------------------------------------------------
Sim_Scen <- VD.computeShares(design = scenario,
                             utils = utils_mat,
                             nlev = c(nLev, 1),
                             weight = NULL,
                             FC = FALSE,
                             dummy = FALSE)

data.table(Conc = paste0("Conc_", 1:length(Sim_Scen$meanShares)),
           SoC = round(Sim_Scen$meanShares, 3) * 100)

# !---------------------------------------------------------------------------------------------------------------------

# STRATEGY PROFILE !---------------------------------------------------------------------------------------------------- STRATEGY PROFILE
selConc <- 1
selPrice <- 1

# debugonce(StrategyProfile)
SP_Test <- VMinR::StrategyProfile(
  scenario = scenario,
  att_List = defIN$att_List,
  utils_mat = utils_mat,
  scenInd = selConc,
  brandAtt = unlist(unique(sapply(asd_rules,
                                  function(x) x$baseAtt)))[1],
  priceInd = price_Atts,
  priceDefault = rep(selPrice, length(price_Atts)))

SP_Test

SP_Test_List <- lapply(
  1:(nrow(scenario) - 1),
  function(c) {
    VMinR::StrategyProfile(
      scenario = scenario,
      att_List = defIN$att_List,
      utils_mat = utils_mat,
      scenInd = c,
      brandAtt = unlist(unique(sapply(asd_rules,
                                      function(x) x$baseAtt)))[1],
      priceInd = price_Atts,
      priceDefault = rep(selPrice, length(price_Atts)))
  })

lapply(SP_Test_List,
       function(sp) {
         sp_out <- sp$Strategy_DT
         sp_out[, Absolute := round(Absolute, 3) * 100]
         sp_out[, Relative := round(Relative, 3) * 100]
         sp_out
       })

# !---------------------------------------------------------------------------------------------------------------------
# !---------------------------------------------------------------------------------------------------------------------


# !---------------------------------------------------------------------------------------------------------------------
# WTP !-----------------------------------------------------------------------------------------------------------------
# !---------------------------------------------------------------------------------------------------------------------
# Scenario !------------------------------------------------------------------------------------------------------------
scenarioWTP <- scenario

# debugonce(WTP)
WTP_test <- VMinR::WTP(scen = scenarioWTP,
                       scenIndex = selConc,
                       nLevels = nLev,
                       attNames = defIN$att_List,
                       brandAttribute = unlist(unique(sapply(asd_rules,
                                                             function(x) x$baseAtt)))[1],
                       priceIndex = price_Atts,
                       priceDefault = rep(selPrice, length(price_Atts)),
                       prices = prices,
                       utilsIN = utils_mat,
                       none = TRUE,
                       weight = NULL,
                       FC = FALSE)
str(WTP_test)

# WTP_DT
scenarioWTP
WTP_test$WTP_DT

# !---------------------------------------------------------------------------------------------------------------------

# !---------------------------------------------------------------------------------------------------------------------
# !       ||||||| |   | ||||    |||| |   | ||||
# !          |    |   | |       |    ||  | |   |
# !          |    ||||| ||||    |||| | | | |    |
# !          |    |   | |       |    |  || |   |
# !          |    |   | ||||    |||| |   | ||||
# !---------------------------------------------------------------------------------------------------------------------

