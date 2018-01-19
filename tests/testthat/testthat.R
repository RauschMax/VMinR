context("Test VMinR")


# Test for ValueDriver import and simulation #----
testthat::test_that('Test import and calculations of VMinR package (ValueDriver)', {
  #Introduce local test variables here
  basecase <- read.table("data/BaseCase_TimTim.csv", header = TRUE, sep = ";")

  nlev <- c(4, 6, 4, 2, 2, 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5)

  #Run the function that are testing the outputs here
  basecase_dummy <- as.matrix(cbind(VMinR::convertSSItoDesign(basecase, nlev = nlev), 0))

  VDdata <- VMinR::get_DriverData(dat_file = "data/TEST_timtim_gew.dat",
                                  def_file = "data/TEST_timtim_gew.def",
                                  nlev = nlev,
                                  none = TRUE)

  sim_BaseR <- VMinR::VD.computeShares(design = as.matrix(basecase_dummy),
                                       utils = VDdata$utils_mat,
                                       nlev = VDdata$nlev,
                                       weight = NULL,
                                       FC = FALSE)

  def <- VDdata$def

  #Run all tests wanted
  testthat::expect_is(VDdata,'list')
  testthat::expect_is(basecase_dummy,'matrix')
  testthat::expect_is(sim_BaseR,'list')
  testthat::expect_is(sim_BaseR$meanShares,'numeric')

  testthat::expect_identical(round(sim_BaseR$meanShares, 3), c(0.199, 0.093, 0.145, 0.114, 0.353, 0.096))

  testthat::expect_identical(def$nlev, nlev)
})

# Test for ValuePricer import and simulation #----
testthat::test_that('Test import and calculations of VMinR package (ValuePricer)', {
  #Introduce local test variables here

  #Run the function that are testing the outputs here
  beer_data <- VMinR::get_PricerData(dat_file = "data/beer_study.dat",
                              def_file = "data/beer_study.def", none = TRUE)

  beer_def <- beer_data$def

  sim_Beer <- VMinR::VP.computeShares(beer_data$utils_mat,
                                      beer_def$prices,
                                      beer_def$prices[,3],
                                      simSKUs = NULL,
                                      nlev = beer_data$nlev,
                                      weight = beer_data$weight,
                                      none = FALSE,
                                      iaw = NULL,
                                      FC = FALSE)

  #Run all tests wanted
  testthat::expect_is(beer_data,'list')
  testthat::expect_is(beer_def,'list')
  testthat::expect_is(sim_Beer,'list')
  testthat::expect_is(sim_Beer$simShares,'numeric')

  testthat::expect_identical(round(sim_Beer$simShares, 3),
                             c(0.071, 0.081, 0.021, 0.021, 0.019, 0.025, 0.033, 0.031,
                               0.034, 0.042, 0.029, 0.072, 0.023, 0.101, 0.020, 0.021,
                               0.039, 0.025, 0.029, 0.035, 0.043, 0.053, 0.073, 0.017,
                               0.043))

  testthat::expect_identical(beer_def$brands, c("Krombacher 24 x 0,33 l",
                                                "Krombacher 20 x 0,5l",
                                                "Becks 24 x 0,33l",
                                                "Becks 20 x 0,5l",
                                                "Jever 24 x 0,33l",
                                                "Jever 20 x 0,5l",
                                                "Veltins 24 x 0,33l",
                                                "Veltins 20 x 0,5l",
                                                "Bitburger 20 x 0,33l",
                                                "Bitburger 20 x 0,5l",
                                                "Warsteiner 20 x 0,5l",
                                                "Hasseroeder 20 x 0,5l",
                                                "Koenig Pilsener 20 x 0,5l",
                                                "Radeberger 20 x 0,5l",
                                                "Freiberger 20 x 0,5l",
                                                "Berliner Kindl 20 x 0,5l",
                                                "Holsten 20 x 0,5l",
                                                "Luebzer 20 x 0,5l",
                                                "Flensburger 20 x 0,3 3l",
                                                "Wernesgruener 20 x 0,5l",
                                                "Ur Krostitzer 20 x 0,5l",
                                                "Paderborner 20 x 0,5 l",
                                                "Oettinger 20 x 0,5l",
                                                "Berliner Pilsner 20 x 0,5l",
                                                "Astra Urtyp 27 x 0,33l"))
})



testthat::test_that('Test ConceptOpt ISBC of VMinR package', {
  #Introduce local test variables here
  library(foreign)

  nlev <- c(3, 4, 2, 2, 2, 2, 2)

  natt <- length(nlev)

  # read dat/def file (ValueDriver structure) ----
  # debugonce(get_DriverData)
  dat_input <- get_DriverData("data/Smartwatches_uncal_effects.dat",
                              "data/ISBC_Points_uncal.def",
                              nlev = nlev, none = TRUE)

  utils <- dat_input$utils_mat

  bw_data <- read.spss("data/data.sav", to.data.frame = TRUE, use.value.labels = FALSE)

  best_concepts <- t(matrix(as.numeric(unlist(lapply(as.character(bw_data$"VM_DCM_Driver_Block1_ISBC_BestConcept"),
                                                     strsplit, split = "[,]"))), nrow = natt)) + 1

  worst_concepts <- t(matrix(as.numeric(unlist(lapply(as.character(bw_data$"VM_DCM_Driver_Block1_ISBC_WorstConcept"),
                                                      strsplit, split = "[,]"))), nrow = natt)) + 1

  purch_best <- bw_data$"VM_DCM_Driver_Block1_VM_DCM_BestWorst_best_PurchaseIntention"

  purch_worst <- bw_data$"VM_DCM_Driver_Block1_VM_DCM_BestWorst_worst_PurchaseIntention"

  BWconcepts <- as.data.frame(cbind(bw_data$Respondent_Serial,
                                    best_concepts, worst_concepts, purch_best, purch_worst))
  names(BWconcepts) <- c("ID",
                         paste0("B_Att_", sequence(natt)),
                         paste0("W_Att_", sequence(natt)),
                         "PI_B", "PI_W")

  TestConc <- data.frame(3, 4, 2, 2, 2, 2, 2)

  # recodes the concepts into DUMMY (0/1) coding ----
  TestConc_dummy <- as.matrix(VMinR::convertSSItoDesign(TestConc, nlev = nlev))


  #Run the function that are testing the outputs here
  calibData <- VMinR::calibEXE(BWconcepts = BWconcepts[, c(paste0("B_Att_", sequence(natt)),
                                                    paste0("W_Att_", sequence(natt)))],
                        PI = BWconcepts[, c("PI_B", "PI_W")],
                        utils = dat_input$utils_mat,
                        cut = 42, nlev = nlev)




  # applies the product acceptance for all concepts on individual level
  shares_ProdAcc <- apply(TestConc_dummy, 1, VMinR::prodAcceptance,
                          utils = calibData$utils[,-ncol(calibData$utils)])
  shares_ProdAcc_calib <- apply(TestConc_dummy, 1, VMinR::prodAcceptance,
                                utils = as.matrix(calibData$utils_calib[,-ncol(calibData$utils_calib)]))


  #Run all tests wanted
  testthat::expect_is(calibData,'list')
  testthat::expect_is(shares_ProdAcc,'matrix')
  testthat::expect_is(shares_ProdAcc_calib,'matrix')

  testthat::expect_identical(round(mean(shares_ProdAcc), 3), 0.144)

  testthat::expect_identical(round(mean(shares_ProdAcc_calib), 3), 0.192)
})













