library(tidyverse)
library(glue)
library(ragg)



SundayCloseDate <- "-04-26"
SundayCloseMonth <- 4
SundayCloseDay <- 26
YearDay <- yday(as.Date(str_c("2026",SundayCloseDate)))

currentWeek <- isoweek(as.Date(str_c("2026",SundayCloseDate)))

cutoff_date <- as.Date(str_c("2026",SundayCloseDate))


sql <- glue("
  SELECT 
      CASE
          WHEN bd.StorageTypeID = 4 THEN 'CA'
          ELSE 'RA'
      END AS [Storage type]
      ,SUM(bu.BinQty) AS BinQty 
  FROM ma_Bin_UsageT AS bu
  LEFT JOIN ma_Bin_DeliveryT AS bd
      ON bd.BinDeliveryID = bu.BinDeliveryID
  LEFT JOIN ma_Grader_BatchT AS gb
      ON gb.GraderBatchID = bu.GraderBatchID
  WHERE bd.SeasonID = 2013
  AND bd.PresizeFlag = 0
  AND (
      bu.GraderBatchID IS NULL
      OR gb.PackDate > '{cutoff_date}'
  )
  GROUP BY
      CASE
          WHEN bd.StorageTypeID = 4 THEN 'CA'
          ELSE 'RA'
      END
")

PackingPlan2026 <- read_csv("data/PackingPlan.csv", show_col_types = F)

con <- DBI::dbConnect(odbc::odbc(),    
                      Driver = "ODBC Driver 18 for SQL Server", 
                      Server = "abcrepldb.database.windows.net",  
                      Database = "ABCPackerRepl",   
                      UID = "abcadmin",   
                      PWD = "Trauts2018!",
                      Port = 1433
)

BinsRemaining <- DBI::dbGetQuery(con, sql)

BD <- DBI::dbReadTable(con, "ma_Bin_DeliveryT")

GBV2024 <- DBI::dbReadTable(con, "ma_Grader_BatchV")

Class152 <- DBI::dbGetQuery(con, "SELECT
GraderBatchID
,ISNULL([Class 1.5],0) AS [Class 1.5]
,ISNULL([Class 2],0) AS [Class 2]
FROM (
  SELECT
  GraderBatchID,
  GradeDesc,
  SUM(KGs) AS KGs
  FROM [dbo].[shiny_class15_KGsV]
  GROUP BY
  GraderBatchID,
  GradeDesc
) AS src
PIVOT (
  SUM(KGs)
  FOR GradeDesc IN (
    [Class 1.5],
    [Class 2]
  )
) AS pvt;")

LibSamp <- DBI::dbReadTable(con, "shiny_Rework_KGsV")

DBI::dbDisconnect(con)

# Join the Class152 and Calculate the Export Kgs and Packout1 (new packout) and Packout2 (Old packout)

GBV2024NP <- GBV2024 |>
  filter(Presize == "Field bin",
         BatchStatus == "Closed") |>
  left_join(Class152, by = "GraderBatchID") |>
  left_join(LibSamp |> 
              rename(LibSampKGs = KGs) |>
              dplyr::select(GraderBatchID, LibSampKGs), 
            by = "GraderBatchID") |>
  mutate(across(.cols = c(`Class 1.5`, `Class 2`, LibSampKGs), ~replace_na(.,0)),
         ExportKgs = InputKgs - RejectKgs - `Class 1.5` - `Class 2` - LibSampKGs,
         Packout1 = ExportKgs/InputKgs,
         Packout2 = 1-(RejectKgs/InputKgs))
#
# Generate data frame from S&OP packing plan
#

BinTipPlan <- PackingPlan2026 |>
  mutate(TotalBins = `Te Ipu`+Freshco+`Green planet`+Sunfruit,
         RABinsTipped = 0,
         RemainingRABins = 0,
         CABinsTipped = 0,
         RemainingCABins = 0)

RemainingBinsRA <- BinsRemaining$BinQty[[1]]
RemainingBinsCA <- BinsRemaining$BinQty[[2]]

#
# Apply the logic to calculate the remaining RA and CA bins at the end of each ISO week
#

PackProgramme <- function(BinTipPlan, RemainingRA, RemainingCA) {

  for (i in 1:nrow(BinTipPlan)) {
    if (i == 1) {
      if({{RemainingRA}}-BinTipPlan$TotalBins[1] <= 0) {
        BinTipPlan$RemainingRABins[i] <- 0
        BinTipPlan$RABinsTipped[i] <- {{RemainingRA}}
      } else {
        BinTipPlan$RemainingRABins[1] <-  {{RemainingRA}}-BinTipPlan$TotalBins[1]
        BinTipPlan$RABinsTipped[1] <- BinTipPlan$TotalBins[1]
      }
    } else {
      if (BinTipPlan$RemainingRABins[i-1]-BinTipPlan$TotalBins[i] >= 0) {
        BinTipPlan$RemainingRABins[i] <-  BinTipPlan$RemainingRABins[i-1]-BinTipPlan$TotalBins[i]
        BinTipPlan$RABinsTipped[i] <- BinTipPlan$TotalBins[i]
      } else {
        BinTipPlan$RemainingRABins[i] <- 0 
        BinTipPlan$RABinsTipped[i] <-  BinTipPlan$RemainingRABins[i-1] 
        break
      }
    }
  }

  for (j in 1:nrow(BinTipPlan)) {
    if(BinTipPlan$RemainingRABins[j]>0 & BinTipPlan$RABinsTipped[j] == BinTipPlan$TotalBins[j]) {
      BinTipPlan$RemainingCABins[j] <- {{RemainingCA}}
      BinTipPlan$CABinsTipped[j] <- 0
    } else if (BinTipPlan$RABinsTipped[j]>0 & BinTipPlan$RemainingRABins[j] == 0) {
      BinTipPlan$RemainingCABins[j] <- RemainingBinsCA-(BinTipPlan$TotalBins[j]-BinTipPlan$RABinsTipped[j])
      BinTipPlan$CABinsTipped[j] <- BinTipPlan$TotalBins[j]-BinTipPlan$RABinsTipped[j]
    } else if (BinTipPlan$RABinsTipped[j] == 0 & BinTipPlan$RemainingRABins[j] == 0) {
      if (j == 1) {
        BinTipPlan$RemainingCABins[j] <- {{RemainingCA}}
        BinTipPlan$CABinsTipped[j] <- BinTipPlan$TotalBins[j]
      } else {
        if (BinTipPlan$RemainingCABins[j-1] >= BinTipPlan$TotalBins[j]) {
          BinTipPlan$RemainingCABins[j] <- BinTipPlan$RemainingCABins[j-1]-BinTipPlan$TotalBins[j]
          BinTipPlan$CABinsTipped[j] <- BinTipPlan$TotalBins[j]
        } else {
          BinTipPlan$RemainingCABins[j] <- 0
          BinTipPlan$CABinsTipped[j] <- BinTipPlan$RemainingCABins[j-1]
        }
      } 
    }
  }

  return(BinTipPlan)
}

PackProgrammeToGo <- PackProgramme(BinTipPlan, RemainingBinsRA, RemainingBinsCA)  

#
# Plot the bin draw down
#

PackProgrammeToGo |>
  dplyr::select(c(`ISO Week`,RemainingRABins,RemainingCABins)) |>
  pivot_longer(cols = c(RemainingRABins,RemainingCABins),
               names_to = "Storage",
               values_to = "Bins") |>
  mutate(Storage = str_sub(Storage,10,11)) |>
  ggplot(aes(x=`ISO Week`, y=Bins, colour=Storage)) +
  geom_line(linewidth = 1) +
  scale_fill_manual(values = c("#a9342c","#48762e","#526280","#f6c15f")) +
  scale_colour_manual(values = c("#a9342c","#48762e","#526280","#f6c15f")) +
  scale_y_continuous("Bins remaining", labels = scales::label_comma(1.0)) +
  ggthemes::theme_economist() + 
  theme(text = element_text(family = "Geograph"),
        axis.title.x = element_text(margin = margin(t = 7), size = 10,colour = "#48762e",face = "bold"),
        axis.title.y = element_text(margin = margin(r = 7), size = 10,colour = "#48762e",face = "bold"),
        axis.text.y = element_text(size = 10, hjust=1,colour = "#48762e",face = "bold"),
        axis.text.x = element_text(size = 10,colour = "#48762e",face = "bold"),
        plot.background = element_rect(fill = "#F7F1DF", colour = "#F7F1DF"),
        plot.title = element_text(margin = margin(b = 10)),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

################################################################################
# Calculate the mean harvest date for for average storage day calculation      #
################################################################################

WAHD <- BD |>
  filter(SeasonID == 2013,
         PresizeFlag == 0) |>
  summarise(WAHD = weighted.mean(HarvestDate, NoOfBins))

################################################################################
# Calculate the Weight mean mass per bin                                       #
################################################################################

WAMPB <- GBV2024NP |>
  filter(Season == "2026") |>
  summarise(InputKgs = sum(InputKgs),
            FieldBinsTipped = sum(FieldBinsTipped)) |>
  mutate(WAMPB = InputKgs/FieldBinsTipped)

################################################################################
# Function to return a data frame that gives the start and finish or RA and CA #
# packing                                                                      #
################################################################################

StorageDayEstimates <- function(PackProgrammeToGo, WAHarvestDate) {
  
  FuturePackDataRA <- PackProgrammeToGo |>
    filter(RABinsTipped > 0) |>
    mutate(weekdate = str_c("2026-W",`ISO Week`,"-3"),
           PackDate = ISOweek::ISOweek2date(weekdate),
           StorageDays = as.integer(PackDate-WAHD$WAHD[[1]]))
  
  FuturePackDataCA <- PackProgrammeToGo |>
    filter(CABinsTipped > 0) |>
    mutate(weekdate = str_c("2026-W",`ISO Week`,"-3"),
           PackDate = ISOweek::ISOweek2date(weekdate),
           StorageDays = as.integer(PackDate-WAHD$WAHD[[1]]))
  
  ##  Account for when RA packing has ended
  if(nrow(FuturePackDataRA) == 0) {
    FuturePackDataRA[1:2,] <- NA
    FuturePackDataRA[[1,12]] <- 1
    FuturePackDataRA[[2,12]] <- as.integer(ISOweek::ISOweek2date("2025-W35-3")-WAHarvestDate[[2,2]])
  } else if (nrow(FuturePackDataCA) == 0) {
    FuturePackDataCA[1:2,] <- NA
  }
  
  StorageDayCuts <- tibble(`Storage type` = c("RA","CA"),
                           StorageDays_Start = c(FuturePackDataRA$StorageDays[[1]],
                                                 FuturePackDataCA$StorageDays[[1]]),
                           StorageDays_End = c(FuturePackDataRA$StorageDays[[nrow(FuturePackDataRA)]],
                                               FuturePackDataCA$StorageDays[[nrow(FuturePackDataCA)]])) |>
    ungroup()
  
  return(StorageDayCuts)
  
}

#
# Run the function to get the range of storage days for each storage type
#

SDEstimates <- StorageDayEstimates(PackProgrammeToGo,WAHD)

################################################################################
# Function to return the ISO week when CA finishes packing                     #
################################################################################

WeekCABeginsEnds <- function(PackProgrammeToGo) {
  
  FuturePackDataCA <- PackProgrammeToGo |>
    filter(CABinsTipped > 0) 
  
  WeekCABegins <- tibble(`CA start week` = FuturePackDataCA$`ISO Week`[[1]],
                         `CA finish week` = FuturePackDataCA$`ISO Week`[[nrow(FuturePackDataCA)]])
  
  return(WeekCABegins)
  
}

CB <- WeekCABeginsEnds(PackProgrammeToGo)

#
# Run the models for the packout for 2026
#
# First the RA portion
#
PackOutPlotSumRaw <- GBV2024NP |>
  mutate(YearDays = yday(PackDate)) |>
  filter(Season == "2026") |>
  mutate(StorageDays = as.integer(PackDate - HarvestDate)) 

#write_csv(PackOutPlotSumRaw |> filter(Storage.type != "CA Smartfresh"),"RAGBV2026.csv")

NLmodel2026RA <- nls(Packout1 ~ alpha *exp(beta * StorageDays) + theta, 
                     data=PackOutPlotSumRaw |> filter(Storage.type != "CA Smartfresh"), 
                     control = nls.control(maxiter = 1000), 
                     start=list(alpha=0.1, beta=-.1, theta=0.60))

summary(NLmodel2026RA)

#
# Generate modeled plot data for RA including the bootstrapped confidence intervals
#

StorageDaysRA <- tibble(StorageDays = seq(0,SDEstimates$StorageDays_End[[1]],1))

yield.boot2026 <- nlstools::nlsBoot(NLmodel2026RA, niter = 999)

meanPO2026 <- data.frame(estimate = predict(NLmodel2026RA,
                                            newdata = StorageDaysRA))
CIPO2026 <- data.frame(nlstools::nlsBootPredict(yield.boot2026, 
                                                newdata = StorageDaysRA, 
                                                interval = "confidence"))

mean_curvePO2026RA <- StorageDaysRA |> 
  bind_cols(meanPO2026) |>
  bind_cols(CIPO2026) |>
  rename(low = X2.5.,
         high = X97.5.) |>
  mutate(Season = "2026") |>
  dplyr::select(-c(Median))
#
# Generate synthetic data for 2026
#
meanPO2026syn <- data.frame(estimate = predict(NLmodel2026RA,
                                               newdata = StorageDaysRA)) |>
  bind_cols(StorageDaysRA) |>
  mutate(Season = 2026,
         `Storage type` = "RA")
#
# Calculate the final packout for the RA fruit (this is the base packout for the CA uplift)
#
PackoutEndRA <- predict(NLmodel2026RA,newdata = tibble(StorageDays = SDEstimates$StorageDays_End[[1]]))
#
# Need to calculate/estimate the CA uplift for 2026 - unusual behaviour in 2025
#
#################################################################################
# 2025 CA Analysis - put this into a separate vignette                          #
#################################################################################
#
# Generate the data frame
#
GBVCA25 <- GBV2024NP |>
  filter(Presize == "Field bin",
         BatchStatus == "Closed",
         Storage.type == "CA Smartfresh",
         Season == "2025") |>
  mutate(StorageDays = as.numeric(PackDate-HarvestDate),
         Packout = 1-(RejectKgs/InputKgs))

#write_csv(GBVCA25,"CA2025.csv")
#
# Explore the 2025 data and find the knot when the fruit is deteriorating
#
# Using piecewise linear regression
#
library(segmented)

fit_lm <- lm(Packout ~ StorageDays, data = GBVCA25)

fit_seg <- segmented(fit_lm, seg.Z rsp_coloursfit_seg <- segmented(fit_lm, seg.Z = ~StorageDays, psi = 230)

summary(fit_seg)

GBVCA25Pl <- GBVCA25 %>%
  mutate(fitted = fitted(fit_seg))

ggplot(GBVCA25Pl, aes(x = StorageDays, y = Packout)) +
  geom_point(alpha = 0.3, colour = "steelblue") +
  geom_line(aes(y = fitted), colour = "firebrick", linewidth = 1) +
  geom_vline(xintercept = fit_seg$psi[, "Est."], 
             linetype = "dashed", colour = "grey40") +
  labs(x = "Storage Days", y = "Packout",
       title = "CA Packout vs Storage Days — Broken-Stick Model") +
  theme_minimal()
#
# Now take 2024 and 2025 and filter StorageDays < 225 and plot together
#
GBVCA2425 <- GBV2024NP |>
  filter(Presize == "Field bin",
         BatchStatus == "Closed",
         Storage.type == "CA Smartfresh",
         Season %in% c("2024","2025")) |>
  mutate(StorageDays = as.numeric(PackDate-HarvestDate)) |>
  filter(StorageDays < 225 & StorageDays > 150)

GBVCA2425|>
  ggplot(aes(x=StorageDays, y=Packout1, colour = Season)) +
  geom_point() +
  geom_smooth(method = "lm")

CAModel <- lm(Packout1 ~ StorageDays*Season, data = GBVCA2425)

summary(CAModel)
#
# Calculate the mean CA packout for 2024 and 2025 given the restricted Storage Day window
#
meanCAPO <- GBVCA2425 |>
  group_by(Season) |>
  summarise(Packout1 = weighted.mean(Packout1, InputKgs))
#
# Generate RA data to do the non-linear model for 2024 and 2025
#
PackOutPlotSum2425 <- GBV2024NP |>
  mutate(YearDays = yday(PackDate)) |>
  filter(Presize == "Field bin",
         BatchStatus == "Closed",
         Storage.type != "CA Smartfresh",
         Season %in% c("2024","2025")) |>
  mutate(StorageDays = as.integer(PackDate - HarvestDate)) 
#
# Calculate the representative baseline packout values for 2025 and 2024
#
RepCASD <- GBVCA2425 |>
  group_by(Season) |>
  summarise(MaxSD = max(StorageDays),
            MinSD = min(StorageDays)) |>
  mutate(RepSD = (MaxSD+MinSD)/2)
#
# Run model for 2024 RA data
#
NLmodel2024RA <- nls(Packout1 ~ alpha *exp(beta * StorageDays) + theta, 
                     data=PackOutPlotSum2425 |> filter(Season == "2024"), 
                     control = nls.control(maxiter = 1000), 
                     start=list(alpha=0.1, beta=-.1, theta=0.60))
#
# Calculate the baseline using the mid point of the storage day window for CA
#
Baseline2024 <- predict(NLmodel2024RA, newdata = tibble(StorageDays = RepCASD$RepSD[[1]]))
#
# Run the model for the 2025 data
#
NLmodel2025RA <- nls(Packout1 ~ alpha *exp(beta * StorageDays) + theta, 
                     data=PackOutPlotSum2425 |> filter(Season == "2025"), 
                     control = nls.control(maxiter = 1000,minFactor = 1/4096), 
                     start=list(alpha=0.15, beta=-.01, theta=0.65))
#
# Calculate the baseline using the mid point of the storage day window for CA
#
Baseline2025 <- predict(NLmodel2025RA, newdata = tibble(StorageDays = RepCASD$RepSD[[2]]))
#
# Calculate the CA uplift using the mean of the 2024 and 2025 uplift
#
CAUplift <- tibble(Season = c("2024","2025"),
                   RepRABL = c(Baseline2024,Baseline2025)) |>
  inner_join(meanCAPO, by = "Season") |>
  mutate(Uplift = Packout1-RepRABL)

MeanUplift <- (CAUplift$Uplift[[1]]+CAUplift$Uplift[[2]])/2
#
# Crate a data frame of model CA packout data vs Storage days (i.e. constant)
#
modelCAPO2026 <- tibble(StorageDays = seq(SDEstimates$StorageDays_Start[[2]],SDEstimates$StorageDays_End[[2]],1)) |>
  mutate(Packout1 = PackoutEndRA+MeanUplift,
         Season = 2026,
         `Storage type` = "CA")
#
# Creat data frame of actual CA batches packed in 2026
#
CAData2026 <- GBV2024NP |>
  mutate(YearDays = yday(PackDate)) |>
  filter(Presize == "Field bin",
         BatchStatus == "Closed",
         Storage.type == "CA Smartfresh",
         Season == "2026") |>
  mutate(StorageDays = as.integer(PackDate - HarvestDate)) 
#
# Plot the existing data
#
mean_curvePO2026RA |>
  ggplot() +
  geom_line(aes(StorageDays, estimate),colour = "#48762e", linewidth=1) +
  geom_ribbon(aes(x=StorageDays, ymin = low, ymax = high, group = factor(Season)), fill="grey50", alpha=0.5) +
  geom_point(data = PackOutPlotSumRaw, aes(x=StorageDays, y=Packout1), colour = "#48762e", alpha=0.6) +
  geom_point(data = CAData2026, aes(x = StorageDays, y = Packout1), colour = "#a9342c", alpha = 0.6) +
  geom_line(data = modelCAPO2026, aes(x= StorageDays, y = Packout1), colour = "#a9342c", linewidth=1) +
  #geom_ribbon(data = GBDTICA2025Mod, aes(x=StorageDays, ymin=lwr, ymax=upr), fill="grey50", alpha=0.5) +
  #geom_line(data = CAPO2025, aes(StorageDays, Packout), colour = "#a9342c", linewidth = 1) +
  scale_y_continuous("Packout / %", labels = scales::label_percent(1.0)) +
  labs(x = "Storage days") +
  scale_fill_manual(values = c("#a9342c","#48762e","#526280","#f6c15f")) +
  scale_colour_manual(values = c("#a9342c","#48762e","#526280","#f6c15f")) +
  ggthemes::theme_economist() + 
  theme(legend.position = "top",
        axis.title.x = element_text(margin = margin(t = 10), size = 8),
        axis.title.y = element_text(margin = margin(r = 10), size = 8),
        axis.text.y = element_text(size = 8, hjust=1),
        axis.text.x = element_text(size = 8),
        plot.background = element_rect(fill = "#F7F1DF", colour = "#F7F1DF"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        strip.text = element_text(margin = margin(b=10), size = 10))

################################################################################
# Seasonal Packout prediction - Build the picture for the seasonal result      #
################################################################################
#
# Total bins by storage type - Already packed
#
BinsTippedFinalSummaryTotal <- GBV2024NP |>
  filter(Season == "2026") |>
  mutate(`Storage type` = if_else(Storage.type == "CA Smartfresh","CA","RA")) |>
  summarise(FieldBinsTipped = sum(FieldBinsTipped),
            Reject = sum(RejectKgs),
            `Class 1.5` = sum(`Class 1.5`),
            `Class 2` = sum(`Class 2`),
            `Library samples` = sum(LibSampKGs),
            `Class 1` = sum(ExportKgs),
            Input = sum(InputKgs))

BTFSTotal_tr <- BinsTippedFinalSummaryTotal |>
  dplyr::select(-c(FieldBinsTipped, Input)) |>
  pivot_longer(everything(), names_to = "Packed component", values_to = "KGs") |>
  summarise(KGs = sum(KGs)) |>
  mutate(`Packed component` = "Input",
         Percentage = 1.0) |>
  relocate(`Packed component`, .before = KGs)

BinsTippedFinalSummaryTotal |>
  dplyr::select(-c(FieldBinsTipped, Input)) |>
  pivot_longer(everything(), names_to = "Packed component", values_to = "KGs") |>
  mutate(Percentage = KGs/sum(KGs)) |>
  bind_rows(BTFSTotal_tr) |>
  mutate(Percentage = scales::percent(Percentage,0.01),
         KGs = scales::comma(KGs, 1.0)) |>
  flextable::flextable() |>
  flextable::align(j=c(2,3), align = "right", part="body") |>
  flextable::align(j=c(2,3), align="center", part="header") |>
  flextable::hline(i=5,part = "body") |>
  flextable::bold(bold = T, part = "header") |>
  flextable::autofit() |>
  flextable::fit_to_width(max_width=6)
  

BinsTippedTable <- BinsTippedFinalSummaryTotal |>
  mutate(`Already packed_Input_tonnes` = Input/1000,
         `Already packed_Reject_tonnes` = Reject/1000,
         `Already packed_Class15_tonnes` = `Class 1.5`/1000,
         `Already packed_Class2_tonnes` = `Class 2`/1000,
         `Already packed_Library_sample_tonnes` = `Library samples`/1000,
         `Already packed_Export_tonnes` = `Class 1`/1000,
         `Already packed_Packout_%` =  `Already packed_Export_tonnes`/`Already packed_Input_tonnes`) |>
  dplyr::select(c(`Already packed_Input_tonnes`,`Already packed_Export_tonnes`,`Already packed_Packout_%`))

FuturePackSummary <- function(PackProgrammeToGo,WAHarvestDate,WAMassPerBin,NLmodel2026RA) {
  
  FuturePackDataRA <- PackProgrammeToGo |>
    filter(RABinsTipped > 0) |>
    mutate(weekdate = str_c("2026-W",`ISO Week`,"-3"),
           PackDate = ISOweek::ISOweek2date(weekdate),
           StorageDays = as.integer(PackDate-WAHD$WAHD[[1]]),
           CumBinsTipped = cumsum(RABinsTipped),
           InputKgs = RABinsTipped*WAMPB$WAMPB[[1]])
  
  Packout <- tibble(Packout1 = predict(NLmodel2026RA, newdata = tibble(StorageDays = FuturePackDataRA$StorageDays))) 
  
  FinalFuturePackDatesRA <- FuturePackDataRA |>
    bind_cols(Packout) |>
    mutate(ExportKgs = Packout1*InputKgs) |>
    dplyr::select(c(PackDate,StorageDays,RABinsTipped,InputKgs,ExportKgs)) 
  
  return(FinalFuturePackDatesRA)
  
}

FPSRA <- FuturePackSummary(PackProgrammeToGo,WAHD,WAMPB,NLmodel2026RA)

################################################################################################
#                                       Include the CA component                               #
################################################################################################

CABinsToGoRaw <- BinsRemaining[2,] |>
  mutate(`CA to go_Input_tonnes` = BinQty*WAMPB$WAMPB[[1]]/1000) |>
  dplyr::select(`CA to go_Input_tonnes`)

POCalculation <- BinsTippedTable |>
  bind_cols(FPSRA |>
              ungroup() |>
              summarise(`RA to go_Input_tonnes` = sum(InputKgs)/1000,
                        `RA to go_Export_tonnes` = sum(ExportKgs)/1000) |>
              mutate(`RA to go_Packout_%` = `RA to go_Export_tonnes`/`RA to go_Input_tonnes`)) |>
  bind_cols(CABinsToGoRaw |>
              mutate(`CA to go_Packout_%` = PackoutEndRA+MeanUplift,
                     `CA to go_Export_tonnes` = `CA to go_Packout_%`*`CA to go_Input_tonnes`) |>
              dplyr::select(`CA to go_Input_tonnes`,`CA to go_Export_tonnes`,`CA to go_Packout_%`))

POCalculation |>
  mutate(across(.cols = c(`Already packed_Input_tonnes`,`Already packed_Export_tonnes`,`RA to go_Input_tonnes`,
                          `RA to go_Export_tonnes`,`CA to go_Input_tonnes`,`CA to go_Export_tonnes`),
                ~scales::comma(.,0.1)),
         across(.cols = c(`Already packed_Packout_%`,`RA to go_Packout_%`,`CA to go_Packout_%`),
                ~scales::percent(.,0.1))) |>
  flextable::flextable() |>
  flextable::separate_header() |>
  flextable::align(j=c(1:9), align = "right", part = "body") |>
  flextable::align(j=c(1:9), align = "center", part = "header") |>
  flextable::bold(bold = T, part = "header") |>
  flextable::autofit() |>
  flextable::fit_to_width(max_width=6)


POCalculation |>
  mutate(Seasonal_Input_tonnes = `Already packed_Input_tonnes`+`RA to go_Input_tonnes`+`CA to go_Input_tonnes`,
         Seasonal_Export_tonnes = `Already packed_Export_tonnes`+`RA to go_Export_tonnes`+`CA to go_Export_tonnes`,
         `Seasonal_Packout_%` = Seasonal_Export_tonnes/Seasonal_Input_tonnes,
         across(.cols = c(Seasonal_Input_tonnes,Seasonal_Export_tonnes), ~scales::comma(.,0.1)),
         `Seasonal_Packout_%` = scales::percent(`Seasonal_Packout_%`,0.1)) |>
  dplyr::select(c(Seasonal_Input_tonnes,Seasonal_Export_tonnes,`Seasonal_Packout_%`)) |>
  flextable::flextable() |>
  flextable::separate_header() |>
  flextable::align(j=c(1:3), align="right", part = "body") |>
  flextable::align(j=c(1:3), align = "center", part = "header") |>
  flextable::bold(bold = T, part = "header") |>
  flextable::autofit() 
