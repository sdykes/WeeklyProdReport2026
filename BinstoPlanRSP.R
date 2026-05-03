library(tidyverse)
library(minpack.lm)

# Drop the single X (Pressure) observation or lump it
df <- PackOutPlotSumRaw %>%
  filter(Storage.type != "CA Smartfresh",
         RSP != "X (Pressure)") %>%
  mutate(RSP = factor(RSP, levels = c("A", "B", "C")))  # A is reference

# One theta per RSP level (intercept + 2 contrasts)
NLmodel_RSP_theta <- nls(
  Packout1 ~ alpha * exp(beta * StorageDays) + 
    theta_A * (RSP == "A") + 
    theta_B * (RSP == "B") + 
    theta_C * (RSP == "C"),
  data = df,
  control = nls.control(maxiter = 1000),
  start = list(alpha = 0.1, beta = -0.1, 
               theta_A = 0.60, theta_B = 0.58, theta_C = 0.62)
)

summary(NLmodel_RSP_theta)



NLmodel_RSP_full <- nls(
  Packout1 ~ (alpha + alpha_dB * (RSP == "B") + alpha_dC * (RSP == "C")) *
    exp(beta * StorageDays) +
    (theta + theta_dB * (RSP == "B") + theta_dC * (RSP == "C")),
  data = df,
  control = nls.control(maxiter = 1000),
  start = list(alpha = 0.1, beta = -0.1, theta = 0.60,
               alpha_dB = 0, alpha_dC = 0,
               theta_dB = 0, theta_dC = 0)
)

summary(NLmodel_RSP_full)

library(dplyr)
library(ggplot2)
library(minpack.lm)  # or use nls if converged

# --- Fit the model ---
df <- PackOutPlotSumRaw %>%
  filter(Storage.type != "CA Smartfresh",
         RSP != "X (Pressure)") %>%
  mutate(RSP = factor(RSP, levels = c("A", "B", "C")))

NLmodel_RSP_theta <- nls(
  Packout1 ~ alpha * exp(beta * StorageDays) + theta +
    delta_B * (RSP == "B") + delta_C * (RSP == "C"),
  data = df,
  control = nls.control(maxiter = 1000),
  start = list(alpha = 0.1, beta = -0.1, theta = 0.60,
               delta_B = 0, delta_C = 0)
)

# --- Extract coefficients ---
cf <- coef(NLmodel_RSP_theta)
# cf["alpha"], cf["beta"], cf["theta"], cf["delta_B"], cf["delta_C"]

# --- Build prediction grid ---
pred_grid <- expand.grid(
  StorageDays = seq(1, 50, length.out = 200),
  RSP = factor(c("A", "B", "C"), levels = c("A", "B", "C"))
) %>%
  mutate(
    theta_eff = cf["theta"] +
      cf["delta_B"] * (RSP == "B") +
      cf["delta_C"] * (RSP == "C"),
    Packout_fit = cf["alpha"] * exp(cf["beta"] * StorageDays) + theta_eff
  )

rsp_colours <- c("A" = "#2166ac", "B" = "#f4a582", "C" = "#d6604d")

ggplot() +
  # Raw data — semi-transparent points
  geom_point(data = df,
             aes(x = StorageDays, y = Packout1, colour = RSP),
             alpha = 0.35, size = 1.8) +
  # Fitted curves
  geom_line(data = pred_grid,
            aes(x = StorageDays, y = Packout_fit, colour = RSP),
            linewidth = 1.1) +
  # RSP asymptote reference lines
  geom_hline(data = pred_grid %>% 
               group_by(RSP) %>% 
               slice_tail(n = 1),   # theta_eff at max days ≈ asymptote
             aes(yintercept = theta_eff, colour = RSP),
             linetype = "dashed", linewidth = 0.5, alpha = 0.6) +
  scale_colour_manual(values = rsp_colours) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.2, 1.0)) +
  labs(
    title    = "Packout decay by RSP profile",
    subtitle = "Exponential decay model with RSP-specific asymptote (θ)",
    x        = "Storage days",
    y        = "Packout (%)",
    colour   = "RSP"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggsave("PackoutModelByRSP.png",width = 10, height = 7)

#
# Establish reminaing bins by storage type and RSP  
#


BinSRemBySTAndRSP  <- glue("
SELECT 
CASE
WHEN bd.StorageTypeID = 4 THEN 'CA'
ELSE 'RA'
END AS [Storage type]
,ma.MaturityCode AS RSP
,SUM(bu.BinQty) AS BinQty 
FROM ma_Bin_UsageT AS bu
LEFT JOIN 
ma_Bin_DeliveryT AS bd
ON bd.BinDeliveryID = bu.BinDeliveryID
LEFT JOIN 
ma_Grader_BatchT AS gb
ON gb.GraderBatchID = bu.GraderBatchID
LEFT JOIN
sw_MaturityT AS ma
ON ma.MaturityID = bd.MaturityID
WHERE bd.SeasonID = 2013
AND bd.PresizeFlag = 0
AND 
(
  bu.GraderBatchID IS NULL
  OR gb.PackDate > '{cutoff_date}'
)
GROUP BY
CASE
WHEN bd.StorageTypeID = 4 THEN 'CA'
ELSE 'RA'
END
,ma.MaturityCode
")

con <- DBI::dbConnect(odbc::odbc(),    
                     Driver = "ODBC Driver 18 for SQL Server", 
                     Server = "abcrepldb.database.windows.net",  
                     Database = "ABCPackerRepl",   
                     UID = "abcadmin",   
                     PWD = "Trauts2018!",
                     Port = 1433
)

BinsRemainingSTAndRSP <- DBI::dbGetQuery(con, BinSRemBySTAndRSP) 
  
DBI::dbDisconnect(con)

BinsRemainingSTAndRSP <- BinsRemainingSTAndRSP |>
  arrange(`Storage type`,RSP)

BinTipPlanRSP <- PackingPlan2026 |>
  mutate(TotalBins = `Te Ipu`+Freshco+`Green planet`+Sunfruit,
         RABinsTippedC = 0,
         RABinsTippedB = 0,
         RABinsTippedA = 0,
         RemainingRABinsC = 0,
         RemainingRABinsB = 0,
         RemainingRABinsA = 0,
         CABinsTippedC = 0,
         CABinsTippedB = 0,
         CABinsTippedA = 0,
         RemainingCABinsC = 0,
         RemainingCABinsB = 0,
         RemainingCABinsA = 0
         )

#write_csv(BinTipPlanRSP,"BinTipPlanRSP.csv")

RemainingRA <- tibble(C = BinsRemainingSTAndRSP$BinQty[[6]],
                      B = BinsRemainingSTAndRSP$BinQty[[5]],
                      A = BinsRemainingSTAndRSP$BinQty[[4]])

#write_csv(RemainingRA,"RemainingRA.csv")

RemainingCA <- tibble(C = BinsRemainingSTAndRSP$BinQty[[3]],
                      B = BinsRemainingSTAndRSP$BinQty[[2]],
                      A = BinsRemainingSTAndRSP$BinQty[[1]])

#write_csv(RemainingCA,"RemainingCA.csv")

# ── Opening stocks ─────────────────────────────────────────────────────────────
ra_open <- c(C = RemainingRA$C, B = RemainingRA$B, A = RemainingRA$A)
ca_open <- c(C = RemainingCA$C, B = RemainingCA$B, A = RemainingCA$A)

# ── Helper: draw bins sequentially from C→B→A within a pool ───────────────────
# Returns a list: tipped = named vector (C,B,A), remaining = named vector (C,B,A)
draw_bins <- function(pool, demand) {
  tipped <- c(C = 0, B = 0, A = 0)
  for (grade in c("C", "B", "A")) {
    take        <- min(pool[grade], demand)
    tipped[grade] <- take
    pool[grade]   <- pool[grade] - take
    demand        <- demand - take
    if (demand == 0) break
  }
  list(tipped = tipped, remaining = pool, leftover = demand)
}

# ── Row-by-row simulation ──────────────────────────────────────────────────────
ra_pool <- ra_open
ca_pool <- ca_open

results <- vector("list", nrow(BinTipPlanRSP))

for (i in seq_len(nrow(BinTipPlanRSP))) {
  demand <- BinTipPlanRSP$TotalBins[i]
  
  # 1. Draw from RA first
  ra_draw  <- draw_bins(ra_pool, demand)
  ra_pool  <- ra_draw$remaining
  leftover <- ra_draw$leftover   # unmet demand after RA exhausted
  
  # 2. Draw remainder from CA
  ca_draw  <- draw_bins(ca_pool, leftover)
  ca_pool  <- ca_draw$remaining
  
  results[[i]] <- list(
    RABinsTippedC   = ra_draw$tipped["C"],
    RABinsTippedB   = ra_draw$tipped["B"],
    RABinsTippedA   = ra_draw$tipped["A"],
    RemainingRABinsC = ra_pool["C"],
    RemainingRABinsB = ra_pool["B"],
    RemainingRABinsA = ra_pool["A"],
    CABinsTippedC   = ca_draw$tipped["C"],
    CABinsTippedB   = ca_draw$tipped["B"],
    CABinsTippedA   = ca_draw$tipped["A"],
    RemainingCABinsC = ca_pool["C"],
    RemainingCABinsB = ca_pool["B"],
    RemainingCABinsA = ca_pool["A"]
  )
}

# ── Bind results back into plan ────────────────────────────────────────────────
res_df <- bind_rows(lapply(results, as.data.frame))

# Strip any named-vector suffixes (e.g. "C.C" → "C") that bind_rows can produce
names(res_df) <- sub("\\.[A-C]$", "", names(res_df))

PackProgrammeToGo <- BinTipPlanRSP |>
  dplyr::select(`ISO Week`, `Te Ipu`, Freshco, `Green planet`, Sunfruit, TotalBins) |>
  bind_cols(res_df) |>
  mutate(
    RemainingRABins = RemainingRABinsC + RemainingRABinsB + RemainingRABinsA,
    RABinsTipped    = RABinsTippedC    + RABinsTippedB    + RABinsTippedA
  )

#an_out, "BinTipPlan_calculated.csv")
#print(plan_out)

PackProgrammeToGo |>
  dplyr::select(c(`ISO Week`,RemainingRABinsC,RemainingRABinsB,RemainingRABinsA,
           RemainingCABinsC,RemainingCABinsB,RemainingCABinsA)) |>
  pivot_longer(cols = -c(`ISO Week`),
               names_to = "Remaining",
               values_to = "Bins") |>
  mutate(RSP = factor(str_sub(Remaining,-1,-1)),
         `Storage type` = factor(str_sub(Remaining,10,11), levels = c("RA","CA"))) |>
  ggplot(aes(x=`ISO Week`, y=Bins, colour=RSP)) +
  geom_line(linewidth=1) +
  facet_wrap(~`Storage type`, scales = "free_y") +
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
#
# Model the CA
#
GBVCA2425 <- GBV2024NP |>
  filter(Presize == "Field bin",
         BatchStatus == "Closed",
         Storage.type == "CA Smartfresh",
         Season %in% c("2024","2025")) |>
  mutate(StorageDays = as.numeric(PackDate-HarvestDate)) |>
  filter(StorageDays < 225 & StorageDays > 150) |>
  filter(Season == "2025") |>
  mutate(RSP = if_else(RSP == "B2","B",RSP))

CA_lm <- lm(Packout1 ~ StorageDays+RSP, data = GBVCA2425)

summary(CA_lm)

#################################################################################
#   Conclusion is there is no significant difference in RSP at CA level         #
#################################################################################

#################################################################################
#   Below is a function to apply model to calculate the aggregate input and     #
#   export kgs for each week of RA Packing                                      #
#################################################################################

FuturePackSummary <- function(PackProgrammeToGo,WAHarvestDate,WAMassPerBin,NLmodel_RSP_theta) {
  
  FuturePackDataRA <- PackProgrammeToGo |>
    mutate(RABinsTipped = RABinsTippedC+RABinsTippedB+RABinsTippedA) |>
    filter(RABinsTipped > 0) |>
    mutate(weekdate = str_c("2026-W",`ISO Week`,"-3"),
           PackDate = ISOweek::ISOweek2date(weekdate),
           StorageDays = as.integer(PackDate-WAHD$WAHD[[1]]),
           InputKgsC = RABinsTippedC*WAMPB$WAMPB[[1]],
           InputKgsB = RABinsTippedB*WAMPB$WAMPB[[1]],
           InputKgsA = RABinsTippedA*WAMPB$WAMPB[[1]])
  
  SDPO <- FuturePackDataRA |>
    pull(StorageDays)
  
  newdata <- expand_grid(StorageDays = SDPO, RSP=c("A","B","C")) |>
    mutate(RSP = factor(RSP, levels = c("A", "B", "C")))
  
  Packout <- tibble(Packout1 = predict(NLmodel_RSP_theta, newdata = newdata)) |>
    bind_cols(newdata) |>
    pivot_wider(id_cols = StorageDays,
                names_from = RSP,
                values_from = Packout1) 
  
  FinalFuturePackDatesRA <- FuturePackDataRA |>
    left_join(Packout, by = "StorageDays") |>
    mutate(InputKgs = InputKgsA + InputKgsB + InputKgsC,
           ExportKgs = C*InputKgsC + B*InputKgsB + A*InputKgsA) |>
    dplyr::select(c(PackDate,StorageDays,RABinsTippedC,RABinsTippedB,RABinsTippedA,
                    InputKgs,ExportKgs)) 
  
  return(FinalFuturePackDatesRA)
  
}

FPSRA <- FuturePackSummary(PackProgrammeToGo,WAHD,WAMPB,NLmodel_RSP_theta)

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
