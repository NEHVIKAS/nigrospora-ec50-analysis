###############################################################
# EC50 ANALYSIS OF FUNGICIDE SENSITIVITY
# Nigrospora oryzae
#
# Outputs:
# 1. Replicate-specific EC50 estimates
# 2. Pooled EC50 estimate for each isolate
# 3. Dose-response curves with 95% confidence bands
# 4. EC50 forest plot
# 5. Replicate EC50 boxplot
# 6. Complete Excel report
###############################################################


# =============================================================
# 1. LOAD PACKAGES
# =============================================================

library(readxl)
library(dplyr)
library(drc)
library(ggplot2)
library(openxlsx)


# =============================================================
# 2. USER SETTINGS
# =============================================================

INPUT_FILE <- "excel name.xlsx"

SHEET_NAME <- "Sheet"

DOSE_UNIT <- "ppm"

# Change these if your positive concentrations are different
EXPECTED_DOSES <- c(
  0.1,
  1,
  10,
  100
)

# Number of horizontal/vertical measurements expected
# for each plate
EXPECTED_DIAMETER_MEASUREMENTS <- 2L


# =============================================================
# 3. CREATE OUTPUT FOLDERS
# =============================================================

if (!dir.exists("Results")) {
  dir.create("Results")
}

if (!dir.exists("Graphs")) {
  dir.create("Graphs")
}


# =============================================================
# 4. READ EXCEL FILE
# =============================================================

raw_data <- read_excel(
  INPUT_FILE,
  sheet = SHEET_NAME
)


# Examine input data

cat("\nData structure:\n")

str(raw_data)

cat("\nFirst rows:\n")

print(head(raw_data))


# =============================================================
# 5. CHECK REQUIRED COLUMNS
# =============================================================

required_columns <- c(
  "ISOLATE ID",
  "REPLICATES",
  "CONCENTRATION",
  "DIAMETER (mm)"
)

missing_columns <- setdiff(
  required_columns,
  names(raw_data)
)

if (length(missing_columns) > 0) {
  
  stop(
    "Missing columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}


# =============================================================
# 6. CLEAN AND RENAME VARIABLES
# =============================================================

data <- raw_data %>%
  transmute(
    
    Isolate = trimws(
      as.character(`ISOLATE ID`)
    ),
    
    Replicate = trimws(
      as.character(REPLICATES)
    ),
    
    Concentration = suppressWarnings(
      as.numeric(CONCENTRATION)
    ),
    
    Diameter_mm = suppressWarnings(
      as.numeric(`DIAMETER (mm)`)
    )
  )


# Identify invalid input rows

invalid_rows <- data %>%
  filter(
    is.na(Isolate) |
      Isolate == "" |
      is.na(Replicate) |
      Replicate == "" |
      !is.finite(Concentration) |
      !is.finite(Diameter_mm)
  )


if (nrow(invalid_rows) > 0) {
  
  write.xlsx(
    invalid_rows,
    "Results/Invalid_Input_Rows.xlsx",
    overwrite = TRUE
  )
  
  stop(
    "Invalid input rows were detected. ",
    "See Results/Invalid_Input_Rows.xlsx"
  )
}


# Check for impossible values

if (any(data$Concentration < 0)) {
  stop("Negative concentrations were detected.")
}

if (any(data$Diameter_mm < 0)) {
  stop("Negative colony diameters were detected.")
}


# =============================================================
# 7. AVERAGE HORIZONTAL AND VERTICAL DIAMETERS
# =============================================================

average_data <- data %>%
  group_by(
    Isolate,
    Replicate,
    Concentration
  ) %>%
  summarise(
    
    Mean_Diameter = mean(
      Diameter_mm,
      na.rm = TRUE
    ),
    
    SD_Diameter = ifelse(
      n() > 1,
      sd(Diameter_mm, na.rm = TRUE),
      NA_real_
    ),
    
    N_Diameter_Measurements = n(),
    
    .groups = "drop"
  )


# Check whether horizontal and vertical measurements are present

Measurement_Check <- average_data %>%
  filter(
    N_Diameter_Measurements !=
      EXPECTED_DIAMETER_MEASUREMENTS
  )


if (nrow(Measurement_Check) > 0) {
  
  warning(
    "Some isolate-replicate-concentration groups do not have ",
    EXPECTED_DIAMETER_MEASUREMENTS,
    " diameter measurements."
  )
}


# =============================================================
# 8. OBTAIN THE UNTREATED CONTROL
# =============================================================

# This assumes that each replicate has a matched control.

controls <- average_data %>%
  filter(
    Concentration == 0
  ) %>%
  group_by(
    Isolate,
    Replicate
  ) %>%
  summarise(
    
    Control = mean(
      Mean_Diameter,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# Check controls

Control_Check <- average_data %>%
  distinct(
    Isolate,
    Replicate
  ) %>%
  left_join(
    controls,
    by = c(
      "Isolate",
      "Replicate"
    )
  )


control_problems <- Control_Check %>%
  filter(
    !is.finite(Control) |
      Control <= 0
  )


if (nrow(control_problems) > 0) {
  
  write.xlsx(
    control_problems,
    "Results/Control_Problems.xlsx",
    overwrite = TRUE
  )
  
  stop(
    "Some replicates have missing or invalid controls. ",
    "See Results/Control_Problems.xlsx"
  )
}


# =============================================================
# 9. CALCULATE RELATIVE GROWTH AND INHIBITION
# =============================================================

normalized_data <- average_data %>%
  left_join(
    controls,
    by = c(
      "Isolate",
      "Replicate"
    )
  ) %>%
  mutate(
    
    Relative_Growth =
      100 * Mean_Diameter / Control,
    
    Percent_Inhibition =
      100 - Relative_Growth
  )


# Keep only positive concentrations for model fitting

fit_data <- normalized_data %>%
  filter(
    Concentration > 0,
    is.finite(Relative_Growth)
  )


# Do not force relative-growth values into the range 0-100.
# Values above 100 may indicate experimental variation.


# =============================================================
# 10. CHECK CONCENTRATIONS
# =============================================================

Design_Check <- fit_data %>%
  group_by(
    Isolate,
    Replicate
  ) %>%
  summarise(
    
    N_Positive_Doses = n_distinct(
      Concentration
    ),
    
    Observed_Doses = paste(
      sort(unique(Concentration)),
      collapse = ", "
    ),
    
    Complete_Dose_Set = setequal(
      round(
        unique(Concentration),
        10
      ),
      round(
        EXPECTED_DOSES,
        10
      )
    ),
    
    .groups = "drop"
  )


# Identify complete replicate dose series

Complete_Keys <- Design_Check %>%
  dplyr::filter(
    Complete_Dose_Set == TRUE
  ) %>%
  dplyr::select(
    Isolate,
    Replicate
  )


# Use only complete dose series for model fitting

model_data <- fit_data %>%
  semi_join(
    Complete_Keys,
    by = c(
      "Isolate",
      "Replicate"
    )
  )


# Record incomplete replicates

Failed_Fits <- Design_Check %>%
  filter(
    !Complete_Dose_Set
  ) %>%
  transmute(
    
    Level = "Replicate",
    
    Isolate,
    
    Replicate,
    
    Reason = paste0(
      "Incomplete concentrations. Observed doses: ",
      Observed_Doses
    )
  )


if (nrow(model_data) == 0) {
  
  stop(
    "No complete dose series were found. ",
    "Check EXPECTED_DOSES."
  )
}


# =============================================================
# 11. MODEL-FITTING FUNCTION
# =============================================================

fit_LL4 <- function(df) {
  
  drm(
    
    Relative_Growth ~ Concentration,
    
    data = df,
    
    fct = LL.4(
      fixed = c(
        NA,   # slope b estimated
        0,    # lower limit fixed at 0
        100,  # upper limit fixed at 100
        NA    # midpoint e estimated
      )
    )
  )
}


# =============================================================
# 12. FUNCTION TO EXTRACT EC50 RESULTS
# =============================================================

extract_EC50 <- function(
    model,
    df
) {
  
  # type = "absolute" means the concentration where
  # predicted relative growth is exactly 50%.
  
  ed_table <- as.matrix(
    
    ED(
      
      model,
      
      respLev = 50,
      
      type = "absolute",
      
      interval = "delta",
      
      level = 0.95,
      
      display = FALSE
    )
  )
  
  
  if (ncol(ed_table) < 4) {
    
    stop(
      "ED() did not return the expected confidence interval."
    )
  }
  
  
  # ED output columns:
  # 1 = Estimate
  # 2 = Standard error
  # 3 = Lower 95% confidence limit
  # 4 = Upper 95% confidence limit
  
  EC50 <- as.numeric(
    ed_table[1, 1]
  )
  
  EC50_SE <- as.numeric(
    ed_table[1, 2]
  )
  
  Lower95 <- as.numeric(
    ed_table[1, 3]
  )
  
  Upper95 <- as.numeric(
    ed_table[1, 4]
  )
  
  
  # Extract slope and its standard error
  
  coefficient_table <-
    summary(model)$coefficients
  
  slope_row <- grep(
    "^b:",
    rownames(coefficient_table)
  )
  
  
  if (length(slope_row) > 0) {
    
    Slope <- as.numeric(
      coefficient_table[
        slope_row[1],
        "Estimate"
      ]
    )
    
    Slope_SE <- as.numeric(
      coefficient_table[
        slope_row[1],
        "Std. Error"
      ]
    )
    
  } else {
    
    Slope <- NA_real_
    
    Slope_SE <- NA_real_
  }
  
  
  minimum_dose <- min(
    df$Concentration,
    na.rm = TRUE
  )
  
  maximum_dose <- max(
    df$Concentration,
    na.rm = TRUE
  )
  
  
  brackets_50 <-
    min(df$Relative_Growth, na.rm = TRUE) <= 50 &&
    max(df$Relative_Growth, na.rm = TRUE) >= 50
  
  
  extrapolated <-
    !is.finite(EC50) ||
    EC50 < minimum_dose ||
    EC50 > maximum_dose
  
  
  fitted_values <- as.numeric(
    fitted(model)
  )
  
  residual_values <-
    df$Relative_Growth - fitted_values
  
  RMSE <- sqrt(
    mean(
      residual_values^2,
      na.rm = TRUE
    )
  )
  
  
  data.frame(
    
    EC50 = EC50,
    
    EC50_SE = EC50_SE,
    
    Lower95 = Lower95,
    
    Upper95 = Upper95,
    
    Slope = Slope,
    
    Slope_SE = Slope_SE,
    
    LowerLimit = 0,
    
    UpperLimit = 100,
    
    AIC = as.numeric(
      AIC(model)
    ),
    
    RMSE = RMSE,
    
    N_Observations = nrow(df),
    
    N_Replicates = n_distinct(
      df$Replicate
    ),
    
    Minimum_Tested_Dose = minimum_dose,
    
    Maximum_Tested_Dose = maximum_dose,
    
    Response_Brackets_50 = brackets_50,
    
    EC50_Extrapolated = extrapolated,
    
    CI_Usable =
      is.finite(Lower95) &&
      is.finite(Upper95) &&
      Lower95 > 0 &&
      Upper95 > 0,
    
    stringsAsFactors = FALSE
  )
}


# =============================================================
# 13. REPLICATE-SPECIFIC EC50 CALCULATION
# =============================================================

replicate_result_rows <- list()


for (i in seq_len(nrow(Complete_Keys))) {
  
  iso <- Complete_Keys$Isolate[i]
  
  rep_value <- Complete_Keys$Replicate[i]
  
  
  cat(
    "\nFitting replicate model:",
    iso,
    "- replicate",
    rep_value,
    "\n"
  )
  
  
  temp <- model_data %>%
    filter(
      Isolate == iso,
      Replicate == rep_value
    )
  
  
  model <- tryCatch(
    
    fit_LL4(temp),
    
    error = function(e) e
  )
  
  
  if (inherits(model, "error")) {
    
    Failed_Fits <- bind_rows(
      
      Failed_Fits,
      
      data.frame(
        
        Level = "Replicate",
        
        Isolate = iso,
        
        Replicate = rep_value,
        
        Reason = paste0(
          "Model failed: ",
          conditionMessage(model)
        )
      )
    )
    
    next
  }
  
  
  result <- tryCatch(
    
    extract_EC50(
      model,
      temp
    ),
    
    error = function(e) e
  )
  
  
  if (inherits(result, "error")) {
    
    Failed_Fits <- bind_rows(
      
      Failed_Fits,
      
      data.frame(
        
        Level = "Replicate",
        
        Isolate = iso,
        
        Replicate = rep_value,
        
        Reason = paste0(
          "EC50 calculation failed: ",
          conditionMessage(result)
        )
      )
    )
    
    next
  }
  
  
  replicate_result_rows[[
    length(replicate_result_rows) + 1
  ]] <- bind_cols(
    
    data.frame(
      
      Isolate = iso,
      
      Replicate = rep_value,
      
      stringsAsFactors = FALSE
    ),
    
    result
  )
}


if (length(replicate_result_rows) > 0) {
  
  Replicate_EC50 <- bind_rows(
    replicate_result_rows
  )
  
} else {
  
  Replicate_EC50 <- data.frame(
    
    Isolate = character(),
    
    Replicate = character(),
    
    EC50 = numeric(),
    
    EC50_SE = numeric(),
    
    Lower95 = numeric(),
    
    Upper95 = numeric()
  )
}


# =============================================================
# 14. POOLED ISOLATE EC50 CALCULATION
# =============================================================

# The pooled model uses all valid replicate observations for
# each isolate. This should be the primary EC50 reported in the
# main table and forest plot.

pooled_result_rows <- list()

prediction_rows <- list()


for (iso in sort(unique(model_data$Isolate))) {
  
  cat(
    "\nFitting pooled isolate model:",
    iso,
    "\n"
  )
  
  
  temp <- model_data %>%
    filter(
      Isolate == iso
    )
  
  
  model <- tryCatch(
    
    fit_LL4(temp),
    
    error = function(e) e
  )
  
  
  if (inherits(model, "error")) {
    
    Failed_Fits <- bind_rows(
      
      Failed_Fits,
      
      data.frame(
        
        Level = "Pooled isolate",
        
        Isolate = iso,
        
        Replicate = NA_character_,
        
        Reason = paste0(
          "Model failed: ",
          conditionMessage(model)
        )
      )
    )
    
    next
  }
  
  
  result <- tryCatch(
    
    extract_EC50(
      model,
      temp
    ),
    
    error = function(e) e
  )
  
  
  if (inherits(result, "error")) {
    
    Failed_Fits <- bind_rows(
      
      Failed_Fits,
      
      data.frame(
        
        Level = "Pooled isolate",
        
        Isolate = iso,
        
        Replicate = NA_character_,
        
        Reason = paste0(
          "EC50 calculation failed: ",
          conditionMessage(result)
        )
      )
    )
    
    next
  }
  
  
  pooled_result_rows[[
    length(pooled_result_rows) + 1
  ]] <- bind_cols(
    
    data.frame(
      
      Isolate = iso,
      
      stringsAsFactors = FALSE
    ),
    
    result
  )
  
  
  # -----------------------------------------------------------
  # Prediction curve and confidence band
  # -----------------------------------------------------------
  
  newdose <- data.frame(
    
    Concentration = 10^seq(
      
      from = log10(
        min(temp$Concentration)
      ),
      
      to = log10(
        max(temp$Concentration)
      ),
      
      length.out = 250
    )
  )
  
  
  predicted <- tryCatch(
    
    as.data.frame(
      
      predict(
        
        model,
        
        newdata = newdose,
        
        interval = "confidence",
        
        level = 0.95
      )
    ),
    
    error = function(e) e
  )
  
  
  if (inherits(predicted, "error")) {
    
    Failed_Fits <- bind_rows(
      
      Failed_Fits,
      
      data.frame(
        
        Level = "Prediction",
        
        Isolate = iso,
        
        Replicate = NA_character_,
        
        Reason = paste0(
          "Prediction failed: ",
          conditionMessage(predicted)
        )
      )
    )
    
    next
  }
  
  
  if (ncol(predicted) >= 4) {
    
    prediction_rows[[
      length(prediction_rows) + 1
    ]] <- data.frame(
      
      Isolate = iso,
      
      Concentration =
        newdose$Concentration,
      
      Predicted =
        predicted[[1]],
      
      Prediction_SE =
        predicted[[2]],
      
      Lower =
        predicted[[3]],
      
      Upper =
        predicted[[4]]
    )
  }
}


if (length(pooled_result_rows) == 0) {
  
  stop(
    "No pooled isolate models were fitted successfully."
  )
}


Pooled_Isolate_EC50 <- bind_rows(
  pooled_result_rows
)


if (length(prediction_rows) > 0) {
  
  Prediction_Data <- bind_rows(
    prediction_rows
  )
  
} else {
  
  Prediction_Data <- data.frame(
    
    Isolate = character(),
    
    Concentration = numeric(),
    
    Predicted = numeric(),
    
    Prediction_SE = numeric(),
    
    Lower = numeric(),
    
    Upper = numeric()
  )
}


# =============================================================
# 15. SUMMARIZE REPLICATE-SPECIFIC EC50
# =============================================================

if (nrow(Replicate_EC50) > 0) {
  
  Replicate_Summary <- Replicate_EC50 %>%
    group_by(
      Isolate
    ) %>%
    summarise(
      
      Replicate_n = n(),
      
      Mean_Replicate_EC50 = mean(
        EC50,
        na.rm = TRUE
      ),
      
      SD_Replicate_EC50 = ifelse(
        n() > 1,
        sd(EC50, na.rm = TRUE),
        NA_real_
      ),
      
      SE_Replicate_EC50 = ifelse(
        n() > 1,
        sd(EC50, na.rm = TRUE) /
          sqrt(n()),
        NA_real_
      ),
      
      Median_Replicate_EC50 = median(
        EC50,
        na.rm = TRUE
      ),
      
      Geometric_Mean_EC50 = ifelse(
        
        all(
          is.finite(EC50) &
            EC50 > 0
        ),
        
        exp(
          mean(
            log(EC50)
          )
        ),
        
        NA_real_
      ),
      
      Minimum_Replicate_EC50 = min(
        EC50,
        na.rm = TRUE
      ),
      
      Maximum_Replicate_EC50 = max(
        EC50,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
  
} else {
  
  Replicate_Summary <- data.frame(
    
    Isolate = character(),
    
    Replicate_n = integer(),
    
    Mean_Replicate_EC50 = numeric(),
    
    SD_Replicate_EC50 = numeric()
  )
}


# Combine pooled and replicate results

Final_Summary <- Pooled_Isolate_EC50 %>%
  left_join(
    Replicate_Summary,
    by = "Isolate"
  )


# =============================================================
# 16. PREPARE MEAN DATA FOR PLOTTING
# =============================================================

plot_data <- model_data %>%
  group_by(
    Isolate,
    Concentration
  ) %>%
  summarise(
    
    Mean_RG = mean(
      Relative_Growth,
      na.rm = TRUE
    ),
    
    SD_RG = ifelse(
      n() > 1,
      sd(Relative_Growth, na.rm = TRUE),
      NA_real_
    ),
    
    N = n(),
    
    SE_RG = SD_RG / sqrt(N),
    
    .groups = "drop"
  )


# Labels use EC50 from the same pooled model used for the curve

EC50_labels <- Pooled_Isolate_EC50 %>%
  mutate(
    
    Label = sprintf(
      
      paste0(
        "EC50 = %.3g ",
        DOSE_UNIT,
        "\n95%% CI: %.3g–%.3g"
      ),
      
      EC50,
      Lower95,
      Upper95
    ),
    
    Label_X = Maximum_Tested_Dose
  )


# =============================================================
# 17. PUBLICATION DOSE-RESPONSE FIGURE
# =============================================================

p_curve <- ggplot() +
  
  # Confidence band
  
  geom_ribbon(
    
    data = Prediction_Data,
    
    aes(
      x = Concentration,
      ymin = Lower,
      ymax = Upper
    ),
    
    fill = "#56B4E9",
    
    alpha = 0.20,
    
    na.rm = TRUE
  ) +
  
  # Fitted curve
  
  geom_line(
    
    data = Prediction_Data,
    
    aes(
      x = Concentration,
      y = Predicted
    ),
    
    colour = "#0072B2",
    
    linewidth = 1
  ) +
  
  # Individual replicate observations
  
  geom_point(
    
    data = model_data,
    
    aes(
      x = Concentration,
      y = Relative_Growth
    ),
    
    size = 1.6,
    
    colour = "grey45",
    
    alpha = 0.45
  ) +
  
  # Mean +/- standard error
  
  geom_linerange(
    
    data = plot_data,
    
    aes(
      x = Concentration,
      ymin = Mean_RG - SE_RG,
      ymax = Mean_RG + SE_RG
    ),
    
    colour = "black",
    
    linewidth = 0.5,
    
    na.rm = TRUE
  ) +
  
  geom_point(
    
    data = plot_data,
    
    aes(
      x = Concentration,
      y = Mean_RG
    ),
    
    shape = 21,
    
    size = 2.8,
    
    stroke = 0.7,
    
    colour = "black",
    
    fill = "white"
  ) +
  
  # Fifty-percent response line
  
  geom_hline(
    
    yintercept = 50,
    
    linetype = "dashed",
    
    colour = "grey35"
  ) +
  
  # EC50 vertical line
  
  geom_vline(
    
    data = Pooled_Isolate_EC50,
    
    aes(
      xintercept = EC50
    ),
    
    colour = "#D55E00",
    
    linetype = "dotted",
    
    linewidth = 0.8
  ) +
  
  # EC50 labels
  
  geom_text(
    
    data = EC50_labels,
    
    aes(
      x = Label_X,
      y = Inf,
      label = Label
    ),
    
    hjust = 1.05,
    
    vjust = 1.2,
    
    size = 3.2
  ) +
  
  facet_wrap(
    
    ~ Isolate,
    
    ncol = 3
  ) +
  
  scale_x_log10(
    
    breaks = EXPECTED_DOSES,
    
    minor_breaks = NULL
  ) +
  
  scale_y_continuous(
    
    expand = expansion(
      mult = c(
        0.05,
        0.18
      )
    )
  ) +
  
  labs(
    
    x = paste0(
      "Fungicide concentration (",
      DOSE_UNIT,
      ")"
    ),
    
    y = "Relative mycelial growth (%)",
    
    caption = paste(
      "Grey points = individual replicates;",
      "open points = mean ± SE;",
      "blue line = pooled LL.4 model;",
      "blue band = 95% confidence interval;",
      "orange line = EC50."
    )
  ) +
  
  theme_bw(
    base_size = 12
  ) +
  
  theme(
    
    panel.grid.minor =
      element_blank(),
    
    strip.background =
      element_rect(
        fill = "grey92",
        colour = "black"
      ),
    
    strip.text =
      element_text(
        face = "bold"
      ),
    
    axis.text =
      element_text(
        colour = "black"
      )
  )


print(p_curve)


# Save dose-response figure

ggsave(
  
  "Graphs/Dose_Response_All_Isolates.pdf",
  
  plot = p_curve,
  
  width = 12,
  
  height = 10
)


ggsave(
  
  "Graphs/Dose_Response_All_Isolates.png",
  
  plot = p_curve,
  
  width = 12,
  
  height = 10,
  
  dpi = 600
)


ggsave(
  
  "Graphs/Dose_Response_All_Isolates.tiff",
  
  plot = p_curve,
  
  width = 12,
  
  height = 10,
  
  dpi = 600,
  
  compression = "lzw"
)


# =============================================================
# 18. EC50 FOREST PLOT
# =============================================================

forest_data <- Pooled_Isolate_EC50 %>%
  filter(
    is.finite(EC50),
    EC50 > 0
  ) %>%
  arrange(
    EC50
  ) %>%
  mutate(
    
    Isolate_Plot = factor(
      Isolate,
      levels = Isolate
    )
  )


forest_order <- levels(
  forest_data$Isolate_Plot
)


forest_CI_data <- forest_data %>%
  filter(
    is.finite(Lower95),
    is.finite(Upper95),
    Lower95 > 0,
    Upper95 > 0
  )


p_forest <- ggplot() +
  
  geom_segment(
    
    data = forest_CI_data,
    
    aes(
      x = Lower95,
      xend = Upper95,
      y = Isolate_Plot,
      yend = Isolate_Plot,
      colour = EC50_Extrapolated
    ),
    
    linewidth = 0.8
  )


# Add replicate-specific EC50 points

if (nrow(Replicate_EC50) > 0) {
  
  replicate_forest_data <- Replicate_EC50 %>%
    filter(
      Isolate %in% forest_order,
      is.finite(EC50),
      EC50 > 0
    ) %>%
    mutate(
      
      Isolate_Plot = factor(
        Isolate,
        levels = forest_order
      )
    )
  
  
  p_forest <- p_forest +
    
    geom_jitter(
      
      data = replicate_forest_data,
      
      aes(
        x = EC50,
        y = Isolate_Plot
      ),
      
      width = 0,
      
      height = 0.10,
      
      size = 1.7,
      
      colour = "grey40",
      
      alpha = 0.55
    )
}


p_forest <- p_forest +
  
  geom_point(
    
    data = forest_data,
    
    aes(
      x = EC50,
      y = Isolate_Plot,
      colour = EC50_Extrapolated
    ),
    
    shape = 18,
    
    size = 3.5
  ) +
  
  scale_x_log10() +
  
  scale_colour_manual(
    
    values = c(
      
      "FALSE" = "#0072B2",
      
      "TRUE" = "#D55E00"
    ),
    
    labels = c(
      
      "FALSE" =
        "EC50 within tested range",
      
      "TRUE" =
        "EC50 outside tested range"
    ),
    
    name = NULL
  ) +
  
  labs(
    
    x = paste0(
      "EC50 (",
      DOSE_UNIT,
      "; logarithmic scale)"
    ),
    
    y = "Isolate",
    
    caption = paste(
      "Diamonds = pooled isolate EC50;",
      "horizontal lines = 95% confidence intervals;",
      "grey points = replicate-specific EC50 estimates."
    )
  ) +
  
  theme_bw(
    base_size = 12
  ) +
  
  theme(
    
    panel.grid.minor =
      element_blank(),
    
    legend.position =
      "bottom"
  )


print(p_forest)


forest_height <- max(
  5,
  0.28 * nrow(forest_data)
)


ggsave(
  
  "Graphs/EC50_Forest_All_Isolates.pdf",
  
  plot = p_forest,
  
  width = 7,
  
  height = forest_height
)


ggsave(
  
  "Graphs/EC50_Forest_All_Isolates.tiff",
  
  plot = p_forest,
  
  width = 7,
  
  height = forest_height,
  
  dpi = 600,
  
  compression = "lzw"
)


# =============================================================
# 19. REPLICATE EC50 BOXPLOT
# =============================================================

boxplot_data <- Replicate_EC50 %>%
  filter(
    is.finite(EC50),
    EC50 > 0
  )


if (nrow(boxplot_data) > 0) {
  
  p_boxplot <- ggplot(
    
    boxplot_data,
    
    aes(
      x = reorder(
        Isolate,
        EC50,
        FUN = median
      ),
      y = EC50
    )
  ) +
    
    geom_boxplot(
      
      width = 0.55,
      
      outlier.shape = NA,
      
      fill = "#D9EAF7",
      
      colour = "black"
    ) +
    
    geom_jitter(
      
      width = 0.12,
      
      height = 0,
      
      size = 2,
      
      alpha = 0.70,
      
      colour = "#0072B2"
    ) +
    
    scale_y_log10() +
    
    labs(
      
      x = "Isolate",
      
      y = paste0(
        "Replicate-specific EC50 (",
        DOSE_UNIT,
        "; logarithmic scale)"
      )
    ) +
    
    theme_bw(
      base_size = 12
    ) +
    
    theme(
      
      panel.grid.minor =
        element_blank(),
      
      axis.text.x =
        element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        )
    )
  
  
  print(p_boxplot)
  
  
  ggsave(
    
    "Graphs/EC50_Replicate_Boxplot.pdf",
    
    plot = p_boxplot,
    
    width = 12,
    
    height = 6
  )
  
  
  ggsave(
    
    "Graphs/EC50_Replicate_Boxplot.tiff",
    
    plot = p_boxplot,
    
    width = 12,
    
    height = 6,
    
    dpi = 600,
    
    compression = "lzw"
  )
}


# =============================================================
# 20. EXPORT INDIVIDUAL TABLES
# =============================================================

write.xlsx(
  
  Replicate_EC50,
  
  "Results/EC50_Replicate_Results.xlsx",
  
  overwrite = TRUE
)


write.xlsx(
  
  Pooled_Isolate_EC50,
  
  "Results/EC50_Pooled_Isolate_Results.xlsx",
  
  overwrite = TRUE
)


write.xlsx(
  
  Final_Summary,
  
  "Results/EC50_Final_Summary.xlsx",
  
  overwrite = TRUE
)


write.xlsx(
  
  Failed_Fits,
  
  "Results/Failed_Fits.xlsx",
  
  overwrite = TRUE
)


# =============================================================
# 21. COMPLETE EXCEL WORKBOOK
# =============================================================

complete_output <- list(
  
  Pooled_Isolate_EC50 =
    Pooled_Isolate_EC50,
  
  Replicate_EC50 =
    Replicate_EC50,
  
  Final_Summary =
    Final_Summary,
  
  Failed_Fits =
    Failed_Fits,
  
  Design_Check =
    Design_Check,
  
  Measurement_Check =
    Measurement_Check,
  
  Normalized_Data =
    normalized_data,
  
  Prediction_Data =
    Prediction_Data
)


write.xlsx(
  
  complete_output,
  
  "Results/Complete_EC50_Report.xlsx",
  
  overwrite = TRUE
)


# Save R and package-version information

capture.output(
  
  sessionInfo(),
  
  file =
    "Results/R_Session_Information.txt"
)


# =============================================================
# 22. FINISHED
# =============================================================

cat("\n")

cat("=================================================\n")

cat("EC50 ANALYSIS COMPLETED SUCCESSFULLY\n")

cat("=================================================\n")

cat("Main results:\n")

cat(
  "1. Results/EC50_Pooled_Isolate_Results.xlsx\n"
)

cat(
  "2. Results/EC50_Replicate_Results.xlsx\n"
)

cat(
  "3. Results/EC50_Final_Summary.xlsx\n"
)

cat(
  "4. Results/Complete_EC50_Report.xlsx\n"
)

cat("\nFigures:\n")

cat(
  "1. Graphs/Dose_Response_All_Isolates.pdf\n"
)

cat(
  "2. Graphs/Dose_Response_All_Isolates.tiff\n"
)

cat(
  "3. Graphs/EC50_Forest_All_Isolates.pdf\n"
)

cat(
  "4. Graphs/EC50_Replicate_Boxplot.pdf\n"
)

cat("=================================================\n")

