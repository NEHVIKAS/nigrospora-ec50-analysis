# Nigrospora Fungicide EC50 Analysis

R-based workflow for estimating the effective concentration required to reduce *Nigrospora oryzae* mycelial growth by 50% (EC50) in fungicide sensitivity assays.

The workflow uses the `drc` package and four-parameter logistic dose-response modelling (LL.4) to calculate replicate-specific and pooled isolate EC50 estimates.

## Overview

This script was developed to analyse fungicide sensitivity experiments in *Nigrospora oryzae*.

The workflow:

* Reads colony-growth measurements from an Excel workbook.
* Calculates mean colony diameter from horizontal and vertical measurements.
* Uses untreated controls (0 concentration) to calculate relative mycelial growth.
* Checks the experimental concentration series.
* Fits LL.4 dose-response models.
* Estimates EC50 values with 95% confidence intervals.
* Calculates replicate-specific EC50 estimates.
* Calculates a pooled EC50 estimate for each isolate.
* Generates publication-oriented dose-response curves.
* Generates an EC50 forest plot.
* Generates a replicate EC50 boxplot.
* Exports detailed results and diagnostic information to Excel.

## Experimental Design

The analysis is designed for fungicide sensitivity assays in which colony diameter is measured at multiple fungicide concentrations.

The default positive concentration series specified in the script is:

| Concentration | Unit |
| ------------: | ---- |
|           0.1 | ppm  |
|             1 | ppm  |
|            10 | ppm  |
|           100 | ppm  |

An untreated control at 0 ppm is required for each isolate-replicate combination.

The script expects two colony-diameter measurements per plate, corresponding to horizontal and vertical measurements. These are averaged before further analysis.

## Data Requirements

The input Excel sheet must contain the following columns:

* `ISOLATE ID`
* `REPLICATES`
* `CONCENTRATION`
* `DIAMETER (mm)`

Before running the analysis, edit the following settings in the R script:

```r
INPUT_FILE <- "excel name.xlsx"

SHEET_NAME <- "Sheet"

DOSE_UNIT <- "ppm"
```

The expected positive concentrations can also be changed through:

```r
EXPECTED_DOSES <- c(
  0.1,
  1,
  10,
  100
)
```

## Data Processing

For each isolate, replicate, and concentration, the script calculates:

* Mean colony diameter
* Standard deviation
* Number of diameter measurements

The untreated control is then used to normalize growth:

```text
Relative Growth (%) =
100 × Mean Diameter / Control Diameter
```

Percent inhibition is also calculated:

```text
Percent Inhibition (%) =
100 − Relative Growth
```

The 0 ppm control is excluded from dose-response model fitting because the model uses the logarithmically spaced positive concentrations.

Relative-growth values are not artificially constrained to the range 0–100%, allowing experimental variation to remain visible in the analysis.

## Dose-Response Model

The script uses the `drc` package and an LL.4 dose-response model.

For model fitting, the lower and upper asymptotes are fixed at:

* Lower limit = 0
* Upper limit = 100

The slope and EC50 parameter are estimated from the data.

The EC50 is calculated using:

```r
ED(
  model,
  respLev = 50,
  type = "absolute",
  interval = "delta",
  level = 0.95
)
```

The resulting EC50 estimate represents the fungicide concentration corresponding to 50% relative mycelial growth under the fitted model.

## Model Quality and Diagnostics

The workflow performs several checks before and during modelling.

### Input validation

The script checks for:

* Missing required columns
* Missing isolate or replicate identifiers
* Invalid concentrations
* Invalid colony-diameter measurements
* Negative concentrations
* Negative colony diameters

Invalid input rows are exported before the analysis stops.

### Experimental design checks

Each isolate-replicate combination is checked for the expected positive concentration series.

Incomplete concentration series are recorded in:

```text
Results/Failed_Fits.xlsx
```

### EC50 interpretation

The script records whether:

* The observed response range brackets 50% relative growth.
* The estimated EC50 falls within the experimentally tested concentration range.
* The 95% confidence interval is usable.

An EC50 outside the tested concentration range is flagged as extrapolated.

## Replicate-Specific EC50

A separate LL.4 model is fitted for each complete isolate × replicate combination.

These results provide individual EC50 estimates that can be used to evaluate experimental variation among replicates.

The output includes:

* EC50
* Standard error
* 95% confidence interval
* Slope
* Slope standard error
* AIC
* RMSE
* Number of observations
* Tested dose range
* EC50 extrapolation status

## Pooled Isolate EC50

A pooled LL.4 model is fitted for each isolate using all valid replicate observations belonging to that isolate.

The pooled isolate EC50 is used as the primary EC50 estimate for the main results and forest plot.

## Figures

The script generates three main types of figures.

### Dose-response curves

`Graphs/Dose_Response_All_Isolates.pdf`

The figure includes:

* Individual replicate observations
* Mean relative growth ± SE
* Pooled LL.4 fitted curve
* 95% confidence band
* 50% response reference line
* EC50 reference line
* EC50 and 95% confidence interval labels
* Separate panels for each isolate

PNG and TIFF versions are also generated.

### EC50 forest plot

`Graphs/EC50_Forest_All_Isolates.pdf`

The forest plot displays:

* Pooled isolate EC50 estimates
* 95% confidence intervals
* Replicate-specific EC50 estimates
* EC50 values inside and outside the tested concentration range

### Replicate EC50 boxplot

`Graphs/EC50_Replicate_Boxplot.pdf`

The boxplot displays the distribution of replicate-specific EC50 estimates for each isolate.

## Output Files

The script creates two directories:

```text
Results/
Graphs/
```

### Results

The following Excel files are generated:

```text
Results/EC50_Replicate_Results.xlsx
Results/EC50_Pooled_Isolate_Results.xlsx
Results/EC50_Final_Summary.xlsx
Results/Complete_EC50_Report.xlsx
Results/Failed_Fits.xlsx
```

Additional diagnostic files may include:

```text
Results/Invalid_Input_Rows.xlsx
Results/Control_Problems.xlsx
Results/R_Session_Information.txt
```

### Graphs

The script generates:

```text
Graphs/Dose_Response_All_Isolates.pdf
Graphs/Dose_Response_All_Isolates.png
Graphs/Dose_Response_All_Isolates.tiff

Graphs/EC50_Forest_All_Isolates.pdf
Graphs/EC50_Forest_All_Isolates.tiff

Graphs/EC50_Replicate_Boxplot.pdf
Graphs/EC50_Replicate_Boxplot.tiff
```

## Required R Packages

The analysis requires:

```r
library(readxl)
library(dplyr)
library(drc)
library(ggplot2)
library(openxlsx)
```

Install them with:

```r
install.packages(c(
  "readxl",
  "dplyr",
  "drc",
  "ggplot2",
  "openxlsx"
))
```

## How to Run

1. Install R and the required packages.
2. Download or clone this repository.
3. Place the input Excel workbook in the project directory.
4. Open `ec50_fungicide_analysis.R`.
5. Change `INPUT_FILE` to the name of your Excel workbook.
6. Change `SHEET_NAME` to the appropriate worksheet.
7. Confirm that `EXPECTED_DOSES` matches the experimental design.
8. Run the script.

The script will automatically create the `Results` and `Graphs` directories and save the analysis outputs.

## Reproducibility

The script records the R session information used during the analysis in:

```text
Results/R_Session_Information.txt
```

This can help document the R version and package environment used to produce the results.

## Repository Structure

```text
nigrospora-ec50-analysis/
│
├── ec50_fungicide_analysis.R
├── README.md
│
├── Results/
│   └── Generated analysis results
│
└── Graphs/
    └── Generated figures
```

Generated results and raw experimental data should generally be kept separate from the analysis script unless they are intentionally being shared as part of a reproducible dataset.

## Research Context

This workflow was developed as part of research on fungicide sensitivity in *Nigrospora* spp., with particular focus on *Nigrospora oryzae* and QoI fungicides.

The analysis is intended to provide a reproducible framework for estimating EC50 values and examining variation among isolates and experimental replicates.



## Author

**Vikas Nehra**

GitHub: [@NEHVIKAS](https://github.com/NEHVIKAS)
