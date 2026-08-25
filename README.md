<img width="120px" alt="hydrochem logo" align="right" src="inst/figures/hydrochem.png">

# hydrochem – Tools for Hydrochemical Data Analysis and Visualization

<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/hydrochem)](https://CRAN.R-project.org/package=hydrochem)
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/hydrochem)](https://cranlogs.r-pkg.org/downloads/total/last-month/hydrochem)
[![CRAN_Total_Downloads_Badge](https://cranlogs.r-pkg.org/badges/grand-total/hydrochem)](https://cranlogs.r-pkg.org/downloads/total/last-month/hydrochem)
<!-- badges: end -->

The **hydrochem** package provides a set of tools for processing, analyzing, and visualizing hydrochemical data in a reproducible, programmatic workflow. It implements unit conversion, censored data management, ionic balance calculation, water type classification, and a range of hydrochemical indices useful for water quality assessment.

The package also offers advanced visualization functions for generating the standard diagrams used in hydrochemical interpretation: Piper, Durov, Stiff, Collins, Schoeller, Gibbs, and ternary diagrams.

Unlike existing solutions that rely on spreadsheets or proprietary graphical software, `hydrochem` is designed for scalability and reproducibility, lowering the technical barrier for hydrochemists working with datasets of varying complexity.

# Installation

The hydrochem package is available on [CRAN](https://cran.r-project.org/package=hydrochem) and can be installed with:

    install.packages("hydrochem")

To install the latest development version using the remotes package, run the following in your R console:

    remotes::install_github("snokarst-tools/hydrochem")

# Usage

## Quick example

```r
library(hydrochem)

# Compute the ionic balance
ion_balance(hc_data)

# Classify water types
classify_water_type(hc_data)

# Generate a Piper diagram
p <- plot_piper(hc_data)

# Save the diagram
save_plot(file.path(tempdir(), "piper.png"), type = "piper", plot = p)
```

## Detailed example

```r
# Install package
install.packages("hydrochem"")
# Load package
library(hydrochem)

# Load the dataset from the hydrochem package
data(data_qc)

# Replace values below detection limit with half the detection limit
data_qc_num <- replace_bdl(data_qc, method = "DL/2")

# Calculate the ionic balance using major and minor ions
data_qc_num$IB <- ionic_balance(data_qc_num, method = "major_minor")

# Retain only samples with IB <= 5% (135 out of 146 samples)
data_qc_num <- data_qc_num[abs(data_qc_num$IB) <= 5, ]

# (1) Generate a statistical summary for selected parameters
stat_summary_table1 <- stat_summary(
  data_qc_num,
  params = c("Ca", "Mg", "Na", "HCO3", "Cl", "SO4"),
  stats = c("min", "mean", "max"),
  na.rm = TRUE)

# (2) Generate a statistical summary for the full numeric hydrochemical dataset
data_qc_stat <- data_qc_num[,c(14:53)]
stat_summary_table2 <- stat_summary(
  data_qc_stat, 
  stats = c("min", "P25", "med", "P75", "max", "mad", "n", "NA_percent"),
  na.rm = T)
# Show the first rows of the statistical summary
head(stat_summary_table2)

# Add a water type column with only the dominant cation and anion 
data_qc_num$WT <- water_type(data_qc_num, format = "short")
# Add a TDS column by considering major ions and SiO2
data_qc_num$TDS <- tds(data_qc_num, method = "major_si")

# Calculate the mean TDS by water types 
mean_TDS <- stat_summary(data_qc_num, params = "TDS",  stats = c("n", "mean"), group = "WT", na.rm = T)

# Add a SAR column
data_qc_num$SAR <- sar(data_qc_num)

# Summarize the number of "Unsuitable" samples per water type
library(dplyr)
wt_unsuitable <- data_qc_num %>%
  group_by(WT) %>%
  summarise(
    Unsuitable = sum(SAR >= 26, na.rm = TRUE),
    total = n()) %>%
  arrange(WT)

# Generate a Piper diagram using the default graphical options 
piper1 <- plot_piper(data_qc_num, group = "Hydro_cond")
# Save the Piper diagram as a PNG file (Figure 1a)
save_plot("plot_piper1.png", type = "piper", plot = piper1)
# Create a custom categorical variable for TDS
data_qc_num$TDS_cat <- cut(
  data_qc_num$TDS,
  breaks = c(-Inf, 750, 1500, Inf),
  labels = c("<750", "750-1500", ">1500"))
# Generate a Piper diagram with customized graphical options 
piper2 <- plot_piper(data_qc_num, 
           group = "TDS_cat", 
           legend.position = c(0.15, 0.85),
           group_custom = T,
           fill = c("white","grey","black"),
           color = c("black","black","white"),
           size = c(1,2,5),
           shape = c(21,21,21))
# Save the customized Piper diagram as a PNG file (Figure 1b)
save_plot("plot_piper2.png", type = "piper", plot = piper2)

# Generate a Durov diagram
plot_durov(data_qc_num, group = "Hydro_cond", tds_log = T)

# Generate a set of biplots
plot_biplot(
  data_qc_num,
  group = "Hydro_cond",
  pairs = c("Na~pH", "Ca+Mg~HCO3", "Na~Cl", "Cl/Br~Cl"),
  slope = c(NA,NA,0.86,0),
  intercept = c(NA,NA,0.25,290),
  log_x = c(F,T,T,T),
  log_y = c(T,T,T,T))

# Compute the mean concentrations of major ions for each cluster
library(tidyr)
library(dplyr)
mean_cluster <- stat_summary(
  data_qc_num,
  params = c("Ca", "Mg", "Na", "K", "HCO3", "Cl", "SO4"),
  group = "Cluster",
  stats = "mean",
  na.rm = TRUE) %>%
  tidyr::pivot_wider(names_from = "param", values_from = "mean") %>%
  dplyr::arrange(Cluster)%>% 
  tidyr::drop_na()

# Generate Collins diagrams
plot_collins(mean_cluster, 
             nrow = 1,
             ratio = 0.075,
             facet_dir = "h",
             idcol = "Cluster",
             ylab = "% meq/L",
             border_color = "black",
             legend.position = "right")
# Generate Stiff diagrams
plot_stiff(mean_cluster,
           nrow = 2,
           facet_dir = "h",
           idcol = "Cluster",
           fill = "cadetblue2",
           scales = "free_x",
           xlab = "meq/L")
# Generate a Schoeller diagram
plot_schoeller(mean_cluster)
```

# Main features

- **Unit conversion** for common hydrochemical units
- **Censored data handling**
- **Ionic balance calculation**
- **Water type classification**
- **Hydrochemical indices** relevant to water quality
- **Diagrams**: Piper, Durov, Stiff, Collins, Schoeller, Gibbs, ternary

# Citation

If you use `hydrochem` in your work, please cite it. Run `citation("hydrochem")` in R to get the appropriate reference.

# License

GPL-2
