# Rifampicin-PopPK-Repository

#### Author: Gehang Ju/ Graham Ju

#### Email: 218101055@csu.edu.cn / jugehang@163.com

#### Address: Central South University, Xiangya Hospital, Hunan Province of China

#### Aim: Establish PopPK repository of Rifampicin

NOTE:
This repository established by MrgSolve, and can be used for teaching use. 
If you have any other demands, please contact me.
Any other problems, contact me.

## Introduction

This repository contains a collection of population pharmacokinetic (PopPK) models for Rifampin, integrated into a Shiny application for easy simulation and analysis.

## Instructions

Before using this model repository and Shiny app, please download and save all contents of the repository to the same directory.

### Prerequisites

Ensure that you have the following R packages installed before running the software:

- `shiny`
- `mrgsolve`
- `dplyr`
- `tidyr`
- `ggplot2`
- `readxl`

You can install these packages using the following commands in R:

```R
install.packages("shiny")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readxl")
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("metrumresearchgroup/mrgsolve")
```

### Use Repository

Please refer to the `user manual`

## Usage

This software is intended for educational and research purposes. You do not need to contact me for permission to use it for these purposes. However, if you plan to iterate on the model repository, reference it, or use it for other commercial purposes, please contact me for authorization.

## Contact

For any queries or authorization requests, please reach out to me at [218101055@csu.edu.cn / jugehang@163.com].
