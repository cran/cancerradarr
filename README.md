# cancerradarr - Cancer RADAR project R package.

 <!-- badges: start -->
  [![CRAN status](https://img.shields.io/cran/v/cancerradarr)](https://CRAN.R-project.org/package=cancerradarr)
  
  [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
  
  [![coverage report](https://gitlab.com/cancerradar/cancerradarr/badges/main/coverage.svg)](https://cancerradar.gitlab.io/cancerradarr/coverage/)
  <!-- badges: end -->

## Cancer RADAR project

**Short summary:**
Migration to and within Europe has increased and diversified in recent years. Individuals with a migration background often have health needs that differ from the general (host) population of a country. At the same time they can face important barriers to access the right health care. A unified quantification of current disparities of cancer by migration background in Europe does not exist impeding policy makers to act upon the health needs of migrants. To fill this knowledge gap Cancer RADAR’s aim is to develop an infrastructure that allows quantifying the risk of cancer by migration background across Europe. To establish this, Cancer RADAR will first focus on infection-related cancers (liver, stomach, and cervical cancer) and screening preventable cancers (breast, cervical, colorectal, and lung cancer). 


## Objective of the cancerradarr package

Cancer RADAR is an infrastructure to systematically collect cancer data stratified by migration background from cancer registries. This data will be combined with other data sources to provide a harmonized Europe-wide perspective on the context-specific incidence and mortality risk and future (preventable) burden of stomach, liver, cervical, breast, colorectal and lung cancer among migrants in Europe.

The objective of this tool (package) is to facilitate data sharing between cancer registries in Europe and the International Association of Cancer Registries (IACR). The tool contains a set of functions that will generate aggregated indicators, for example totals, incidence rates, incidence rate ratios, incidence rate differences. These numbers will serve as input for the Cancer RADAR project. Please e-mail cancer.radar@iarc.who.int for the most up-to-date protocol.

## Confidentiality, security and ethical approval
The data shared are in the form of tables of aggregated frequencies (observed cases and population at risk) and/or indicators (Rates, SMRs, PIRs, standard errors, etc) and are thus considered anonymous. Data will be stored on a dedicated server at IARC-WHO according to the standard requirements for data security. Cancer RADAR was approved by the IARC Ethics Committee (IEC) under project number IEC 23-38.

### How does it work

Please install the package as explained below.

This package will provide you with an empty excel file in which the data can be entered.

This empty excel file contains different sheets in which the indicated data can be provided by country-of-birth of the individual in 5-year age-groups and as totals. Country-of-birth will serve as proxy for migration background.

Depending on the availability of the data at the cancer registry the excel file will provide the opportunity to complete a list of variables.

**Type of data collected:**

1. number new cancer cases for the respective cancer
2. number of deaths due to the respective cancer (this will be requested at a later stage of the project)
3. population-at-risk*

*Data on the population of the area covered by each registry are optional. If population at risk is not available we will use indirect methode Proportional Incidence Ratio (PIR).

**Type of cancer for which data will be collected:**

1. C53: Cervix uteri
2. C22.0: Liver
3. C16: Stomach
4. C50: Breast
5. C18-20: Colorectal cancer
6. C33-34: Lung cancer
7. All cancers excl. non-melanoma skin cancer (C00-97/C44)

**Depenidng on the availability, three input files may be requested for the following three time periods:**

- CI5-XII 2018-2022
-	CI5-XII 2013-2017
-	CI5-XI 2008-2012
-	CI5-X 2003-2007


### How to install the package

The package is hosted on `gitlab.com` ([dev version](https://gitlab.com/cancerradar/cancerradarr)) and `CRAN` ([stable version](https://CRAN.R-project.org/package=cancerradarr)) and can be installed in R using one of the following option:

- Install the latest dev version from `GitLab`

  ```r
  ## check if remotes package is installed and install remotes if not.
  if(!('remotes' %in% rownames(installed.packages()))) install.packages('remotes', dep = TRUE)
  
  ## install the latest dev version of cancerradarr package
  remotes::install_gitlab("cancerradar/cancerradarr", dependencies = TRUE)
  ```

- Install the latest stable version from `CRAN`

  ```r
  ## install the latest stable version of cancerradarr package
  install.packages("cancerradarr", dependencies = TRUE)
  ```

### Download empty excel file (input file) to enter the data

The first step of the workflow is to create an input template file.
The `create_registry_input_file()` function has been designed to create a template excel file that cancer registries should fill in order to produce all cancer RADAR project summary statistics. 

The function requires one parameter which is the path to the location where the input file will be created. The following command will create a file named `cancerRADAR_input.xlsx` in `/PATH_TO_INPUT_FILE/` a directory on the hard drive. This file needs to be completed by the cancer registry.

```r
library(cancerradarr)
create_registry_input_file('/PATH_TO_INPUT_FILE/cancerRADAR_input.xlsx')
```

You can access a example of filled file (i.e. `ex_cancaerRADAR_input_filled.xlsx`) typing:

```r
file.copy(file.path(path.package('cancerradarr'), 'extdata/ex_cancerRADAR_input_filled.xlsx'), '/PATH_TO_INPUT_FILE/ex_cancaerRADAR_input_filled.xlsx')
```

### Enter data in the empty input file in excel

1. Open the created input file and enter the data in the different sheets (for now only incidence data is requested).
2. You will see different empty sheets. The first sheet provides an explanation of the data that needs to be entered. 
3. Save the file under a different file name providing the period and version date (e.g `cancerRADAR_input_completed_xx_xx_vsxxxx2024.xlsx`)


### Computing Cancer RADAR summary statistics 


Now that the data has been entered in the excel file you can create summary statistics using the `cancerradarr` package. The outputs of this package are the effect measures that will be shared with International Association of Cancer Registries (IACR).

To do this run the `create_canradar_summary_file()` function by copy-pasting it to the R console as shown below. 
This function requires 2 files paths as parameters: 
1. The path to the filled template file from the previous step (e.g `/PATH_TO_INPUT_FILE/cancerRADAR_input_completed_xx_xx_vsxxxx2024_vsX.xlsx`, please replace `PATH_TO_INPUT_FILE` with the full path to the directory where the input file is stored) and, 
2. the path to an output file where summary statistics will be stored (e.g. `/PATH_TO_INPUT_FILE/cancerRADAR_output_xx_xx_vsxxxx2024.xlsx`).

To compute the summary statistics execute the following command in R (adapting the file name and path of the input and output files as appropriate)

```r
create_canradar_summary_file('/PATH_TO_INPUT_FILE/cancerRADAR_input_completed_xx_xx_vsxxxx2024.xlsx', '/PATH_TO_OUTPUT_FILE/cancerRADAR_ouput_xx_xx_2023_vsX.xlsx')
```

This will create `'cancerRADAR_ouput_xx_xx_2023_vsX.xlsx`, a multi-tabs excel file containing 
the summary statistics as listed in the protocol.

It is possible to visualize the output created using the dynamic report creation function (`run_dynamic_report`). This tool can quickly give you an idea of the difference in risk in cancer between individuals with a migration background and general population covered by the registry. Here is an example showing how to build such a report.

```r
run_dynamic_report('/PATH_TO_OUTPUT_FILE/cancerRADAR_ouput_xx_xx_vsxxxx2024.xlsx')
```

Note that a static report containing some key tables and figures can also be generated:

```r
create_static_report('/PATH_TO_OUTPUT_FILE/cancerRADAR_ouput_xx_xx_vsxxxx2024.xlsx')
```


### Sending Cancer RADAR summary statistics 

When you have completed all steps please send the created file through the RedCap link created for this project.
