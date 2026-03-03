# VBD-scenarios

This repository contains the code associated with the paper:

Mariken M. de Wit, Martha Dellar, Gertjan Geerling, Eline Boelee, Mart C.M. de Jong, Quirine ten Bosch - **"Scenario-driven shifts in future Usutu and West Nile virus outbreak characteristics in the Netherlands "**. The paper has been published in Scientific Reports.


# Overview
This code creates next generation matrices for future scenarios of Usutu and West Nile virus transmission. The reproduction number and generation time related output measures are calculated for all scenarios. 

# System requirements
Data preparation and output analyses require a standard computer. Calculating outcome measures (R0 and generation time) requires a high-performance cluster (HPC) system.

# Instructions for use

The folder `Scripts` contains code to run analyses and reproduce figures in the manuscript and supplements. Files starting with 'prep_' contain data preparation steps for model inputs. 'main_' files are used to calculate outcome measures & analyse results including plots and are associated with "functions_" files.

The folder `Data` contains input data used in Scripts.

## Data preparation
The model raster is created from _prep_createRaster.R_. Bird abundance, mosquito abundance, and temperature data are prepared in _prep_inputData_ with the associated function file _functions_inputData.R_.
The files _prep_parameters.R_ creates all input parameters and input dataframes for the NGM and generation time for each scenario. 

### Input data
All data used in this study have been published before. 
Future climate data was taken from scenarios developed by the Dutch Meteorological Organisation for the period 2036-2065 (van Dorland et al., KNMI National Climate Scenarios 2023 for the Netherlands. De Bilt: 2024.). Data for these scenarios, including the reference scenario, is available from the KNMI website (https://klimaatscenarios-data.knmi.nl/downloads). 
Cx pipiens abundance estimates have been published in Krol et al. 2024 (https://doi.org/10.21203/RS.3.RS-5298493/V1) and are available upon request from the corresponding author. 
Bird abundance estimates have been published in Dellar et al. 2024 (https://doi.org/10.1007/s10393-025-01727-9). The explanatory variables, model outputs, code, and supplementary information for the bird abundance estimates can be accessed via the Dryad repository: https://doi.org/10.5061/dryad.r2280gbmc. The bird count data is not publicly available but can be requested from the Dutch Centre for Field Ornithology (Sovon: sovon.nl) or Vigie-Nature (https://www.vigienature.fr/fr/suivi-temporel-desoiseaux-communs-stoc). A summary of the bird data is available via the above Dryad link. 
The land use maps associated with each scenario have been published in Dellar et al. 2024 (https://doi.org/10.1038/s41597-024-04059-5). For land use maps analysis, all information and results is available via the Dryad repository: https://doi.org/10.5061/dryad.sj3tx96bs. 

## Calculating R0 and generation time 
The R0 and generation time are calculated in _main_NGM_x.R_ using _functions_NGM.R_. This is set up to run on a HPC and local machine. These calculation are submitted to the HPC through _run.scenarios.py_.

## Analysing output
Results can be analysed by using _main_results.R_ and the related function file _functions_results.R_.

