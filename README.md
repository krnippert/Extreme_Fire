# High-intensity fire and long-term destocking causes top kill but fails to reverse encroachment and restore biodiversity.
## Authors: K.R. Nippert-Churchman<sup>1,2</sup> and Z. Ratajczak<sup>1</sup>
  1. Division of Biology, Kansas State University, Manhattan, KS 66506
  2. Department of Biology, University of North Carolina Greensboro, Greensboro, NC 27412

Open Research: The data from this study is openly available in the Environmental Data Initiative data repository at https://doi.org/10.6073/pasta/e56e5bd937c274be8c1669a5d10522a3.

## Introduction:
This repository contains R scripts and data that analyze how high-intensity fires effect woody plant communities, specifically looking at understanding changes in mortality, woody plant traits, and changes in cover at Konza Prairie Biological Station in Manhattan, KS, USA. This experiment leverages two watersheds of native tallgrass prairie that were burned in 2021, a low-intensity fire that burned under typical prescribed fire conditions (watershed K4A), and one that burned in a wildfire that had characteristics of a high-intensity fire (watershed 4B). 

## Stem diameter and allometry analysis:
The first R script, shrub_code_w_Allom.R, looks at change in stem densities between both catchments using stem diameters and allometric data from sampled woody plants. This script utilized data from WPE021.csv, WPE022.csv, and woody_allometry_master.csv to understand how shrub height, stem density, and leaf area change after the 2021 fire events, and track recovery for two years post-fire. Note that WPE021 is the combined "shrub_counts" dataset on line 70.

## Remote sensing analysis:
The second R script, can_height_raster_math.R, utilized LiDAR data collected by NEON to identify changes in shrub height and cover in both treatments after the 2021 fire events. This script uses .tif files from https://data.neonscience.org/data-products/DP3.30015.001 (site KS-DO6_KONZ) as well as the GIS032 and drainages_5msink_1000_8M_buffer folder to process lidar images to create maps of shrub height in watershed 4B (high-intensity fire) and K4A (low-intensity fire) from 2020-2024.

## Weather conditions analysis:
The final R script, weather data.R, analyzes and creates graphs of the weather conditions for the day of each fire event using APT024.csv, AWE011.csv, and KFH011.csv. The high-intensity fire occured on April 3, 2021, while the low-intensity fire occured on April 12, 2021.

## Spatiotemporal extent and resolution: 
Spatial Extent: 
- This project took place at Konza Prairie Biological Station in Manhattan, KS, USA. It focused on two watersheds, 4B and K4A that have similar sizes (54.2 and 53.0 ha), fire history, and have a long-term history of grazer exclusion.

Temporal Extent: 
- Years: 2020-2024
- Stem diameter and allometry: 2020-2023
- Remote sensing: 2020-2024

## LiDAR data acquisiton:
- Data was acquired through the National Science Foundation (NSF) National Earth Observatory Network (NEON) Aerial Observation Platform (AOP).
- A Riegl Q780 LiDAR sensor was used in 2020 and 2024
- An Optech Gemini LiDAR sensor was used in 2022 and 2023
- Flyovers occured on 07/07/2020, 06/13/2022, 05/26/2023, and 06/05/2024

## Usage:
All data was analyzed using R version 4.4.1. (R Core Team, 2024)

Attached packages and versions: AICmodavg 2.3-4, emmeans 1.11.2-8, dplyr 1.1.4, ggplot2 4.0.0, ggpubr 0.6.2, multcomp 1.4-29, raster 3.6-32, readr 2.1.5, rpart 4.1.24, segmented 2.1-4, sf 1.0-24, strucchange 1.5-4, tibble 3.3.0, and tidyr 1.3.1

## Additonal data information:
Below are links to publically available data that was used in this experiment: 
- https://doi.org/10.6073/pasta/e56e5bd937c274be8c1669a5d10522a3
- http://dx.doi.org/10.6073/pasta/c2dde97352fb0e25ab749765967997b9
- https://data.neonscience.org/data-products/DP3.30015.001
- https://doi.org/10.6073/pasta/a267f9b0995f6fa91340ba5886ee2273
- https://doi.org/10.6073/pasta/f7a0875273b934171aa7fd2cf4dd5ef3
- https://doi.org/10.6073/pasta/d81b100b0737880b7950ec0d93625da9
- https://doi.org/10.6073/pasta/892ba203b202c308ea03841ed7051970

## Metadata:
The metadata for all publically available data are found in the metadata.txt file. Additonally, see each EDI or NEON data package for more metadata information using the links above. 

## Contact Information:
For any questions related to the data or scripts, contact Kalea Nippert-Churchman at krnippert@uncg.edu or Zak Ratajczak at zarata@ksu.edu

## Funding:
This work was funded by NSF DEB-2025849 (Konza Prairie LTER), NSF DEB 2324988, MAPS-KS NSF EPSCoR OIA-1656006, Kansas State Division of Biology, and Kansas Agricultural Experimental Station. 
