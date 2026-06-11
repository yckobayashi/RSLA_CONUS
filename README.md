The files necessary for all preprocessing can be found in the following links:

National Hydrography Dataset Plus Version 2: https://nhdplus.com/NHDPlus/NHDPlusV2_home.php
Bankfull Hydraulic Geometry Width for the Contiguous United States: https://www.sciencebase.gov/catalog/item/5cf02bdae4b0b51330e22b85
ECMWF ERA5 Downward Shortwave Radiation Flux: https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=overview
MODIS Leaf Area Index: https://modis.gsfc.nasa.gov/data/dataprod/mod15.php
ETH Canopy Height Model: https://langnico.github.io/globalcanopyheight/

The validation set can be foud in the following link:
NEON In-Situ River Surface Photosynthetic Active Radiation: https://data.neonscience.org/data-products/DP1.20042.001

1. The NHDPlus dataset is preprocessed in ArcGIS Pro before using in the R script. We first joined the bankfull hydraulic geometry widths attribute to the NHDPlus dataset. Specifically, we used the NHDFlowline_Network within the NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z file. We applied the "Smooth Line" geoprocessing tool on the NHDFlowline_Network using a smoothing algorithm of "Polynomial Approximation with Exponential Kernel". We then applied the "Simplify Line" tool on the output of the previous step using a simplification algorithm of "Retain critical points (Douglas-Peucker)". Afterwards, we used the "Split Line At Vertices" tool to obtain individual polylines.

2. The ERA5 DSWRF is preprocessed in Python before using in the R script. The script for processing the DSWRF dataset is provided. Within the script, there is a code chunk which requests the netCDF file. Additional steps are necessary to set up the CDSAPI (https://cds.climate.copernicus.eu/how-to-api) if preferring to download through the python script. 

3. The MODIS LAI is preprocessed in R. The script for the preprocessing is available within the RSLA_Parameters.

4. The ETH CHM is mosaiced in ArcGIS Pro to obtain a raster which encompasses the CONUS.
