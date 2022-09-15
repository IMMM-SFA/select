# SELECT

The Spatially-Explicit, Long-term, Empirical City developmenT (SELECT) model.

SELECT produces urban fraction in decadal timesteps for a given SSP at 1/8 degree
resolution, and downscales to 1km resolution, in decadal timesteps from 2010-2100.

# AUTO-SELECT

The `auto_select.py` script iteratively runs SELECT for a specified SSP and then downscales the output.

Several requirements must be met in order to successfully run the script:
* A Windows machine
* Plentiful storage space (>200GB)
* This codebase
* The [input dataset](https://doi.org/10.5281/zenodo.6646697), unzipped into the codebase directory
  * Note that paths to the input files are hardcoded relative to the root of the codebase
* ArcGIS installed and licensed with the Spatial package
  * Tested on ArcGIS Pro 2.8
* R installed and RScript available on the PATH
  * Tested on R-4.0.3
  * To add RScript to the path for the current terminal session, run `path %PATH%;C:\Program Files\R\R-4.0.3\bin`, replacing with the actual path on your system
* The following R libraries must be installed:
  * `gam`
  * `mgcv`
  * `dplyr`
  * These can be installed from an R command line with: `install.packages(c('gam', 'mgcv', 'dplyr'))`
* A Conda environment cloned from the default ArcGIS conda environment
  * There are a few ways to accomplish this, but I found this method worked for me:
    * open ArcGIS
    * navigate to Settings
    * navigate to Python
    * click on Manage Environments
    * click the clone icon next to the 'arcgispro-py3' entry
    * activate the new environment (requires restarting ArcGIS)
* The `geopandas` Python package
  * Install using conda within the cloned environment:
    * see the bug report [here](https://stackoverflow.com/questions/69523996/arcpy-scripts-keep-giving-an-error-typeerror-expected-string-or-bytes-like-obj)
    * install with command: `conda install geopandas libtiff=4.0.10`
* Patience

Once all the above requirements are met, run `auto_select.py` like so:
* Launch the Python Command Prompt provided with ArcGIS
  * On my system this is available by searching for Python from the Windows icon, or directly at `C:\Program Files\ArcGIS\Pro\bin\Python\Scripts\proenv.bat`
* Ensure your cloned conda environment is active
* Ensure RScript is available on your path, as mentioned above
* `python auto_select.py SSP5`
* Running one SSP takes about 48 hours on my system
* Output is written to the `./output` directory relative to the codebase

Sample data for running SSP5 is available in the input dataset; running other SSPs will require preparing new input files based on files found in the references.


# References

[1] Gao, J., and B.C. O’Neill. 2019. Data-driven spatial modeling of long-term urban land development potential for climatic impact assessment: the SELECT model. Environmental Modeling & Software. https://doi.org/10.1016/j.envsoft.2019.06.015.

[2] Gao, J. and B.C. O’Neill. 2020. Mapping global urban land expansion for 21st century using data science based simulations. Nature Communications. https://doi.org/10.1038/s41467-020-15788-7.

[3] Gao, J., 2020, "Global National Total Amounts of Urban Land, SSP-Consistent Projections and Base Year, v1 (2000 - 2100)", https://doi.org/10.7910/DVN/85PJ1D, Harvard Dataverse, V1.

[4] Gao, J. and B.C. O'Neill. 2022. Sample Input Data and Supporting Files for the SELECT Model of Urbanization. https://doi.org/10.5281/zenodo.6646697.
