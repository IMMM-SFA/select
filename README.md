# select
The Spatially-Explicit, Long-term, Empirical City developmenT (SELECT) model

#The sequence to run the scripts:
1. Run the R code script_SpatialModel_projection_2010.R which takes in tbl_attr_1-8-dgr_training.csv (now in the shared google folder) to make projections for 2010 in .csv format
2. Run the  python code script_SpatialModel_Covariate.py
3. Repeat script_SpatialModel_projection_2020&on.R 
4. Run python code script_SpatialModel_Covariate.py going back and forth to evolve through the century. 

#Note: 
the ***_header.docx files in the scripts_SpatialModel_projection.zip folder list the names and the sequence of columns in the *** files used by the scripts. 