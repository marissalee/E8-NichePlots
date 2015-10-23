---
output:
  pdf_document: default
  html_document: default
---
e8_repository
=============

The purpose of this R project and repository is for version control and to document workflow on this project.


#Structure

##1. e8CodePackage [folder]

  1. **clean.R**: Cleans up the raw .txt data files for analysis.  This includes chopping off empty columns and rows.  Adding and aggregating column variables.
    
    1. *Actions* (a) Makes 'plotname' and 'plothalfname' identifiers (b) Adds a column in the plot location data to indicate whether the plot was in the forest understory or not (c) Adds a column in the veg data for total understory biomass (aggregated M.v. biomass and native biomass) (d) Adds a column in the Updated data directories to reflect the column changes described above (e) Export cleaned files: soil data, veg data, plot location data, plot tree data, timeline data (and their data directories)
    
    2. *Files needed*: e8DataPackage (folder of .txt files)
    
    3. *Files produced*: e8DataPackage_clean (folder of .txt files)
    
  2. **fxns_q1.R**: Function used to answer question1
    
    1. *Actions*
    
    2. *Files needed*: None
    
    3. *Files produced*: None 
    
  2. **question1.R**: Addresses the question - How does invader biomass vary across resource availability?
    
    1. *Actions*
    
    2. *Files needed*: e8DataPackage_clean (folder of .txt files)
    
    3. *Files produced*: None   


##2. e8DataPackage [folder]

  1. **e8_plothalfSoilData.txt**
  
  2. **e8_plothalfSoilData_dictionary.txt**
  
  3. **e8_plothalfVegData.txt**
  
  4. **e8_plothalfVegData_dictionary.txt**
  
  5. **e8_plotLoc.txt**
  
  6. **e8_plotLoc_dictionary.txt**
  
  7. **e8_plotTrees.txt**
  
  8. **e8_plotTrees_dictionary.txt**
  
  9. **e8_timeline.txt**
   


##3. e8DataPackage_clean [folder]

  1. **e8_plothalfSoilData_clean.txt**
  
  2. **e8_plothalfSoilData_dictionary_clean.txt**
  
  3. **e8_plothalfVegData_clean.txt**
  
  4. **e8_plothalfVegData_dictionary_clean.txt**
  
  5. **e8_plotLoc_clean.txt**
  
  6. **e8_plotLoc_dictionary_clean.txt**
  
  7. **e8_plotTrees_clean.txt**
  
  8. **e8_plotTrees_dictionary_clean.txt**
  
  9. **e8_timeline_clean.txt**

################################
#Not filed
1. **leeFunctions.R**: Stores most of the functions used in the rest of the pieces of code.  There are a lot of other functions in this script that are not relevant to this project.
  1. *Actions*: No stand-alone actions
  2. *Files needed*: None
  3. *Files produced*: None     
       





