# Replication files for "A perfect storm and the natural endowments of trade-enabling infrastructure"

## 1. Raw data sources:

### A. Danish census data from Link Lives 
I use the publicly available from Link Lives, available at www.rigsarkivet.dk/udforsk/link-lives-data/

### B. Sound toll registers online
This is the database of the Sound Toll Registers, which is available at http://www.soundtoll.nl/index.php/en/over-het-project/str-online
It is not possible to download it directly at the website, but you they will send you a full copy of the database if you write them an email describing you project.

### C. Digdag
Parish borders from 1820 are found in. Digital Atlas of Danish historical Administrative Geography. It is available for download at: https://digdag.dk/

## 2. Processed data sources 
This data is available in this repository. 

### A. Popdata.csv
This contains demographic data at the parish level. The data contains the following variables:
**Year**: Census year 
**GIS_ID**: Unique identifier for each parish, which 

### B. sogne_shape
'Sogne' tranlates to parishes. This is the shape file of Danish parishes as of January 1, 1820. Which was passed on to me from the authors of Boberg-Fazlic et al (2023). Originally this comes from www.digdag.dk. The market town of Lemvig (which is important in this application) was missing and added manually using borders downloaded directly from www.digdag.dk. The shape file has a an associated dataframe, which contains the following variables: 

- **STEDNR**: ID used by digdag 
- **SOGN**: Name of the parish 
- **HERRED**: The herred in which the parish is located. Administrative division above parish and below county. Roughly translates to 'hundred'. 
- **AMT**: County of the the parish.
- **GIS_ID**: Unique identified used in this project. 
- **long**: Longitude of the centroid 
- **lat**: Latitude of the centroid

## References 
Boberg-Fazlic, N., Jensen, P.S., Lampe, M. et al. ‘Getting to Denmark’: the role of agricultural elites for development. J Econ Growth (2023). https://doi.org/10.1007/s10887-023-09226-8
