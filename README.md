# Introduction

This collection of data reduction routines, written in [Interactive Data Language (IDL)](http://www.harrisgeospatial.com/SoftwareTechnology/IDL.aspx), was developed by Greg Holsclaw at the University of Colorado Laboratory for Atmospheric and Space Physics to process the data collected by the MAVEN IUVS and GOLD breadboard instruments and used by Joe Ajello and coinvestigators for studies of electron-impact processes in planetary atmospheres.

# Obtaining the software

## 1. Method 1 - Cloning

To clone the repository using the [git command line tools](https://git-scm.com/downloads) (pre-installed on a Mac), use:
```
git clone git@github.com:Ajello-Lab/data_reduction.git
```

This will create a folder called \'data_reduction\'.  Enter the folder:
```
cd data_reduction
```

There is one git submodule used, the [IDL astronomy library](https://github.com/wlandsman/IDLAstro).  In order to obtain these tools, use: 
```
git submodule init
git submodule update
```
## 2. Method 2 - Download zip file

Click on green drop-down button at top right labeled "Clone or Download" and then click on "zip file".  

# Using the software

If using the IDL development environment, it is recommended to add the \'tools\' path as a Project.

Before using any software, first edit the routine \'ajello_lab_set_paths\' and create a new entry for the user and local location of the data.  This will define a system variable called \'!UVIS_PATH\' that is used by the code to locate other routines and support files.  Now run the routine.

# Useful References

MAVEN IUVS instrument paper:

McClintock, W.E., Schneider, N.M., Holsclaw, G.M., Clarke, J.T., Hoskins, A.C., Stewart, I., Montmessin, F., Yelle, R.V., Deighan, J., 2015. The Imaging Ultraviolet Spectrograph (IUVS) for the MAVEN Mission. Space Sci Rev 195, 75–124. https://doi.org/10.1007/s11214-014-0098-7
