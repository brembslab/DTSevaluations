# DTS evaluations
-----------------

## Contents

- [About](#about)
- [Required files](#Required-files)
- [Instructions](#Instructions)
- [Troubleshooting](#Troubleshooting)
- [Bugs](#Submit-issues-or-bugs)

## About
R-Code to evaluate Drosophila Time Series data.
DTSevaluation is an evaluation R-code to analyze, visualize and statistically evaluate  **D**rosophila **T**ime **S**eries data.


## Required files

| R Files | Description |
| --- | --- |
| HTML_DTS_project.R | R-script reads the YAML DTS project file and analyses, visualizes and statistically evaluates the DTS data. The output is amalgamated into individual data files for each fly as well as a comprehensive HTML output file for the entire project.|
| single_file_HTML.R | Single fly data evaluation script. Evaluates each file separately and visualizes this as an HTML output.|
|project.Rmd | Project evaluation sheet. Uses the grouped information to perform a statistical data evaluation of each individual group as well as a group-to-group comparison.|
| readXMLdatafile.R | Script for importing the XML DTS data. |
| DTS_plotfunctions.R | Contains all the plotting functions for DTS data.|
Additional R-script files
| R Files | Description |
| --- | --- |
| single_fly.Rmd | Evaluate a single file without the need to add any information to the YAML project file.  |

---------
| Data Files | Description |
| --- | --- |
|project_file.yml | The project YAML file contains exclusively meta.data containing grouping-information which links each individual datafile to a project|
|datafile.xml | The xml file contains all the necessary meta-data for that particular fly: name of the experimenter, genotype, FlyBaseID, fly identifier(name), as well as the experimental sequence. The xml is generated through the DTS FS software. In addition to this the experimental file also contains the Time Series data|

## Instructions
 The following instructions is a brief explanation on how to use the DTS evaluation script. For a more detailed description of the The Drosophila Time Series Data Model with all its parameters you can access it [here](https://github.com/brembslab/dtsdatamodel).

### Essential information 
+ R-script files (DTS_plotfunctions.R, HTML_DTS_project.R, project.Rmd, single_file_HTML.R) need to be in the same directory.
+ Datafiles must be within the same folder as the Project YAML file, but not necessarily as the same folder as the R-script files.
+  The working directory need to be manually set to the directory where the R-script files are.

### Example_project_file.yml
The project file contain all the meta-data for the particular experiment/project. It contains grouping information that links each experimental fly/file to a particular project. In addition to this it also allows has information about the project such as a description and experimental comments. For each project, the YAML file needs to be updated with the necessary user information:
+ Author: Information about the experimenter.
+ Resources: The list of files for each experimental group, between 1 and 3 groups. Each datafile (with .xml file extension) needs to be listed under the "data" heading in resources. In addition to the filelist the user also needs to provide name, title and description of each experimental group. Name and title is free-text descriptions whereas description is categorical and limited to the following: Ctrl/Control, Experimental/Test/Exp (depending on if that particular group is experimental or a control group)
+ The statistical evaluation is determined in the "data" heading in single.group, two.groups and three.groups. The data is binary where 1 corresponds to "yes" and 0 to "no".
See example file or [The Drosophila Time Series Data Model](https://github.com/brembslab/dtsdatamodel) on how to appropriately add more than one group of data. 

### HTML_DTS_project.R
The HTML_DTS_project.R reads YAML DTS project files, as well as visualizes and statistically evaluates the data. The output of the file is reported in HTML.
- To run HTML_DTS_project.R script, select the entire block of code within the HTML_DTS_project.R and click either "Run" or simply use Ctrl+Enter. A user input will ask the user to select the project YAML file. While the code is running, the progress and an estimated completion time will be shown in the plots window. The script creates an evaluation folder containing all the single and group evaluations, in the same directory as the data is kept.

## Troubleshooting

| Error | Suggested solution |
| --- | --- |
| **Error: You have selected files with non-equal metadata. Please check the file(s) above for consistency!"** |The HTML_DTS_project.R checks for matching metadata between the data files. In case of mismatching metadata an error message will display which ones are differing. The code will still proceed but it will include the differing metadata.|
| **Error: You have selected files with non-equal metadata. Please check the file(s) above for consistency!"** | Single fly descriptive data evaluation script. Evaluates each file separately as well as transfer the data to the HTML_DTS_project for group comparison. |
| **"Error: XML-content does not seem to be XML"** |    Make sure all xml files exist in the data folder, check for potential spelling errors in the filename and that the .xml file extension has been added to all files
| **No group comparison HTML outputs** | Check that the data heading in single/two/three.groups is set to 1. |
| **"Error: In file(filename, "r", encoding = encoding) :  cannot open file 'readXMLdatafile.R': No such file or directory"** | Make sure the right working directory is selected
|**"Error: Quitting from lines 860-985 (project.Rmd) Error in Exp[, (names(Exp) %in% "value")] :  incorrect number of dimensions"** | Check that the descriptions for the groups are any of the ones permitted (Ctrl/Control, Experimental/Test/Exp)
|**"Quitting from lines 61-66 (project.Rmd) Error in png(..., res = dpi, units = "in") : unable to start png() device"** | Non permitted use of characters in the title name
|**"Quitting from lines 95-102 (project.Rmd) Error in grouped.flyhistos[[x]] : subscript out of bounds"** | Multiple reasons, most likely no data was added to one of the groups

## Submit issues or bugs

Please open a [new issue](https://github.com/brembslab/DTSevaluations/issues/new). Describe the issue and include a reproducible example and error message if possible.