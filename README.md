# DTS evaluations


## Contents

- [About](#about)

- [Usage](#usage)
- [Bugs](#bugs)
- [Troubleshooting](#troubleshooting)





## About
R-Code to evaluate Drosophila Time Series data.
DTSevaluation is an evaluation R-code to analyze, visualize and statistically evaluate  **D**rosophila **T**ime **S**eries data


##Required files

###HTML_DTS_project.R
R-script reads the YAML DTS project file and analyses, visualizes and statistically evaluates the DTS data. The output is amalgamated into individual data files for each fly as well as a comprehensive HTML output file for the entire project. 

###single_file_HTML.R
Single fly descriptive data evaluation script. Evaluates each file separately as well as transfer the data to the HTML_DTS_project for group comparison.

###project.Rmd
Project evaluation sheet. Uses the grouped information to perform a statistical data evaluation of each individual group as well as a group-to-group comparison. 

###readXMLdatafile.R
Functions needed to import XML DTS data.

###DTS_plotfunctions.R
Contains all the plotting functions for DTS data.

###single_fly.Rmd
Analyses a single file without the need to add any information to the YAML project file. 

###Data files
####Example_project_file.yml
All files associated with the project are added to the corresponding group in the YAML-file. Each group is provided with the necessary information needed for that particular group: name, title, description as well as the xml data files. In addition to this, the YAML also contains which type of evaluation the project was designed to be used with: single fly analysis, two/three/multiple group comparison. Each individual group comparison contains a title, description as well as a binary data input deciding if the corresponding group is to be analysed or not (1 for yes, 0 for no). 

####Example_fly.xml
Each individual fly has its own xml file. The file containts all the necessary metadata for that particular experiment. In addition to information about the experimenter the document also carries data about the particular fly: name, genotype and a description as well as Flybase information if available. In addition to this, the xml file also has data about the experimental structure (under "sequence"). The experimental structure is automatically provided by the FPGA SOFTWARE, but can be modified using manual input if needed. Lastly, the actual data for that particular fly can be found in csv_data heading.    

## Usage
###Example_project_file.yml
- User will provide the yml file with the necessary user information for the particular evaluation. The group description of the experiment, which flight simulator was used as well as a title and description of the project.

- Under resources all information about the different experimental groups are provided (name, title and description). Description decides which group is the control and which one is the experimental. The following three terms can be used to describe these groups: Ctrl/Control, Experimental/Test/Exp. In the case of multiple control groups the are to be separated by numbering. 

In addition to this, the names for all the data files for each group is added with the appropriate extension (.xml). 

- User need to alter the binary data evaluation vector for single.group, two.groups or three.groups: 1 for yes and 0 for no. Default is set to 0. 

###HTML_DTS_project.R
- All evaluation files (DTS_plotfunctions.R, HTML_DTS_project.R, project.Rmd, single_file_HTML.R) need to be within the same folder. All data files (Example_project_file.yml, Example_fly.xml) need to be stored in the same folder, but not necessarily within the same as the evaluation files folder.

- The working directory need to be manually set to the source file location.

- Select the entire block of code within the HTML_DTS_project.R and click either "Run" or simply use Ctrl+Enter. A user input will ask for the Example_project_file.yml. While the code is running, the progress will be printed in the plots window. Upon completion, an evaluation folder will store all the output files.  

## Troubleshooting
## Bugs

Please open a [new issue](https://github.com/brembslab/DTSevaluations/issues/new). Describe the issue or the bug and include a reproducible example if possible.

