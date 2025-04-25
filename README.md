# FlexERScale

**The FlexER Scale** is a new self-report measure to assess the context- and goal-dependent variability of emotion regulation strategies. The scale was derived from theoretical models of emotion regualtion flexibility and validated using data from three independet samples (*N* = 857). This repository contains materials and scripts for validation of both, the 12-item and the final 10-item version of the FlexER Scale.

## Content of the folders
* [Flex_ER Scale](https://github.com/ChScheffel/FlexERScale_Validation/tree/main/FlexER_Scale): Contains a PDF file of the German version of the FlexER Scale
* [datasheets](https://github.com/ChScheffel/FlexERScale_Validation/tree/main/datasheets): Contains the datasheets and respective code books of all relevant data used to validate the 12-item version and the final 10-item version of the FlexER Scale
* [figures](https://github.com/ChScheffel/FlexERScale_Validation/tree/main/figures): All figures that are relevant for the validation study
* [renv](https://github.com/ChScheffel/FlexERScale_Validation/tree/main/renv): Folder created by the R renv-package
* [scripts](https://github.com/ChScheffel/FlexERScale_Validation/tree/main/scripts): Contains the script of the validation of the 12-item version, the script of the validation of the 10-item version, and a script with a R function (item difficulty)

## Requirements
- R version: 4.4.1
- OS: Tested on Windows 11
- All required R packages will be installed via `renv::restore()`

## Reproducibility

To reproduce results of the validation of the FlexER scale, follow these steps:
1. Open the R-Project `FlexERScale.Rproj`
2. After opening the project, *renv* will automatically bootstrap itself, downloading and installing the appropriate version of *renv*
3. The package *renv* will ask if you want to download and install all the packages it needs by running `renv::restore()`. After finishing the installation, you should have installed all required packages in the correct version.
4. Navigate to the script `Validation_10item.R`
5. Please note that you can only start reproducing the results by following the script from line 118 onward. This is because the original data sheets contained a number of variables that were not relevant for the validation. 

## Outputs
The validation scripts generate:
- All results used in the validation
- Figures in the R workspace
- An R workspace containing all variables and plots is provided in `Workspace_FlexER.RData`

## License
This project is licensed under the CC-BY 4.0 License. See the `LICENSE-CC-BY` file for details.

## Citation
If you use this scale, please cite the following:
> Gärtner, A., Scheffel, C., Schweikert, T., & Dörfel, D. (2025, March 13). The FlexER-Scale: A new Self-report Measure of Individual Differences in Emotion Regulation Flexibility. https://doi.org/10.31234/osf.io/hprbz_v1
