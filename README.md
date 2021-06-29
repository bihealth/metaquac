# MeTaQuaC

MeTaQuaC is an R package to facilitate efficient quality control (QC) for
targeted metabolomics analysis with focus on
[Biocrates Kits](https://www.biocrates.com/).
It provides a simple interface to create extensive, reproducible and
well documented HTML QC reports in an automated fashion.

Please refer to our article to learn more about the underlying ideas:  
[Kuhring M. et al., 2020, Concepts and Software Package
for Efficient Quality Control in Targeted Metabolomics Studies:
MeTaQuaC](https://pubs.acs.org/doi/10.1021/acs.analchem.0c00136)

**Recent changes**:
* [v0.1.32] Add parameter to automatically export main data sets
* [v0.1.32] Add parameter to retain report intermediates (markdown and figures)
* [v0.1.32] Update and restructured documentation
* [v0.1.32] Changed default kit to "Biocrates MxP Quant 500 Kit"

MeTaQuaC supports the following Biocrates Kits:

* Biocrates Bile Acids Kit
* Biocrates AbsoluteIDQ p180 Kit
* Biocrates AbsoluteIDQ p400 HR Kit
* Biocrates AbsoluteIDQ Stero17 Kit
* Biocrates MxP Quant 500 Kit
* Generic Data (for custom targeted experiments)

Before using MeTaQuaC to create a QC report for Biocrates data, please
make sure to be familiar with the Biocrates kit used, i.e familiarize yourself
with the compounds, sample types, status values, terminology, analytical
specification, etc. (please refer to Biocrates' manuals and documents provided
with the kit used).


## Critical Notes
* MeTaQuaC reports sometimes struggle with **memory issues**,
either during pandoc conversion to HTML or when opening the HTML in a browser.
This is a result of providing integrated data sets
as feature-rich JavaScript tables and also in redundant occurrence.
To reduce report size, use the parameter `data_tables`
("stats", so the main data tables in the report get disabled)
in combination with the parameter `data_export_{long|wide}`
(TRUE, so all the main data tables get automatically exported as csv).
* Several of MeTaQuaC's filters and QC measures are designed around the
integrity of technical replicates.
MeTaQuaC intends to make use of Biocrates standard QC samples (QC Level 2,
referred to as Reference QC in the reports) and pooled QC samples.
To infer e.g. missing value rates, technical variability (%RSD) and batch drift,
these QC samples need to be replicated uniformly within the sample sequence.
A minimum of five replicates each is recommended.
Note that by default, Biocrates kit plates only contain one single standard QC
samples of three different concentrations each (QC Level 1, 2, and 3).
Thus, QC Level 2 sample replicates need to be applied separately. 
Running replicate QC samples is highly recommended
to assure reliable high quality data according to community standards.


## Requirements
In general, to run the following code please
copy and paste the code chunks into your R interface and then press return.

MeTaQuaC is a package developed and tested with
[GNU R](https://www.r-project.org/) (version >= 3.4.4).

In R, install the ``devtools`` package to enable installation from git:

```r
install.packages("devtools")
```

Other package dependencies (as stated in the [DESCRIPTION](DESCRIPTION) file)
should be resolved automatically during installation.


## Installation
To install and activate the latest release of MeTaQuaC
execute the following commands in R:

```r
devtools::install_github("https://github.com/bihealth/metaquac")
library(metaquac)
packageVersion("metaquac")
```


## Execution
The MeTaQuaC package provides one simple function to create an extensive,
reproducible and well documented report.
The data for this can be taken directly from Biocrates IDQ software.
There are instructions below on how to do this.

To create the report, execute the function `metaquac::create_qc_report` in R,
including the relevant parameters below.
An example piece of code and full description of all parameters is provided
in the package and can be accessed in R (run `?metaquac::create_qc_report`).

`create_qc_report` requires only one obligatory parameter `data_files`,
consisting of an R list of at least one named batch
indicating corresponding files per vector, each (e.g.
*list(Batch1 = c("Batch1_LC1.txt", "Batch1_LC2.txt"),
Batch2 = c("Batch2_LC1.txt", "Batch2_LC2.txt"))*).
Furthermore, commonly used parameters should include the `kit`
(defaults to "Biocrates MxP Quant 500 Kit")
and the `measurement_type` (defaults to "LC").

Always create separate reports for LC and FIA data.
Indicating LC and FIA data files within one report is not supported and
will result in unexpected behavior including results difficult to interpret.

**Note**:
Depending on the number of compounds per kit and injection
as well as the number of samples in a study,
the resulting HTML reports may considerably grow in size
due to the extensive amount of figures and intermediate data provided.


## Data
### Biocrates (via MetIDQ Data Export)
The data needed for the QC report is directly exported from Biocrates' MetIDQ
software.

1. After acquisition and processing with MetIDQ switch to the `MetSTAT` module
(please refer to Biocrates' manuals and documents provided with the kit used
to infer the relevant steps for your data).
2. Select your project and samples
(including at least biological, QC and calibration standards samples)
so they show up under the `Display Data` - `Data` tab.
3. Data normalization may be applied as described in the MetIDQ manual.
4. In the `Save Options` section,
**select** `Export Status Information` and `Export all types of values at once`.
Do **not** select `Transpose Values` and `Add comments to Excel files`
nor enable the replacement of any values!
5. Press `Export` and save the data as tab-separated text file.
6. Repeat the export for all corresponding injection types and batches
(watch out for a **consistent** `Display Unit` between batches).

The necessary settings are additionally highlighted in the figure below.

![MetIDQ export settings](inst/resources/metidq_export.png)


### Generic
Instead of data exported from MetIDQ (recommended when using Biocrates kits),
MeTaQuaC reports can be created using a basic generic data format, thus enabling
QC reports for any custom targeted experiment.

MeTaQuaC adapts to different terminologies within the reports, in particular
with respect to column names, sample types and status values (as these can be
highly input specific). While MetIDQ exports data with predefined terms, the
required or recommended terms for the generic data are definied below.

Currently, the generic data consists of one or several files (with same
dimensions) containing one type of measurement information each. I.e. one file
contains e.g. the concentrations, one the areas, one status values and so on.
Please refer to the parameter `data_files` and `generic_data_types` to see how
to map the currently supported data types with your files.

Generic data files are tab-separated text files containing typical sample (row)
to compound (column) matrices, plus some additional sample annotation columns.
Beside compounds in the latter columns (watch out to specify their position
correctly with parameter `generic_index_first_compound`), the following columns
can be used by MeTaQuaC:

* Required:
  * `Sample Identification` The ID of the sample.
  * `Sample Type` The type of sample (see below).
  * `Sample Position` The position of the sample in the acquisition sequence.
  **and/or**
  * `Well Position` The position of the sample on the well plate
  (currently only row-major order and only for 96 well plates!)
* Optional
  * `Batch` The ID of a batch, if the data contains several batches.
  * Any study variables of interest to use with `*_variables` parameters.

The following sample types (in column `Sample Type`) are recognized for specific
QC analysis:
* `Sample` The biological/study samples (required).
* `Reference QC` QC samples based on injected standards
(preferentially replicated).
* `Pooled QC` QC samples based on pooling biological samples
(preferentially replicated).
* Other sample types may appear in stats and visualizations, but are currently
not considered for specific processing such e.g. filtering, variances, etc.

As status values in generic data will be software specific, the parameter
`preproc_keep_status` might need modification if other than "Valid" measurements
should be considered.

Generic data files would look like this
(same format for different measurement informations,
just with different values in compound columns,
e.g. concentrations, areas or statuses):

| Sample Identification | Sample Type | Well Position | Sex | Ala | Arg | Asn
| - | - | - | - | - | - | - |
| A | Sample | 74 | Male | 123 | 68.6 | 29.2 |
| B | Sample | 3 | Female | 205 | 111 | 32.7 |
| C | Pooled QC | 27 |  | 192 | 67.5 | 35.3 |
| Cal2 | Standard L2 | 61 |  | 44.4 | 7.84 | 9.53 |
| QCb | Reference QC | 50 |  | 411 | 121 | 18.3 |
| QCa | Reference QC | 87 |  | 375 | 102 | 16.9 |
| Cal3 | Standard L3 | 73 |  | 189 | 43.4 | 46.9 |
| Cal1 | Standard L1 | 49 |  | 18.4 | 4.18 | 4.49 |
| ... |  |  |  |  |  |  |


## Parameters & Examples
Currently, QC report creation is controlled by the one main function of the
MeTaQuaC package, `metaquac::create_qc_report`.

To test the QC report creation, the package includes sample data for a Biocrates
MxP Quant 500 Kit with one batch as well as for a Biocrates AbsoluteIDQ p400 HR
Kit with four batches.

To improve documentation consistency,
parameters and example calls are no longer reported in this readme.
Please refer to the integrated package documenation in R
(run `?metaquac::create_qc_report`).

For filtering parameters, please note that:
* Default missing value ratio thresholds are set according to
[Broadhurst *et al.* 2018](https://link.springer.com/article/10.1007%2Fs11306-018-1367-3)  
* Default %RSD (precision) thresholds are set according to
[EMA 2018](https://www.ema.europa.eu/en/bioanalytical-method-validation)


## Additional Notes

* In case of the AbsoluteIDQ® p400 HR kit,
MetIDQ may produce data exports without any notation of a metabolite
if it hasn’t been measured in any sample of a batch
(i.e. the compound is not mentioned in the export at all).
However, listing the complete set of metabolites is a necessary information,
in particular when analyzing e.g. missing value ratios and comparing batches.
Therefore, imported data is extended using an internal list
of all the potential metabolites of the AbsoluteIDQ® p400 HR kit.
A prominent note in the report indicates this correction (if necessary)
and lists the affected metabolites.
We like to stress that this procedure does not alter any measurement data.
This is merely a computational fix recreating the full list of target compounds.
MeTaQuaC will just complement such missing compounds
(i.e. basically add a column of missing values for these compounds).
The selection of the kit (p400) as well as the injection type (LC or FIA)
is relevant to know which set of compounds need to be used for completion.
