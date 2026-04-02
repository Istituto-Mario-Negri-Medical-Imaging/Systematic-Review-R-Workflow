# Systematic Review R Workflow

## Overview

This repository contains the R scripts supporting the systematic review workflow. It is organized according to the main stages of the workflow, from initial bibliographic import and screening to Rayyan-based screening and downstream analyses of the systematic review database about articles, membranes, cells, and readouts.

This repository supports collaborative code sharing, workflow traceability, and reproducibility.

------------------------------------------------------------------------

## Repository Structure

``` text
01_IMPORT/
02_RAYYAN/
03_RESULTS_articles/
04_RESULTS_membranes/
05_RESULTS_cells/
06_RESULTS_readouts/
Database.accdb
README.md
.gitignore
CITATION.cff
```

### Folder Description

#### `01_IMPORT/`

Contains scripts and input files used for the bibliographic import and filtering, including:

-   loading R packages
-   preprocessing and filtering scripts
-   bibliographic files from PubMed and Scopus
-   support Excel files used in the scripts

**Output**:

-   Excel with the counts of records passing each filtering step
-   Excel with the list of included records for each filtering step - .bib file for subsequent title and abstract screening in Rayyan

#### `02_RAYYAN/`

Contains scripts and files used for screening refinement after title and abstract screening in Rayyan, including:

-   import and refinement of Rayyan screening export file
-   extraction of inclusion and exclusion metadata set during title and abstract screening in Rayyan

**Output**:

-   Excel with the updates counts
-   Excel with the list of included records and their attributes for subsequent full-text screening

#### `03_RESULTS_articles/`

Contains the script necessary to import data from the custom-built Microsoft Access database: `00_access-import.R`

Contains scripts for article-level analyses, including:

-   inclusion rates across organs
-   exclusion reasons
-   publication year of included articles
-   source of chip platforms: in-house vs commercial
-   aim category of included articles

**Output**:

-   Excel files
-   Graphs (.png, .svg)

#### `04_RESULTS_membranes/`

Contains scripts for membrane-related analyses, including:

-   membrane material and origin
-   coating
-   macroscale characteristics
-   microscale characteristics
-   frequency reporting of membrane-related parameters
-   specific membrane parameter analyses (thickness, pore size, spacing, stiffness)

**Output**:

-   Excel files
-   Graphs (.png, .svg)

#### `05_RESULTS_cells/`

Contains scripts for cell-related analyses, including:

-   distribution of cell combinations across organs
-   species classification
-   frequency reporting of cell-related details
-   shear stress analyses

**Output**:

-   Excel files
-   Graphs (.png, .svg)

#### `06_RESULTS_readouts/`

Contains scripts for readout-related analyses, including:

-   readouts distribution across organs
-   readouts count summaries
-   organ-specific readout analyses

**Output**:

-   Excel files
-   Graphs (.png, .svg)

#### `Database.accdb`

Custom-built Microsoft Access database used during full-text screening to store extracted data. It is an essential resource for downstream analyses.

------------------------------------------------------------------------

## Workflow

The workflow is intended to be followed in sequential order.

### 1. Bibliographic import and systematic screening by publication type, language, publication year, and organs

Scripts in `01_IMPORT/` are used to:

-   load required libraries
-   import bibliographic records from source databases (PubMed, Scopus)
-   merge records
-   remove duplicates
-   apply publication type, language, year, and organ-related filtering
-   generate output for title and abstract screening in Rayyan

### 2. Screening refinement after Rayyan

Scripts in `02_RAYYAN/` are used to:

-   import records after Rayyan screening (title and abstract)
-   create list of records for full-text screening

### 3. Import data from the purpose-built custom Database

The script `03_RESULTS_articles/00_access-import.R` is used to:

-   load required libraries
-   import previously generated objects
-   connect to the Microsoft Access database
-   prepare tables used in downstream analytical scripts

### 4. Article-level analyses

Scripts in `03_RESULTS_articles/` are used to analyze:

-   inclusion and exclusion rates across organs
-   exclusion reasons
-   publication trends over time
-   study aims
-   sources of chip platforms

### 5. Membrane-level analyses

Scripts in `04_RESULTS_membranes/` are used to evaluate:

-   membrane materials and origins
-   coatings
-   structural characteristics at the macroscale and microscale level
-   reporting frequency of membrane-related parameters
-   analysis of membrane-related parameters (thickness, pore size, spacing, stiffness)

### 6. Cell-level analyses

Scripts in `05_RESULTS_cells/` are used to assess:

-   distribution of cell combinations across organs
-   cell species used
-   reporting frequency of cell-related parameters
-   shear stress-related analysis

### 7. Readout-level analyses

Scripts in `06_RESULTS_readouts/` are used to summarize:

-   readout distribution across organs
-   organ-specific readout distributions
-   readout frequency and count analyses

------------------------------------------------------------------------

## Included Input Files

This repository includes essential workflow inputs required for reproducibility, such as:

-   bibliographic exports from PubMed and Scopus
-   support Excel files used to set filter/organ-assignment criteria
-   Rayyan export file
-   the Microsoft Access database (`Database.accdb`)

These files are included because they are integral to the internal analytical workflow.

------------------------------------------------------------------------

## Software Requirements

### Recommended software

-   **RStudio**
-   **Microsoft Access Database Engine / compatible ODBC driver** for `.accdb` access

### Additional dependencies

Some scripts may also rely on:

-   temporary local directories for export steps
-   previously generated `.RData` objects

------------------------------------------------------------------------

## How to Use This Repository

1.  Clone the repository locally.
2.  Open the project in RStudio.
3.  Install all missing R packages.
4.  Start with scripts in `01_IMPORT/`.
5.  Continue with scripts in `02_RAYYAN/`.
6.  Run `03_RESULTS_articles/00_access-import.R`.
7.  Proceed with downstream analysis scripts in the `03_RESULTS_articles/`, `04_RESULTS_membranes/`, `05_RESULTS_cells/`, and `06_RESULTS_readouts/` folders.
8.  Review all local paths and dependencies before execution.

Because several scripts rely on outputs generated in previous steps, the workflow should be executed sequentially.

This repository supports reproducibility by preserving the code and essential input files used throughout the systematic review workflow. However, some steps may still depend on intermediate `.RData` objects, database connectivity, and temporary directories.
