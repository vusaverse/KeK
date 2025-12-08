# KeK

This repository contains scripts for the KeK (Kwaliteit en Kosten) project of UNL (Universiteiten van Nederland).

> In 00_project/authenticate_kek.R set the appropriate environment (test or prod).

# Project Goals

- Retrieve data from the KeK database

- Process and join data from multiple sources

- Send processed data to the Data Entry App (built by EY) using an API


# Project tree

```
KeK/
├── README.md                # Project overview and instructions
├── R/                       # Helper functions (e.g., utility functions)
├── 00_project/              # Scripts to setup RStudio project
├── 01_read/                 # Scripts for reading data from various sources
├── 02_prepare/              # Scripts for data preparation and cleaning
├── 03_validate/             # Scripts for data validation
├── 04_send/                 # Scripts for sending data via API
├── 99_setup.R               # Setup RStudio project settings
└── 99_run_pipeline.R        # Script to run and validate the entire pipeline
```


# Usage

1. Run the project setup script to initialize the environment:

```
source("99_setup.R")
```


This script will execute all necessary setup steps in the correct order.

2. To run the entire pipeline, which includes reading, preparing, validating, and sending data, use:


```
source("99_run_pipeline.R")
```


This script will execute all stages of the pipeline in the correct order and perform validation checks.

3. If you need to run specific stages or scripts individually:

- Use scripts in `01_read/` for reading data from various sources
- Use scripts in `02_prepare/` for data preparation and cleaning
- Use scripts in `03_validate/` for data validation
- Use scripts in `04_send/` for sending data via API

4. Helper functions can be found in the `R/` directory. These can be sourced individually as needed or will be automatically loaded when running the full pipeline.

5. For any project-specific setup or configuration, refer to scripts in the `00_project/` directory.

**Note:** It's recommended to use the `99_run_pipeline.R` script for normal operations as it ensures all steps are executed in the correct order and includes validation checks.


# Key Functions

The helpers.R script in the R/ directory contains several utility functions, including:

- get_kek_data(): Retrieves data from the KeK database

- send_data_to_kek(): Sends processed data to the KeK API

# Authentication

Authentication with the API is handled automatically in the setup process. The access token is stored as an environment variable for security.

# Notes

- It's recommended to source the helper functions in scripts that interact with the API.

- Data from multiple sources are joined in the respective scripts within the prepare/ directory.

- The validate/ directory contains scripts for data validation, which were used in earlier stages of the project.
