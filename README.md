# KeK

This repository contains scripts for the KeK (Kwaliteit en Kosten) project of UNL (Universiteiten van Nederland).

# Project Goals

- Retrieve data from the KeK database

- Process and join data from multiple sources

- Send processed data to the Data Entry App (built by EY) using an API


# Project tree

```
KeK/
├── README.md                # Project overview and instructions
├── R/                       # Helper functions (e.g., utility functions)
├── 00_project/                    # Scripts to setup RStudio project
├── 01_read/                    # Scripts for reading data from various sources
├── 02_prepare/                 # Scripts for data preparation and cleaning
├── 03_validate/                # Scripts for data validation
├── 04_send/                    # Scripts for sending data via API
```


# Usage

1. Run the project setup script to initialize the environment:

```
source("00_Project_Setup.R")
```

This script will execute all necessary setup steps in the correct order.

2. Use the appropriate scripts in the read/, prepare/, validate/, and send/ directories to process and send data.

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
