# STICr Functions Repository

## Overview
R functions for processing STIC (Stream Temperature, Intermittency, and Conductivity) data using the STICr package and FaaSr framework.

## Repository Structure
**Repository**: [nirali112/sticr-functions](https://github.com/nirali112/sticr-functions)

```
sticr-functions/
├── faasr_tidy_hobo_sticr.R
├── faasr_calibrate_stic.R
├── faasr_classify_wetdry.R
├── faasr_final_stic.R
└── README.md
```

## Data Source
HydroShare South Fork Kings Creek STIC dataset: https://www.hydroshare.org/resource/77d68de62d6942ceab6859fc5541fd61/

## Functions

### 1. `faasr_tidy_hobo_sticr.R`
- **Purpose**: Cleans and formats raw STIC data
- **Input**: Raw CSV files from `faasr/stic-data`
- **Output**: Tidied data in `faasr/sticr-workflow/step1-tidy/`
- **STICr Functions**: Uses STICr `tidy_hobo_data()` for raw HOBO file processing, custom tidying for research data

### 2. `faasr_calibrate_stic.R`
- **Purpose**: Applies conductivity calibration using STICr functions
- **Input**: Step 1 outputs from `step1-tidy/`
- **Output**: Calibrated data in `faasr/sticr-workflow/step2-calibrated/`
- **STICr Functions**: `get_calibration()`, `apply_calibration()`

### 3. `faasr_classify_wetdry.R`
- **Purpose**: Classifies wet/dry conditions based on conductivity
- **Input**: Step 2 outputs from `step2-calibrated/`
- **Output**: Classified data in `faasr/sticr-workflow/step3-classified/`
- **STICr Functions**: `classify_wetdry()`

### 4. `faasr_final_stic.R`
- **Purpose**: Generates final analysis-ready datasets with QAQC
- **Input**: Step 3 outputs from `step3-classified/`
- **Output**: Final datasets in `faasr/sticr-workflow/step4-final/`
- **STICr Functions**: `qaqc_stic_data()`

## Processing Pipeline
```
Raw STIC Data → Tidy → Calibrate → Classify → Final Output
```

## Usage

### Execute via GitHub Actions Workflow
**Repository**: [nirali112/sticr-workflow-step](https://github.com/nirali112/sticr-workflow-step1)

1. **Upload data** to MinIO bucket: `faasr/stic-data`
2. **Go to workflow repository** → Actions tab
3. **Select** "Click on each fucntions"
4. **Click** "Run workflow"
5. **Monitor progress** through all 4 steps
6. **Check results** in MinIO: `faasr/sticr-workflow/step4-final/`


## Features
-  **STICr compliant** - uses official STICr package functions
-  **FaaSr integration** - designed for cloud execution

## Dependencies
- `STICr` - Official STIC data processing package
- `tidyverse` - Data manipulation and analysis
- `lubridate` - Date/time handling
- `FaaSr` - Cloud function execution framework

## Output Format
All functions produce STICr-compliant CSV files with columns:
- `datetime` - Timestamp
- `condUncal` - Uncalibrated conductivity
- `tempC` - Temperature in Celsius
- `SpC` - Specific conductivity (after calibration)
- `wetdry` - Wet/dry classification (after classification)
- `QAQC` - Quality flags (after final processing)

## Related Repositories
- **Workflow Execution**: [sticr-sequential-workflow](https://github.com/nirali112/sticr-sequential-workflow) - GitHub Actions workflow
- **Individual Steps**: [sticr-workflow-step1](https://github.com/nirali112/sticr-workflow-step1) - Individual function workflows
