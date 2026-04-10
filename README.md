# Lusekalkulator (Lice Calculator)

A tool to predict the development of salmon lice (*Lepeophtheirus salmonis*) in salmon farms.

## Model Overview

The application has been updated to use the latest **Stige et al. (2024)** joint life-cycle model. This state-of-the-art model replaces the previous ZINB approach and provides more accurate predictions by simultaneously modeling three distinct lice stages.

### Key Features
- **Joint Stage Modeling**: Simultaneously predicts Sessile (Chalimus), Other Motile, and Adult Female lice.
- **Environmental Factors**: Incorporates sea temperature, fish weight, and fish abundance (thousands per cage).
- **Biological Interactions**: Accounts for the presence of Wrasse (cleaner fish) and their effect on adult female mortality.
- **Lagged Infection Pressure**: Uses a temperature-dependent development lag ($dT$) to accurately account for the time between larval production at neighboring farms and settlement as sessile lice on the target farm.
- **Uncertainty Estimation**: Employs Monte Carlo simulations (1,000 runs) to calculate the probability of exceeding the regulatory limit of 0.5 adult female lice per fish.

## Input Variables

| Variable | Description | Units |
| --- | --- | --- |
| Temperature | Sea surface temperature | °C |
| Sessile Lice | Attached stages (Chalimus) | lice per fish |
| Other Mobile | Pre-adult and non-mature mobile stages | lice per fish |
| Adult Female | Sexually mature female lice | lice per fish |
| Fish Weight | Mean weight of fish in the cage | kg |
| Fish Count | Number of fish in thousands | 1,000s |
| Wrasse | Presence of wrasse cleaner fish | Yes/No |
| Infection Pressure | Larval pressure from neighboring farms | Index (P) |

## Python Implementation

### Tech Stack
- **Framework**: [Streamlit](https://streamlit.io/) for the web interface.
- **Mathematics**: `NumPy` and `SciPy` for statistical modeling and Monte Carlo simulations.
- **Data Handling**: `Pandas` for data processing.
- **Visualization**: `Matplotlib` for generating development plots with uncertainty intervals.
- **API Integration**: Robust client for the Institute of Marine Research (HI/IMR) API to fetch real-time infection pressure data.

### Structure
- `logic.py`: Implements the Stige et al. (2024) mathematical model and simulation engine.
- `imr_api.py`: API client for fetching external infestation pressure data.
- `main.py`: Streamlit entry point, managing UI, inputs, and coordination between the API and model.

### How to Run
1. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```
2. Start the application:
   ```bash
   streamlit run main.py
   ```
