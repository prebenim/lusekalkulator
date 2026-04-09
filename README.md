# Lusekalkulator (Lice Calculator)

A tool to predict the development of salmon lice (*Lepeophtheirus salmonis*) in salmon farms. It uses a statistical model based on historical data (2012–2014) from various locations along the Norwegian coast.

## Overview

The application forecasts the number of adult female lice and other mobile stages of lice per fish for the next one, two, and three weeks. It also calculates the probability of exceeding the regulatory limit of 0.5 adult female lice per fish.

The model is a **Zero-Inflated Negative Binomial (ZINB)** model, accounting for the fact that many observations have zero lice and the remaining follow a negative binomial distribution.

## Original R Shiny Implementation

The original project was built using the R Shiny framework.

### Key Components:
- `app.R`: Main Shiny application file containing UI and Server logic.
- `form.html` & `header.html`: HTML templates for the UI.
- `www/`: Static assets (CSS, JS, images).
- `Dokumentasjon.Rmd`: Documentation of the model and its background.

### Current Status: Broken
The R Shiny implementation is currently non-functional due to:
1. **Missing Data Files**: Several required files are missing from the repository:
   - `MobileTotaltFra2012SisteUker.txt`
   - `lusedata.csv`
   - `MobileTotaltFra2012.txt`
2. **Hardcoded Logic**: The application relies on specific local file structures and Norwegian naming conventions.
3. **Inconsistencies**: There appear to be some bugs in the R code, such as indexing mismatches in parameter arrays.

## Norwegian to English Dictionary

| Norwegian | English |
| --- | --- |
| Lus | Lice |
| Hunnlus | Female lice |
| Merd | Fish cage / Pen |
| Lokalitet | Location / Site |
| Rensefisk | Cleaner fish |
| Smittepress | Infection pressure |
| Fastsittende | Attached (lice stage) |
| Mobile stadier | Mobile stages (lice) |
| Kjønnsmodne | Sexually mature |
| Uke | Week |
| År | Year |
| Vekt | Weight |

## Transition to Python

We are migrating this application to a modern Python framework (Streamlit) to improve maintainability, portability, and to fix the existing bugs.
