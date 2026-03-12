# breast-aesthetics-analysis
Quantifying Aesthetic and Naturalness Outcomes from Post-Operative Breast Augmentation Images 

This project evaluates whether image-derived geometric implant-position metrics are associated with aesthetic and naturalness ratings from surgeons and laypersons. Using cross-classified mixed-effects logistic and ordinal models, I assessed whether post-operative ratio, ratio difference, and upper-pole proportion predict higher ratings for outcomes while accounting for clustering by rater and image. I then translated model results into predicted probability curves to improve clinical interpretability. 

## Data Availability

The raw survey data used in this project cannot be shared publicly due to participant confidentiality agreements.

To reproduce the analysis pipeline:

1. Place the raw survey files in `data/raw/`
2. Run `R/build_analysis_dataset.R`
3. Processed datasets will be written to `data/processed/`

File names expected:

- LAY_PERSON_SURVEY_DATA.xlsx
- SURGEON_SURVEY_DATA.xlsx
- PATIENT_DATA.xlsx
