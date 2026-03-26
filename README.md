# Helicopter_ARU_Pilot

ARU01spbyloc_sens1.25.csv - BirdNet Analyzer output with sensitivity 1.25 for ARU01 (helicopter landing pad)
ARU02spbyloc_sens1.25.csv - BirdNet Analyzer output with sensitivity 1.25 for ARU02 (Snowbird Pass)
BirdNet_CombinedTable.csv - Combined BirdNET output table containing all species detections, timestamps, and confidence scores.
BirdNet_Output_Merge.R - Script used to read, merge, and clean BirdNET CSV outputs into a unified dataset.
ARU_Detections_Summary.R - Script for validation analysis, including: filtering detections by confidence, generating species‑level detection counts, plotting validation outcomes by confidence bin, fitting logistic regression to estimate true‑positive probability, computing precision vs. threshold curves, identifying optimal confidence thresholds, visualizing threshold trade‑offs (precision vs. data retention)
