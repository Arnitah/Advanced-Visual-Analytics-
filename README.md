# Advanced-Visual-Analytics

Computer-vision pipelines and visualization tools for facial photograph preprocessing and fairness-focused model evaluation.

Overview
This repository contains utilities, notebooks, and scripts for preprocessing facial photographs, building reproducible computer-vision evaluation pipelines, and producing visualizations that make per-skin-tone performance easy to inspect.

Key features
- Image preprocessing pipelines (face detection, alignment, color normalization)
- Visualization tools for per-group performance and failure-mode analysis
- Reproducible evaluation notebooks and scripts for computing per‑tone metrics
- Helper utilities for reconstructing public datasets from source URLs and label files when licensing permits

Goals
- Provide reusable tooling to audit and visualize model performance across Fitzpatrick skin types and other demographic bands
- Enable independent developers and researchers to run per‑tone analyses with minimal setup
- Share clear examples for fairness-focused evaluation and mitigation experiments

Quickstart
1. Clone the repo and open a Jupyter environment (local or Colab).
2. Place images or reconstructed image paths under data/sources/ and label files under data/labels/.
3. Run notebooks in /notebooks to reproduce sample preprocessing and evaluation flows.
4. Use scripts in /eval to compute per‑tone metrics and generate visualization reports.

Recommended structure
- /notebooks — analysis and reproducible evaluation notebooks
- /src — preprocessing and model evaluation code
- /eval — scripts for per‑tone metric computation and report generation
- /data — expected location for source images and label files (not committed)

Contact
Enita Tankari — Independent Developer, Nitah Skincare Project
anitaenita94@gmail.com | https://github.com/Arnitah

Notes
- This README intentionally focuses on evaluation and fairness tooling. If you want an expanded README with badges, license, contribution guidelines, or sample notebooks, tell me which additions you'd like and I will update the file.
