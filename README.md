# 🔬 Proteomic Insights into Inflammatory Bowel Disease  
**Biomarker Discovery in the UK Biobank**

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)  
![Python](https://img.shields.io/badge/Python-3.10+-blue.svg)  
![Status](https://img.shields.io/badge/status-Research%20Project-yellow)

## 📌 Overview

This project aims to develop predictive models for **Inflammatory Bowel Disease (IBD)** onset and relapse using **longitudinal proteomic data** from the **UK Biobank**. By identifying early proteomic signatures of disease activity, we aim to contribute to precision medicine tools that enable risk stratification and early clinical intervention.

---

## 👨‍💻 Authors

- **Francisco Salamanca¹** — MSc Bioinformatics Student, Universidad Nacional de Colombia  
- **David Gomez²** — MSc Industrial Engineering Student, Universidad Nacional de Colombia  
- **Daniel Bonilla³** — System Engineering Student, Universidad Nacional de Colombia  

---

## 🎯 Objective

To discover and model **omic biomarkers** associated with IBD progression using longitudinal multi-omic datasets, focusing on the **temporal trajectories** of proteomic signatures and their relation to clinical outcomes.

---

## 📚 Background

IBD, including **Crohn’s disease** and **ulcerative colitis**, is a chronic and relapsing disorder. Despite treatment advances, predicting disease course remains difficult. Integrating **multi-omic profiling** with clinical variables opens new opportunities for personalized disease monitoring.

---

## 🧪 Approach

### 🔹 Data  
- **UK Biobank** longitudinal data  
- **Olink proteomic panels**  
- Genomic and clinical/phenotypic modules  

### 🔹 Participants  
- Individuals with **multiple time-points** of proteomic data  
- With or without IBD diagnosis, matched by age and sex where needed  

### 🔹 Outcome Variables  
- IBD diagnosis and subtype (Crohn's or UC)  
- Relapse or progression indicators (clinical codes, hospital visits)

### 🔹 Methods  
- Data harmonization and preprocessing  
- Covariate extraction (BMI, smoking, medications, etc.)  
- Temporal modeling of proteomic trajectories  
- Predictive modeling:  
  - Random Forest  
  - Survival Analysis  
  - Neural Networks  
- Cross-validation and model comparison  

---

## ✅ Deliverables

- [x] Cleaned longitudinal dataset  
- [x] Integrated data tables with covariates  
- [x] Feature engineering and preprocessing pipeline  
- [x] Modeling scripts and performance evaluation  
- [ ] Final visualizations and report  
- [ ] Draft manuscript for submission  

---

## 🧩 Folder Structure

```bash


📁Horizontal_UKB/
├── README.md
├── requirements.txt # Project dependencies
├── main/
│ ├── Horizontal_UKB.ipynb # Main analysis notebook
│ ├── data/ # UK Biobank data
│ │ └── UK_BIOBANK_DATA/ # Multi-omic & phenotypic datasets
│ ├── outputs/
│ │ ├── graphs/ # Boxplots, distributions, etc.
│ │ └── results/ # Intermediate model/data outputs
│ └── src/ # Source code
├── phenotype/ # Phenotype-specific notebook
│ └── phenotype.ipynb
├── physical_measures/ # Notebook on physical metrics
│ └── physical_measures.ipynb
```
---
## 👤 Author

**Francisco Salamanca**  
Bioinformatician | MSc in Bioinformatics  
[GitHub](https://github.com/fsalamancar) • [Website](https://fsalamancar.github.io/) • [LinkedIn](https://www.linkedin.com/in/fjosesala/) • [IKMB](https://www.ikmb.uni-kiel.de/people/francisco-salamanca/)
