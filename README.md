# bibliometric-data-preprocessing
This repository contains the code and resources needed to perform a complete and rigorous bibliometric analysis in the field of *low-code* and *no-code* technologies. It has been designed to facilitate the cleaning, merging and normalization of bibliometric data extracted from the **Scopus** and **Web of Science (WoS)** databases, while respecting the steps defined by the **PRISMA** method.
### Repository contents
- Automation scripts:
  - Merge data sets from multiple bibliometric databases.
  - Data cleansing (normalization of author names, keywords, affiliations, etc.).
  - Removal of incomplete or redundant records.
- Main features:
  - Use of the **mergeDbSources** algorithm to merge data from WoS and Scopus.
  - Standardize abbreviations and frequent terms using custom dictionaries.
  - Export of cleaned and enriched data in Excel format for further analysis.
- Technologies used:
  - Language : **R**
  - Libraries: `bibliometrix`, `dplyr`, `writexl`, `stringr`
### Project objectives
1. Provide a reproducible and transparent tool for bibliometric analysis.
2. Ensure rigorous data preparation prior to processing with tools such as **Biblioshiny** or **Bibliometrix**.
3. Facilitate the exploration of research dynamics in emerging fields such as *low-code* and *no-code* technologies.
### Use
Users can adapt the code to analyze similar domains or to meet their own bibliometric analysis needs. **The code is available in this repository to encourage transparency and collaboration.
### Contribution
Contributions are welcome to improve functionality or add new steps to the bibliometric analysis process.
