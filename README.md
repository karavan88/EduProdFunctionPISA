# Education Production Function: PISA 2018 Analysis

[![GitHub Pages](https://img.shields.io/badge/GitHub%20Pages-Live-brightgreen)](https://karavan88.github.io/EduProdFunctionPISA/) [![Quarto](https://img.shields.io/badge/Made%20with-Quarto-blue)](https://quarto.org/) [![R](https://img.shields.io/badge/R-4.0+-blue)](https://www.r-project.org/) [![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

## 📚 Research Overview

**Estimating Cognitive Skill Formation in Brazil, China, and Russia: An Education Production Function Approach**

This study examines cognitive skill formation in secondary education across Brazil, Russia, and selected Chinese administrative units using PISA 2018 data. It applies a value-added estimation strategy, using grade progression as a proxy for annual learning gains and controlling for age–grade effects through random intercepts.

## 🔗 Quick Access

-   **📖 [Read the Full Manuscript](https://karavan88.github.io/EduProdFunctionPISA/manuscript.html)** (Interactive HTML with sidebar navigation)
-   **📄 [Download PDF](docs/manuscript.pdf)** (Academic format)
-   **📝 [Download DOCX](docs/manuscript.docx)** (Editable format)

## 🏗️ Project Structure

```         
EduProdFunctionPISA/
├── 📁 00_documentation/          # Project documentation
├── 📁 01_data/                   # Raw and processed datasets (PISA 2018)
├── 📁 02_code/                   # Analysis scripts and utilities
│   ├── 00_libraries.R            # Package dependencies
│   ├── 00_quarto_utils.R         # Rendering utilities
│   ├── 01_data_prep.R            # Data preparation pipeline
│   ├── 02_descriptive.R          # Descriptive analysis & correlations
│   └── 03_modeling.R             # Multilevel regression modeling
├── 📁 03_output/                 # Analysis outputs and results
├── 📁 04_manuscript/             # Quarto manuscript source
│   ├── 00_run.R                  # Multi-format rendering script
│   ├── manuscript.qmd            # Main manuscript source
│   ├── styles.css                # Custom CSS styling
│   └── references.bib            # Bibliography
├── 📁 05_presentation/           # Presentation materials
├── 📁 docs/                      # GitHub Pages deployment
│   ├── index.html                # Landing page
│   └── manuscript.html           # Interactive manuscript
├── project_config.R              # Environment configuration
├── renv.lock                     # Package version control
└── README.md                     # This file
```

## 🚀 Getting Started

### Prerequisites

-   **R** (version 4.0 or higher)
-   **R Studio** or **VS Code**
-   **Quarto** (latest version)
-   **Git** (for version control)

### Installation

1.  **Clone the repository**

    ``` bash
    git clone https://github.com/karavan88/EduProdFunctionPISA.git
    cd EduProdFunctionPISA
    ```

2.  **Open R and restore the environment**

    ``` r
    # R will automatically activate renv
    renv::restore()
    ```

3.  **Project configuration is loaded automatically**

## 📊 Data Analysis Pipeline

### 1. Data Preparation (`02_code/01_data_prep.R`)

-   Loads PISA 2018 datasets (global and Moscow-specific)
-   Merges student and school data
-   Creates derived variables and standardizes measures
-   Generates analysis-ready datasets

### 2. Descriptive Analysis (`02_code/02_descriptive.R`)

-   Summary statistics by country/territory
-   Correlation analysis between learning outcomes
-   Distribution analysis across regions
-   Multiple visualization approaches

### 3. Multilevel Modeling (`02_code/03_modeling.R`)

-   **Baseline Model**: Null model with random intercepts
-   **Model 1**: Fixed effects with random intercepts
-   **Model 2**: Random slopes for SES
-   **Model 3**: Random slopes for school SES composition

## 🎨 Manuscript Rendering

### Render All Formats

``` r
# From the manuscript folder
source("04_manuscript/00_run.R")
```

This generates: - **PDF** → `04_manuscript/manuscript.pdf` - **DOCX** → `04_manuscript/manuscript.docx` - **HTML** → `docs/manuscript.html` (GitHub Pages ready)

### Individual Format Rendering

``` bash
# PDF only
quarto render 04_manuscript/manuscript.qmd --to pdf

# DOCX only
quarto render 04_manuscript/manuscript.qmd --to docx

# HTML for web
quarto render 04_manuscript/manuscript.qmd --to html --output-dir docs
```

## 📦 Key Dependencies

### Statistical Analysis

-   `tidyverse` - Data manipulation and visualization
-   `lme4` & `lmerTest` - Multilevel modeling
-   `haven` - SPSS/Stata data import
-   `performance` & `psych` - Model diagnostics

### Visualization & Tables

-   `ggplot2` & `ggeffects` - Publication-quality plots
-   `gt` & `gtsummary` - Formatted tables
-   `modelsummary` - Regression tables
-   `sjPlot` - Statistical visualizations

### Reporting

-   `quarto` - Reproducible manuscripts
-   `knitr` & `rmarkdown` - Dynamic documents

## 📈 Results Summary

### Learning Gains (EYOS - Equivalent Years of Schooling)

-   **Brazil**: 0.85 years per grade
-   **BSJZ (China)**: 1.12 years per grade\
-   **Hong Kong**: 0.94 years per grade
-   **Russia**: 0.89 years per grade
-   **Moscow**: 0.92 years per grade

### School-Level Variance (ICC)

-   **Highest**: BSJZ China (52%)
-   **Moderate**: Russia regions (25-35%)
-   **Lower**: Brazil, Hong Kong (15-20%)

### Key Predictors

1.  **Socioeconomic Status (ESCS)**: Strongest individual predictor
2.  **School SES Composition**: Peer effect equivalent to 1+ years schooling
3.  **Task Mastery**: Non-cognitive skills matter significantly
4.  **Grade Level**: Clear learning progression effects

## 👤 Author

**Garen Avanesian**\
Southern Federal University\
📧 avanesian\@sfedu.ru\
🔗 [GitHub Profile](https://github.com/karavan88)

## 📄 Citation

``` bibtex
@article{avanesian2024education,
  title={Estimating Cognitive Skill Formation in Brazil, China, and Russia: An Education Production Function Approach},
  author={Avanesian, Garen},
  year={2024},
  institution={Southern Federal University},
  url={https://github.com/karavan88/EduProdFunctionPISA}
}
```

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🤝 Contributing

Contributions, issues, and feature requests are welcome! Feel free to check the [issues page](https://github.com/karavan88/EduProdFunctionPISA/issues).

## 🔄 Version Control

This project uses `renv` for package management ensuring reproducibility across different R installations. The `renv.lock` file contains exact package versions used in the analysis.

## 📞 Support

If you encounter any issues or have questions about the analysis:

1.  **Check the manuscript** for methodological details
2.  **Review the code comments** in analysis scripts\
3.  **Open an issue** on GitHub
4.  **Contact the author** directly

------------------------------------------------------------------------

**📊 Made with ❤️ using R, Quarto, and rigorous statistical methods**

------------------------------------------------------------------------

::: {align="center"}
**🎓 Research • 📊 Analytics • 🔬 Reproducible Science**

*Making labor market research reproducible and accessible*
:::