# FlorestaR 🌳

Authors
Mateus Pinheiro - Master's Student at University of Brasília (UnB)

**FlorestaR** is an R package for 3D simulation and visualization of forest dynamics. It allows users to simulate growth, mortality, and recruitment based on forest inventory data and neural network models.

## Features
* **3D Visualization:** Render immersive forest environments using `rayshader` and `tree3d`.
* **Management Simulation:** Apply logging scenarios and observe collateral damage.
* **Growth Modeling:** Integrated with Artificial Neural Networks for DBH prediction.
* **Time-lapse:** Generate GIFs of the forest evolution over time.

## Installation

You can install the development version of FlorestaR from GitHub with:

```r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("Teliteu/FlorestaR")
```
## Data Requirements

To run the simulation, your working directory must contain a trained model (`model.rds`) and an inventory file (`TEST.csv`) with the following mandatory columns:

| Column | Description |
| :--- | :--- |
| **`i_arbre`** | Unique tree identifier (essential for tracking individuals). |
| **`Parcela`** | Plot ID (used to organize the spatial grid). |
| **`specie`** | Scientific name (used for logging rules and reports). |
| **`GrEc_xx`** | Ecological Group (used by the ANN and recruitment logic). |
| **`DAP`** | Diameter at Breast Height in cm (base for growth). |
| **`X`, `Y`** | Cartesian coordinates (local position in meters). |
| **`Volume`** | Individual volume in m³ (used for harvest balance). |

## Quick Start Example

**Important:** Ensure that both your `.csv` inventory and `model.rds` are located in the same folder before running the script.

```r
# 1. Install the package from GitHub (only needed once)
# install.packages("devtools")
devtools::install_github("Teliteu/FlorestaR")

# 2. Load the library
library(FlorestaR)

# 3. Set your working directory
setwd("path")

# 4. Run the full forest project simulation
results <- executar_projeto_florestal(
  arq_csv = ".csv",                 # Forest inventory data
  arq_rds = "model.rds",            # Trained ANN model
  pasta_saida = "",                 # Output folder name
  area_total = 18,                  # Total area in hectares
  anos_simulacao = 18,              # Number of years to simulate
  salvar_animacao = TRUE            # Generate the 3D time-lapse GIF
)

```
## Outputs Generated

After the simulation finishes, the specified output folder will contain:

* **`FINAL.csv`**: A clean, wide-format spreadsheet detailing the complete history of the forest. It contains one row per tree, including static attributes (Species, Plot, X, Y) and dynamic DBH values across all simulated years. Cells are left blank for years before a tree was recruited or after it died/was harvested.
* **`Animacao/`** *(if `salvar_animacao = TRUE`)*: A subfolder containing the final 3D time-lapse in (`Animacao`).
