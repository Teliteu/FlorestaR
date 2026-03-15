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

**Important:** Ensure that both `.csv` inventory and `model.rds` are located in the same folder before running the script.

```r
library(FlorestaR)

# 1. Set the working directory to the folder containing your files
setwd("path_folder")

# 2. Run the full forest project simulation
results <- executar_projeto_florestal(
  arq_csv = ".csv",                 # Forest inventory data
  arq_rds = "model.rds",            # Trained ANN model
  pasta_saida = "",                 # Output folder name (will be created)
  area_total = 18,                  # Total area in hectares
  anos_simulacao = 18,              # Number of years to simulate
  gerar_animacao = TRUE             # Generate the 3D time-lapse GIF
)
