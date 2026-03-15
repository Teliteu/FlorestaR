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

