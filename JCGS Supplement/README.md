# README: Supplemental Files 
## "Clusters beat Trend!? Testing feature hierarchy in statistical graphics"

******

Included Files:

* __Article Code and Figures.R__    
This file contains code to generate all of the figures and models presented in the paper. Data about users has been anonymized to comply with IRB regulations, and is provided in an Rdata file. This file requires the following code and data files:
    * __MixtureLineups.R__ - Functions to generate lineups, plots, and simulation data. 
    * __theme_lineup.R__ - ggplot2 theme for displaying lineups
    * __color\_perceptual\_kernel.csv__, __shape\_perceptual\_kernel.csv__ - Kernels from Demiralp et al. (2014) used to identify maximally different colors and shapes (subject to some constraints) for use in this experiment.    
    Data source: [UWData github repository](https://github.com/uwdata/perceptual-kernels)
    * __SmallSimulation.Rdata__ - Simulation data used to generate Figure 4. Provided for convenience, as the simulation takes a considerable amount of time to run. Code to generate the simulation file is also included if verification of the simulation methodology is desired. 
    * __modeldata.Rdata__ - Participant response data from the experiment. Used to fit all of the models presented in the paper and supplement. 
    
* __Appendix Code and Figures.R__    
This file contains code to generate all of the figures and models presented in the online appendix, with some additional comments/documentation. This file requires the following code and data files: 
    * __MixtureLineups.R__ - Functions to generate lineups, plots, and simulation data. 
    * __theme_lineup.R__ - ggplot2 theme for displaying lineups
    * __SimulationDatasetCriteriaTurk16.Rdata__ - Simulation data to generate figures. Code is included to generate this file if verification of the simulation methodology is desired. 
    * __modeldata.Rdata__ - Participant response data from the experiment. Used to fit all of the models presented in the paper and supplement. 
    

