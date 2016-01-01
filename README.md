# pbrr
R Code for analysis of PBR Tier System simulations.

Typical `source()` order for running scripts would be:

1. *PBR_FileIO.R*
  * The `write_inits()` function is relied on heavily for batching runs of simulations.
  
2. *PBR_create_input_files.R*
  * Requires the boiler plate *input.par* file [located here](https://github.com/John-Brandon/PBR-Tier-System/tree/master/PBR%20Netbeans) 
  * See the Notes in the header comments for the naming convention of input files for each trial.
  * Please note the comment regarding directory structure next to the initialization of the `code_dir` variable. This could be improved, but for now, it will require modification by the user to match where the code resides in their personal directories. 

3. *PBR_batch.R*
  * Contains functions for running batches of simulations given lists of input file names. 
  * Note: These functions were developed to run under Mac OS X, and the `system()` calls would likely need to be modified under a different OS. 
  * Future versions of these functions could be made more cross-platform friendly by making the `system()` calls conditional on the value returned from, for example `.Platform$OS.type`

4. *PBR_fortran_output.R*
  * Contains functions for compiling depletion statistics and plotting. 

5. *PBR_Fortran_controller.R*
  * Runs scripts above, compiles depletion statistics and creates some plots.

  


