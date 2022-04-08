#!/bin/bash

#SBATCH --partition=general
#SBATCH --ntasks-per-node=32
#SBATCH --nodes=1
#SBATCH --time=96:00:00
#SBATCH --mem=250G

module load R/3.4.2-fasrc01

R CMD BATCH /Scripts/Step_2_run_model_express.R