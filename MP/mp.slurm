#!/bin/bash

#SBATCH --job-name=mp
#SBATCH --output=/home/WUR/mosqu003/backup/swo/MP/model/log/log_%A.txt
#SBATCH --error=/home/WUR/mosqu003/backup/swo/MP/model/log/error_%A.txt
#
#SBATCH --time=01:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=20
#SBATCH --mem-per-cpu=2500
#
#SBATCH --mail-type=ALL
#SBATCH --mail-user=iago.mosqueira@wur.nl

# load modules
module load gcc/7.2.0
module load R/4.1.2

# export variables

# your job

Rscript model_jabba.R
