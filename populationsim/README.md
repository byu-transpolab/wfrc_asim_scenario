# PopulationSim scenarios

This directory contains several PopulationSim configs/scenarios for the WFRC region.
Each scenario should be a directory with subdirectories `configs`, `data`, and `output`.
The `.gitignore` file here ignores these `output` subdirectories since the synthetic population files can be quite large.

There is also an R script here that combines and cleans the raw SE data files provided by the WFRC model so they are in a nicer format for PopulationSim.
This should be done before running the popsim setup targets and PopulationSim itself.

**The script at the moment needs a bit of work.**
