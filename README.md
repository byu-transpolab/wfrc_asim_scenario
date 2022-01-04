<!-- ReadME to instruct how to use the input files on box
my folder will eventually match -->

# Making sense of the input files to ActivitySim
The following files are used to run ActivitySim in the
Salt Lake City Area.

* *skims_wfrc.omx* - This is the all of the skims for the Salt
Lake City Area and includes modifications such as removing
external zones.

* *tour_mode_choice_coeffs.csv* and *trip_mode_choice_coeffs.csv* -
These files contain the adjusted coefficients to match the WFRC
mode choice distribution by purpose.

* *tour_mode_choice.csv* and *trip_mode_choice.csv* - These are edited
so that Ferry is not included in the Salt Lake City Area model.

* *synthetic_persons* and *synthetic_households* - These contain
all the attribute information for all of the persons and
households in the simulation.

* *land_use_taz.csv* - This file contains the land use information
specific to the Salt Lake City Area.

* *settings.yaml* - This file accounts for the above input files
and includes the steps for multiprocessing.
