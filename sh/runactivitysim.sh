#!/bin/fish

/opt/anaconda3/bin/activate ASIM_DEV
activitysim --config configs --data data_activitysim --output output_activitysim
exit 0