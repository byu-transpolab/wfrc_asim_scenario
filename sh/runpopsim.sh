#!/opt/homebrew/bin/fish
conda activate popsim
python py/runpopsim.py --config configs_popsim --data data_popsim --output output_popsim 
exit 0