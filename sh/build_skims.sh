#!/opt/homebrew/bin/fish

conda activate ASIM_DEV

# uses fish getopts, https://github.com/jorgebucaran/getopts.fish
# 

getopts $argv | while read -l key value
  switch $key
    case m manifest
      set manifest $value
      echo "manifest directory: $manifest"
    case t tazmap
      set tazmap $value
      echo "tazmap: $tazmap"
    case o outfile
      set outfile $value
      echo "outfile: $outfile"
    case h help
      echo "Print help statement"
    case \*
      printf "error: Unknown option %s\n" $option
  end
end

python py/build_omx.py $manifest $outfile
