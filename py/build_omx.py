# ActivitySim
# Copyright (C) 2016 RSG Inc
# See full license in LICENSE.txt.
# run from the mtc tm1 skims folder

import os
import sys

import pandas as pd
import numpy as np
import openmatrix as omx


def read_manifest(manifest_file_name):

    column_map = {
        'Token': 'skim_key1',
        'TimePeriod': 'skim_key2',
        'File': 'source_file_name',
        'Matrix': 'source_key',
    }
    converters = {
        col: str for col in column_map.keys()
    }

    manifest = pd.read_csv(manifest_file_name, header=0, comment='#', converters=converters)
    manifest.rename(columns=column_map, inplace=True)
    return manifest

def read_taz_map(taz_map_name):
    taz_map = pd.read_csv(taz_map_name, header=0, comment='#')
    empty = taz_map[taz_map['asim_taz'].isnull()]
    deleted_zones = empty['wfrc_taz'].tolist()
    return [x-1 for x in deleted_zones]


def omx_getMatrix(omx_file_name, omx_key):
  with omx.open_file(omx_file_name, 'r') as omx_file: 
    if omx_key not in omx_file.list_matrices():
      print("Source matrix with key '%s' not found in file '%s" % (omx_key, omx_file,))
      print(omx_file.list_matrices())
      raise RuntimeError("Source matrix with key '%s' not found in file '%s" % (omx_key, omx_file,))

  data = omx_file[omx_key]

  return data


def build_omx(manifest_dir, source_data_dir, dest_data_dir):

  manifest_file_name = os.path.join(manifest_dir, 'skim_manifest.csv')
  taz_map_name = os.path.join(manifest_dir, "skim_taz_map.csv")
  dest_file_name = os.path.join(dest_data_dir, 'skims.omx')
  
  print("Manifest file: ", manifest_file_name)
  print("Taz map: ", taz_map_name)
  print("Dest file: ", dest_file_name)
  
  with omx.open_file(dest_file_name, 'a') as dest_omx:
      manifest = read_manifest(manifest_file_name)
      delete_zones = read_taz_map(taz_map_name) 
      for row in manifest.itertuples(index=True):
  
          source_file_name = os.path.join(source_data_dir, row.source_file_name)
  
          if row.skim_key2:
              dest_key = row.skim_key1 + '__' + row.skim_key2
          else:
              dest_key = row.skim_key1
  
          print("Reading '%s' from '%s' in %s" % (dest_key, row.source_key, source_file_name))
          with omx.open_file(source_file_name, 'r') as source_omx:
  
              if row.source_key not in source_omx.list_matrices():
                  print ("Source matrix with key '%s' not found in file '%s" \
                  % (row.source_key, source_file_name,))
                  print(source_omx.list_matrices())
                  raise RuntimeError("Source matrix with key '%s' not found in file '%s" % (row.source_key, dest_omx,))
  
              data = source_omx[row.source_key]
  
              if dest_key in dest_omx.list_matrices():
                  print("Matrix '%s' already exists" % (dest_key,))
              else: 
                data = np.delete(data, np.r_[delete_zones], axis = 0)
                data = np.delete(data, np.r_[delete_zones], axis = 1)
                dest_omx[dest_key] = data
  


if __name__=='__main__':    
  print(f"Arguments count: {len(sys.argv)}")
  for i, arg in enumerate(sys.argv):
    print(f"Argument {i:>6}: {arg}")
  
  build_omx(sys.argv[1], sys.argv[2], sys.argv[3])
