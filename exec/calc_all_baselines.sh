#!/bin/bash

for fixed in 'threshold' 'n'
do
  for split in '015' '03' '02' '05'
  do
      ./calc_baseline_scores.R --split_fraction $split --fixed $fixed
      sleep 30
  done
done