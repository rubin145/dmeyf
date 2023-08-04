#!/bin/bash

for split in '015' '02' '03' '05'
do
  ./run_exp_12.R --split_fraction $split
  sleep 60
done