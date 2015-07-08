#!/bin/bash
FILES=../data/data_dump/risk_info*
for f in $FILES;
do
  for i in `seq 1 4`;
  do
    ./status_base sample num_samples=2000 num_warmup=2000 \
      random seed=420 \
      id=$i \
      data file=$f \
      output file=../data/mcmc_out/status_base_${i}.csv &
  done
  wait
#  for i in `seq 1 4`;
#  do
#    ./status_phylo sample num_samples=5000 num_warmup=5000 \
#      random seed=420 \
#      id=$i \
#      data file=$f \
#      output file=../data/mcmc_out/status_phylo_${i}.csv &
#  done
#  wait
done
