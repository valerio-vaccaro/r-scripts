#!/bin/sh

rm scriptpubkeys_per_block.csv
wget https://opreturn.org/scriptpubkeys_per_block.csv

Rscript analize.R

wget https://opreturn.org/raw/price.csv
cat price.csv | tr '[]' '  ' > price2.csv

Rscript price_from_chain.R
date > update.txt