#!/bin/bash

cd $(dirname "BASH_SOURCE[0]")

rm -rf ./*.fasta

rm -rf ./*.csv

rm -rf ./*.tsv

#rm -rf seqkited.fasta

rm -rf temp.snps

rm -rf nucmer.delta

rm -rf nucmer.coords

rm -rf nucmer.snps

cat *.tar | tar -xvf - -i

cat *.fasta > all.fasta

seqkit grep -s -p - all.fasta -v |sed 's/^M$//' |awk '/^>/{f=!d[$1];d[$1]=1}f' > seqkited.fasta

#nucmer --forward -p nucmer ./ref.fa ./seqkited.fasta

nucmer --forward -p nucmer ../data/ref_.fasta ./seqkited.fasta

show-coords -r -c -l nucmer.delta > nucmer.coords

show-snps nucmer.delta -T -l > nucmer.snps

#java -jar ../snps-process-1.0-SNAPSHOT-jar.jar ./nucmer.snps

#./snps-process ./nucmer.snps ../data/gff_c.tsv ../data/ref_.fasta

#Rscript run.R
