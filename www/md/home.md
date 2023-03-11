

<h3>
    <b>Exploring the SARS-CoV-2 genomic variants</b>
</h3>

<p>&nbsp;</p>
<p>Reverse transcription polymerase chain reaction (RT-PCR) was the first method developed for COVID-19 detection and is the current gold standard as it offers both high accuracy and throughput(<a href="https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.3.2000045">Corman et al. 2020</a>). it is increasingly critical to evaluate the performance of RT-PCR tests using both experimental tests and in-silico analysis. Here we present a feasible method to evaluate the detection efficiency of different real-time reverse-transcription polymerase chain reaction (RT-PCR) assays available as example data and users can submit their own assay for analysis. To begin with, we present several aspects of SARS-CoV-2 genomic mutational features, such as average mutating frequency of different gene regions, single nucleotide polymorphism (SNP) distribution, mutation hotspots in both RNA and proteins, etc. The reference sequence of SARS-CoV-2 we used is NC_045512.2(<a href="https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2/">Wu, F. et al. 2020</a>). By tracking mutational trends in viral sequences and evaluating the effectiveness of assay based on mutation ratio in detecting regions, we provide options for further design of highly sensitive, clinically useful assays. Our analysis may provide new strategies of antiviral therapy based on the molecular characteristics of  this novel virus(<a href="https://www.preprints.org/manuscript/202004.0529/v1">Mercatelli and Giorgi. 2020</a>). (Fig. 1).</p>
<p>&nbsp;</p>
<p align="center">
<img src="img/workflow.png" width="800" hegiht="1000">
</p>
<p style="text-align:center">Fig. 1 Framework of CovidMutations. (A) Mutation analysis workflow of CovidMutations. (B) RT-PCR assay validation. </p>
<p>The fasta file of SARS-CoV-2 genome is made available through the <a href="https://www.gisaid.org/">GISAID</a> platform. For every week we would refresh the sequence data in our web to track mutation profile in SARS-CoV-2 genome for better assay design. In some cases, users can log in GISAID to access the real-time data. In the Browse session, the hCoV-19 can be downloaded following the instructions below (Fig. 2):</p>
<p>&nbsp;</p>
<p align="center">
<img src="img/GISAID.png" width="800" hegiht="1000">
</p>
<p style="text-align:center">Fig. 2 Retrieving data from GISAID </p>
<p>&nbsp;</p>
<p>In this platform, we make it convenient for users to evaluate their RT-PCR assays efficiency by assessing the mutation ratio in both primers and probes location in virus genome. Users can input their own primer sequence to first validate their design and guide fast diagnostic kits development. Also, users can submit a list of assay in .txt format (separated by tab) for comparison. A demo assay list is like:</p>
<p align="center">
    <img src="img/assaydemo.png" width="750" hegiht="1000"> 
</p>
<p style="text-align:center">Fig. 3 Assay demo for input </p>
<p <br>
    The "Assay", "F1", "F2", "R1", "R2", "P1", "P2" column is required.<br>
    "Assay": assay name<br>
    <b>'F1'</b>: 5' of forward primer<br>
    <b>'F2'</b>: 3' of forward primer<br>
    <b>'R1'</b>: 3' of reverse primer<br>
    <b>'R2'</b>: 5' of reverse primer<br>
    <b>'P1'</b>: start of probe in reference genome (5' for forward probe, 3' for reverse probe)<br>
    <b>'P2'</b>: end of probe in reference genome (3' for forward probe, 5' for reverse probe)
</p>










