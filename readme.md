# Characterizing Cellular Differentiation Potency and Waddington Landscape via Energy Indicator

We quantitatively characterize the cellular differentiation potency and Waddington landscape using Hopfield energy.

 

## Systems Requirements

 

The scripts were written in R language.

##  

## Usage

 

There are two main folders:

 

The folder "data" contains related test data and annotation files.

1. “test.txt” is a gene expression matrix (read count), which contains 500 stem cells (totipotent blastomere-like cells, TBLC; and pluripotent stem cells, PSCs) with different differentiation potentials.


2. “barcode.txt” is an annotation file containing barcodes and corresponding stem cell types.


3. "updata_test.txt" is embryogenesis data, which is an example to simulate the cell transitions. It is worth noting that the “updata_test.txt” is the pre-processed data.


4. "index_test.txt" is an annotation file containing development stages and corresponding cell states.


5. "node.txt" is a result file of node update during reprogramming process of somatic cell.


6. "HVG.txt" is a list of highly variable genes.

   ​

The folder "scripts" contains following scripts: 

1. "Pre-processing.R" is used for data preprocessing and feature selection.

2. "NetworkConstruction.R" is used to construct the Hopfield network at stable fixed point.

3. "NetworkUpdata.R" is used to simulate the cell transitions from state A to state B and reconstruct the energy landscape.

   When using these scripts, you need to write the right file path.

