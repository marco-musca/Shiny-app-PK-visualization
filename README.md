# Shiny app PK visualization 

## Objective

The aim of this project is to build a shiny application to visualize and interact with PK data starting from SDTMs.
The idea is to make accessible to everyone with basic knowledge of the study data structure, a basic visualization of PK data through a web-interface application.

## Rationale
CDISC standards are usually known by statistical programmers, data managers and statisticians, but less known by other figures like pharmacologists or pharmacometricians.
Therefore, listing, figures and about PK data are often requested by above mentioned professionals to statistical programmers.

The idea is to let explore PK data even to professionals that don't know CDISC standards enough.

Why SDTMs as starting point:
- Although ADaMs might appear the most suitable data structure to explore this type of data, PK analyses are often performed by CROs, weeks after DBlock, therefore ADPC or ADPP datasets are not available for several weeks. Having the possibility to explore PK data just after SDTMs transfer can save time and reduce future workload for pharmacometricians and pharmacologists.
- PC-like domain is available in the library 'pharmaversesdtm', therefore it's easier to build this application using that type of dataset as refence. On the other hand, ADPC-like domains are not available.
