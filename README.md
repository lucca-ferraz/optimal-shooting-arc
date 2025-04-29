# optimal-shooting-arc

This repo contains my initial code for a research project with Dr. Powers concerning optimal shooting arcs in the NBA. The code is intended to run using the SportVU publically-available NBA tracking data from the 2015-16 season.

To run the code, first run the Python file in the same directory you have the tracking data in (from this repo: https://github.com/sealneaward/nba-movement-data/tree/master?tab=readme-ov-file). This creates CSV files with the tracking data for each game in the repo. I recommend stopping the Python file after a few games, as it takes hours to run for all games and will clog up your computer storage. 

After converting the JSONs to CSVs, you can run the R code. The file shotarc.R contains all the code used to attempt to replicate the results in this paper: https://www.degruyterbrill.com/document/doi/10.1515/jqas-2018-0064/html. The file animation.R contains a basic script to animate different plays based on their eventids. 
