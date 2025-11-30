# The benefits of alternative reproductive tactics depend upon spatial context

This repository contains the datasets and Rscripts used for the statistical analysis and graphical visualization present in the submitted manuscript.


## Abstract

Alternative reproductive tactics are observed in many animal species. Typically, less dominant/attractive males exploit the presence of dominant /attractive males to intercept approaching females. In tree crickets, where males attract mates using acoustic signals, non-signalling males may act as satellites to exploit signalling males. Alternatively, smaller, quieter males may use leaf baffles to amplify their songs, but the relative benefits of these tactics are unclear. Using experimental manipulations, we investigated the mating benefits of silent males behaving as satellites in the presence of calling or baffling males, and the potential mating costs incurred by callers in the presence of satellites, in the tree cricket *Oecanthus henryi*. We also examined the benefits of baffling in relation to satellites and callers. We found that silent males, satellites, callers and bafflers had similar mating success when females were on the same bush as males. Silent males did not gain additional mating benefits by behaving as satellites of callers. Calling males did not incur mating costs in the presence of satellites, but incurred heavy costs from simultaneously calling bafflers, when females moved across bushes. Our results demonstrate that the benefits of alternative reproductive tactics depend on spatial context.


## Guide to the Repository

-   The `code.R` (v1.4, manual version number) Rscript contains the entire R code that was used for the analysis and visualization of the data in the associated manuscript. More about the code and its execution later.

-   The `data_virgin_female_same_bush.csv` file contains the data for the within-bush experimental trials conducted to estimate the mating success of callers, satellites, and silent males when virgin females were used. Call effort data for figure S2 in supplementary document is extracted from this dataset. Data for parts of figures 3a and 3b is contained in this file.

-   The `data_virgin_female_same_bush_baffler.csv` file contains the data for the within-bush experimental trials conducted to estimate the mating success of bafflers and their satellites when virgin females were used. Data for parts of figures 3a and 3b is contained in this file.

-   The `data_summary_virgin_female_same_bush.csv` file contains all the data generated for the within-bush experimental trials conducted to estimate the mating success of bafflers and their satellites when virgin females were used.

-   The `data_summary_virgin_female_same_bush_wo_repeating_males.csv` file is similar to `data_summary_virgin_female_same_bush.csv` except that it contains data only for naive males. The analyses in the supplementary files concerning non-repeating males in within-bush trials not involving bafflers is performed using this dataset.

-   The `data_virgin_female_same_bush_baffler_wo_repeating_males.csv` file is similar to `data_virgin_female_same_bush_baffler.csv` except that it contains data only for naive males. The analyses in the supplementary files concerning non-repeating males in within-bush trials with bafflers is performed using this dataset.

-   The `data_mated_female_same_bush.csv` file contains the data for the within-bush experimental trials conducted to estimate the mating success of callers, satellites, and silent males when mated females were used. Data for parts of figures S4a and S4b from the supplementary information is contained in this file.

-   The `data_across_bush_single_male.csv` file contains the data for the across-bush (two bush) experimental trials conducted to estimate the mating success of lone callers and silent males when virgin females were used. Data for the analyses in table 2 and parts of figures 4a and 4b is contained in this file. Call effort data for figure S3 in supplementary document is extracted from this dataset. Likewise, the bodysize data is used to produce figures S11, S12 and tables S2, S3.

-   The `data_across_bush_two_male.csv` file contains the data for the across-bush (two bush) experimental trials conducted to estimate the mating success of callers and their satellites when virgin females were used. Data for the analyses in table 2 and parts of figures 4a and 4b is contained in this file. Call effort data for figure S3 in supplementary document is extracted from this dataset. Likewise, the bodysize data is used to produce figure S11 and table S2.

-   The `data_across_bush_baffler.csv` file contains the data for the across-bush (two bush) experimental trials conducted to estimate the mating success of bafflers and non-baffling callers when virgin females could hear both of them. Data for figure 5a is contained in this file.

-   The `data_across_bush_baffler_wo_repeating_males.csv` file contains the data for the across-bush (two bush) experimental trials conducted to estimate the mating success of naive bafflers and non-baffling callers when virgin females could hear both of them. The analyses in the supplementary files concerning non-repeating males in across-bush trials with bafflers is performed using this dataset.

-   The `data_mating_success_all_tactics_same_bush.csv` file contains the collated data for mating success of all males in within-bush experiments. It is used to run the analyses shown in table 1.

-   The `data_mating_success_wo_repeating_males_all_tactics_same_bush.csv` file contains the collated data for mating success of all males, cluding males used more than once, in within-bush experiments. It is used to run the analyses shown in table S1.

-   The `suppl_pilot.csv` file contains the data for the pilot across-bush experiments conducted with baffling males to standardize the experimental setup. This can be used to make figures S6 and S7.

-   The `suppl_across_bush_baffler.csv` file contains supplementary data for the across-bush experiments with bafflers and callers. It includes information about the relative distances and perceived loudness of bafflers and callers with respect to the the female and also the female choice parameters like decision latency and movement. It can be used to make figures 5b, S8, S9, and S10.

-   The `suppl_body_size_baffler.csv` file contains body length data (in mm) for bafflers and callers used in the across-bush experimental trials. It can be used to make figure S13.

-   The `suppl_baffling_propensity.csv` file contains body length data (in mm) for males that behave as bafflers or non-bafflers on plant 1 (conducive to the baffling tactic) of the across-bush experimental trials. It can be used to make figure S14.

-   The `data_bootstrapped_CI.csv` file contains the exact bootstrapped values used to make the 95% confidence intervals of all the plots present in the submitted manuscript (figures 3a, 3b, 4a, 4b, and 5a) and supplementary information (figures S4a and S4b). Future executions of the `code.R` script will result in slightly different values of the confidence intervals because of the randomized nature of the bootstrapping procedure.


## Executing the Rscript

-   Executing the entire `code.R` script (one way is to click 'Source' in RStudio) would import data from the .CSV files in the folder and conduct the analyses required to estimate the mating success for our experimental paradigms and also conduct other supplementary analyses.
-   Two folders are created as a result of running the entire script:
    -   The ***output*** folder, which will contain six .CSV files: `chi_square_tests.csv` which contains the results ($\chi^2$, df, and p-values) for the chi-square tests, `confidence_intervals.csv` which contains the bootstrapped 95% confidence intervals (calculated during that run of the code) for the mating success of each of the male reproductive tactics (treatments), and four files, namely, `table1/2/3/S1.csv` containing the binomial GLM results of their respective counterparts in the manuscript.
    -   The ***plots*** folder, which will contain .PNG figures with names corresponding to the figures in the associated manuscript and supplementary material.

## 
