# Tables and Figures — Source Code Mapping


---

## Main Text

| ID | Label | Caption (shortened) | Output file | R Script | Line |
|----|-------|---------------------|-------------|----------|------|
| Figure 1 | `fig:main_map` | Map of Denmark and the event in 1825 | `Plots/Map.png` | `201_Main_map.R` | 178 |
| Table 1 | `tab:desc_pop` | Summary statistics — parish-level census data | `Tables/203_pop_descriptive.txt` | `203_Pop_results.R` | 49 |
| Figure 2 | `fig:bal` | Variable distributions (balancing plot) | `Plots/Balancing_plot.png` | `203_Pop_results.R` | 114 |
| Figure 3 | `fig:Sound_toll` | Number of ships — sum of inbound/outbound | `Plots/Ship_trafic.png` | `202_Sound_toll_results.R` | 114 |
| Figure 4 | `fig:channel` | Number of ships passing the Agger channel | `Plots/Ship_trafic_channel.png` | `202_Sound_toll_results.R` | 154 |
| Table 2 | `tab:reg_trade` | Channel introduction and trade | `Tables/202_sound_toll.txt` | `202_Sound_toll_results.R` | 190 |
| Figure 5 | `fig:pop1` | Effect of the Agger channel on population size | `Plots/Regression_plots/pop_dummy.png`, `pop_MA.png` | `203_Pop_results.R` | 128, 138 |
| Figure 6 | `fig:mech_occ` | Impact on occupational structure in 1901 | `Plots/Mechanism/All_occupations_dummy.png` | `205_Pop_mechanism.R` | 489 |
| Figure 7 | `fig:mech_occ2` | Effects on detailed occupational structure | `Plots/Mechanism/Detailed6789/Dummy_asi.png` | `205_Pop_mechanism.R` | 886 |
| Figure 8 | `fig:migr_fert` | Effects on fertility and internal migration | `Plots/Mechanism/fertility_Dummy.png`, `born_different_share_Dummy.png` | `205_Pop_mechanism.R` | (via `plot_mod()` loop) |
| Figure 9 | `fig:prop_score` | Soil type propensity scores before/after matching | `Plots/Propensity_before.png`, `Propensity_after.png` | `102_Matching_soil_types.R` | 103, 158 |
| Figure 10 | `fig:arch_desc` | Rate of coin findings over time | `Plots/Arch_descriptive.png` | `204_Archaeological_results.R` | 241 |
| Figure 11 | `fig:arch_reg` | Archaeological results — coins and buildings | `Plots/Regression_plots/arch_*_coins.png`, `arch_*_buildings.png` | `204_Archaeological_results.R` | 283, 325, 368, 409 |
| Figure 12 | `fig:arch_reg_boot` | Bootstrap distribution of parameter estimates at 1350 | `Plots/Regression_plots/arch_*_boot.png` | `204_Archaeological_results.R` | 283, 325, 368, 409 |
| Table 3 | `tab:arch1` | Archaeological regression results | `Tables/204_arch_main.txt` | `204_Archaeological_results.R` | 581 |

---

## Appendix

| ID | Label | Caption (shortened) | Output file | R Script | Line |
|----|-------|---------------------|-------------|----------|------|
| Table A1 | `tab:pop1` | Regression results for population size | `Tables/203_pop_main.txt` | `203_Pop_results.R` | 150 |
| Figure A1 | `fig:pop2_1787` | Multiverse of effect in 1787 | `Plots/Regression_plots/Multiverse_*_1787.png` | `203_Pop_results.R` | 461, 494, 532 |
| Figure A2 | `fig:pop2` | Multiverse of effect in 1901 | `Plots/Regression_plots/Multiverse_*.png` | `203_Pop_results.R` | 263, 364, 426 |
| Table A2 | `tab:cs_estimates` | Callaway and Sant'Anna estimates | (printed to console / inline) | `203_Pop_results.R` | — |
| Table A3 | `tab:occ1` | Effect on occupation in 1901 (HISCO 1–3) | `Tables/205_pop_mechanism_hisco.txt` | `205_Pop_mechanism.R` | 391 |
| Table A4 | `tab:occ2` | Effect on occupation in 1901 (HISCO 4–9) | `Tables/205_pop_mechanism_hisco.txt` | `205_Pop_mechanism.R` | 391 |
| Figure A3 | `fig:fishing_spinners` | Event plots — fishermen and spinners | `Plots/Mechanism/fish_*.png`, `spinning_*.png` | `205_Pop_mechanism.R` | (via `plot_mod()` loop) |
| Figure A4 | `fig:age_group` | Age group composition effects | `Plots/Mechanism/Age_composition_*.png` | `205_Pop_mechanism.R` | 1179, 1202 |
| Table A5 | `tab:A_arch1` | All parameters of Table 3, columns 1–4 | `Tables/204_arch_appendix_mod1_4.txt` | `204_Archaeological_results.R` | 593 |
| Table A6 | `tab:A_arch2` | All parameters of Table 3, columns 5–8 | `Tables/204_arch_appendix_mod5_8.txt` | `204_Archaeological_results.R` | 603 |
| Figure A5 | `fig:arch_reg1` | Archaeological results — full sample, normal dist. | `Plots/Regression_plots/arch_*_norm.png` | `204_Archaeological_results.R` | (norm loop) |
| Figure A6 | `fig:arch_reg_boot1` | Bootstrap distribution 1350 — full sample | `Plots/Regression_plots/arch_*_boot_norm.png` | `204_Archaeological_results.R` | (norm loop) |
| Figure A7 | `fig:arch_reg2` | Archaeological results — matched sample | `Plots/Regression_plots/arch_*_matched_norm.png` | `204_Archaeological_results.R` | (norm loop) |
| Figure A8 | `fig:arch_reg_boot2` | Bootstrap distribution 1350 — matched sample | `Plots/Regression_plots/arch_*_matched_boot_norm.png` | `204_Archaeological_results.R` | (norm loop) |

---

## Table output files summary

| File | Content | Script | Line |
|------|---------|--------|------|
| `Tables/202_sound_toll.txt` | Sound toll shipping regressions (Table 2) | `202_Sound_toll_results.R` | 190 |
| `Tables/203_pop_descriptive.txt` | Population descriptive stats (Table 1) | `203_Pop_results.R` | 49 |
| `Tables/203_pop_main.txt` | Population main regressions (Table A1) | `203_Pop_results.R` | 150 |
| `Tables/204_arch_main.txt` | Archaeological main results (Table 3) | `204_Archaeological_results.R` | 581 |
| `Tables/204_arch_appendix_mod1_4.txt` | Arch appendix full params, mod1–4 (Table A5) | `204_Archaeological_results.R` | 593 |
| `Tables/204_arch_appendix_mod5_8.txt` | Arch appendix full params, mod5–8 (Table A6) | `204_Archaeological_results.R` | 603 |
| `Tables/205_pop_mechanism_hisco.txt` | HISCO occupation mechanism (Tables A3–A4) | `205_Pop_mechanism.R` | 391 |
