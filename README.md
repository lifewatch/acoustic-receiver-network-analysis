# Belgian passive acoustic network (BPAN) analysis (C-POD network)
Analysis of data from C-POD stations:
1. Data exploration & statistical tests: cpod_analysis.R
2. Correlation of different variables (e.g. lost minutes, detection positive minutes (DPM), days of activity): correlation.R
3. Heatmap to visualize data per hour per day per year: heatmap.R
4. NMDS & PERMANOVA: NMDS_PCA.R
5. Cross correlation function for time series of tide and noise: cross_correlation_function.R

# Passive Belgian acoustic receiver network (PBARN) analysis
Analysis of data from acoustic receivers:
1. Data exploration: explore_data.R
2. Calculation of receiver efficiency index (REI): REI_compute.R
3. Interpolation of calculated REI: interpolation.py
4. Assessment of station performance based on how much time a receiver was lost/broken/replaced and data availability: station_performance.R

# Reference
Ellis, R. D., Flaherty-Walia, K. E., Collins, A. B., Bickford, J. W., Boucek, R., Walters Burnsed, S. L., & Lowerre-Barbieri, S. K. (2019). Acoustic telemetry array evolution: From species- and project-specific designs to large-scale, multispecies, cooperative networks. Fisheries Research, 209(April 2018), 186–195. https://doi.org/10.1016/j.fishres.2018.09.015

