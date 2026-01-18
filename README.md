# Taiwan Traffic Accident
This repo is based on ST-RTA, but analyze with QGIS and R, analyze goal including:
- Mapper Algorithm: Identify high-risk road segments and intersections for traffic accidents in Taiwan, and integrate SNA with TDA for reveal main hotspot factor.
- Spatial Analysis: Explore spatial patterns and trends of traffic accidents in Taiwan using various spatial analysis techniques.

### Data Source
taiwan-latest-free : Data downloaded from [OpenStreetMap](https://download.geofabrik.de/asia/taiwan.html), with road network, which will be use in future, and also infrastructures, landuse, etc.
county, city, village : downloaded from [nlsc](https://maps.nlsc.gov.tw/MbIndex_qryPage.action?fun=8), with counties, city, and village boundaries.

### Useful tutorials

-   [QGIS Tutorials and Tips](https://www.qgistutorials.com/en/)
-   [GeoTech Center Model Courses](https://www.geotechcenter.org/model-courses.html)


### Description
This repo includes the following folders:
- `Analyze/Entropy`: Entropy analysis for traffic accident data.
- `Analyze/Mapper`: Topological Data Analysis (TDA) Mapper algorithm analysis for traffic accident data.
- `Analyze/SNA`: Social Network Analysis (SNA) for traffic accident data based on Mapper Algorithm.
- `Analyze/SpeedDifference`: Calculate speed difference for traffic accident data based onb the road network.

KDE:
- `Analyze/KDE`: Kernel Density Estimation (KDE) for traffic accident data.
- `Analyze/WKDE`: Weighted Kernel Density Estimation (KDE) for traffic accident data.
- `Analyze/NKDE`: Network-based Kernel Density Estimation (KDE) for traffic accident data.
