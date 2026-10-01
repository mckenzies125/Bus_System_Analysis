# OCCT Bus Ridership & Service Analysis

This project analyzes **1,044,087 ridership records** from Off Campus College Transport (OCCT), Binghamton University's student-operated bus system, to identify transportation patterns and evaluate potential service improvements.

Using **R, Tableau, geospatial visualization, inferred origin-destination patterns, and negative binomial regression**, the analysis focuses on three operational questions:

1. How does ridership vary across routes, stops, locations, and times of day?
2. Is there evidence of demand for bidirectional Campus Shuttle service?
3. Does Main Street ridership change as Westside buses become more crowded?

The results show substantial variation in demand across routes, directions, stops, and time periods. They also provide evidence that Main Street may function as a complementary corridor during periods of higher Westside demand.

## Project Overview

OCCT provides transportation between Binghamton University's main campus, residential areas, and downtown Binghamton.

Rather than evaluating the system using aggregate ridership alone, this project examines how transportation demand changes according to:

- Route
- Direction of travel
- Stop
- Time of day
- Day of week
- Bus fullness
- Interactions between related corridors

The analysis combines exploratory visualization with statistical modeling to translate more than one million individual boarding records into potential operational insights.

## Data

The analysis uses **1,044,087 recorded bus boardings** containing information about riders, routes, stops, vehicles, dates, and boarding times.

The dataset records **entries rather than exits**, which required destination and return-trip behavior to be inferred for portions of the analysis.

Data preparation included:

- Cleaning route and stop labels
- Processing dates and boarding times
- Identifying route direction
- Linking stops to geographic coordinates
- Aggregating boardings by stop, hour, route, and direction
- Estimating vehicle fullness from sequential boarding records
- Constructing inferred origin-destination and reverse-trip patterns

## Exploratory Ridership Analysis

Interactive Tableau dashboards and R visualizations were used to examine system-wide ridership patterns.

The analysis showed that ridership is concentrated around a relatively small number of high-activity routes and stops.

### Major System Hub

**University Union** was the most frequently observed stop in the dataset and functions as the central hub of the OCCT system.

Other high-activity locations included:

- Mohawk
- Engineering Building
- Hillside
- UCLUB
- University Downtown Center

Removing University Union from geographic visualizations revealed additional clusters around campus-adjacent locations, off-campus housing, and downtown routes that were otherwise obscured by the Union's high boarding volume.

### Temporal Patterns

Ridership was concentrated primarily during **weekday daytime and evening hours**, particularly from late morning through early evening.

Different corridors also exhibited distinct directional patterns. For example:

- **Westside Inbound** was more active earlier in the day
- **Westside Outbound** became more active later in the afternoon and evening

These patterns are consistent with recurring travel between residential areas and the main campus.

## Bidirectional Campus Shuttle Analysis

The Campus Shuttle operates primarily as a one-directional loop. One goal of this project was to investigate whether some riders experience inefficient travel because their desired destination is geographically close but located in the opposite direction of the route.

Because the dataset contains boarding locations but not exit locations, trips were reconstructed by examining sequential boardings by the same rider on the same day.

### Inferring Reverse Trips

For each rider and date, sequential trips were compared to identify cases in which a later trip appeared to reverse an earlier trip.

Two trips were classified as an approximate reverse pair when:

- The origin of the second trip was within **200 meters** of the inferred destination of the first trip, and
- The inferred destination of the second trip was within **200 meters** of the origin of the first trip.

This procedure identified:

- **18,794 reverse-trip matches**
- **11,897 rider-day combinations** containing at least one reverse trip
- **18,471 exact reverse matches**
- **323 approximate reverse matches**

The median direct distance between the paired stops was approximately **313 meters**.

### Interpretation

The analysis provides evidence of repeated bidirectional travel within the Campus Shuttle system, but the demand was **not evenly distributed across all stop pairs**.

Some pairs demonstrated substantially more balanced travel in both directions than others.

The results therefore do not support treating every Campus Shuttle stop as having equal need for bidirectional service. Instead, they suggest that bidirectional service could be evaluated for **specific high-demand stop pairs and time periods**.

Multiple major stops were active simultaneously from approximately **10:00 AM to 8:00 PM**, providing a potential window for targeted bidirectional service.

## Main Street & Westside Corridor Analysis

The project also investigated whether Main Street ridership changes when Westside buses experience greater demand.

Westside had higher overall ridership than Main Street, but the central question was whether Main Street might function as a **complementary corridor** when Westside buses become more crowded.

Inbound and outbound service were modeled separately.

## Estimating Bus Fullness

The original dataset did not contain direct onboard passenger counts.

Bus fullness was therefore estimated by tracking cumulative boardings for each:

- Date
- Vehicle
- Route
- Direction

The count was reset after arrival at University Union or after a sufficiently long gap between observations.

Assuming a bus capacity of **51 passengers**, estimated fullness was calculated as:

\[
\text{Fullness} =
\frac{\text{Estimated Onboard Passengers}}{51}
\]

Because this measure is inferred from boardings rather than observed passenger counts, it should be interpreted as an approximation of vehicle occupancy.

## Negative Binomial Regression

Boarding counts were overdispersed, making a Poisson model inappropriate for the corridor analysis.

A **negative binomial regression** was therefore used to model expected Main Street boardings while allowing the variance to exceed the mean.

The model took the general form:

\[
\log(\mu_i)
=
\beta_0
+
\beta_1(\text{Westside Fullness}_i)
+
\beta_2(\text{Stop}_i)
+
\beta_3(\text{Hour}_i)
+
\beta_4(\text{Day of Week}_i)
\]

where \(\mu_i\) represents expected Main Street boardings.

This allowed the relationship between Westside fullness and Main Street activity to be evaluated while controlling for routine differences associated with **stop, hour, and day of week**.

## Results

### Inbound Service

Westside Inbound fullness was **positively and statistically significantly associated** with Main Street Inbound boardings.

The estimated incidence rate ratio was:

\[
IRR = 1.83
\]

Because fullness was measured as a proportion from 0 to 1, a more practical interpretation is that a **10 percentage-point increase in Westside Inbound fullness was associated with approximately a 6% increase in expected Main Street Inbound boardings**, holding stop, hour, and day of week constant.

Descriptively, mean Main Street Inbound boardings increased from **3.43 to 3.78** during higher-Westside-demand periods, while the median remained at 2.

The model therefore suggests a positive but relatively modest relationship between the two inbound corridors.

### Outbound Service

The relationship was stronger for outbound service.

The estimated incidence rate ratio for Westside Outbound fullness was:

\[
IRR = 3.91
\]

A **10 percentage-point increase in Westside Outbound fullness was associated with approximately a 15% increase in expected Main Street Outbound boardings**, holding stop, hour, and day of week constant.

The descriptive results were more nuanced: median Main Street Outbound boardings decreased from 7 to 4 during high-Westside periods, while mean boardings remained approximately constant at 10.

This suggests that outbound ridership was relatively skewed and that some high-ridership stop-hour observations may contribute substantially to the modeled relationship.

## Key Findings

The analysis produced several operational findings:

- OCCT demand was concentrated around major routes, central campus stops, and recurring weekday travel periods.
- University Union functioned as the dominant hub of the system.
- **18,794 inferred reverse-trip matches** provided evidence of bidirectional travel demand within the Campus Shuttle system.
- Bidirectional demand varied considerably across stop pairs rather than occurring uniformly across the route.
- Main Street ridership was positively associated with Westside fullness after controlling for stop, hour, and day of week.
- A **10 percentage-point increase in Westside Inbound fullness corresponded to approximately 6% higher expected Main Street Inbound boardings**.
- A **10 percentage-point increase in Westside Outbound fullness corresponded to approximately 15% higher expected Main Street Outbound boardings**.
- Service planning may benefit from considering **route, direction, stop, time of day, and corridor interactions** rather than relying solely on aggregate ridership.

## Operational Implications

The results suggest that Main Street may function as a **complementary corridor** during periods of higher Westside demand.

However, the analysis does not establish that individual riders switch from Westside to Main Street when buses become crowded. The regression identifies an association between corridor conditions and boarding activity under comparable stop, hour, and day-of-week conditions.

Similarly, the Campus Shuttle analysis does not support converting the entire route to bidirectional service. Instead, the evidence suggests that potential changes should focus on **specific stop pairs and high-demand time windows**.

## Limitations

Several limitations are important when interpreting the results.

### Inferred Destinations

The dataset records boardings but does not record where passengers exit.

Destination and reverse-trip analyses therefore rely on subsequent boarding behavior to approximate previous destinations. Riders may walk between stops or make other trips between observations.

### Estimated Bus Fullness

Fullness was reconstructed from sequential boarding records and an assumed **51-passenger capacity** rather than directly observed onboard passenger counts.

The resulting measure is therefore an estimate rather than an exact occupancy measurement.

### Incomplete Boarding Records

Students are expected to scan their university IDs when entering buses, but not every rider necessarily does so.

The dataset may therefore undercount actual ridership.

### Observational Analysis

Associations identified by the statistical models should not be interpreted as causal effects.

For example, the positive relationship between Westside fullness and Main Street ridership does not establish that Westside crowding directly causes riders to switch routes.

## Tools & Methods

- **R**
- **Tableau**
- `dplyr`
- `ggplot2`
- `glmmTMB`
- `lubridate`
- `tidyr`
- `stringr`
- Negative binomial regression
- Geospatial analysis and visualization
- Origin-destination inference
- Count-data modeling
- Exploratory data analysis
- Data cleaning and aggregation

## Repository Structure

### `Prelim_Vis.R`

Performs initial exploratory analysis of rider demographics, stops, routes, academic periods, and other system characteristics.

### `Code_for_Explor_Vis.r`

Creates exploratory visualizations of route, stop, and temporal ridership patterns.

### `Code_for_GeoGraph_Vis.r`

Processes stop coordinates and produces geographic representations of boarding and inferred destination activity.

### `Extra_Campus_Shuttle_Analysis.r`

Reconstructs sequential rider trips and identifies potential reverse-trip patterns used to evaluate bidirectional Campus Shuttle demand.

### `Bidirectional_graphical_representation.r`

Creates visualizations of inferred bidirectional travel patterns and stop-pair imbalance.

### `Westside_MainSt_Popularity.r`

Estimates bus fullness and fits negative binomial regression models to evaluate the relationship between Westside fullness and Main Street ridership.

### `Skrastins_IMP_Capstone.pdf`

Full capstone report describing the methodology, visualizations, statistical models, results, limitations, and transportation recommendations.

## Key Takeaway

This project demonstrates an end-to-end analysis of **more than one million transportation records**, moving from raw boarding data to exploratory visualization, geographic analysis, inferred travel behavior, and statistical modeling.

Rather than treating ridership as a single aggregate measure, the analysis shows how **route direction, stop-level demand, time of day, vehicle fullness, and interactions between corridors** can provide a more detailed basis for evaluating public transportation service.

## Academic Context

This project was completed as the capstone for my **Individualized Major Program in Data Science** at **Binghamton University, State University of New York**.
