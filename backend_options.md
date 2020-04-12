# Dashboard back end options

Draft version of 2020-04-12

A summary of options for tools for creating and hosting the Virginia Renewables Progress Dashboard.

**Background:** The front-end design for the dashboard is currently under discussion with DMME. Salient issues include:

 * *Selection of data elements:* Choice of exactly which data elements will be incorporated into the dashboard.
    - Example elements: 
      - Total installed renewables generation capacity (MW, by year); 
      - Renewables generation as a fraction of total generation (% GWh/GWh, by year)
      - etc.
    - Almost surely, elements will be added incrementally: start simple, then build

 * *Visual design of individual data elements:* Exactly how each data element will be visualized -- dials, line graphs, maps, etc.
 * *Overall dashboard layout:* How the individual data elements will be arranged, framed, and presented to create an overall user experience.

These front-end design issues are the subject of a separate [design document](https://3.basecamp.com/4370323/buckets/15566178/google_documents/2472959220).
 
Based on identified requirements for the front end, we need to design the dashboard's *back end* - the system that prepares and
delivers the required visual elements to end users, to display in their respective web browsers. 

## Requirements and preferences for the dashboard back end

A non-exhaustive list...

 * Dashboard must be suitable for use by the general public. 
 * Shouldn't require user to install specialized plug-ins (e.g., Flash) or other extra software. Ideally, should run on any major browser.
   - Don't necessarily need to support older versions of major browsers (e.g., Internet Explorer v. 2.0)
   - **Question about accessibility:** What requirements apply regarding accessibility by, e.g., blind users or others with disabilities?
 * Should work smoothly and cleanly on mobile devices.
 * User experience should be snappy. Avoid anything that creates lags to reload data or render visualizations.
 * User experience should be solid and robust.  Must not feel kvetchy, subject to program hangs, error messages, etc.
 * Updates with new data should be automatic and, to the extent feasible, immediate.
 * Need for manual maintenance of back-end (installing software patches, updating data series, etc.) should be very low. Once it is set up, it should just run, requiring very little attention.
 * System should be easy to modify and extend. It must be easy and low-cost to change or add any visual elements. 
   - "Easy" here does not necessarily mean "by someone without training". Can allow that implementing modifications requires specialized skills.
 * Finished system must be transferrable to DMME. No lock-in.
   - In practice, DMME may contract with CCPS for continuing system support, subject to terms mutually agreed.
   - If DMME opts to take over responsibility for system operations in-house, or with a different vendor, DMME would then be responsible for all continuing service charges and maintenance, including skilled staff support.
   - Any system modifications or improvements, including changes to or addition of visual elements, would involve DMME arranging for their choice of
      (i) a follow-on contract with CCPS, (ii) in-house skilled resources, or (iii) a different third-party vendor.
 * No corporate branding can appear prominently. Only branding should be those of DMME and/or Cooper Center/UVA.
   - Data sources, software dependencies, and other resources should be acknowledged discretely.
 * URL must be customizable. Hosting must appear to come from dmme.virginia.gov, virginia.edu, cooopercenter.org, or a new custom URL. (Not, e.g., shinyapps.io.)
 * System absolutely must be secure. Keep out hackers.
 * Subject to the all the above: keep expenses low. Can't support high monthly total cost of ownership.



Issues already decided:

 * *Database:* Postgres
 * *Data munging and structuring:* R scripts

Issues still under discussion:



Selection of dashboard elements: choice of exactly which data elements will be incorporated into the dashboard.

    


Render visualizations:
R code, esp. `ggplot2`
Dash/Plotly

Arrange visualizations into a dashboard layout:
R Shiny + 
Dash


