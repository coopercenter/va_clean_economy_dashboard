# Dashboard back end options

Draft version of 2020-04-12

A summary of options for tools for creating and hosting the Virginia Renewables Progress Dashboard.

**Background:** The Virgina Renewables Progress Dashboard will display information about the Commonwealth's progress towards acheiving its quantitative targets towards its adopted clean energy goals. The design for the dashboard is currently under discussion with staff at the Virginia Department of Mines, Minerals & Energy (DMME). Salient issues include:

 * *Selection of data elements:* Choice of exactly which data elements will be incorporated into the dashboard.
    - Example elements: 
      - Total installed renewables generation capacity (MW, by year); 
      - Renewables generation as a fraction of total generation (% GWh/GWh, by year)
      - etc.
    - Almost surely, elements will be added incrementally: start simple, then build.

 * *Visual design of individual data elements:* Exactly how each data element will be visualized -- dials, line graphs, maps, etc.
 * *Overall dashboard layout:* How the individual data elements will be arranged, framed, and presented to create an overall user experience.

These front-end design issues are the subject of a separate [design document](https://3.basecamp.com/4370323/buckets/15566178/google_documents/2472959220).
 
Based on identified requirements for the front end, we need to design the dashboard's *back end* - the system that prepares
and delivers the required visual elements to end users, for display in their respective web browsers. 

## Requirements and preferences for dashboard back end

A non-exhaustive list...

 * Dashboard must be suitable for use by the general public. 
 * Shouldn't require user to install specialized plug-ins (e.g., Flash) or other extra software. Ideally, should run on any major browser.
   - Don't necessarily need to support older versions of major browsers (e.g., Internet Explorer v. 2.0)
   - **Question about accessibility:** What requirements apply regarding accessibility by, e.g., blind users or others with disabilities?
 * Should work smoothly and cleanly on mobile devices.
 * User experience should be snappy. Avoid anything that creates lags to reload data or render visualizations.
 * User experience should be solid and robust.  Must not feel kvetchy, subject to program hangs, error messages, etc.
 * Should be able to scale up smoothly to many simultaneous users without performance degrading.
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

## Back-end design: Issues and choices

Some issues about the back-end design have already been settled:

 * *Database:* Postgres db hosted on Azure
   - Postgres is open-source, industry-standard, and free to use. 
   - Postgres commands are crafted in PostgreSQL, which is likewise standard.
   - The database can be moved very easily to any other cloud service or in-house server. Monthly rental charges on cloud services are low. Operation is secure at industry standard.
 * *Data munging and structuring:* R scripts
   - R is open-source and free to use. It is the second-most popular language for data science (after Python), with millions of users, thousands of add-on packages, and a deep and active market for skilled staff and consultants.

Other issues about the back end are still under discussion:

  * Tools for generating visualizations (dials, line graphs, maps, etc.) from structured data objects.
  * Tools for organizing visual elements into a dashboard.
  * Possibly, a system for containerizing and porting the finished dashboard to a hosting solution.
  * A hosting solution.

For each issue, there are many options available. Here we focus on a narrowed list of "finalists" that we've identified through research.

All coding options identified here are free and open-source. Hosting options are available at manageable cost and don't create lock-in problems. Solutions apparently meet all the other requirements listed above.

## Visualization tools

### Option 1: R code, esp. `ggplot2`

R offers great options for creating figures, tables, maps (including interactive maps) that are extremely flexible and customizable. CCPS staff are already fluent in these tools. All are free and open source.

### Option 2: Dash Open Source for R, by Plotly

[Dash](https://dash.plotly.com/introduction) is an open-source framework for building web applications for data visualization that sit on top of Python or R models. Its viz tools look great. Because they are designed specifically for integration into web apps, they may have some additional advantages.

### Comments

It is surely possible in principle to mix visualizations based on Dash, ggplot2, and other open-source data visualization packages for R. However, our visualization strategy would tilt towards Dash if we plan to use Dash Enterprise as our hosting solution. If we plan to implement the dashboard using R Shiny, then `ggplot2` and friends probably make the most sense, especially as CCPS is already familiar with them.

## Dashboarding tools

### Option 1: R Shiny

### Option 2: Dash


## Containerization option: Docker




## Hosting solution

### Shinyapps.io

A hosted solution from R Studio. 

Pros:

  * 

One-click deployment and updating.

Cons:

  * Costs $300/mo (or $3,300/yr) for the tier of service that is appropriate for this deployment, which is relatively expensive both in absolute terms and relative to Heroku.

Other notes/issues/questions:

  * Only for deployment of R Shiny apps.



### Heroku

A general-purpose "Platform as a Service" (PaaS) hosting solution for web apps. Will launch mult

Pros:

  * Supports "one-click" deployment of web apps from Github.
  * Supports Docker containers.

Other notes/issues/questions:

  * Unclear whether Heroku will fully and effectively support an R Shiny deployment.

### Dash Enterprise

(Have reached out to learn more about pricing and technical specs of Dash Enterprise hosting solution.)



