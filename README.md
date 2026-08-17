# SAFPI TICS Submission Timing Dashboard

This repository contains a Shiny dashboard for exploring SAFPI teacher submission timing by form, week, and teacher grouping.

The app can be deployed from GitHub to Posit Connect. The main Shiny entry point is `app.R`.

## Data

The app uses two aggregated datasets:

`data/submission_timing_counts.rds` contains counts by:

- dashboard week
- week end date
- form number
- submission timing category
- scope: all teachers pooled, Zona, or Distrito

`data/teacher_scope_counts.rds` contains the number of unique teachers included in each dashboard selection:

- all teachers pooled
- each Zona
- each Distrito

The deployed app does not include teacher names, citizenship IDs, or individual-level teacher records.

## Required R Packages

- shiny
- dplyr
- ggplot2
- scales

## Submission Timing Labels

- **On time:** submitted on or before the end date of the week.
- **One week late:** submitted after the week end date, but within seven days.
- **More than one week late:** submitted more than seven days after the week end date.
- **Not submitted:** no submission date is registered for that teacher-week.

## Forms

- **Form 2:** student attendance and visit records.
- **Form 3:** pedagogical planning, including domains, activities, materials, skills, achievements, and related content.
- **Form 4:** weekly report on whether the planned content was covered.
