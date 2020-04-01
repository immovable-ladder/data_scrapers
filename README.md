# data_scrapers
Scrapers developed to pull information from various sources (PDFs, Email text files) into Google Sheets. The scripts are scheduled to run regularly via CRON to keep the Sheets files updated.

## INSO
The INSO scraper reads text files and extracts relevant data, formats it in a data frame, and appends it to a master data file retrieved from Google Sheets. That data is then merged with a locations database to find lat-long points which match the administrative units of each report.

Master sheets and Drive folders would need to be manually set up for this script to work on others' systems, and IDs for them entered into the script. Anyone running the script themselves must authenticate themselves to the Google Drive API.

## Ebola
The Ebola scraper reads PDF files and extracts relevant data, formats it in a data frame, and appends it to a master data file retrieved from Google Sheets. This scraper is ultimately intended to extract multiple data frames and upload to multiple separate sheets, as the source files cover a number of different aspects of the crisis.

As with the INSO scraper, master sheets and Drive folders would need to be manually set up for this script to work on others' systems, and IDs for them entered into the script. Anyone running the script themselves must authenticate themselves to the Google Drive API.

## Development
These scrapers are intended for ongoing use, and may be updated in future as the source files change.
