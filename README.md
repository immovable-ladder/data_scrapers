# data_scrapers
Scrapers developed to pull information from various sources (PDFs, Email text files) into a master Google Sheets file. The scripts are run on a schedule to keep the Sheets files updated.

## INSO
The INSO scraper reads text files and Excel files, extracts relevant data, formats it in a data frame, and appends it to a master data file retrieved from Google Sheets. That data is then merged with a locations database to find lat-long points which match the administrative units of each report.

Users must authenticate themselves to the Google Drive API and provide links to Drive folders/Sheet IDs.

## Ebola
The Ebola scraper reads PDF files and extracts relevant data, formats it in a data frame, and appends it to a master data file retrieved from Google Sheets. This scraper is ultimately intended to extract multiple data frames and upload to multiple separate sheets, as the source files cover a number of different aspects of the crisis.

As with the INSO scraper, users must authenticate themselves to the Google Drive API and provide links to Drive folders/Sheet IDs.
