library(googledrive)

# first, find the thingy
osl_sheet_location <- drive_find(pattern = 'OSL', type = 'spreadsheet')

# then grab the matching sheet
osl_to_watch <- drive_get(as_id(osl_sheet_location$id))

# download it
drive_download(osl_to_watch, type = 'xlsx')

