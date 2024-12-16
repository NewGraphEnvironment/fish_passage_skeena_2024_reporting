Workflows for pre-processing of field data so it aligns with requirements for PSCIS upload, bcfishpass outputs and what is needed to complete reporting. These are often 2 way workflows where we use output csv spreadsheets to update our raw input datasheets through a manual copy-paste-special. These are designed to be self sufficient scripts with everything in them necessary to do there specific task with a clean working environment.

<br>


It may be a bit confusing to follow but we try to document it just the same. Tasks completed include:

# 0100-extract-inputs.R

-   Get UTMs of PSCIS, modelled crossing and dam sites from bcfishpass generated database when field survey indicates correct location.
-   Find road tenure information from bcfishpass.
-   Summarize fish sizes. When inputting data from the field, only one row needs to be inputted for each site in "Step 2 (Fish Coll. Data)". Once the script is run it processes the fish data and burns to a csv ready to copy-paste-special into Step 2.   
-   Determine replacement structure type and size based on field measured metrics.
-   Find the PSCIS reference ID for crossings that underwent Phase 1 assessments within the same program and cross-reference to Phase 2 data.

# 0120-photos.R

-   Backup original photos on a remote drive or server.
-   Resize photos to be under 1mb as per PSCIS requirements.
-   Rename jpg and jpeg to JPG to simplify reporting and eliminate issues with PSCIS upload.
-   Build directories of folders related to each site based on PSCIS input spreadsheet site ids.
-   Sort into directories and rename photos uploaded to mergin during field collection on phones.
-   ##THIS NEEDS UPDATING## Do an initial drop of photos into the generated site folders based on dates, times and surveyor. 
-   **QA renamed photos** -  to determine that all 5 photos (upstream, downstream, inlet, outlet, barrel) required for PSCIS as well as a road photo are present.
-   **Build photo amalgamation for each site** - containing all 6 of the previously mentioned photos. If there is no road over the crossing then use picture from nearest road with site card in corner.
-   Phase 2 photo directory generation - After Phase 1 data submission - duplicate directories for Phase 1 sites that where Phase 2 was also conducted and rename to reflect assigned Phase 2 PSCIS ID


-   Generate a csv file that contains the locations and names of all photos after they are sorted and renamed to facilitate reproducability.
