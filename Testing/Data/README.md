ITK Testing Data
================

The ITK `Testing/Data` folder hosts the files that are used by ITK's automatic
tests and hence ensure the health of the toolkit:

  * The `Baseline` directory contains valid images created by tests. Generated
  images are compared with these baseline images during regression testing.
  * The `Input` directory contains data files that are used by the tests.

Adding test data
----------------

Test data is fetched at build time from content-addressed storage by
`CMake/ITKExternalData.cmake`. Large files are *not* committed to the ITK git
repository; instead, a small `.cid` (or `.md5` / `.sha256`) content-link file
is committed next to where the data is referenced.

To add new test data, use the upload skill at
`Utilities/Maintenance/ExternalDataUpload/`:

```bash
Utilities/Maintenance/ExternalDataUpload/ipfs-upload.sh <path-to-file>
```

The script uploads the file to IPFS, pins it on the redundant pinning
services, replaces the original with a `.cid` content-link, and records the
CID in `Testing/Data/content-links.manifest`. See the skill's `README.md`
for one-time setup and the full workflow, including the optional
`ITKTestingData` GitHub Pages mirror step.
