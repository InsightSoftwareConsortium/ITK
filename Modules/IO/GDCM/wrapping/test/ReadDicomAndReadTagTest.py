#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

# Tests:
# - Reading dicom files from a directory
# - Read a tag (patient's name) through the MetaDataDictionary
# - Read a tag (patient's name) directly with the GetValueFromTag() method
# - Compare the two tags if they are the same

from __future__ import print_function

import itk
import sys

imdir = sys.argv[1]
image_t = itk.Image[itk.F, 3]

# Set up reader
reader = itk.ImageSeriesReader[image_t].New()
dicomIO = itk.GDCMImageIO.New()
dicomFN = itk.GDCMSeriesFileNames.New()
reader.SetImageIO(dicomIO)

# Get file names
dicomFN.SetUseSeriesDetails(True)
dicomFN.SetDirectory(imdir)

# Get the first image series
uids = dicomFN.GetSeriesUIDs()
fnames = dicomFN.GetFileNames(uids[0])

# Read in the files
reader.SetFileNames(fnames)
reader.Update()
image = reader.GetOutput()

# Now access the meta data dictionary
metad = dicomIO.GetMetaDataDictionary()

# Get the patient's name
name1 = metad["0010|0010"]

# Other way to get the tag
# GetValueFromTag(tagkey, tagvalue)
# tagvalue is an empty string, in C++ it is passed by
# reference. Here we pass an empty string, and the
# actual value is returned as the second variable.
found, name2 = dicomIO.GetValueFromTag("0010|0010", "")
assert(name1 == name2)

# Check also if we returned the right tag
found, name3 = dicomIO.GetLabelFromTag("0010|0010", "")
assert(name3 == "Patient's Name")
