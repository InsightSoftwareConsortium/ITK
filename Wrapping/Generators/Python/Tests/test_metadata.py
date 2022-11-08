# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================*/

# Ensure we can convert all Nifti metadata members to Python

import sys
import numpy as np
import pathlib
from pprint import pprint

import itk

nifti_image_2d_filepath = sys.argv[1]
image = itk.imread(nifti_image_2d_filepath)
metadata = dict(image)
print('2D image')
pprint(metadata)

nifti_image_3d_filepath = sys.argv[2]
image = itk.imread(nifti_image_3d_filepath, itk.F)
metadata = dict(image)
print('3D image')
pprint(metadata)

nifti_displacement_2D_filepath = sys.argv[3]
field = itk.imread(nifti_displacement_2D_filepath, itk.F)
metadata = dict(field)
print('2D displacement field')
pprint(metadata)

nifti_displacement_3D_filepath = sys.argv[4]
field = itk.imread(nifti_displacement_3D_filepath, itk.F)
metadata = dict(field)
print('3D displacement field')
pprint(metadata)
