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
import itk
import sys

filename = sys.argv[1]
numberOfConnectedComponents = int(sys.argv[2])
numberOfCellsInLargestComponent = int(sys.argv[3])

input_image = itk.imread(filename, itk.UC)

# Threshold the 3D image to get binary mask for mesh generation
threshold_filter = itk.BinaryThresholdImageFilter.IUC3IUC3.New()
threshold_filter.SetInput(input_image)
threshold_filter.SetLowerThreshold(0)
threshold_filter.SetUpperThreshold(200)
threshold_filter.SetOutsideValue(1)
threshold_filter.SetInsideValue(0)
threshold_filter.Update()

output_image = threshold_filter.GetOutput()

# Get mesh from binary image
mesh_source = itk.BinaryMask3DMeshSource.IUC3MF3.New()
mesh_source.SetInput(output_image)
mesh_source.SetObjectValue(1)
mesh_source.Update()
mesh = mesh_source.GetOutput()

# Check number of Connected Components in the mesh
connectivity_filter = itk.ConnectedRegionsMeshFilter.MF3MF3.New()
connectivity_filter.SetInput(mesh)
connectivity_filter.SetExtractionModeToAllRegions()
connectivity_filter.Update()

assert connectivity_filter.GetNumberOfExtractedRegions() == numberOfConnectedComponents

# Check the number of cells in the largest connected component
connectivity_filter.SetExtractionModeToLargestRegion()
connectivity_filter.Update()
largest_component = connectivity_filter.GetOutput()

assert largest_component.GetNumberOfCells() == numberOfCellsInLargestComponent
