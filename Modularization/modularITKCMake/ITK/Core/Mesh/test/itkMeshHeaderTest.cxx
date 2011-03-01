/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkAutomaticTopologyMeshSource.h"
#include "itkAutomaticTopologyMeshSource.txx"
#include "itkBinaryMask3DMeshSource.h"
#include "itkBinaryMask3DMeshSource.txx"
#include "itkConnectedRegionsMeshFilter.h"
#include "itkConnectedRegionsMeshFilter.txx"
#include "itkImageToMeshFilter.h"
#include "itkImageToMeshFilter.txx"
#include "itkImageToParametricSpaceFilter.h"
#include "itkImageToParametricSpaceFilter.txx"
#include "itkInteriorExteriorMeshFilter.h"
#include "itkInteriorExteriorMeshFilter.txx"
#include "itkMesh.h"
#include "itkMeshRegion.h"
#include "itkMeshSource.h"
#include "itkMeshSource.txx"
#include "itkMeshToMeshFilter.h"
#include "itkMeshToMeshFilter.txx"
#include "itkMesh.txx"
#include "itkParametricSpaceToImageSpaceMeshFilter.h"
#include "itkParametricSpaceToImageSpaceMeshFilter.txx"
#include "itkRegularSphereMeshSource.h"
#include "itkRegularSphereMeshSource.txx"
#include "itkSimplexMeshAdaptTopologyFilter.h"
#include "itkSimplexMeshAdaptTopologyFilter.txx"
#include "itkSimplexMeshToTriangleMeshFilter.h"
#include "itkSimplexMeshToTriangleMeshFilter.txx"
#include "itkSimplexMeshVolumeCalculator.h"
#include "itkSimplexMeshVolumeCalculator.txx"
#include "itkSphereMeshSource.h"
#include "itkSphereMeshSource.txx"
#include "itkTransformMeshFilter.h"
#include "itkTransformMeshFilter.txx"
#include "itkTriangleMeshToBinaryImageFilter.h"
#include "itkTriangleMeshToBinaryImageFilter.txx"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkTriangleMeshToSimplexMeshFilter.txx"
#include "itkVTKPolyDataReader.h"
#include "itkVTKPolyDataReader.txx"
#include "itkVTKPolyDataWriter.h"
#include "itkVTKPolyDataWriter.txx"
#include "itkWarpMeshFilter.h"
#include "itkWarpMeshFilter.txx"


int itkMeshHeaderTest ( int , char ** )
{

  return EXIT_SUCCESS;
}
