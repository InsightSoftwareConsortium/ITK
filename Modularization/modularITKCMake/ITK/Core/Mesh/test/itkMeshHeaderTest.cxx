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

#include "itkAutomaticTopologyMeshSource.txx"
#include "itkBinaryMask3DMeshSource.txx"
#include "itkConnectedRegionsMeshFilter.txx"
#include "itkImageToMeshFilter.txx"
#include "itkImageToParametricSpaceFilter.txx"
#include "itkInteriorExteriorMeshFilter.txx"
#include "itkMeshRegion.h"
#include "itkMeshSource.txx"
#include "itkMeshToMeshFilter.txx"
#include "itkMesh.txx"
#include "itkParametricSpaceToImageSpaceMeshFilter.txx"
#include "itkRegularSphereMeshSource.txx"
#include "itkSimplexMeshAdaptTopologyFilter.txx"
#include "itkSimplexMeshToTriangleMeshFilter.txx"
#include "itkSimplexMeshVolumeCalculator.txx"
#include "itkSphereMeshSource.txx"
#include "itkTransformMeshFilter.txx"
#include "itkTriangleMeshToBinaryImageFilter.txx"
#include "itkTriangleMeshToSimplexMeshFilter.txx"
#include "itkVTKPolyDataReader.txx"
#include "itkVTKPolyDataWriter.txx"
#include "itkWarpMeshFilter.txx"


int itkMeshHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
