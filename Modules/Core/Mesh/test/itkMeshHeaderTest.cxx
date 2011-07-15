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

#include "itkAutomaticTopologyMeshSource.hxx"
#include "itkBinaryMask3DMeshSource.hxx"
#include "itkConnectedRegionsMeshFilter.hxx"
#include "itkImageToMeshFilter.hxx"
#include "itkImageToParametricSpaceFilter.hxx"
#include "itkInteriorExteriorMeshFilter.hxx"
#include "itkMeshRegion.h"
#include "itkMeshSource.hxx"
#include "itkMeshToMeshFilter.hxx"
#include "itkMesh.hxx"
#include "itkParametricSpaceToImageSpaceMeshFilter.hxx"
#include "itkRegularSphereMeshSource.hxx"
#include "itkSimplexMeshAdaptTopologyFilter.hxx"
#include "itkSimplexMeshToTriangleMeshFilter.hxx"
#include "itkSimplexMeshVolumeCalculator.hxx"
#include "itkSphereMeshSource.hxx"
#include "itkTransformMeshFilter.hxx"
#include "itkTriangleMeshToBinaryImageFilter.hxx"
#include "itkTriangleMeshToSimplexMeshFilter.hxx"
#include "itkVTKPolyDataReader.hxx"
#include "itkVTKPolyDataWriter.hxx"
#include "itkWarpMeshFilter.hxx"


int itkMeshHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
