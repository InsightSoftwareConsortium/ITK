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

#include "itkParametricSpaceToImageSpaceMeshFilter.h"
#include "itkMesh.h"


int itkParametricSpaceToImageSpaceMeshFilterTest(int, char* [] )
{
  typedef   itk::Point<float,2>                          MeshPointDataType;

  typedef   itk::Mesh< MeshPointDataType, 3 >            InputMeshType;
  typedef   itk::Mesh< InputMeshType::PointType, 2 >     ImageSpaceMeshType;

  typedef   itk::ParametricSpaceToImageSpaceMeshFilter<
                                      InputMeshType,
                                      ImageSpaceMeshType
                                     >         ParametricFilterType;

  ParametricFilterType::Pointer
                      parametercFilter = ParametricFilterType::New();
  if( parametercFilter.IsNull() )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
