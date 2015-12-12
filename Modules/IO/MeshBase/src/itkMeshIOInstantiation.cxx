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

#include "ITKIOMeshBaseExport.h"

// Mesh Types
#include "itkMesh.h"
#include "itkQuadEdgeMesh.h"
#include "itkSimplexMesh.h"
#include "itkVoronoiDiagram2D.h"

// Pixel Types
#include "itkCovariantVector.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkVariableLengthVector.h"

// Traits types
#include "itkQuadEdgeMeshExtendedTraits.h"
#include "itkQuadEdgeMeshTraits.h"

// Reader
#include "itkMeshFileReader.h"
#include "itkMeshFileReader.hxx"
#include "itkMeshSource.h"

// Writer
#include "itkMeshFileWriter.h"
#include "itkMeshFileWriter.hxx"

// -----------------------------------------------------------------------------
// Reader / Writer instantiation macros

#define ITKIOMESH_DefaultTraits_TRAITS(PIXEL_TYPE, DIMENSION)

#define ITKIOMESH_QuadEdgeMeshTraits_TRAITS(PIXEL_TYPE, DIMENSION) \
  , QuadEdgeMeshTraits< PIXEL_TYPE, DIMENSION, bool, bool>

#define ITKIOMESH_QuadEdgeMeshExtendedTraits_TRAITS(PIXEL_TYPE, DIMENSION) \
  , QuadEdgeMeshExtendedTraits< PIXEL_TYPE, DIMENSION, 2, CoordType, CoordType, PIXEL_TYPE, bool, bool>

#define ITKIOMESH_INSTANTIATE_Reader(MESH_TYPE, PIXEL_TYPE, DIMENSION, TRAITS) \
  template class ITKIOMeshBase_EXPORT MeshSource< \
    MESH_TYPE<PIXEL_TYPE, DIMENSION ITKIOMESH_##TRAITS##_TRAITS(PIXEL_TYPE, DIMENSION) > \
  >;\
  template class ITKIOMeshBase_EXPORT MeshFileReader< \
    MESH_TYPE<PIXEL_TYPE, DIMENSION ITKIOMESH_##TRAITS##_TRAITS(PIXEL_TYPE, DIMENSION) >, \
    MeshConvertPixelTraits< PIXEL_TYPE >, MeshConvertPixelTraits< PIXEL_TYPE > \
  >;

#define ITKIOMESH_INSTANTIATE_Writer(MESH_TYPE, PIXEL_TYPE, DIMENSION, TRAITS) \
  template class ITKIOMeshBase_EXPORT MeshFileWriter< \
    MESH_TYPE<PIXEL_TYPE, DIMENSION ITKIOMESH_##TRAITS##_TRAITS(PIXEL_TYPE, DIMENSION) > \
  >;

#define ITKIOMESH_INSTANTIATE_IOS(MESH_TYPE, PIXEL_TYPE, DIMENSION, TRAITS) \
  ITKIOMESH_INSTANTIATE_Reader(MESH_TYPE, PIXEL_TYPE, DIMENSION, TRAITS) \
  ITKIOMESH_INSTANTIATE_Writer(MESH_TYPE, PIXEL_TYPE, DIMENSION, TRAITS)

#define ITKIOMESH_INSTANTIATE_IO_FOR_PIXEL_TYPES(IO_TYPE, MESH_TYPE, DIMENSION, TRAITS) \
  ITKIOMESH_INSTANTIATE_##IO_TYPE(MESH_TYPE, float, DIMENSION, TRAITS) \
  ITKIOMESH_INSTANTIATE_##IO_TYPE(MESH_TYPE, double, DIMENSION, TRAITS)

#define ITKIOMESH_INSTANTIATE_IOS_FOR_PIXEL_TYPES(MESH_TYPE, DIMENSION, TRAITS) \
  ITKIOMESH_INSTANTIATE_IO_FOR_PIXEL_TYPES(Reader, MESH_TYPE, DIMENSION, TRAITS) \
  ITKIOMESH_INSTANTIATE_IO_FOR_PIXEL_TYPES(Writer, MESH_TYPE, DIMENSION, TRAITS)

namespace itk
{

typedef double CoordType;

typedef CovariantVector< float, 3 >           CovariantVector_float_3;
typedef SymmetricSecondRankTensor< float, 2 > SymmetricSecondRankTensor_float_2;
typedef SymmetricSecondRankTensor< float, 3 > SymmetricSecondRankTensor_float_3;
typedef VariableLengthVector< float >         VariableLengthVector_float_3;

// -----------------------------------------------------------------------------
// MeshFileReader / MeshFileWriter

ITKIOMESH_INSTANTIATE_IOS_FOR_PIXEL_TYPES(Mesh, 2, DefaultTraits)
ITKIOMESH_INSTANTIATE_IOS_FOR_PIXEL_TYPES(Mesh, 3, DefaultTraits)

ITKIOMESH_INSTANTIATE_IOS_FOR_PIXEL_TYPES(QuadEdgeMesh, 3, QuadEdgeMeshExtendedTraits)
ITKIOMESH_INSTANTIATE_IOS_FOR_PIXEL_TYPES(QuadEdgeMesh, 3, QuadEdgeMeshTraits)

ITKIOMESH_INSTANTIATE_IOS_FOR_PIXEL_TYPES(SimplexMesh, 2, DefaultTraits)
ITKIOMESH_INSTANTIATE_IOS_FOR_PIXEL_TYPES(SimplexMesh, 3, DefaultTraits)

// Required by ITKIOMesh itkMeshFileReadWriteVectorAttributeTest
ITKIOMESH_INSTANTIATE_IOS(Mesh, CovariantVector_float_3, 3, DefaultTraits)
ITKIOMESH_INSTANTIATE_IOS(QuadEdgeMesh, CovariantVector_float_3, 3, QuadEdgeMeshTraits)

// Required by ITKIOMesh itkMeshFileWriteReadTensorTest
ITKIOMESH_INSTANTIATE_IOS(Mesh, SymmetricSecondRankTensor_float_2, 2, DefaultTraits)
ITKIOMESH_INSTANTIATE_IOS(Mesh, SymmetricSecondRankTensor_float_3, 3, DefaultTraits)

// Required by ITKIOMesh itkPolylineReadWriteTest
ITKIOMESH_INSTANTIATE_IOS(Mesh, VariableLengthVector_float_3, 3, DefaultTraits)

// Required by ITKVoronoi itkVoronoiDiagram2DTest
template class MeshFileReader< VoronoiDiagram2D< double > >;
template class ITKIOMeshBase_EXPORT MeshFileWriter< VoronoiDiagram2D< double > >;

}  // end namespace itk
