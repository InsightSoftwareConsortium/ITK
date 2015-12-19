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

#ifndef ITKMeshIOInstantiation_IO_TYPE
#  error "ITKMeshIOInstantiation_IO_TYPE macro is expected to set to either MeshFileReader or MeshFileWriter"
#endif

#ifndef ITK_TEMPLATE_EXPLICIT_MeshIOInstantiation
#  define MeshIO_TEMPLATE_EXPORT ITKIOMeshBase_EXPORT_EXPLICIT
#  define MeshIO_TEMPLATE_EXTERN extern
#else
#  define MeshIO_TEMPLATE_EXPORT ITKIOMeshBase_EXPORT
#  define MeshIO_TEMPLATE_EXTERN
#endif

#define MeshIO_DefaultTraits_TRAITS(PIXEL_TYPE, DIMENSION)

#define MeshIO_QuadEdgeMeshTraits_TRAITS(PIXEL_TYPE, DIMENSION) \
  , QuadEdgeMeshTraits< PIXEL_TYPE, DIMENSION, bool, bool>

#define MeshIO_QuadEdgeMeshExtendedTraits_TRAITS(PIXEL_TYPE, DIMENSION) \
  , QuadEdgeMeshExtendedTraits< PIXEL_TYPE, DIMENSION, 2, CoordType, CoordType, PIXEL_TYPE, bool, bool>

#define MeshIO_INSTANTIATE(MESH_TYPE, PIXEL_TYPE, DIMENSION, TRAITS) \
  MeshIO_TEMPLATE_EXTERN template class MeshIO_TEMPLATE_EXPORT ITKMeshIOInstantiation_IO_TYPE< \
    MESH_TYPE<PIXEL_TYPE, DIMENSION MeshIO_##TRAITS##_TRAITS(PIXEL_TYPE, DIMENSION) > \
  >;

#define MeshIO_INSTANTIATE_FOR_PIXEL_TYPES(MESH_TYPE, DIMENSION, TRAITS) \
  MeshIO_INSTANTIATE(MESH_TYPE, float, DIMENSION, TRAITS) \
  MeshIO_INSTANTIATE(MESH_TYPE, double, DIMENSION, TRAITS)

namespace itk
{

typedef double CoordType;

typedef CovariantVector< float, 3 >           CovariantVector_float_3;
typedef SymmetricSecondRankTensor< float, 2 > SymmetricSecondRankTensor_float_2;
typedef SymmetricSecondRankTensor< float, 3 > SymmetricSecondRankTensor_float_3;
typedef VariableLengthVector< float >         VariableLengthVector_float_3;

MeshIO_INSTANTIATE_FOR_PIXEL_TYPES(Mesh, 2, DefaultTraits)
MeshIO_INSTANTIATE_FOR_PIXEL_TYPES(Mesh, 3, DefaultTraits)

MeshIO_INSTANTIATE_FOR_PIXEL_TYPES(QuadEdgeMesh, 3, QuadEdgeMeshExtendedTraits)
MeshIO_INSTANTIATE_FOR_PIXEL_TYPES(QuadEdgeMesh, 3, QuadEdgeMeshTraits)

MeshIO_INSTANTIATE_FOR_PIXEL_TYPES(SimplexMesh, 2, DefaultTraits)
MeshIO_INSTANTIATE_FOR_PIXEL_TYPES(SimplexMesh, 3, DefaultTraits)

// Required by ITKIOMesh itkMeshFileReadWriteVectorAttributeTest
MeshIO_INSTANTIATE(Mesh, CovariantVector_float_3, 3, DefaultTraits)
MeshIO_INSTANTIATE(QuadEdgeMesh, CovariantVector_float_3, 3, QuadEdgeMeshTraits)

// Required by ITKIOMesh itkMeshFileWriteReadTensorTest
MeshIO_INSTANTIATE(Mesh, SymmetricSecondRankTensor_float_2, 2, DefaultTraits)
MeshIO_INSTANTIATE(Mesh, SymmetricSecondRankTensor_float_3, 3, DefaultTraits)

// Required by ITKIOMesh itkPolylineReadWriteTest
MeshIO_INSTANTIATE(Mesh, VariableLengthVector_float_3, 3, DefaultTraits)

// Required by ITKVoronoi itkVoronoiDiagram2DTest
MeshIO_TEMPLATE_EXTERN template class MeshIO_TEMPLATE_EXPORT
    ITKMeshIOInstantiation_IO_TYPE< VoronoiDiagram2D< double > >;

}  // end namespace itk

#undef MeshIO_TEMPLATE_EXPORT
#undef MeshIO_TEMPLATE_EXTERN
#undef MeshIO_DefaultTraits_TRAITS
#undef MeshIO_QuadEdgeMeshTraits_TRAITS
#undef MeshIO_QuadEdgeMeshExtendedTraits_TRAITS
#undef MeshIO_INSTANTIATE
#undef MeshIO_INSTANTIATE_FOR_PIXEL_TYPES
