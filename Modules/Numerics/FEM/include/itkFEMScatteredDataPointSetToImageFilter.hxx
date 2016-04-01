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

#ifndef itkFEMScatteredDataPointSetToImageFilter_hxx
#define itkFEMScatteredDataPointSetToImageFilter_hxx

#include "itkFEMScatteredDataPointSetToImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageDuplicator.h"
#include "itkCastImageFilter.h"
#include "itkNumericTraits.h"

#include "itkMath.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/vnl_vector.h"
#if !defined( ITK_LEGACY_FUTURE_REMOVE )
# include "vcl_limits.h"
#endif
#include <limits>

namespace itk
{
namespace fem
{

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::FEMScatteredDataPointSetToImageFilter()
{
  this->m_FEMObject = FEMObjectType::New();
  this->m_Material = MaterialType::New();
  this->m_Material->SetYoungsModulus(694.0);
  this->m_Material->SetPoissonsRatio(0.45);
  this->m_FEMSolver = FEMSolverType::New();

  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::~FEMScatteredDataPointSetToImageFilter()
{
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::SetElementSpacing(const SpacingType & elementSpacing)
{
  this->m_SpacingPerElement = elementSpacing;
  typename ImageType::Pointer image = this->GetOutput();
  const SpacingType & imageSpacing = image->GetSpacing();

  for(unsigned int i = 0; i < ImageDimension; i++)
    {
    this->m_PixelsPerElement[i] = elementSpacing[i]/imageSpacing[i];
    }

  this->Modified();
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::GenerateData()
{
  // create the output image
  itkDebugMacro( "Size: " << this->m_Size );
  itkDebugMacro( "Origin: " << this->m_Origin );
  itkDebugMacro( "Spacing: " << this->m_Spacing );
  itkDebugMacro( "Direction: " << this->m_Direction );

  // error checking
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if( this->m_Size[i] == 0 )
      {
      itkExceptionMacro("Size must be specified.");
      }
    }

  typename ImageType::Pointer output = this->GetOutput();
  output->SetOrigin( this->m_Origin );
  output->SetSpacing( this->m_Spacing );
  output->SetDirection( this->m_Direction );
  output->SetRegions( this->m_Size );
  output->Allocate();

  if(this->m_Mesh.IsNull())
    {
    // provide a rectilinear mesh based on the output deformation field if users do not specify one.
    this->GenerateRectilinearMesh();
    }

  // convert the mesh and feature points into a FEMObject
  this->InitializeFEMObject( this->m_FEMObject );

  this->m_FEMSolver->SetInput( this->m_FEMObject );

  // set the interpolation grid of the FEMSolver
  // note that let the interpolation grid be same with the deformation field
  // in order to accelarate the generation of the deformation field.
  this->m_FEMSolver->SetOrigin( this->m_Origin );
  this->m_FEMSolver->SetSpacing( this->m_Spacing );
  RegionType region;
  region.SetSize( this->m_Size );
  this->m_FEMSolver->SetRegion( region );
  this->m_FEMSolver->SetDirection( this->m_Direction );

  // if the feature points are the grid point of the interpolation grid, set true.
  // note that since feature points come from the image, this setting is always true.
  this->m_FEMSolver->SetUseInterpolationGrid( true );

  this->m_FEMSolver->Update();

  this->ProduceDeformationField();
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::GenerateRectilinearMesh()
{
  if(this->m_Mesh.IsNotNull())
    {
    return;
    }

  if( ImageDimension == 2 )
    {
    this->Generate2DQuadrilateralMesh();
    }
  else if(ImageDimension == 3)
    {
    this->Generate3DHexahedralMesh();
    }
  else
    {
    itkExceptionMacro("Only support 2 and 3 dimension");
    }
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::Generate2DQuadrilateralMesh()
{
  ImageType *  image  = this->GetOutput();
  RegionType   region = image->GetLargestPossibleRegion();
  SizeType     size   = region.GetSize();

  const double elementFraction0 = (size[0] - 1) / m_PixelsPerElement[0];
  const double elementFraction1 = (size[1] - 1) / m_PixelsPerElement[1];

  this->m_NumberOfElements[0] = static_cast<int>(ceil(elementFraction0));
  this->m_NumberOfElements[1] = static_cast<int>(ceil(elementFraction1));

  this->m_Mesh = MeshType::New();

  // create nodes
  ContinuousIndexType  pointIndex;
  typename ImageType::PointType pointCoordinate;
  int globalNumbering = 0;

  for( float j = 0; j <= m_NumberOfElements[1]; j++ )
    {
    pointIndex[1] = j * m_PixelsPerElement[1];
    for( float i = 0; i <= m_NumberOfElements[0]; i++ )
      {
      pointIndex[0] = i * m_PixelsPerElement[0];
      image->TransformContinuousIndexToPhysicalPoint(pointIndex, pointCoordinate);

      this->m_Mesh->SetPoint(globalNumbering, pointCoordinate);

      globalNumbering++;
      }
    }

  // create elements
  globalNumbering = 0;

  for( unsigned int j = 0; j < this->m_NumberOfElements[1]; j++ )
    {
    for( unsigned int i = 0; i < this->m_NumberOfElements[0]; i++ )
      {
      CellAutoPointer cell;
      cell.TakeOwnership(new QuadrilateralType);
      cell->SetPointId(0, i + ( this->m_NumberOfElements[0] + 1 ) * j );
      cell->SetPointId(1, i + 1 + ( this->m_NumberOfElements[0] + 1 ) * j);
      cell->SetPointId(2, i + 1 + ( this->m_NumberOfElements[0] + 1 ) * ( j + 1 ));
      cell->SetPointId(3, i + ( this->m_NumberOfElements[0] + 1 ) * ( j + 1 ));

      this->m_Mesh->SetCell(globalNumbering, cell);

      globalNumbering++;
      }
    }
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::Generate3DHexahedralMesh()
{
  ImageType *  image = this->GetOutput();
  RegionType   region = image->GetLargestPossibleRegion();
  SizeType     size   = region.GetSize();

  const double elementFraction0 = (size[0] - 1) / m_PixelsPerElement[0];
  const double elementFraction1 = (size[1] - 1) / m_PixelsPerElement[1];
  const double elementFraction2 = (size[2] - 1) / m_PixelsPerElement[2];

  this->m_NumberOfElements[0] = static_cast<int>(ceil(elementFraction0));
  this->m_NumberOfElements[1] = static_cast<int>(ceil(elementFraction1));
  this->m_NumberOfElements[2] = static_cast<int>(ceil(elementFraction2));

  this->m_Mesh = MeshType::New();

  // create nodes
  ContinuousIndexType  pointIndex;
  typename ImageType::PointType pointCoordinate;

  int globalNumbering = 0;
  for( float k = 0; k <= m_NumberOfElements[2]; k++ )
    {
    pointIndex[2] = k * m_PixelsPerElement[2];
    for( float j = 0; j <= m_NumberOfElements[1]; j++ )
      {
      pointIndex[1] = j * m_PixelsPerElement[1];
      for( float i = 0; i <= m_NumberOfElements[0]; i++ )
        {
        pointIndex[0] = i * m_PixelsPerElement[0];
        image->TransformContinuousIndexToPhysicalPoint(pointIndex, pointCoordinate);
        this->m_Mesh->SetPoint(globalNumbering, pointCoordinate);

        globalNumbering++;
        }
      }
    }

  // create elements
  globalNumbering = 0;
  for( unsigned int k = 0; k < m_NumberOfElements[2]; k++ )
    {
    for( unsigned int j = 0; j < m_NumberOfElements[1]; j++ )
      {
      for( unsigned int i = 0; i < m_NumberOfElements[0]; i++ )
        {
        CellAutoPointer cell;
        cell.TakeOwnership(new HexahedronType);

        unsigned int value;

        // set the global point ID for the cell local ID0
        value = i +  ( m_NumberOfElements[0] + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * k);
        cell->SetPointId(0,value);

        // set the global point ID for the cell local ID1
        value = i + 1 + ( m_NumberOfElements[0] + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * k);
        cell->SetPointId(1,value);

        // set the global point ID for the cell local ID2
        value = i + 1 + ( m_NumberOfElements[0] + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * k);
        cell->SetPointId(2,value);

        // set the global point ID for the cell local ID3
        value = i + ( m_NumberOfElements[0] + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * k );
        cell->SetPointId(3,value);

        // set the global point ID for the cell local ID4
        value = i + ( m_NumberOfElements[0] + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ));
        cell->SetPointId(4, value);

        // set the global point ID for the cell local ID5
        value = i + 1 + ( m_NumberOfElements[0] + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ) );
        cell->SetPointId(5, value);

        // set the global point ID for the cell local ID6
        value = i + 1 + ( m_NumberOfElements[0] + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ));
        cell->SetPointId(6, value);

        // set the global point ID for the cell local ID7
        value = i + ( m_NumberOfElements[0] + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ));
        cell->SetPointId(7,value);

        this->m_Mesh->SetCell(globalNumbering, cell);

        globalNumbering++;
        }
      }
    }
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::InitializeFEMObject(FEMObjectType * femObject)
{
  this->InitializeMaterials(femObject);
  this->InitializeNodes(femObject);
  this->InitializeElements(femObject);
  this->InitializeLoads(femObject);

  // produce DOF
  femObject->FinalizeMesh();
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::InitializeMaterials(FEMObjectType * femObject)
{
  MaterialContainerType *materialContainer = femObject->GetModifiableMaterialContainer();

  if(!materialContainer)
    {
    itkExceptionMacro("Missing material container");
    }

  materialContainer->Initialize();

  // fix material to linear elasticity
  femObject->AddNextMaterial(this->m_Material);
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::InitializeNodes(FEMObjectType * femObject)
{
  NodeContainerType *nodeContainer = femObject->GetModifiableNodeContainer();

  if(!nodeContainer)
    {
    itkExceptionMacro("Missing node container");
    }

  nodeContainer->Initialize();

  typename PointsContainer::Pointer meshPoints = this->m_Mesh->GetPoints();
  PointsIterator it = meshPoints->Begin();

  FEMVectorType point(ImageDimension);

  // initialize nodes
  while(it != meshPoints->End())
    {
    for(unsigned i = 0; i < ImageDimension; i++)
      {
      point[i]= it.Value()[i];
      }

    NodeType::Pointer node = NodeType::New();
    node->SetCoordinates(point);
    node->SetGlobalNumber(it.Index());

    femObject->AddNextNode(node);

    ++it;
    }
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::InitializeElements(FEMObjectType * femObject)
{
  ElementContainerType *elementContainer = femObject->GetModifiableElementContainer();

  if(!elementContainer)
    {
    itkExceptionMacro("Missing element container");
    }

  elementContainer->Initialize();

  unsigned int globalNumbering = 0;

  // mesh cell iterator
  typename CellsContainer::Pointer cells = this->m_Mesh->GetCells();
  CellIterator cellIterator = cells->Begin();

  while( cellIterator != cells->End() )
    {
    CellType * cell = cellIterator.Value();

    switch( cell->GetType() )
      {
      case CellType::TRIANGLE_CELL:
        {
        FEM2DTriangleType::Pointer triangleEle = FEM2DTriangleType::New();

        TriangleType * triangleCell = static_cast<TriangleType *>( cell );

        PointIdIterator pointIdIter = triangleCell->PointIdsBegin();
        PointIdIterator pointIdEnd = triangleCell->PointIdsEnd();

        unsigned int i = 0;

        while( pointIdIter != pointIdEnd )
          {
          triangleEle->SetNode(i++, femObject->GetNode(*pointIdIter));
          ++pointIdIter;
          }

        triangleEle->SetGlobalNumber(globalNumbering++);
        triangleEle->SetMaterial( static_cast<MaterialType *>( femObject->GetMaterial(0).GetPointer() ) );
        femObject->AddNextElement(triangleEle.GetPointer());

        break;
        }

      case CellType::TETRAHEDRON_CELL:
        {
        FEMTetrahedronType::Pointer tetrahedronEle = FEMTetrahedronType::New();

        TetrahedronType * tetrahedron = static_cast<TetrahedronType *>( cell );

        PointIdIterator pointIdIter = tetrahedron->PointIdsBegin();
        PointIdIterator pointIdEnd = tetrahedron->PointIdsEnd();

        unsigned int i = 0;

        while( pointIdIter != pointIdEnd )
          {
          tetrahedronEle->SetNode(i++, femObject->GetNode(*pointIdIter));

          ++pointIdIter;
          }

        tetrahedronEle->SetGlobalNumber(globalNumbering++);
        tetrahedronEle->SetMaterial( static_cast<MaterialType *>( femObject->GetMaterial(0).GetPointer() ) );
        femObject->AddNextElement(tetrahedronEle.GetPointer());

        break;

        }

      case CellType::QUADRILATERAL_CELL:
        {
        FEM2DQuadrilateralType::Pointer quadrilateralEle = FEM2DQuadrilateralType::New();

        // use Cell and Ele to distinguish itk element and FEM element
        QuadrilateralType * quadrilateralCell = static_cast<QuadrilateralType *>( cell );

        PointIdIterator pointIdIter = quadrilateralCell->PointIdsBegin();
        PointIdIterator pointIdEnd = quadrilateralCell->PointIdsEnd();

        unsigned int i = 0;

        while( pointIdIter != pointIdEnd )
          {
          quadrilateralEle->SetNode(i++, femObject->GetNode(*pointIdIter));

          ++pointIdIter;
          }

        quadrilateralEle->SetGlobalNumber(globalNumbering++);
        quadrilateralEle->SetMaterial( static_cast<MaterialType *>( femObject->GetMaterial(0).GetPointer() ) );
        femObject->AddNextElement(quadrilateralEle.GetPointer());

        break;

        }

      case CellType::HEXAHEDRON_CELL:
        {
        FEMHexahedronType::Pointer hexahedronEle = FEMHexahedronType::New();

        HexahedronType * hexahedron = static_cast<HexahedronType *>( cell );

        PointIdIterator pointIdIter = hexahedron->PointIdsBegin();
        PointIdIterator pointIdEnd = hexahedron->PointIdsEnd();

        unsigned int i = 0;

        while( pointIdIter != pointIdEnd )
          {
          hexahedronEle->SetNode(i++, femObject->GetNode(*pointIdIter));

          ++pointIdIter;
          }

        hexahedronEle->SetGlobalNumber(globalNumbering++);
        hexahedronEle->SetMaterial( static_cast<MaterialType *>( femObject->GetMaterial(0).GetPointer() ) );
        femObject->AddNextElement(hexahedronEle.GetPointer());

        break;

        }

      default:
        {
        itkExceptionMacro("Do not support element type: " << cell->GetType());
        break;
        }

      }//end of switch on cell type

    ++cellIterator;

    }//end of while not at end of cells
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::InitializeLoads(FEMObjectType * femObject)
{
  LoadContainerType *loadContainer = femObject->GetModifiableLoadContainer();

  if(!loadContainer)
    {
    itkExceptionMacro("Missing load container");
    }

  loadContainer->Initialize();

  FEMMatrixType tensor(ImageDimension, ImageDimension);

  const PointSetType *input = this->GetInput();

  if(!input)
    {
    itkExceptionMacro("No feature points");
    }

  const PointDataContainerType *displacementVector = input->GetPointData();

  if(!displacementVector)
    {
    itkExceptionMacro("No displacement vectors associated with feature point set");
    }

  const PointsContainer *featurePoints = input->GetPoints();

  PointsIterator it = featurePoints->Begin();
  PointDataIterator displacementIt = displacementVector->Begin();
  typename ConfidencePointDataContainerType::ConstIterator confidenceIt;
  typename TensorPointDataContainerType::ConstIterator tensorIt;

  if( this->m_ConfidencePointSet.IsNotNull() )
    {
    const ConfidencePointDataContainerType *confidenceVector = this->m_ConfidencePointSet->GetPointData();
    confidenceIt = confidenceVector->Begin();
    }

  if( this->m_TensorPointSet.IsNotNull() )
    {
    const TensorPointDataContainerType *tensorVector = this->m_TensorPointSet->GetPointData();
    tensorIt = tensorVector->Begin();
    }

  FEMVectorType point(ImageDimension);
  FEMVectorType displacement(ImageDimension);

  while(it != featurePoints->End())
    {
    for(unsigned int i = 0; i < ImageDimension; i++)
      {
      point[i] = it.Value()[i];
      displacement[i] = displacementIt.Value()[i];
      }

    LoadType::Pointer load =  LoadType::New();

    load->SetSource(point);
    load->SetRealDisplacement(displacement);

    if(this->m_ConfidencePointSet.IsNotNull() )
      {
      load->SetConfidence( ConfidencePointDataType(confidenceIt.Value()));
      ++confidenceIt;
      }

    if(this->m_TensorPointSet.IsNotNull() )
      {
      for(unsigned int i = 0; i < ImageDimension; i++)
        {
        for(unsigned int j = 0; j < ImageDimension; j++)
          {
          tensor[i][j]= tensorIt.Value()[i][j];
          }
        }

      load->SetStructureTensor(tensor);

      ++tensorIt;
      }

    femObject->AddNextLoad(fem::Load::Pointer(load));

    ++displacementIt;
    ++it;
    }
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::ProduceDeformationField()
{
  // Produce deformation field based on the solution.

  ImageType *output = this->GetOutput();
  RegionType region = output->GetLargestPossibleRegion();
  ImageRegionIterator<ImageType> iter(output, region);

  PointType point;
  unsigned int solutionIndex = 0;
  FEMVectorType globalPoint(ImageDimension);
  FEMVectorType localPoint(ImageDimension);
  PixelType displacement;

  // step over all points within the region
  for( iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
    output->TransformIndexToPhysicalPoint(iter.GetIndex(), point);
    for( unsigned int d = 0; d < ImageDimension; d++ )
      {
      globalPoint[d] = point[d];
      }

    fem::Element::ConstPointer element = m_FEMSolver->GetElementAtPoint(globalPoint);

    if(element.IsNull())
      {
      for(unsigned i = 0; i < ImageDimension; i++)
        {
        displacement[i] = 0.0;
        }

      iter.Set(displacement);
      continue;
      }

    // the point is inside the element.
    if( element->GetLocalFromGlobalCoordinates(globalPoint, localPoint) )
      {
      const FEMVectorType & shape = element->ShapeFunctions(localPoint);

      const unsigned int NnDOF = element->GetNumberOfDegreesOfFreedomPerNode();
      const unsigned int Nnodes = element->GetNumberOfNodes();

      FEMVectorType simulatedDisplacement(NnDOF, 0.0);
      FEMVectorType nodeSolution(NnDOF);

      for(unsigned int m = 0; m < Nnodes; ++m)
        {
        for(unsigned int j = 0; j < NnDOF; ++j)
          {
          unsigned int dofId = element->GetDegreeOfFreedom(m * NnDOF + j);
          nodeSolution[j] = m_FEMSolver->GetSolution(dofId,solutionIndex);
          }

        simulatedDisplacement += shape[m] * nodeSolution;

        }

      for(unsigned i = 0; i < ImageDimension; i++)
        {
        displacement[i] = simulatedDisplacement[i];
        }

      iter.Set(displacement);
      }

    }
}

template<typename TInputPointSet, typename TInputMesh, typename TOutputImage, typename TInputConfidencePointSet, typename TInputTensorPointSet>
void
FEMScatteredDataPointSetToImageFilter<TInputPointSet, TInputMesh, TOutputImage, TInputConfidencePointSet, TInputTensorPointSet>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "  Origin:    " << this->m_Origin << std::endl;
  os << indent << "  Spacing:   " << this->m_Spacing << std::endl;
  os << indent << "  Size:      " << this->m_Size << std::endl;
  os << indent << "  Direction: " << this->m_Direction << std::endl;
}

} // end namespace fem
} // end namespace itk

#endif
