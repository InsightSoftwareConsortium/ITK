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
#ifndef itkBSplineDeformableTransform_hxx
#define itkBSplineDeformableTransform_hxx

#include "itkBSplineDeformableTransform.h"
#include "itkContinuousIndex.h"
#include "itkImageScanlineConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkIdentityTransform.h"

namespace itk
{

// Constructor with default arguments
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::BSplineDeformableTransform() :
  m_GridRegion(Superclass::m_CoefficientImages[0]->GetLargestPossibleRegion() ),
  m_GridOrigin(Superclass::m_CoefficientImages[0]->GetOrigin() ),
  m_GridSpacing(Superclass::m_CoefficientImages[0]->GetSpacing() ),
  m_GridDirection(Superclass::m_CoefficientImages[0]->GetDirection() )
{

  // Instantiate an identity transform
  typedef IdentityTransform<TParametersValueType, SpaceDimension> IdentityTransformType;
  typename IdentityTransformType::Pointer id = IdentityTransformType::New();
  this->m_BulkTransform = id;

  // Setup variables for computing interpolation
  this->m_Offset = SplineOrder / 2;
  if( SplineOrder % 2 )
    {
    this->m_SplineOrderOdd = true;
    }
  else
    {
    this->m_SplineOrderOdd = false;
    }
  this->m_ValidRegion = this->m_GridRegion; // HACK:  Perhaps this->m_ValidRegion is redundant also.
  this->m_ValidRegionFirst.Fill( 0 );
  this->m_ValidRegionLast.Fill( 1 );

  /** Fixed Parameters store the following information:
   *     Grid Size
   *     Grid Origin
   *     Grid Spacing
   *     Grid Direction
   *  The size of these is equal to the  NInputDimensions
   */
  // For example 3D image has FixedParameters of:
  // [size[0],size[1],size[2],
  // origin[0],origin[1],origin[2],
  // spacing[0],spacing[1],spacing[2],
  // dir[0][0],dir[1][0],dir[2][0],
  // dir[0][1],dir[1][1],dir[2][1],
  // dir[0][2],dir[1][2],dir[2][2]]

  this->SetFixedParametersFromTransformDomainInformation();
}

template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::~BSplineDeformableTransform()
{
}

// Get the number of parameters
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
typename BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>::NumberOfParametersType
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::GetNumberOfParameters() const
{
  // The number of parameters equal SpaceDimension * number of
  // of pixels in the grid region.
  return static_cast<NumberOfParametersType>( SpaceDimension * this->m_GridRegion.GetNumberOfPixels() );
}

// Get the number of parameters per dimension
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
typename BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>::NumberOfParametersType
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::GetNumberOfParametersPerDimension() const
{
  // The number of parameters per dimension equal number of
  // of pixels in the grid region.
  return static_cast<NumberOfParametersType>( this->m_GridRegion.GetNumberOfPixels() );
}

// Set the grid region
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::UpdateValidGridRegion()
{
  // Set the valid region
  // If the grid spans the interval [start, last].
  // The valid interval for evaluation is [start+offset, last-offset]
  // when spline order is even.
  // The valid interval for evaluation is [start+offset, last-offset)
  // when spline order is odd.
  // Where offset = floor(spline / 2 ).
  // Note that the last pixel is not included in the valid region
  // with odd spline orders.
  typename RegionType::SizeType size;
  typename RegionType::IndexType index;
  for( unsigned int j = 0; j < NDimensions; ++j )
    {
    index[j] = this->m_GridRegion.GetIndex()[j];
    size[j] = this->m_GridRegion.GetSize()[j];
    index[j] += static_cast<typename RegionType::IndexValueType>( this->m_Offset );
    size[j] -= static_cast<typename RegionType::SizeValueType>( 2 * this->m_Offset );
    this->m_ValidRegionFirst[j] = index[j];
    this->m_ValidRegionLast[j] = index[j] + static_cast<typename RegionType::IndexValueType>( size[j] ) - 1;
    }
  this->m_ValidRegion.SetSize(size);
  this->m_ValidRegion.SetIndex(index);
}

// Set the grid region
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetGridRegion(const RegionType & region)
{
  if( this->m_GridRegion != region )
    {
    this->m_CoefficientImages[0]->SetRegions(region);
    // set regions for each coefficient image
    for( unsigned int j = 1; j < SpaceDimension; j++ )
      {
      this->m_CoefficientImages[j]->SetRegions(region);
      }

    this->UpdateValidGridRegion();
    //
    // If we are using the default parameters, update their size and set to
    // identity.
    //

    // Check if we need to resize the default parameter buffer.
    if( this->m_InternalParametersBuffer.GetSize() != this->GetNumberOfParameters() )
      {
      this->m_InternalParametersBuffer.SetSize( this->GetNumberOfParameters() );
      // Fill with zeros for identity.
      this->m_InternalParametersBuffer.Fill( 0 );
      }
    this->SetFixedParametersGridSizeFromTransformDomainInformation();
    this->Modified();
    }
}

// Set the grid spacing
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetGridSpacing(const SpacingType & spacing)
{
  if( this->m_GridSpacing != spacing )
    {
    this->m_CoefficientImages[0]->SetSpacing(spacing);
    // set spacing for each coefficient image
    for( unsigned int j = 1; j < SpaceDimension; j++ )
      {
      this->m_CoefficientImages[j]->SetSpacing( spacing );
      }
    this->SetFixedParametersGridSpacingFromTransformDomainInformation();
    this->Modified();
    }
}

// Set the grid direction
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetGridDirection(const DirectionType & direction)
{
  if( this->m_GridDirection != direction )
    {
    this->m_CoefficientImages[0]->SetDirection(direction);
    // set direction for each coefficient image
    for( unsigned int j = 1; j < SpaceDimension; j++ )
      {
      this->m_CoefficientImages[j]->SetDirection(direction);
      }
    this->SetFixedParametersGridDirectionFromTransformDomainInformation();
    this->Modified();
    }
}

// Set the grid origin
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetGridOrigin(const OriginType & origin)
{
  if( this->m_GridOrigin != origin )
    {
    this->m_CoefficientImages[0]->SetOrigin( origin );
    // set spacing for each coefficient image
    for( unsigned int j = 1; j < SpaceDimension; j++ )
      {
      this->m_CoefficientImages[j]->SetOrigin( origin );
      }
    this->SetFixedParametersGridOriginFromTransformDomainInformation();
    this->Modified();
    }
}

template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetCoefficientImageInformationFromFixedParameters()
{

  // Fixed Parameters store the following information:
  //  grid size
  //  grid origin
  //  grid spacing
  //  grid direction
  //  The size of these is equal to the  NInputDimensions
    {
    // set the grid size parameters
    SizeType gridSize;
    for( unsigned int i = 0; i < NDimensions; i++ )
      {
      gridSize[i] = static_cast<int>( this->m_FixedParameters[i] );
      }
    RegionType bsplineRegion;
    bsplineRegion.SetSize(gridSize);
    this->SetGridRegion(bsplineRegion);
    }

    {
    // Set the origin parameters
    OriginType origin;
    for( unsigned int i = 0; i < NDimensions; i++ )
      {
      origin[i] = this->m_FixedParameters[NDimensions + i];
      }
    this->SetGridOrigin(origin);
    }

    {
    // Set the spacing parameters
    SpacingType spacing;
    for( unsigned int i = 0; i < NDimensions; i++ )
      {
      spacing[i] = this->m_FixedParameters[2 * NDimensions + i];
      }
    this->SetGridSpacing(spacing);
    }

    {
    // Set the direction parameters
    DirectionType direction;
    for( unsigned int di = 0; di < NDimensions; di++ )
      {
      for( unsigned int dj = 0; dj < NDimensions; dj++ )
        {
        direction[di][dj] = this->m_FixedParameters[3 * NDimensions + ( di * NDimensions + dj )];
        }
      }
    this->SetGridDirection(direction);
    }
}

template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetFixedParametersGridSizeFromTransformDomainInformation() const
{
  // Set the grid size parameters
  const SizeType & gridSize = this->m_CoefficientImages[0]->GetLargestPossibleRegion().GetSize();
  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    this->m_FixedParameters[i] = static_cast<FixedParametersValueType>(
      gridSize[i] );
    }
}

template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetFixedParametersGridOriginFromTransformDomainInformation() const
{
  // Set the origin parameters

  const OriginType & origin = this->m_CoefficientImages[0]->GetOrigin();

  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    this->m_FixedParameters[NDimensions + i] = static_cast<FixedParametersValueType>(
      origin[i] );
    }
}

template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetFixedParametersGridSpacingFromTransformDomainInformation() const
{
  // Set the spacing parameters
  const SpacingType & spacing = this->m_CoefficientImages[0]->GetSpacing();
  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    this->m_FixedParameters[2 * NDimensions + i] = static_cast<FixedParametersValueType>( spacing[i] );
    }
}

template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetFixedParametersGridDirectionFromTransformDomainInformation() const
{
  /** Set the direction parameters */
  const DirectionType & direction = this->m_CoefficientImages[0]->GetDirection();
  for( unsigned int di = 0; di < NDimensions; di++ )
    {
    for( unsigned int dj = 0; dj < NDimensions; dj++ )
      {
      this->m_FixedParameters[3 * NDimensions + ( di * NDimensions + dj )] =
        static_cast<FixedParametersValueType>( direction[di][dj] );
      }
    }
}


// Set the Fixed Parameters
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetFixedParameters( const FixedParametersType & passedParameters )
{
  // check if the number of passedParameters match the
  // expected number of this->m_FixedParameters
  if( passedParameters.Size() == this->m_FixedParameters.Size() )
    {
    for( unsigned int i = 0; i < NDimensions * ( 3 + NDimensions ); ++i )
      {
      this->m_FixedParameters[i] = passedParameters[i];
      }
    }
  else if( passedParameters.Size() == NDimensions * 3 )
    {
    // This option was originally valid for backwards compatibility
    // with BSplines saved to disk from before image orientation was used.
    // Those transforms would no longer be valid with respect to images
    // with explicit directions.
    itkExceptionMacro( << "Mismatched between parameters size "
                       << passedParameters.size()
                       << " and required number of fixed parameters "
                       << this->m_FixedParameters.Size()
                       << ".  Implicit setting of identity direction is no longer supported." );
    }
  else
    {
    itkExceptionMacro( << "Mismatched between parameters size "
                       << passedParameters.size()
                       << " and the required number of fixed parameters "
                       << this->m_FixedParameters.Size() );
    }
  this->SetCoefficientImageInformationFromFixedParameters();
}

// Set the B-Spline coefficients using input images
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetCoefficientImages( const CoefficientImageArray & images )
{
  bool validArrayOfImages = true;

  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    validArrayOfImages &= ( images[0].IsNotNull() );
    }

  if( validArrayOfImages )
    {
    // The BufferedRegion MUST equal the LargestPossibleRegion.
    this->SetGridRegion( images[0]->GetLargestPossibleRegion() );
    this->SetGridOrigin( images[0]->GetOrigin() );
    this->SetGridSpacing( images[0]->GetSpacing() );
    this->SetGridDirection( images[0]->GetDirection() );

    const SizeValueType totalParameters = this->GetNumberOfParameters();
    this->m_InternalParametersBuffer.SetSize(totalParameters);
    for( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      const SizeValueType numberOfPixels =
        images[j]->GetLargestPossibleRegion().GetNumberOfPixels();
      if( numberOfPixels * SpaceDimension != totalParameters )
        {
        itkExceptionMacro( << "SetCoefficientImage() has array of images that are "
          << "not the correct size. "
          << numberOfPixels * SpaceDimension << " != " << totalParameters
          << " for image at index " << j << "  \n" << images[j]
          );
        }
      const ParametersValueType * const baseImagePointer =
        images[j]->GetBufferPointer();

      ParametersValueType *dataPointer = this->m_InternalParametersBuffer.data_block();
      std::copy(baseImagePointer,
                baseImagePointer+numberOfPixels,
                dataPointer);
      }
    this->SetParameters( this->m_InternalParametersBuffer );
    }
  else
    {
    itkExceptionMacro( << "SetCoefficientImage() requires that an array of "
                       << "correctly sized images be supplied.");
    }
}

// Print self
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "ValidRegion: " << this->m_ValidRegion << std::endl;
  os << indent << "BulkTransform: ";
  os << this->m_BulkTransform.GetPointer() << std::endl;
  os << indent << "WeightsFunction: ";
  os << this->m_WeightsFunction.GetPointer() << std::endl;

  if( this->m_BulkTransform )
    {
    os << indent << "BulkTransformType: "
       << this->m_BulkTransform->GetNameOfClass() << std::endl;
    }
  os << indent << "GridOrigin: "
    << this->m_GridOrigin << std::endl;
  os << indent << "GridSpacing: "
    << this->m_GridSpacing << std::endl;
  os << indent << "GridDirection: "
    << this->m_GridDirection << std::endl;
  os << indent << "GridRegion: "
    << this->m_GridRegion << std::endl;

}

template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
bool
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::InsideValidRegion( ContinuousIndexType & index ) const
{
  bool inside = true;

  if( inside && this->m_SplineOrderOdd )
    {
    typedef typename ContinuousIndexType::ValueType ValueType;
    for( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      if( index[j] >= static_cast<ValueType>( this->m_ValidRegionLast[j] ) )
        {
        inside = false;
        break;
        }
      if( index[j] < static_cast<ValueType>( this->m_ValidRegionFirst[j] ) )
        {
        inside = false;
        break;
        }
      }
    }
  return inside;
}

template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::TransformPoint( const InputPointType & inputPoint, OutputPointType & outputPoint,
  WeightsType & weights, ParameterIndexArrayType & indices, bool & inside ) const
{
  inside = true;

  InputPointType point=inputPoint;
  if( this->m_BulkTransform )
    {
    point = this->m_BulkTransform->TransformPoint(point);
    }
  else
    {
    point = point;
    }

  // if no coefficients are set, this isn't a proper BSpline Transform
  if( this->m_CoefficientImages[0]->GetBufferPointer() == ITK_NULLPTR)
    {
    itkExceptionMacro( "B-spline coefficients have not been set" );
    }

  ContinuousIndexType index;
  this->m_CoefficientImages[0]->TransformPhysicalPointToContinuousIndex( inputPoint, index );

  // NOTE: if the support region does not lie totally within the grid
  // we assume zero displacement and return the input point
  inside = this->InsideValidRegion( index );
  if( !inside )
    {
    outputPoint = point;
    return;
    }

  IndexType supportIndex;
  // Compute interpolation weights
  this->m_WeightsFunction->Evaluate( index, weights, supportIndex );

  // For each dimension, correlate coefficient with weights
  RegionType supportRegion;
  SizeType supportSize = this->m_WeightsFunction->GetSupportSize();
  supportRegion.SetSize( supportSize );
  supportRegion.SetIndex(supportIndex);

  outputPoint.Fill( NumericTraits<ScalarType>::ZeroValue() );

  typedef ImageScanlineConstIterator<ImageType> IteratorType;
  IteratorType               coeffIterator[SpaceDimension];
  unsigned long              counter = 0;
  const ParametersValueType *basePointer =
    this->m_CoefficientImages[0]->GetBufferPointer();
  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    coeffIterator[j] =
      IteratorType( this->m_CoefficientImages[j], supportRegion );
    }

  while( !coeffIterator[0].IsAtEnd() )
    {
      while( !coeffIterator[0].IsAtEndOfLine() )
        {
        // multiply weigth with coefficient
        for( unsigned int j = 0; j < SpaceDimension; j++ )
          {
          outputPoint[j] += static_cast<ScalarType>(
            weights[counter] * coeffIterator[j].Get() );
          }

        // populate the indices array
        indices[counter] = &( coeffIterator[0].Value() ) - basePointer;

        // go to next coefficient in the support region
        ++counter;
        for( unsigned int j = 0; j < SpaceDimension; j++ )
          {
          ++( coeffIterator[j] );
          }
        } // end of scanline

      for( unsigned int j = 0; j < SpaceDimension; j++ )
        {
        coeffIterator[j].NextLine();
        }
    }
  // return results
  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    outputPoint[j] += point[j];
    }
}

// Compute the Jacobian in one position
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TParametersValueType, NDimensions, VSplineOrder>
::ComputeJacobianWithRespectToParameters( const InputPointType & point,
  JacobianType & jacobian ) const
{
  // Zero all components of jacobian
  jacobian.SetSize( SpaceDimension, this->GetNumberOfParameters() );
  jacobian.Fill( 0.0 );
  RegionType   supportRegion;
  SizeType     supportSize;
  supportSize.Fill( SplineOrder + 1 );
  supportRegion.SetSize( supportSize );

  ContinuousIndexType index;
  this->m_CoefficientImages[0]->
    TransformPhysicalPointToContinuousIndex( point, index );

  // NOTE: if the support region does not lie totally within the grid we assume
  // zero displacement and do no computations beyond zeroing out the value
  // return the input point
  if( !this->InsideValidRegion( index ) )
    {
    return;
    }

  // Compute interpolation weights
  WeightsType weights( this->m_WeightsFunction->GetNumberOfWeights() );

  IndexType supportIndex;
  this->m_WeightsFunction->Evaluate( index, weights, supportIndex );

  supportRegion.SetIndex( supportIndex );

  IndexType startIndex =
    this->m_CoefficientImages[0]->GetLargestPossibleRegion().GetIndex();

  const SizeType &MeshGridSize=this->m_GridRegion.GetSize();
  SizeType cumulativeGridSizes;
  cumulativeGridSizes[0] = ( MeshGridSize[0] );
  for( unsigned int d = 1; d < SpaceDimension; d++ )
    {
    cumulativeGridSizes[d] = cumulativeGridSizes[d-1] * MeshGridSize[d];
    }

  SizeValueType numberOfParametersPerDimension = this->GetNumberOfParametersPerDimension();

  ImageRegionConstIteratorWithIndex<ImageType> It( this->m_CoefficientImages[0], supportRegion );
  unsigned long counter = 0;
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    typename ImageType::OffsetType currentIndex = It.GetIndex() - startIndex;

    unsigned long number = currentIndex[0];
    for( unsigned int d = 1; d < SpaceDimension; d++ )
      {
      number += ( currentIndex[d] * cumulativeGridSizes[d-1] );
      }

    for( unsigned int d = 0; d < SpaceDimension; d++ )
      {
      jacobian( d, number + d * numberOfParametersPerDimension ) = weights[counter];
      }
    counter++;
    }
}

} // namespace

#endif
