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
#ifndef itkBSplineBaseTransform_hxx
#define itkBSplineBaseTransform_hxx

#include "itkBSplineBaseTransform.h"

#include "itkContinuousIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

// Constructor with default arguments
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::BSplineBaseTransform() :
  Superclass( 0 ),
  m_CoefficientImages( Self::ArrayOfImagePointerGeneratorHelper() )
{
  this->m_InternalParametersBuffer = ParametersType( 0 );

  // Instantiate a weights function
  this->m_WeightsFunction = WeightsFunctionType::New();
}

// Destructor
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::~BSplineBaseTransform()
{
}


// Set the parameters
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetIdentity()
{
  if( this->m_InternalParametersBuffer.Size() != this->GetNumberOfParameters() )
    {
    this->m_InternalParametersBuffer.SetSize( this->GetNumberOfParameters() );
    }
  this->m_InternalParametersBuffer.Fill( 0.0 );

  this->SetParameters( this->m_InternalParametersBuffer );
  this->Modified();
}

// Set the parameters
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetParameters( const ParametersType & parameters )
{
  // check if the number of parameters match the
  // expected number of parameters
  if( parameters.Size() != this->GetNumberOfParameters() )
    {
    itkExceptionMacro( << "Mismatch between parameters size "
                       << parameters.Size() << " and expected number of parameters "
                       << this->GetNumberOfParameters()
                       << ( this->m_CoefficientImages[0]->GetLargestPossibleRegion().GetNumberOfPixels() == 0 ?
                            ". \nSince the size of the grid region is 0, perhaps you forgot to "
                            "SetGridRegion or SetFixedParameters before setting the Parameters."
                            : "" ) );
    }

  if( &parameters != &( this->m_InternalParametersBuffer ) )
    {
    // Clean up this->m_InternalParametersBuffer because we will
    // use an externally supplied set of parameters as the buffer
    this->m_InternalParametersBuffer = parameters;
    }

  // Wrap flat array as images of coefficients
  this->WrapAsImages();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();
}

// Set the parameters by value
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetParametersByValue( const ParametersType & parameters )
{
  // check if the number of parameters match the
  // expected number of parameters
  if( parameters.Size() != this->GetNumberOfParameters() )
    {
    itkExceptionMacro( << "Mismatched between parameters size "
                       << parameters.size() << " and region size "
                       << this->GetNumberOfParameters() );
    }

  // copy parameters to this->m_InternalParametersBuffer
  this->m_InternalParametersBuffer = parameters;
  this->SetParameters( this->m_InternalParametersBuffer );
}


template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::SetFixedParametersFromTransformDomainInformation() const
{
  //  Fixed Parameters store the following information:
  //  Grid Size
  //  Grid Origin
  //  Grid Spacing
  //  Grid Direction
  //  The size of each of these is equal to NDimensions

  this->m_FixedParameters.SetSize( NDimensions * ( NDimensions + 3 ) );

  this->SetFixedParametersGridSizeFromTransformDomainInformation();
  this->SetFixedParametersGridOriginFromTransformDomainInformation();
  this->SetFixedParametersGridSpacingFromTransformDomainInformation();
  this->SetFixedParametersGridDirectionFromTransformDomainInformation();

  this->Modified();
}

/**
 * UpdateTransformParameters
 */
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::UpdateTransformParameters( const DerivativeType & update, TParametersValueType factor )
{
  NumberOfParametersType numberOfParameters = this->GetNumberOfParameters();

  if( update.Size() != numberOfParameters )
    {
    itkExceptionMacro("Parameter update size, " << update.Size() << ", must "
                      " be same as transform parameter size, "
                                                << numberOfParameters << std::endl);
    }

  /* Make sure m_Parameters is updated to reflect the current values in
   * the transform's other parameter-related variables. This is effective for
   * managing the parallel variables used for storing parameter data,
   * but inefficient. However for small global transforms, shouldn't be
   * too bad. Dense-field transform will want to make sure m_Parameters
   * is always updated whenever the transform is changed, so GetParameters
   * can be skipped in their implementations of UpdateTransformParameters. */

  if( factor == 1.0 )
    {
    for( NumberOfParametersType k = 0; k < numberOfParameters; k++ )
      {
      this->m_InternalParametersBuffer[k] += update[k];
      }
    }
  else
    {
    for( NumberOfParametersType k = 0; k < numberOfParameters; k++ )
      {
      this->m_InternalParametersBuffer[k] += update[k] * factor;
      }
    }

  /* Call SetParameters with the updated parameters.
   * SetParameters in most transforms is used to assign the input params
   * to member variables, possibly with some processing. The member variables
   * are then used in TransformPoint.
   * In the case of dense-field transforms that are updated in blocks from
   * a threaded implementation, SetParameters doesn't do this, and is
   * optimized to not copy the input parameters when == m_Parameters.
   */
  this->SetParameters( this->m_InternalParametersBuffer );

  /* Call Modified, following behavior of other transform when their
   * parameters change, e.g. MatrixOffsetTransformBase */
  this->Modified();
}

// Wrap flat parameters as images
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::WrapAsImages()
{
  /**
   * Wrap flat parameters array into SpaceDimension number of ITK images
   * NOTE: For efficiency, parameters are not copied locally. The parameters
   * are assumed to be maintained by the caller.
   */
  PixelType *dataPointer = const_cast<PixelType *>( this->m_InternalParametersBuffer.data_block() );
  const NumberOfParametersType numberOfPixels = this->GetNumberOfParametersPerDimension();

  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    this->m_CoefficientImages[j]->GetPixelContainer()->
      SetImportPointer( dataPointer + j * numberOfPixels, numberOfPixels );
    }
}

// Get the parameters
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
const typename BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>::ParametersType &
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::GetParameters() const
{
  return this->m_InternalParametersBuffer;
}

// Get the parameters
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
const typename BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>::FixedParametersType &
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::GetFixedParameters() const
{
  // HACK:  This should not be necessary if the
  //       class is kept in a consistent state
  //  this->SetFixedParametersFromCoefficientImageInformation();
  return this->m_FixedParameters;
}

// Print self
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "CoefficientImage: [ ";
  for( unsigned int j = 0; j < SpaceDimension - 1; j++ )
    {
    os << this->m_CoefficientImages[j].GetPointer() << ", ";
    }
  os << this->m_CoefficientImages[SpaceDimension - 1].GetPointer()
     << " ]" << std::endl;
}


/** Get Jacobian at a point. A very specialized function just for BSplines */
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::ComputeJacobianFromBSplineWeightsWithRespectToPosition(
  const InputPointType & point, WeightsType & weights,
  ParameterIndexArrayType & indexes ) const
{
  ContinuousIndexType index;

  this->m_CoefficientImages[0]->TransformPhysicalPointToContinuousIndex( point, index );

  // NOTE: if the support region does not lie totally within the grid
  // we assume zero displacement and return the input point
  if( !this->InsideValidRegion( index ) )
    {
    weights.Fill( 0.0 );
    indexes.Fill( 0 );
    return;
    }

  // Compute interpolation weights
  IndexType supportIndex;
  this->m_WeightsFunction->Evaluate(index, weights, supportIndex);

  // For each dimension, copy the weight to the support region
  RegionType supportRegion;
  SizeType   supportSize;
  supportSize.Fill( SplineOrder + 1 );
  supportRegion.SetSize( supportSize );
  supportRegion.SetIndex( supportIndex );
  unsigned long counter = 0;

  typedef ImageRegionIterator<ImageType> IteratorType;

  IteratorType coeffIterator = IteratorType(
      this->m_CoefficientImages[0], supportRegion );
  const ParametersValueType *basePointer =
    this->m_CoefficientImages[0]->GetBufferPointer();
  while( !coeffIterator.IsAtEnd() )
    {
    indexes[counter] = &( coeffIterator.Value() ) - basePointer;

    // go to next coefficient in the support region
    ++counter;
    ++coeffIterator;
    }
}

template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
unsigned int
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::GetNumberOfAffectedWeights() const
{
  return this->m_WeightsFunction->GetNumberOfWeights();
}

// This helper class is used to work around a race condition where the dynamically
// generated images must exist before the references to the sub-sections are created.
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
typename BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>::CoefficientImageArray
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::ArrayOfImagePointerGeneratorHelper(void)
{
  CoefficientImageArray tempArrayOfPointers;

  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    tempArrayOfPointers[j] = ImageType::New();
    }
  return tempArrayOfPointers;
}

// Transform a point
template<typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
typename BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::OutputPointType
BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
::TransformPoint(const InputPointType & point) const
{
  WeightsType             weights( this->m_WeightsFunction->GetNumberOfWeights() );
  ParameterIndexArrayType indices( this->m_WeightsFunction->GetNumberOfWeights() );
  OutputPointType         outputPoint;
  bool                    inside;

  this->TransformPoint( point, outputPoint, weights, indices, inside );

  return outputPoint;
}

} // namespace
#endif
