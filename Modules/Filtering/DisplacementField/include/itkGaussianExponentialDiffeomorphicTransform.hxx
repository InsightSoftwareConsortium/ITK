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
#ifndef itkGaussianExponentialDiffeomorphicTransform_hxx
#define itkGaussianExponentialDiffeomorphicTransform_hxx

#include "itkGaussianExponentialDiffeomorphicTransform.h"

#include "itkAddImageFilter.h"
#include "itkImageDuplicator.h"
#include "itkImportImageFilter.h"
#include "itkMultiplyImageFilter.h"

namespace itk
{

template<typename TParametersValueType, unsigned int NDimensions>
GaussianExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>
::GaussianExponentialDiffeomorphicTransform():
  m_GaussianSmoothingVarianceForTheUpdateField( 0.5 ),
  m_GaussianSmoothingVarianceForTheConstantVelocityField( 0.5 )
{
}

template<typename TParametersValueType, unsigned int NDimensions>
GaussianExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>::
~GaussianExponentialDiffeomorphicTransform()
{
}

template<typename TParametersValueType, unsigned int NDimensions>
void
GaussianExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>
::UpdateTransformParameters( const DerivativeType & update, ScalarType factor )
{
  //
  // Smooth the update field
  //
  bool smoothUpdateField = true;
  if( this->m_GaussianSmoothingVarianceForTheUpdateField <= 0.0 )
    {
    itkDebugMacro( "Not smooothing the update field." );
    smoothUpdateField = false;
    }

  ConstantVelocityFieldPointer velocityField = this->GetModifiableConstantVelocityField();
  if( !velocityField )
    {
    itkExceptionMacro( "The velocity field has not been set." );
    }

  const typename ConstantVelocityFieldType::RegionType & bufferedRegion = velocityField->GetBufferedRegion();
  const SizeValueType numberOfPixels = bufferedRegion.GetNumberOfPixels();

  DisplacementVectorType *updateFieldPointer = reinterpret_cast<DisplacementVectorType *>( const_cast<DerivativeType &>( update ).data_block() );

  typedef ImportImageFilter<DisplacementVectorType, NDimensions> ImporterType;
  const bool importFilterWillReleaseMemory = false;

  typename ImporterType::Pointer importer = ImporterType::New();
  importer->SetImportPointer( updateFieldPointer, numberOfPixels, importFilterWillReleaseMemory );
  importer->SetRegion( velocityField->GetBufferedRegion() );
  importer->SetOrigin( velocityField->GetOrigin() );
  importer->SetSpacing( velocityField->GetSpacing() );
  importer->SetDirection( velocityField->GetDirection() );

  ConstantVelocityFieldPointer updateField = importer->GetOutput();
  updateField->Update();
  updateField->DisconnectPipeline();

  if( smoothUpdateField )
    {
    itkDebugMacro( "Smoothing the update field." );

    ConstantVelocityFieldPointer updateSmoothField =
      this->GaussianSmoothConstantVelocityField( updateField, this->m_GaussianSmoothingVarianceForTheUpdateField );

    updateField = updateSmoothField;
    }

  typedef Image<ScalarType, NDimensions> RealImageType;

  typedef MultiplyImageFilter<ConstantVelocityFieldType, RealImageType, ConstantVelocityFieldType> MultiplierType;
  typename MultiplierType::Pointer multiplier = MultiplierType::New();
  multiplier->SetInput( updateField );
  multiplier->SetConstant( factor );
  multiplier->Update();

  typedef AddImageFilter<ConstantVelocityFieldType, ConstantVelocityFieldType, ConstantVelocityFieldType> AdderType;
  typename AdderType::Pointer adder = AdderType::New();
  adder->SetInput1( velocityField );
  adder->SetInput2( multiplier->GetOutput() );

  ConstantVelocityFieldPointer updatedVelocityField = adder->GetOutput();
  updatedVelocityField->Update();
  updatedVelocityField->DisconnectPipeline();

  //
  // Smooth the velocity field
  //
  bool smoothVelocityField = true;
  if( this->m_GaussianSmoothingVarianceForTheConstantVelocityField <= 0 )
    {
    itkDebugMacro( "Not smoothing the velocity field." );
    smoothVelocityField = false;
    }

  if( smoothVelocityField )
    {
    itkDebugMacro( "Smoothing the velocity field." );

    // The update field is the velocity field but since it's constant
    // we smooth it using the parent class smoothing functionality

    ConstantVelocityFieldPointer velocitySmoothField =
      this->GaussianSmoothConstantVelocityField( updatedVelocityField, this->m_GaussianSmoothingVarianceForTheConstantVelocityField );

    this->SetConstantVelocityField( velocitySmoothField );
    }
  else
    {
    this->SetConstantVelocityField( updatedVelocityField );
    }

  this->IntegrateVelocityField();
}

template<typename TParametersValueType, unsigned int NDimensions>
typename GaussianExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>::ConstantVelocityFieldPointer
GaussianExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>
::GaussianSmoothConstantVelocityField( ConstantVelocityFieldType *field, ScalarType variance )
{
  if( variance <= 0.0 )
    {
    return field;
    }

  typedef ImageDuplicator<ConstantVelocityFieldType> DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( field );
  duplicator->Update();

  ConstantVelocityFieldPointer smoothField = duplicator->GetModifiableOutput();

  typename GaussianSmoothingSmootherType::Pointer smoother = GaussianSmoothingSmootherType::New();

  for( unsigned int dimension = 0; dimension < Superclass::Dimension; ++dimension )
    {
    // smooth along this dimension
    this->m_GaussianSmoothingOperator.SetDirection( dimension );
    this->m_GaussianSmoothingOperator.SetVariance( variance );
    this->m_GaussianSmoothingOperator.SetMaximumError( 0.001 );
    this->m_GaussianSmoothingOperator.SetMaximumKernelWidth( smoothField->GetRequestedRegion().GetSize()[dimension] );
    this->m_GaussianSmoothingOperator.CreateDirectional();

    // todo: make sure we only smooth within the buffered region
    smoother->SetOperator( this->m_GaussianSmoothingOperator );
    smoother->SetInput( smoothField );
    try
      {
      smoother->Update();
      }
    catch( ExceptionObject & exc )
      {
      std::string msg("Caught exception: ");
      msg += exc.what();
      itkExceptionMacro( << msg );
      }

    smoothField = smoother->GetOutput();
    smoothField->Update();
    smoothField->DisconnectPipeline();
    }

  const DisplacementVectorType zeroVector( 0.0 );

  //make sure boundary does not move
  ScalarType weight1 = 1.0;
  if( variance < 0.5 )
    {
    weight1 = 1.0 - 1.0 * ( variance / 0.5 );
    }
  ScalarType weight2 = 1.0 - weight1;

  const typename ConstantVelocityFieldType::RegionType region = field->GetLargestPossibleRegion();
  const typename ConstantVelocityFieldType::SizeType size = region.GetSize();
  const typename ConstantVelocityFieldType::IndexType startIndex = region.GetIndex();

  ImageRegionIteratorWithIndex<ConstantVelocityFieldType> fieldIt( field, field->GetLargestPossibleRegion() );
  ImageRegionConstIteratorWithIndex<ConstantVelocityFieldType> smoothedFieldIt( smoothField, smoothField->GetLargestPossibleRegion() );
  for( fieldIt.GoToBegin(), smoothedFieldIt.GoToBegin(); !fieldIt.IsAtEnd(); ++fieldIt, ++smoothedFieldIt )
    {
    typename ConstantVelocityFieldType::IndexType index = fieldIt.GetIndex();
    bool isOnBoundary = false;
    for ( unsigned int dimension = 0; dimension < Dimension; ++dimension )
      {
      if( index[dimension] == startIndex[dimension] || index[dimension] == static_cast<IndexValueType>( size[dimension] ) - startIndex[dimension] - 1 )
        {
        isOnBoundary = true;
        break;
        }
      }
    if( isOnBoundary )
      {
      fieldIt.Set( zeroVector );
      }
    else
      {
      fieldIt.Set( smoothedFieldIt.Get() * weight1 + fieldIt.Get() * weight2 );
      }
  }

  return field;
}

/**
 * Standard "PrintSelf" method
 */
template<typename TParametersValueType, unsigned int NDimensions>
void
GaussianExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Calculate number of integration steps automatically = " << this->m_CalculateNumberOfIntegrationStepsAutomatically << std::endl;
  os << indent << "Gaussian variance for the velocity field = "
    << this->m_GaussianSmoothingVarianceForTheConstantVelocityField << std::endl;
  os << indent << "Gaussian variance for the update field = "
    << this->m_GaussianSmoothingVarianceForTheUpdateField << std::endl;
}

} // namespace itk

#endif
