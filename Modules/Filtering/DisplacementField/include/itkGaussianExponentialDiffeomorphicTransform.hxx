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
#ifndef __itkGaussianExponentialDiffeomorphicTransform_hxx
#define __itkGaussianExponentialDiffeomorphicTransform_hxx

#include "itkGaussianExponentialDiffeomorphicTransform.h"

#include "itkComposeDisplacementFieldsImageFilter.h"
#include "itkExponentialDisplacementFieldImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkVectorResampleImageFilter.h"

namespace itk
{

template<class TScalar, unsigned int NDimensions>
GaussianExponentialDiffeomorphicTransform<TScalar, NDimensions>
::GaussianExponentialDiffeomorphicTransform():
  m_CalculateNumberOfIntegrationStepsAutomatically( true ),
  m_NumberOfIntegrationSteps( 10 ),
  m_ComputeInverse( false ),
  m_ConstantVelocityField( NULL )
{
}

template<class TScalar, unsigned int NDimensions>
GaussianExponentialDiffeomorphicTransform<TScalar, NDimensions>::
~GaussianExponentialDiffeomorphicTransform()
{
}

template<class TScalar, unsigned int NDimensions>
void
GaussianExponentialDiffeomorphicTransform<TScalar, NDimensions>
::UpdateTransformParameters( const DerivativeType & update, ScalarType factor )
{
  DisplacementFieldPointer displacementField = this->GetDisplacementField();

  const typename DisplacementFieldType::RegionType & bufferedRegion = displacementField->GetBufferedRegion();
  const SizeValueType numberOfPixels = bufferedRegion.GetNumberOfPixels();

  typedef ImportImageFilter<DisplacementVectorType, NDimensions> ImporterType;
  const bool importFilterWillReleaseMemory = false;

  //
  // Smooth the update field
  //
  bool smoothUpdateField = true;
  if( this->m_GaussianSmoothingVarianceForTheUpdateField <= 0.0 )
    {
    itkDebugMacro( "Not smooothing the update field." );
    smoothUpdateField = false;
    }

  DisplacementVectorType *updateFieldPointer = reinterpret_cast<DisplacementVectorType *>( const_cast<DerivativeType &>( update ).data_block() );

  typename ImporterType::Pointer importer = ImporterType::New();
  importer->SetImportPointer( updateFieldPointer, numberOfPixels, importFilterWillReleaseMemory );
  importer->SetRegion( displacementField->GetBufferedRegion() );
  importer->SetOrigin( displacementField->GetOrigin() );
  importer->SetSpacing( displacementField->GetSpacing() );
  importer->SetDirection( displacementField->GetDirection() );

  ConstantVelocityFieldPointer updateField = importer->GetOutput();
  updateField->Update();
  updateField->DisconnectPipeline();

  if( smoothUpdateField )
    {
    itkDebugMacro( "Smoothing the update field." );

    // The update field is the velocity field but since it's constant
    // we smooth it using the parent class smoothing functionality

    ConstantVelocityFieldPointer updateSmoothField = this->GaussianSmoothDisplacementField( updateField, this->GetGaussianSmoothingVarianceForTheUpdateField() );

    updateField = updateSmoothField;
    }

  typedef Image<ScalarType, NDimensions> RealImageType;

  typedef MultiplyImageFilter<DisplacementFieldType, RealImageType, DisplacementFieldType> MultiplierType;
  typename MultiplierType::Pointer multiplier = MultiplierType::New();
  multiplier->SetInput( updateField );
  multiplier->SetConstant( factor );

  typename DisplacementFieldType::Pointer scaledUpdateField = multiplier->GetOutput();
  scaledUpdateField->Update();
  scaledUpdateField->DisconnectPipeline();

  // Compose the scaled update field with the

  if( !this->m_ConstantVelocityField )
    {
    DisplacementVectorType zeroVector( 0.0 );

    this->m_ConstantVelocityField = ConstantVelocityFieldType::New();
    this->m_ConstantVelocityField->CopyInformation( scaledUpdateField );
    this->m_ConstantVelocityField->SetRegions( scaledUpdateField->GetRequestedRegion() );
    this->m_ConstantVelocityField->Allocate();
    this->m_ConstantVelocityField->FillBuffer( zeroVector );
    }
  else
    {
    // Check to see if the velocity field needs to be resampled to match the size
    // of the displacement field

    const typename DisplacementFieldType::SizeType displacementFieldSize = displacementField->GetRequestedRegion().GetSize();

    if( displacementFieldSize != this->m_ConstantVelocityField->GetRequestedRegion().GetSize() )
      {
      typedef IdentityTransform<ScalarType, NDimensions> IdentityTransformType;
      typename IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();
      identityTransform->SetIdentity();

      typedef VectorLinearInterpolateImageFunction<DisplacementFieldType, ScalarType> LinearInterpolatorType;
      typename LinearInterpolatorType::Pointer interpolator = LinearInterpolatorType::New();
      interpolator->SetInputImage( this->m_ConstantVelocityField );

      typedef VectorResampleImageFilter<DisplacementFieldType, DisplacementFieldType, ScalarType> ResamplerType;
      typename ResamplerType::Pointer resampler = ResamplerType::New();
      resampler->SetInput( this->m_ConstantVelocityField );
      resampler->SetOutputDirection( scaledUpdateField->GetDirection() );
      resampler->SetOutputOrigin( scaledUpdateField->GetOrigin() );
      resampler->SetOutputSpacing( scaledUpdateField->GetSpacing() );
      resampler->SetSize( displacementFieldSize );
      resampler->SetTransform( identityTransform );
      resampler->SetInterpolator( interpolator );

      ConstantVelocityFieldPointer resampledVelocityField = resampler->GetOutput();
      resampledVelocityField->Update();
      resampledVelocityField->DisconnectPipeline();

      this->m_ConstantVelocityField = resampledVelocityField;
      }
    }

  typedef AddImageFilter<ConstantVelocityFieldType, ConstantVelocityFieldType, ConstantVelocityFieldType> AdderType;
  typename AdderType::Pointer adder = AdderType::New();
  adder->SetInput1( this->m_ConstantVelocityField );
  adder->SetInput2( scaledUpdateField );

  ConstantVelocityFieldPointer velocityField = adder->GetOutput();
  velocityField->Update();
  velocityField->DisconnectPipeline();

  //
  // Smooth the velocity field
  //
  bool smoothVelocityField = true;
  if( this->m_GaussianSmoothingVarianceForTheVelocityField <= 0 )
    {
    itkDebugMacro( "Not smoothing the velocity field." );
    smoothVelocityField = false;
    }

  if( smoothVelocityField )
    {
    itkDebugMacro( "Smoothing the velocity field." );

    // The update field is the velocity field but since it's constant
    // we smooth it using the parent class smoothing functionality

    ConstantVelocityFieldPointer velocitySmoothField = this->GaussianSmoothDisplacementField( velocityField, this->GetGaussianSmoothingVarianceForTheVelocityField() );

    velocityField = velocitySmoothField;
    }

  typedef ExponentialDisplacementFieldImageFilter<ConstantVelocityFieldType, DisplacementFieldType> ExponentiatorType;
  typename ExponentiatorType::Pointer exponentiator = ExponentiatorType::New();
  exponentiator->SetInput( velocityField );
  if( this->m_CalculateNumberOfIntegrationStepsAutomatically || this->m_NumberOfIntegrationSteps == 0 )
    {
    exponentiator->SetAutomaticNumberOfIterations( true );
    if( this->m_NumberOfIntegrationSteps == 0 )
      {
      itkWarningMacro( "Number of integration steps is 0.  Calculating the number of integration steps automatically." );
      }
    }
  else
    {
    exponentiator->SetAutomaticNumberOfIterations( false );
    exponentiator->SetMaximumNumberOfIterations( this->m_NumberOfIntegrationSteps );
    }
  exponentiator->SetComputeInverse( false );
  exponentiator->Update();

  this->SetDisplacementField( exponentiator->GetOutput() );

  // Compute the inverse displacement field if requested

  if( this->m_ComputeInverse )
    {
    typename ExponentiatorType::Pointer exponentiatorInv = ExponentiatorType::New();
    exponentiatorInv->SetInput( velocityField );
    if( this->m_CalculateNumberOfIntegrationStepsAutomatically || this->m_NumberOfIntegrationSteps == 0 )
      {
      exponentiatorInv->SetAutomaticNumberOfIterations( true );
      if( this->m_NumberOfIntegrationSteps == 0 )
        {
        itkWarningMacro( "Number of integration steps is 0.  Calculating the number of integration steps automatically." );
        }
      }
    else
      {
      exponentiatorInv->SetAutomaticNumberOfIterations( false );
      exponentiatorInv->SetMaximumNumberOfIterations( this->m_NumberOfIntegrationSteps );
      }
    exponentiatorInv->SetComputeInverse( true );
    exponentiatorInv->Update();

    this->SetInverseDisplacementField( exponentiatorInv->GetOutput() );
    }

  this->m_ConstantVelocityField = velocityField;
}

/**
 * Standard "PrintSelf" method
 */
template<class TScalar, unsigned int NDimensions>
void
GaussianExponentialDiffeomorphicTransform<TScalar, NDimensions>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Calculate number of integration steps automatically = " << this->m_CalculateNumberOfIntegrationStepsAutomatically << std::endl;
  os << indent << "Number of integration steps = " << this->m_NumberOfIntegrationSteps << std::endl;
  os << indent << "Compute inverse = " << this->m_ComputeInverse << std::endl;
  os << indent << "Gaussian variance for the velocity field = "
    << this->m_GaussianSmoothingVarianceForTheVelocityField << std::endl;
}

} // namespace itk

#endif
