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
#ifndef itkGaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform_hxx
#define itkGaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform_hxx

#include "itkGaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform.h"

#include "itkImageAlgorithm.h"
#include "itkImageDuplicator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImportImageFilter.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"
#include "itkGaussianOperator.h"

namespace itk
{

template<typename TParametersValueType, unsigned int NDimensions>
GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<TParametersValueType, NDimensions>
::GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform() :
  m_GaussianSmoothingTempFieldModifiedTime(0),
  m_GaussianSpatialSmoothingVarianceForTheUpdateField(3.0),
  m_GaussianSpatialSmoothingVarianceForTheTotalField(0.5),
  m_GaussianTemporalSmoothingVarianceForTheUpdateField(0.25),
  m_GaussianTemporalSmoothingVarianceForTheTotalField(0.0)
{
}

template<typename TParametersValueType, unsigned int NDimensions>
GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<TParametersValueType, NDimensions>::
~GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform()
{
}

template<typename TParametersValueType, unsigned int NDimensions>
void
GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<TParametersValueType, NDimensions>
::UpdateTransformParameters( const DerivativeType & update, ScalarType factor )
{
  TimeVaryingVelocityFieldPointer velocityField = this->GetModifiableVelocityField();

  const typename VelocityFieldType::RegionType & bufferedRegion = velocityField->GetBufferedRegion();
  const SizeValueType numberOfPixels = bufferedRegion.GetNumberOfPixels();

  typedef ImportImageFilter<DisplacementVectorType, NDimensions + 1> ImporterType;
  const bool importFilterWillReleaseMemory = false;

  //
  // Smooth the update field
  //
  bool smoothUpdateField = true;
  if( this->m_GaussianSpatialSmoothingVarianceForTheUpdateField <= 0.0 && this->m_GaussianTemporalSmoothingVarianceForTheUpdateField <= 0.0 )
    {
    itkDebugMacro( "Not smooothing the update field." );
    smoothUpdateField = false;
    }
  if( smoothUpdateField )
    {
    itkDebugMacro( "Smooothing the update field." );

    DisplacementVectorType *updateFieldPointer = reinterpret_cast<DisplacementVectorType *>( const_cast<DerivativeType &>(update).data_block() );

    typename ImporterType::Pointer importer = ImporterType::New();
    importer->SetImportPointer( updateFieldPointer, numberOfPixels, importFilterWillReleaseMemory );
    importer->SetRegion( velocityField->GetBufferedRegion() );
    importer->SetOrigin( velocityField->GetOrigin() );
    importer->SetSpacing( velocityField->GetSpacing() );
    importer->SetDirection( velocityField->GetDirection() );

    TimeVaryingVelocityFieldPointer updateField = importer->GetOutput();
    updateField->Update();
    updateField->DisconnectPipeline();

    TimeVaryingVelocityFieldPointer updateSmoothField = this->GaussianSmoothTimeVaryingVelocityField( updateField,
      this->m_GaussianSpatialSmoothingVarianceForTheUpdateField, this->m_GaussianTemporalSmoothingVarianceForTheUpdateField );

    ImageAlgorithm::Copy< VelocityFieldType, VelocityFieldType >( updateSmoothField, updateField, updateSmoothField->GetBufferedRegion(), updateField->GetBufferedRegion() );
    }

  //
  // Add the update field to the current total field before (optionally)
  // smoothing the total field
  //
  Superclass::UpdateTransformParameters( update, factor );

  //
  // Smooth the total field
  //
  bool smoothTotalField = true;
  if( this->m_GaussianSpatialSmoothingVarianceForTheTotalField <= 0.0 && this->m_GaussianTemporalSmoothingVarianceForTheTotalField <= 0.0 )
    {
    itkDebugMacro( "Not smooothing the total field." );
    smoothTotalField = false;
    }
  if( smoothTotalField )
    {
    itkDebugMacro( "Smooothing the total field." );

    typename ImporterType::Pointer importer = ImporterType::New();
    importer->SetImportPointer( velocityField->GetBufferPointer(), numberOfPixels, importFilterWillReleaseMemory );
    importer->SetRegion( velocityField->GetBufferedRegion() );
    importer->SetOrigin( velocityField->GetOrigin() );
    importer->SetSpacing( velocityField->GetSpacing() );
    importer->SetDirection( velocityField->GetDirection() );

    TimeVaryingVelocityFieldPointer totalField = importer->GetOutput();
    totalField->Update();
    totalField->DisconnectPipeline();

    TimeVaryingVelocityFieldPointer totalSmoothField = this->GaussianSmoothTimeVaryingVelocityField( totalField,
      this->m_GaussianSpatialSmoothingVarianceForTheTotalField, this->m_GaussianTemporalSmoothingVarianceForTheTotalField );

    ImageAlgorithm::Copy< VelocityFieldType, VelocityFieldType >( totalSmoothField, velocityField, totalSmoothField->GetBufferedRegion(), velocityField->GetBufferedRegion() );
    }

  this->IntegrateVelocityField();
}

template<typename TParametersValueType, unsigned int NDimensions>
typename GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<TParametersValueType, NDimensions>::TimeVaryingVelocityFieldPointer
GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<TParametersValueType, NDimensions>
::GaussianSmoothTimeVaryingVelocityField( VelocityFieldType *field, ScalarType spatialVariance, ScalarType temporalVariance )
{
  if( spatialVariance <= 0.0 && temporalVariance <= 0.0 )
    {
    return field;
    }

  typedef ImageDuplicator<VelocityFieldType> DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( field );
  duplicator->Update();

  TimeVaryingVelocityFieldPointer smoothField = duplicator->GetModifiableOutput();

  typedef VectorNeighborhoodOperatorImageFilter<VelocityFieldType, VelocityFieldType> SmootherType;
  typename SmootherType::Pointer smoother = SmootherType::New();

  for( unsigned int d = 0; d < TimeVaryingVelocityFieldDimension; d++ )
    {
    typedef GaussianOperator<DisplacementVectorValueType, NDimensions + 1> GaussianType;
    GaussianType gaussian;
    if( d < NDimensions )
      {
      gaussian.SetVariance( spatialVariance );
      }
    else
      {
      gaussian.SetVariance( temporalVariance );
      }

    if( gaussian.GetVariance() > 0.0 )
      {
      gaussian.SetMaximumError( 0.001 );
      gaussian.SetDirection( d );
      gaussian.SetMaximumKernelWidth( smoothField->GetRequestedRegion().GetSize()[d] );
      gaussian.CreateDirectional();

      smoother->SetOperator( gaussian );
      smoother->SetInput( smoothField );

      smoothField = smoother->GetOutput();
      smoothField->Update();
      smoothField->DisconnectPipeline();
      }
    }

  //make sure boundary does not move

  const DisplacementVectorType zeroVector( 0.0 );

  ScalarType weight1 = 1.0;
  if( spatialVariance < 0.5 )
    {
    weight1 = 1.0 - 1.0 * ( spatialVariance / 0.5 );
    }
  ScalarType weight2 = 1.0 - weight1;

  typedef typename VelocityFieldType::SizeType TimeVaryingVelocityFieldSizeType;
  typedef typename VelocityFieldType::IndexType TimeVaryingVelocityFieldIndexType;

  TimeVaryingVelocityFieldSizeType size = field->GetLargestPossibleRegion().GetSize();
  TimeVaryingVelocityFieldIndexType startIndex = field->GetLargestPossibleRegion().GetIndex();

  ImageRegionIteratorWithIndex<VelocityFieldType> fieldIt( field, field->GetLargestPossibleRegion() );
  ImageRegionConstIteratorWithIndex<VelocityFieldType> smoothedFieldIt( smoothField, smoothField->GetLargestPossibleRegion() );
  for( fieldIt.GoToBegin(), smoothedFieldIt.GoToBegin(); !fieldIt.IsAtEnd(); ++fieldIt, ++smoothedFieldIt )
    {
    TimeVaryingVelocityFieldIndexType index = fieldIt.GetIndex();

    bool isOnBoundary = false;
    for( unsigned int d = 0; d < NDimensions; d++ )
      {
      if( index[d] == startIndex[d] || index[d] == static_cast<IndexValueType>( size[d] ) - startIndex[d] - 1 )
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

template<typename TParametersValueType, unsigned int NDimensions>
void
GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<TParametersValueType, NDimensions>::
PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  os << indent << "Gaussian smoothing parameters: " << std::endl
     << indent << "Gaussian spatial smoothing variance for the update field: " << this->m_GaussianSpatialSmoothingVarianceForTheUpdateField << std::endl
     << indent << "Gaussian temporal smoothing variance for the update field: " << this->m_GaussianTemporalSmoothingVarianceForTheUpdateField << std::endl
     << indent << "Gaussian spatial smoothing variance for the total field: " << this->m_GaussianSpatialSmoothingVarianceForTheTotalField << std::endl
     << indent << "Gaussian temporal smoothing variance for the total field: " << this->m_GaussianTemporalSmoothingVarianceForTheTotalField << std::endl
     << std::endl;
}
} // namespace itk

#endif
