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
#ifndef itkGaussianSmoothingOnUpdateDisplacementFieldTransform_hxx
#define itkGaussianSmoothingOnUpdateDisplacementFieldTransform_hxx

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"

#include "itkAddImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkImageAlgorithm.h"
#include "itkImageDuplicator.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImportImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"

namespace itk
{

template<typename TParametersValueType, unsigned int NDimensions>
GaussianSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, NDimensions>
::GaussianSmoothingOnUpdateDisplacementFieldTransform()
{
  this->m_GaussianSmoothingVarianceForTheUpdateField = 3.0;
  this->m_GaussianSmoothingVarianceForTheTotalField = 0.5;
}

template<typename TParametersValueType, unsigned int NDimensions>
GaussianSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, NDimensions>::
~GaussianSmoothingOnUpdateDisplacementFieldTransform()
{
}

template<typename TParametersValueType, unsigned int NDimensions>
void
GaussianSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, NDimensions>
::UpdateTransformParameters( const DerivativeType & update, ScalarType factor)
{
  DisplacementFieldPointer displacementField = this->GetModifiableDisplacementField();

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
  if( smoothUpdateField )
    {
    itkDebugMacro( "Smooothing the update field." );

    DisplacementVectorType *updateFieldPointer = reinterpret_cast<DisplacementVectorType *>( const_cast<DerivativeType &>(update).data_block() );

    typename ImporterType::Pointer importer = ImporterType::New();
    importer->SetImportPointer( updateFieldPointer, numberOfPixels, importFilterWillReleaseMemory );
    importer->SetRegion( displacementField->GetBufferedRegion() );
    importer->SetOrigin( displacementField->GetOrigin() );
    importer->SetSpacing( displacementField->GetSpacing() );
    importer->SetDirection( displacementField->GetDirection() );

    DisplacementFieldPointer updateField = importer->GetOutput();
    updateField->Update();
    updateField->DisconnectPipeline();

    DisplacementFieldPointer smoothedField = this->GaussianSmoothDisplacementField( updateField, this->m_GaussianSmoothingVarianceForTheUpdateField );

    ImageAlgorithm::Copy< DisplacementFieldType, DisplacementFieldType >( smoothedField, updateField, smoothedField->GetBufferedRegion(), updateField->GetBufferedRegion() );
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
  if( this->m_GaussianSmoothingVarianceForTheTotalField <= 0.0 )
    {
    itkDebugMacro( "Not smooothing the total field." );
    smoothTotalField = false;
    }
  if( smoothTotalField )
    {
    itkDebugMacro( "Smooothing the total field." );

    typename ImporterType::Pointer importer = ImporterType::New();
    importer->SetImportPointer( displacementField->GetBufferPointer(), numberOfPixels, importFilterWillReleaseMemory );
    importer->SetRegion( displacementField->GetBufferedRegion() );
    importer->SetOrigin( displacementField->GetOrigin() );
    importer->SetSpacing( displacementField->GetSpacing() );
    importer->SetDirection( displacementField->GetDirection() );

    DisplacementFieldPointer totalField = importer->GetOutput();
    totalField->Update();
    totalField->DisconnectPipeline();

    DisplacementFieldPointer totalSmoothField = this->GaussianSmoothDisplacementField( totalField, this->m_GaussianSmoothingVarianceForTheTotalField );

    ImageAlgorithm::Copy< DisplacementFieldType, DisplacementFieldType >( totalSmoothField, totalField, totalSmoothField->GetBufferedRegion(), totalField->GetBufferedRegion() );
    }
}

template<typename TParametersValueType, unsigned int NDimensions>
typename GaussianSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, NDimensions>::DisplacementFieldPointer
GaussianSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, NDimensions>
::GaussianSmoothDisplacementField( DisplacementFieldType *field, ScalarType variance )
{
  if( variance <= 0.0 )
    {
    return field;
    }

  typedef ImageDuplicator< DisplacementFieldType > DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( field );
  duplicator->Update();

  DisplacementFieldPointer smoothField = duplicator->GetModifiableOutput();

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

  const typename DisplacementFieldType::RegionType region = field->GetLargestPossibleRegion();
  const typename DisplacementFieldType::SizeType size = region.GetSize();
  const typename DisplacementFieldType::IndexType startIndex = region.GetIndex();

  ImageRegionIteratorWithIndex< DisplacementFieldType > fieldIt( field, field->GetLargestPossibleRegion() );
  ImageRegionConstIteratorWithIndex< DisplacementFieldType > smoothedFieldIt( smoothField, smoothField->GetLargestPossibleRegion() );
  for( fieldIt.GoToBegin(), smoothedFieldIt.GoToBegin(); !fieldIt.IsAtEnd(); ++fieldIt, ++smoothedFieldIt )
    {
    typename DisplacementFieldType::IndexType index = fieldIt.GetIndex();
    bool isOnBoundary = false;
    for ( unsigned int dimension = 0; dimension < Superclass::Dimension; ++dimension )
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

template<typename TParametersValueType, unsigned int NDimensions>
typename LightObject::Pointer
GaussianSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, NDimensions>
::InternalClone() const
{
  LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval =
    dynamic_cast<Self *>(loPtr.GetPointer());
  if(rval.IsNull())
    {
    itkExceptionMacro(<< "downcast to type "
                      << this->GetNameOfClass()
                      << " failed.");
    }

  //
  // set fields not in the fixed parameters.
  rval->SetGaussianSmoothingVarianceForTheUpdateField
    (this->GetGaussianSmoothingVarianceForTheUpdateField());
  rval->SetGaussianSmoothingVarianceForTheTotalField
    (this->GetGaussianSmoothingVarianceForTheTotalField());

  rval->SetFixedParameters(this->GetFixedParameters());
  rval->SetParameters(this->GetParameters());

  return loPtr;
}

template<typename TParametersValueType, unsigned int NDimensions>
void
GaussianSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, NDimensions>::
PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  os << indent << "Gaussian smoothing parameters: " << std::endl
     << indent << "m_GaussianSmoothingVarianceForTheUpdateField: " << this->m_GaussianSmoothingVarianceForTheUpdateField
     << std::endl
     << indent << "m_GaussianSmoothingVarianceForTheTotalField: " << this->m_GaussianSmoothingVarianceForTheTotalField
     << std::endl;
}
} // namespace itk

#endif
