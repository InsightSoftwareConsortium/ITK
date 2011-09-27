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
#ifndef __itkGaussianSmoothingOnUpdateDisplacementFieldTransform_hxx
#define __itkGaussianSmoothingOnUpdateDisplacementFieldTransform_hxx

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"

#include "itkAddImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkImageDuplicator.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImportImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"

namespace itk
{

/**
 * Constructor
 */
template<class TScalar, unsigned int NDimensions>
GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::GaussianSmoothingOnUpdateDisplacementFieldTransform()
{
  this->m_GaussianSmoothingVarianceForTheUpdateField = 3.0;
  this->m_GaussianSmoothingVarianceForTheTotalField = 0.5;
}

/**
 * Destructor
 */
template<class TScalar, unsigned int NDimensions>
GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>::
~GaussianSmoothingOnUpdateDisplacementFieldTransform()
{
}

template<class TScalar, unsigned int NDimensions>
void
GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::UpdateTransformParameters( DerivativeType & update, ScalarType factor)
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
  if( smoothUpdateField )
    {
    itkDebugMacro( "Smooothing the update field." );

    DisplacementVectorType *updateFieldPointer = reinterpret_cast<DisplacementVectorType *>( update.data_block() );

    typename ImporterType::Pointer importer = ImporterType::New();
    importer->SetImportPointer( updateFieldPointer, numberOfPixels, importFilterWillReleaseMemory );
    importer->SetRegion( displacementField->GetBufferedRegion() );
    importer->SetOrigin( displacementField->GetOrigin() );
    importer->SetSpacing( displacementField->GetSpacing() );
    importer->SetDirection( displacementField->GetDirection() );

    DisplacementFieldPointer updateField = importer->GetOutput();
    updateField->Update();
    updateField->DisconnectPipeline();

    DisplacementFieldPointer updateSmoothField = this->GaussianSmoothDisplacementField( updateField, this->m_GaussianSmoothingVarianceForTheUpdateField );

    DerivativeValueType *updatePointer = reinterpret_cast<DerivativeValueType *>( updateSmoothField->GetBufferPointer() );

    memcpy( update.data_block(), updatePointer, sizeof( DisplacementVectorType ) * numberOfPixels );
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

    memcpy( displacementField->GetBufferPointer(), totalSmoothField->GetBufferPointer(), sizeof( DisplacementVectorType ) * numberOfPixels );
    }
}

template<class TScalar, unsigned int NDimensions>
typename GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>::DisplacementFieldPointer
GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::GaussianSmoothDisplacementField( DisplacementFieldType *inputField, ScalarType variance )
{
  typedef ImageDuplicator<DisplacementFieldType> DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( inputField );
  duplicator->Update();
  DisplacementFieldPointer outputField = duplicator->GetOutput();

  const typename DisplacementFieldType::RegionType & bufferedRegion = outputField->GetBufferedRegion();

  typedef VectorNeighborhoodOperatorImageFilter<DisplacementFieldType, DisplacementFieldType> SmootherType;
  typename SmootherType::Pointer smoother = SmootherType::New();

  typedef GaussianOperator<typename DisplacementVectorType::ValueType, NDimensions> GaussianType;
  GaussianType gaussian;
  gaussian.SetVariance( variance );
  gaussian.SetMaximumError( 0.001 );

  for( unsigned int d = 0; d < NDimensions; d++ )
    {
    gaussian.SetDirection( d );
    gaussian.SetMaximumKernelWidth( bufferedRegion.GetSize()[d] );
    gaussian.CreateDirectional();

    smoother->SetOperator( gaussian );
    smoother->SetInput( outputField );

    outputField = smoother->GetOutput();
    outputField->Update();
    outputField->DisconnectPipeline();
    }

  // Ensure zero motion on the boundary

  typename DisplacementVectorType::ValueType weight1 = 1.0;
  if( variance < 0.5 )
    {
    weight1 = 1.0 - 1.0 * ( variance / 0.5 );
    }
  typename DisplacementVectorType::ValueType weight2 = 1.0 - weight1;

  typedef MultiplyImageFilter<DisplacementFieldType, DisplacementFieldType, DisplacementFieldType> MultiplierType;

  typename MultiplierType::Pointer multiplier1 = MultiplierType::New();
  multiplier1->SetInput1( outputField );
  multiplier1->SetConstant2( weight1 );

  typename MultiplierType::Pointer multiplier2 = MultiplierType::New();
  multiplier2->SetInput1( inputField );
  multiplier2->SetConstant2( weight2 );

  typedef AddImageFilter<DisplacementFieldType, DisplacementFieldType, DisplacementFieldType> AdderType;
  typename AdderType::Pointer adder = AdderType::New();
  adder->SetInput1( multiplier1->GetOutput() );
  adder->SetInput2( multiplier2->GetOutput() );

  outputField = adder->GetOutput();
  outputField->Update();
  outputField->DisconnectPipeline();

  DisplacementVectorType zeroVector( 0.0 );

  ImageLinearIteratorWithIndex<DisplacementFieldType> It( outputField, outputField->GetRequestedRegion() );
  for( unsigned int d = 0; d < NDimensions; d++ )
    {
    It.SetDirection( d );
    It.GoToBegin();
    while( !It.IsAtEnd() )
      {
      It.GoToBeginOfLine();
      It.Set( zeroVector );
      It.GoToEndOfLine();
      --It;
      It.Set( zeroVector );

      It.NextLine();
      }
    }

  return outputField;
}

template <class TScalar, unsigned int NDimensions>
void
GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>::
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
