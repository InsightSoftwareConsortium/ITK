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
#ifndef __itkBSplineExponentialDiffeomorphicTransform_hxx
#define __itkBSplineExponentialDiffeomorphicTransform_hxx

#include "itkBSplineExponentialDiffeomorphicTransform.h"

#include "itkExponentialDisplacementFieldImageFilter.h"

namespace itk
{

/**
 * Constructor
 */
template<class TScalar, unsigned int NDimensions>
BSplineExponentialDiffeomorphicTransform<TScalar, NDimensions>
::BSplineExponentialDiffeomorphicTransform() :
  m_CalculateNumberOfIterationsAutomatically( true ),
  m_MaximumNumberOfIterations( 10 ),
  m_ComputeInverse( true )
{
}

/**
 * Destructor
 */
template<class TScalar, unsigned int NDimensions>
BSplineExponentialDiffeomorphicTransform<TScalar, NDimensions>::
~BSplineExponentialDiffeomorphicTransform()
{
}

template<class TScalar, unsigned int NDimensions>
void
BSplineExponentialDiffeomorphicTransform<TScalar, NDimensions>
::UpdateTransformParameters( const DerivativeType & update, ScalarType factor )
{
  DisplacementFieldPointer displacementField = this->GetDisplacementField();

  const typename DisplacementFieldType::RegionType & bufferedRegion = displacementField->GetBufferedRegion();
  const SizeValueType numberOfPixels = bufferedRegion.GetNumberOfPixels();

  typedef ImportImageFilter<DisplacementVectorType, Dimension> ImporterType;
  const bool importFilterWillReleaseMemory = false;

  // Temporarily set the direction cosine to identity since the B-spline
  // approximation algorithm works in parametric space and not physical
  // space.
  typename DisplacementFieldType::DirectionType identity;
  identity.SetIdentity();

  //
  // Smooth the update field
  //
  bool smoothUpdateField = true;
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    if( this->GetNumberOfControlPointsForTheUpdateField()[d] <= this->GetSplineOrder() )
      {
      itkDebugMacro( "Not smooothing the update field." );
      smoothUpdateField = false;
      break;
      }
    }

  DisplacementVectorType *updateFieldPointer = reinterpret_cast<DisplacementVectorType *>( const_cast<DerivativeType &>( update ).data_block() );

  typename ImporterType::Pointer importer = ImporterType::New();
  importer->SetImportPointer( updateFieldPointer, numberOfPixels, importFilterWillReleaseMemory );
  importer->SetRegion( displacementField->GetBufferedRegion() );
  importer->SetOrigin( displacementField->GetOrigin() );
  importer->SetSpacing( displacementField->GetSpacing() );
  importer->SetDirection( identity );

  ConstantVelocityFieldPointer updateField = importer->GetOutput();
  updateField->Update();
  updateField->DisconnectPipeline();

  if( smoothUpdateField )
    {
    itkDebugMacro( "Smooothing the update field." );

    // The update field is the velocity field but since it's constant
    // we smooth it using the parent class smoothing functionality

    ConstantVelocityFieldPointer updateSmoothField = this->BSplineSmoothDisplacementField( updateField, this->GetNumberOfControlPointsForTheUpdateField() );

    updateField = updateSmoothField;
    }

  typedef ExponentialDisplacementFieldImageFilter<ConstantVelocityFieldType, DisplacementFieldType> ExponentiatorType;
  typename ExponentiatorType::Pointer exponentiator = ExponentiatorType::New();
  exponentiator->SetInput( updateField );
  exponentiator->SetAutomaticNumberOfIterations( this->m_CalculateNumberOfIterationsAutomatically );
  exponentiator->SetMaximumNumberOfIterations( this->m_MaximumNumberOfIterations );
  exponentiator->SetComputeInverse( this->m_ComputeInverse );
  exponentiator->Update();

  DerivativeValueType *updatePointer = reinterpret_cast<DerivativeValueType *>( exponentiator->GetOutput()->GetBufferPointer() );

  // Add the update field to the current total field
  bool letArrayManageMemory = false;
  // Pass data pointer to required container. No copying is done.
  DerivativeType expUpdate( updatePointer, update.GetSize(), letArrayManageMemory );
  SuperSuperclass::UpdateTransformParameters( expUpdate, factor );

  //
  // Smooth the total field
  //
  bool smoothTotalField = true;
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    if( this->GetNumberOfControlPointsForTheTotalField()[d] <= this->GetSplineOrder() )
      {
      itkDebugMacro( "Not smooothing the total field." );
      smoothTotalField = false;
      break;
      }
    }
  if( smoothTotalField )
    {
    itkDebugMacro( "Smooothing the total field." );

    typename ImporterType::Pointer importer2 = ImporterType::New();
    importer2->SetImportPointer( displacementField->GetBufferPointer(), numberOfPixels, importFilterWillReleaseMemory );
    importer2->SetRegion( displacementField->GetBufferedRegion() );
    importer2->SetOrigin( displacementField->GetOrigin() );
    importer2->SetSpacing( displacementField->GetSpacing() );
    importer2->SetDirection( identity );

    DisplacementFieldPointer totalField = importer2->GetOutput();
    totalField->Update();
    totalField->DisconnectPipeline();

    DisplacementFieldPointer totalSmoothField = this->BSplineSmoothDisplacementField( totalField, this->GetNumberOfControlPointsForTheTotalField() );

    ImageAlgorithm::Copy<DisplacementFieldType, DisplacementFieldType>( totalSmoothField, totalField, totalSmoothField->GetBufferedRegion(), totalField->GetBufferedRegion() );
    }
}

/**
 * Standard "PrintSelf" method
 */
template<class TScalar, unsigned int NDimensions>
void
BSplineExponentialDiffeomorphicTransform<TScalar, NDimensions>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Calculate number of iterations automatically = " << this->m_CalculateNumberOfIterationsAutomatically << std::endl;
  os << indent << "Number of iterations = " << this->m_MaximumNumberOfIterations << std::endl;
  os << indent << "Compute inverse = " << this->m_ComputeInverse << std::endl;
}

} // namespace itk

#endif
