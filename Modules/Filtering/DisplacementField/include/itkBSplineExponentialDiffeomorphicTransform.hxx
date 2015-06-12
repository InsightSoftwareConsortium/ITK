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
#ifndef itkBSplineExponentialDiffeomorphicTransform_hxx
#define itkBSplineExponentialDiffeomorphicTransform_hxx

#include "itkBSplineExponentialDiffeomorphicTransform.h"

#include "itkAddImageFilter.h"
#include "itkImageDuplicator.h"
#include "itkImportImageFilter.h"
#include "itkMultiplyImageFilter.h"

namespace itk
{

/**
 * Constructor
 */
template<typename TParametersValueType, unsigned int NDimensions>
BSplineExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>
::BSplineExponentialDiffeomorphicTransform() :
  m_SplineOrder( 3 )
{
  this->m_NumberOfControlPointsForTheConstantVelocityField.Fill( 4 );
  this->m_NumberOfControlPointsForTheUpdateField.Fill( 4 );
}

/**
 * Destructor
 */
template<typename TParametersValueType, unsigned int NDimensions>
BSplineExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>::
~BSplineExponentialDiffeomorphicTransform()
{
}

/**
 * set mesh size for update field
 */
template<typename TParametersValueType, unsigned int NDimensions>
void
BSplineExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>
::SetMeshSizeForTheUpdateField( const ArrayType &meshSize )
{
  ArrayType numberOfControlPoints;
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    numberOfControlPoints[d] = meshSize[d] + this->m_SplineOrder;
    }
  this->SetNumberOfControlPointsForTheUpdateField( numberOfControlPoints );
}

/**
 * set mesh size for update field
 */
template<typename TParametersValueType, unsigned int NDimensions>
void
BSplineExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>
::SetMeshSizeForTheConstantVelocityField( const ArrayType &meshSize )
{
  ArrayType numberOfControlPoints;
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    numberOfControlPoints[d] = meshSize[d] + this->m_SplineOrder;
    }
  this->SetNumberOfControlPointsForTheConstantVelocityField( numberOfControlPoints );
}

template<typename TParametersValueType, unsigned int NDimensions>
void
BSplineExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>
::UpdateTransformParameters( const DerivativeType & update, ScalarType factor )
{
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
      this->BSplineSmoothConstantVelocityField( updateField, this->GetNumberOfControlPointsForTheUpdateField() );

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
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    if( this->GetNumberOfControlPointsForTheConstantVelocityField()[d] <= this->GetSplineOrder() )
      {
      itkDebugMacro( "Not smoothing the velocity field." );
      smoothVelocityField = false;
      break;
      }
    }

  if( smoothVelocityField )
    {
    itkDebugMacro( "Smoothing the velocity field." );

    ConstantVelocityFieldPointer velocitySmoothField =
      this->BSplineSmoothConstantVelocityField( updatedVelocityField, this->GetNumberOfControlPointsForTheConstantVelocityField() );

    this->SetConstantVelocityField( velocitySmoothField );
    }
  else
    {
    this->SetConstantVelocityField( updatedVelocityField );
    }

  this->IntegrateVelocityField();
}

template<typename TParametersValueType, unsigned int NDimensions>
typename BSplineExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>::ConstantVelocityFieldPointer
BSplineExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>
::BSplineSmoothConstantVelocityField( const ConstantVelocityFieldType * field, const ArrayType &numberOfControlPoints )
{
  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetUseInputFieldToDefineTheBSplineDomain( true );
  bspliner->SetDisplacementField( field );
  bspliner->SetNumberOfControlPoints( numberOfControlPoints );
  bspliner->SetSplineOrder( this->m_SplineOrder );
  bspliner->SetNumberOfFittingLevels( 1 );
  bspliner->SetEnforceStationaryBoundary( true );
  bspliner->SetEstimateInverse( false );
  bspliner->Update();

  ConstantVelocityFieldPointer smoothField = bspliner->GetOutput();

  return smoothField;
}

/**
 * Standard "PrintSelf" method
 */
template<typename TParametersValueType, unsigned int NDimensions>
void
BSplineExponentialDiffeomorphicTransform<TParametersValueType, NDimensions>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Spline order = " << this->m_SplineOrder << std::endl;
  os << indent << "Number of control points for the velocity field = "
    << this->m_NumberOfControlPointsForTheConstantVelocityField << std::endl;
  os << indent << "Number of control points for the update field = "
    << this->m_NumberOfControlPointsForTheUpdateField << std::endl;
}

} // namespace itk

#endif
