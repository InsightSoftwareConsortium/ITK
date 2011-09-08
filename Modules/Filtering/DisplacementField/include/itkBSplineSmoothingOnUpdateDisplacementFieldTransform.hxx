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
#ifndef __itkBSplineSmoothingOnUpdateDisplacementFieldTransform_hxx
#define __itkBSplineSmoothingOnUpdateDisplacementFieldTransform_hxx

#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransform.h"

#include "itkContinuousIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImportImageFilter.h"

namespace itk
{

/**
 * Constructor
 */
template<class TScalar, unsigned int NDimensions>
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::BSplineSmoothingOnUpdateDisplacementFieldTransform()
{
  this->m_SplineOrder = 3;
  this->m_NumberOfControlPointsForTheUpdateField.Fill( 4 );
  this->m_NumberOfControlPointsForTheTotalField.Fill( 0 );
  this->m_EnforceStationaryBoundary = true;
}

/**
 * Destructor
 */
template<class TScalar, unsigned int NDimensions>
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>::
~BSplineSmoothingOnUpdateDisplacementFieldTransform()
{
}

/**
 * set mesh size for update field
 */
template<class TScalar, unsigned int NDimensions>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
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
 * set mesh size for total field
 */
template<class TScalar, unsigned int NDimensions>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::SetMeshSizeForTheTotalField( const ArrayType &meshSize )
{
  ArrayType numberOfControlPoints;
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    numberOfControlPoints[d] = meshSize[d] + this->m_SplineOrder;
    }
  this->SetNumberOfControlPointsForTheTotalField( numberOfControlPoints );
}

template<class TScalar, unsigned int NDimensions>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::UpdateTransformParameters( DerivativeType &update, ScalarType factor )
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
    if( this->m_NumberOfControlPointsForTheUpdateField[d] <= this->m_SplineOrder )
      {
      itkDebugMacro( "Not smooothing the update field." );
      smoothUpdateField = false;
      break;
      }
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
    importer->SetDirection( identity );

    DisplacementFieldPointer updateField = importer->GetOutput();
    updateField->Update();
    updateField->DisconnectPipeline();

    DisplacementFieldPointer updateSmoothField = this->BSplineSmoothDisplacementField( updateField, this->m_NumberOfControlPointsForTheUpdateField );

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
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    if( this->m_NumberOfControlPointsForTheTotalField[d] <= this->m_SplineOrder )
      {
      itkDebugMacro( "Not smooothing the total field." );
      smoothTotalField = false;
      break;
      }
    }
  if( smoothTotalField )
    {
    itkDebugMacro( "Smooothing the total field." );

    typename ImporterType::Pointer importer = ImporterType::New();
    importer->SetImportPointer( displacementField->GetBufferPointer(), numberOfPixels, importFilterWillReleaseMemory );
    importer->SetRegion( displacementField->GetBufferedRegion() );
    importer->SetOrigin( displacementField->GetOrigin() );
    importer->SetSpacing( displacementField->GetSpacing() );
    importer->SetDirection( identity );

    DisplacementFieldPointer totalField = importer->GetOutput();
    totalField->Update();
    totalField->DisconnectPipeline();

    DisplacementFieldPointer totalSmoothField = this->BSplineSmoothDisplacementField( totalField, this->m_NumberOfControlPointsForTheTotalField );

    memcpy( displacementField->GetBufferPointer(), totalSmoothField->GetBufferPointer(), sizeof( DisplacementVectorType ) * numberOfPixels );
    }
}

/**
 * set displacement field and project it onto the space of b-spline transforms
 */
template<class TScalar, unsigned int NDimensions>
typename BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>::DisplacementFieldPointer
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::BSplineSmoothDisplacementField( const DisplacementFieldType * field, const ArrayType &numberOfControlPoints )
{
  const typename DisplacementFieldType::RegionType & bufferedRegion = field->GetBufferedRegion();
  const typename DisplacementFieldType::IndexType startIndex = bufferedRegion.GetIndex();
  const typename DisplacementFieldType::SizeType size = bufferedRegion.GetSize();

  typename PointSetType::Pointer fieldPoints = PointSetType::New();
  fieldPoints->Initialize();

  itkDebugMacro( "Extracting points from field. " )

  typename WeightsContainerType::Pointer weights = WeightsContainerType::New();

  IdentifierType numberOfPoints = NumericTraits< IdentifierType >::Zero;

  const typename WeightsContainerType::Element boundaryWeight = 1.0e10;

  ImageRegionConstIteratorWithIndex<DisplacementFieldType> It( field, field->GetBufferedRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    typename DisplacementFieldType::IndexType index = It.GetIndex();

    DisplacementVectorType data = It.Get();
    typename WeightsContainerType::Element weight = 1.0;

    if( this->m_EnforceStationaryBoundary )
      {
      bool isOnBoundary = false;
      for( unsigned int d = 0; d < Dimension; d++ )
        {
        if( index[d] == startIndex[d] || index[d] == startIndex[d] + static_cast<int>( size[d] ) - 1 )
          {
          isOnBoundary = true;
          break;
          }
        }
      if( isOnBoundary )
        {
        data.Fill( 0.0 );
        weight = boundaryWeight;
        }
      }

    typename PointSetType::PointType point;
    field->TransformIndexToPhysicalPoint( index, point );

    fieldPoints->SetPointData( numberOfPoints, data );
    fieldPoints->SetPoint( numberOfPoints, point );
    weights->InsertElement( numberOfPoints, weight );
    numberOfPoints++;
    }

  itkDebugMacro( "Calculating the B-spline field." );

  ArrayType close;
  close.Fill( false );
  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetOrigin( field->GetOrigin() );
  bspliner->SetSpacing( field->GetSpacing() );
  bspliner->SetSize( size );
  bspliner->SetDirection( field->GetDirection() );
  bspliner->SetNumberOfLevels( 1 );
  bspliner->SetSplineOrder( this->m_SplineOrder );
  bspliner->SetNumberOfControlPoints( numberOfControlPoints );
  bspliner->SetCloseDimension( close );
  bspliner->SetInput( fieldPoints );
  bspliner->SetPointWeights( weights );
  bspliner->SetGenerateOutputImage( true );

  DisplacementFieldPointer smoothField = bspliner->GetOutput();
  smoothField->Update();
  smoothField->DisconnectPipeline();

  return smoothField;
}

template <class TScalar, unsigned int NDimensions>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>::
PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  if( this->m_EnforceStationaryBoundary )
    {
    os << indent << "Enforce stationary boundary" << std::endl;
    }
  else
    {
    os << indent << "Does not enforce stationary boundary" << std::endl;
    }
  os << indent << "B-spline parameters: " << std::endl;
  os << indent << "  spline order = " << this->m_SplineOrder << std::endl;
  os << indent << "  number of control points for the update field = "
    << this->m_NumberOfControlPointsForTheUpdateField << std::endl;
  os << indent << "  number of control points for the total field = "
    << this->m_NumberOfControlPointsForTheTotalField << std::endl;
}
} // namespace itk

#endif
