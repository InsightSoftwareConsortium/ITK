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
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>::
BSplineSmoothingOnUpdateDisplacementFieldTransform()
{
  this->m_SplineOrder = 3;
  this->m_NumberOfFittingLevelsPerDimension.Fill( 1 );
  this->m_NumberOfControlPoints.Fill( 4 );
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
 * set mesh size
 */
template<class TScalar, unsigned int NDimensions>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::SetMeshSize( const ArrayType &meshSize )
{
  ArrayType numberOfControlPoints;
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    numberOfControlPoints[d] = meshSize[d] + this->m_SplineOrder;
    }
  this->SetNumberOfControlPoints( numberOfControlPoints );
}

/**
 * set number of fitting levels
 */
template<class TScalar, unsigned int NDimensions>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::SetNumberOfFittingLevels( const ArrayValueType n )
{
  ArrayType nlevels;

  nlevels.Fill( n );
  this->SetNumberOfFittingLevelsPerDimension( nlevels );
}

template<class TScalar, unsigned int NDimensions>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::UpdateTransformParameters( DerivativeType &update, ScalarType factor )
{
  //This simply adds the values.
  //TODO: This should be multi-threaded probably, via image add filter.
  Superclass::UpdateTransformParameters( update, factor );

  //Now we smooth the result. Not thread safe. Does it's own
  // threading.
  this->BSplineSmoothDisplacementField();
}

/**
 * set displacement field and project it onto the space of b-spline transforms
 */
template<class TScalar, unsigned int NDimensions>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
::BSplineSmoothDisplacementField()
{
  typename PointSetType::Pointer fieldPoints = PointSetType::New();
  fieldPoints->Initialize();

  typename DisplacementFieldType::Pointer displacementField = this->GetDisplacementField();
  const typename DisplacementFieldType::RegionType & bufferedRegion = displacementField->GetBufferedRegion();
  const typename DisplacementFieldType::IndexType startIndex = bufferedRegion.GetIndex();
  const typename DisplacementFieldType::SizeType size = bufferedRegion.GetSize();
  const SizeValueType numberOfPixels = bufferedRegion.GetNumberOfPixels();

  itkDebugMacro( "Extracting points from update displacement field. " )

  // Temporarily set the direction cosine to identity since the B-spline
  // approximation algorithm works in parametric space and not physical
  // space.
  typename DisplacementFieldType::DirectionType identity;
  identity.SetIdentity();

  typedef ImportImageFilter<DisplacementVectorType, Dimension> ImporterType;
  typename ImporterType::Pointer importer = ImporterType::New();
  const bool importFilterWillReleaseMemory = false;
  importer->SetImportPointer( displacementField->GetBufferPointer(), numberOfPixels, importFilterWillReleaseMemory );
  importer->SetRegion( displacementField->GetBufferedRegion() );
  importer->SetOrigin( displacementField->GetOrigin() );
  importer->SetSpacing( displacementField->GetSpacing() );
  importer->SetDirection( identity );
  importer->Update();

  const DisplacementFieldType * canonicalField = importer->GetOutput();

  typename WeightsContainerType::Pointer weights = WeightsContainerType::New();

  IdentifierType numberOfPoints = NumericTraits< IdentifierType >::Zero;

  ImageRegionConstIteratorWithIndex<DisplacementFieldType> It( canonicalField, canonicalField->GetBufferedRegion() );
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
        weight = 1.0e10;
        }
      }

    typename PointSetType::PointType point;
    canonicalField->TransformIndexToPhysicalPoint( index, point );

    fieldPoints->SetPointData( numberOfPoints, data );
    fieldPoints->SetPoint( numberOfPoints, point );
    weights->InsertElement( numberOfPoints, weight );
    numberOfPoints++;
    }

  itkDebugMacro( "Calculating the B-spline displacement field." );

  ArrayType close;
  close.Fill( false );
  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetOrigin( displacementField->GetOrigin() );
  bspliner->SetSpacing( displacementField->GetSpacing() );
  bspliner->SetSize( displacementField->GetBufferedRegion().GetSize() );
  bspliner->SetDirection( displacementField->GetDirection() );
  bspliner->SetNumberOfLevels( this->m_NumberOfFittingLevelsPerDimension );
  bspliner->SetSplineOrder( this->m_SplineOrder );
  bspliner->SetNumberOfControlPoints( this->m_NumberOfControlPoints );
  bspliner->SetCloseDimension( close );
  bspliner->SetInput( fieldPoints );
  bspliner->SetPointWeights( weights );
  bspliner->SetGenerateOutputImage( true );
  bspliner->Update();

  const OutputImageType * smoothedField = bspliner->GetOutput();

  memcpy( displacementField->GetBufferPointer(), smoothedField->GetBufferPointer(),
    sizeof( DisplacementVectorType ) * numberOfPixels );
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
  os << indent << "  number of control points = " << this->m_NumberOfControlPoints << std::endl;
  os << indent << "  number of fitting levels per dimension = " << this->m_NumberOfFittingLevelsPerDimension << std::endl;
}
} // namespace itk

#endif
