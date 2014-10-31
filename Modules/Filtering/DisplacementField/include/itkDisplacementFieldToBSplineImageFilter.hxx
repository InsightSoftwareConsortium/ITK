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
#ifndef __itkDisplacementFieldToBSplineImageFilter_hxx
#define __itkDisplacementFieldToBSplineImageFilter_hxx

#include "itkDisplacementFieldToBSplineImageFilter.h"

#include "itkContinuousIndex.h"
#include "itkDisplacementFieldToBSplineImageFilter.h"
#include "itkImportImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/*
 * DisplacementFieldToBSplineImageFilter class definitions
 */
template<typename TInputImage, typename TOutputImage>
DisplacementFieldToBSplineImageFilter<TInputImage, TOutputImage>
::DisplacementFieldToBSplineImageFilter() :
  m_EstimateInverse( false ),
  m_EnforceStationaryBoundary( true ),
  m_SplineOrder( 3 )
{
  this->SetNumberOfRequiredInputs( 1 );

  this->m_NumberOfFittingLevels.Fill( 1 );
  this->m_NumberOfControlPoints.Fill( 4 );

  this->m_DisplacementFieldControlPointLattice = ITK_NULLPTR;
}

template<typename TInputImage, typename TOutputImage>
DisplacementFieldToBSplineImageFilter<TInputImage, TOutputImage>
::~DisplacementFieldToBSplineImageFilter()
{
}

template<typename TInputImage, typename TOutputImage>
void
DisplacementFieldToBSplineImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  const InputFieldType * inputField = this->GetInput();
  const RealImageType * confidenceImage = this->GetConfidenceImage();

  typename InputFieldType::DirectionType identity;
  identity.SetIdentity();

  const typename InputFieldType::RegionType & bufferedRegion = inputField->GetBufferedRegion();
  const SizeValueType numberOfPixels = bufferedRegion.GetNumberOfPixels();
  const typename DisplacementFieldType::SizeType inputSize = bufferedRegion.GetSize();
  const typename DisplacementFieldType::IndexType startIndex = bufferedRegion.GetIndex();

  const bool filterHandlesMemory = false;

  typedef ImportImageFilter<VectorType, ImageDimension> ImporterType;
  typename ImporterType::Pointer importer = ImporterType::New();
  importer->SetImportPointer( const_cast<VectorType *>( inputField->GetBufferPointer() ), numberOfPixels, filterHandlesMemory );
  importer->SetRegion( inputField->GetBufferedRegion() );
  importer->SetOrigin( inputField->GetOrigin() );
  importer->SetSpacing( inputField->GetSpacing() );
  importer->SetDirection( identity );
  importer->Update();

  const typename ImporterType::OutputImageType * parametricInputField = importer->GetOutput();

  typename PointSetType::Pointer fieldPoints = PointSetType::New();
  fieldPoints->Initialize();

  typedef typename InputFieldType::PointType InputFieldPointType;
  typedef ContinuousIndex<typename InputFieldPointType::CoordRepType, ImageDimension> ContinuousIndexType;

  typename WeightsContainerType::Pointer weights = WeightsContainerType::New();

  IdentifierType numberOfPoints = NumericTraits< IdentifierType >::ZeroValue();

  const typename WeightsContainerType::Element boundaryWeight = 1.0e10;

  ImageRegionConstIteratorWithIndex<typename ImporterType::OutputImageType> It( parametricInputField, parametricInputField->GetBufferedRegion() );

  itkDebugMacro( "Extracting points from input displacement field. " )

  for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    typename DisplacementFieldType::IndexType index = It.GetIndex();

    bool isOnStationaryBoundary = false;
    if( this->m_EnforceStationaryBoundary )
      {
      for( unsigned int d = 0; d < ImageDimension; d++ )
        {
        if( index[d] == startIndex[d] || index[d] == startIndex[d] + static_cast<int>( inputSize[d] ) - 1 )
          {
          isOnStationaryBoundary = true;
          break;
          }
        }
      }

    if( confidenceImage && confidenceImage->GetPixel( index ) <= 0.0 && !isOnStationaryBoundary )
      {
      continue;
      }

    typename WeightsContainerType::Element weight = 1.0;

    if( confidenceImage && confidenceImage->GetPixel( index ) > 0.0 )
      {
      weight = static_cast<typename WeightsContainerType::Element>( confidenceImage->GetPixel( index ) );
      }

    VectorType data;
    typename PointSetType::PointType point;

    parametricInputField->TransformIndexToPhysicalPoint( index, point );

    bool isInside = true;

    if( isOnStationaryBoundary )
      {
      data.Fill( 0.0 );
      weight = boundaryWeight;
      }
    else
      {
      data = It.Get();

      if( this->m_EstimateInverse == true )
        {
        InputFieldPointType imagePoint;

        for( unsigned int d = 0; d < ImageDimension; d++ )
          {
          point[d] += data[d];
          imagePoint[d] = static_cast<typename InputFieldPointType::CoordRepType>( point[d] );
          }

        ContinuousIndexType cidx;
        isInside = parametricInputField->TransformPhysicalPointToContinuousIndex( imagePoint, cidx );

        if( isInside )
          {
          data *= -1.0;
          }
        }
      }

    if( isInside )
      {
      fieldPoints->SetPoint( numberOfPoints, point );
      fieldPoints->SetPointData( numberOfPoints, data );
      weights->InsertElement( numberOfPoints, weight );
      numberOfPoints++;
      }
    }

  itkDebugMacro( "Calculating the B-spline displacement field. " );

  typename OutputFieldType::PointType origin;
  typename OutputFieldType::SpacingType spacing;
  typename OutputFieldType::SizeType size;
  for ( unsigned int d = 0; d < ImageDimension; d++ )
    {
    origin[d] = parametricInputField->GetOrigin()[d];
    spacing[d] = parametricInputField->GetSpacing()[d];
    size[d] = parametricInputField->GetBufferedRegion().GetSize()[d];
    }

  ArrayType close;
  close.Fill( false );

  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetOrigin( origin );
  bspliner->SetSpacing( spacing );
  bspliner->SetSize( size );
  bspliner->SetDirection( inputField->GetDirection() );
  bspliner->SetNumberOfLevels( this->m_NumberOfFittingLevels );
  bspliner->SetSplineOrder( this->m_SplineOrder );
  bspliner->SetNumberOfControlPoints( this->m_NumberOfControlPoints );
  bspliner->SetCloseDimension( close );
  bspliner->SetInput( fieldPoints );
  bspliner->SetPointWeights( weights );
  bspliner->SetGenerateOutputImage( true );
  bspliner->Update();

  this->m_DisplacementFieldControlPointLattice = bspliner->GetPhiLattice();

  this->SetNthOutput( 0, bspliner->GetOutput() );
}

template<typename TInputImage, typename TOutputImage>
void
DisplacementFieldToBSplineImageFilter<TInputImage, TOutputImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Estimate inverse: ";
  if( this->m_EstimateInverse )
    {
    os << "true" << std::endl;
    }
  else
    {
    os << "false" << std::endl;
    }
  os << indent << "Enforce stationary boundary: ";
  if( this->m_EnforceStationaryBoundary )
    {
    os << "true" << std::endl;
    }
  else
    {
    os << "false" << std::endl;
    }
  os << indent << "Spline order: " << this->m_SplineOrder << std::endl;
  os << indent << "Number of fitting levels: "
     << this->m_NumberOfFittingLevels << std::endl;
  os << indent << "Number of control points: "
     << this->m_NumberOfControlPoints << std::endl;
}

}  //end namespace itk

#endif
