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
#ifndef itkDisplacementFieldToBSplineImageFilter_hxx
#define itkDisplacementFieldToBSplineImageFilter_hxx

#include "itkDisplacementFieldToBSplineImageFilter.h"

#include "itkContinuousIndex.h"
#include "itkImportImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/*
 * DisplacementFieldToBSplineImageFilter class definitions
 */
template<typename TInputImage, typename TInputPointSet, typename TOutputImage>
DisplacementFieldToBSplineImageFilter<TInputImage, TInputPointSet, TOutputImage>
::DisplacementFieldToBSplineImageFilter() :
  m_EstimateInverse( false ),
  m_EnforceStationaryBoundary( true ),
  m_SplineOrder( 3 ),
  m_UsePointWeights( false ),
  m_BSplineDomainIsDefined( true ),
  m_UseInputFieldToDefineTheBSplineDomain( false )
{
  this->SetNumberOfRequiredInputs( 0 );

  this->m_NumberOfFittingLevels.Fill( 1 );
  this->m_NumberOfControlPoints.Fill( 4 );

  this->m_PointWeights = ITK_NULLPTR;

  this->m_BSplineDomainOrigin.Fill( 0.0 );
  this->m_BSplineDomainSpacing.Fill( 1.0 );
  this->m_BSplineDomainSize.Fill( 0 );
  this->m_BSplineDomainDirection.SetIdentity();
}

template<typename TInputImage, typename TInputPointSet, typename TOutputImage>
DisplacementFieldToBSplineImageFilter<TInputImage, TInputPointSet, TOutputImage>
::~DisplacementFieldToBSplineImageFilter()
{
}

template<typename TInputImage, typename TInputPointSet, typename TOutputImage>
void
DisplacementFieldToBSplineImageFilter<TInputImage, TInputPointSet, TOutputImage>
::SetBSplineDomain( OriginType origin, SpacingType spacing, SizeType size, DirectionType direction )
{
  if( this->m_BSplineDomainOrigin != origin ||
      this->m_BSplineDomainSpacing != spacing ||
      this->m_BSplineDomainSize != size ||
      this->m_BSplineDomainDirection != direction )
    {
    this->m_BSplineDomainOrigin = origin;
    this->m_BSplineDomainSpacing = spacing;
    this->m_BSplineDomainSize = size;
    this->m_BSplineDomainDirection = direction;

    this->m_BSplineDomainIsDefined = true;
    this->m_UseInputFieldToDefineTheBSplineDomain = false;
    this->Modified();
   }
}

template<typename TInputImage, typename TInputPointSet, typename TOutputImage>
void
DisplacementFieldToBSplineImageFilter<TInputImage, TInputPointSet, TOutputImage>
::SetBSplineDomainFromImage( RealImageType *image )
{
  this->SetBSplineDomain( image->GetOrigin(), image->GetSpacing(),
    image->GetRequestedRegion().GetSize(), image->GetDirection() );
}

template<typename TInputImage, typename TInputPointSet, typename TOutputImage>
void
DisplacementFieldToBSplineImageFilter<TInputImage, TInputPointSet, TOutputImage>
::SetBSplineDomainFromImage( InputFieldType *field )
{
  this->SetBSplineDomain( field->GetOrigin(), field->GetSpacing(),
    field->GetRequestedRegion().GetSize(), field->GetDirection() );
}

template<typename TInputImage, typename TInputPointSet, typename TOutputImage>
void
DisplacementFieldToBSplineImageFilter<TInputImage, TInputPointSet, TOutputImage>
::SetPointSetConfidenceWeights( WeightsContainerType *weights )
{
  this->m_PointWeights = weights;
  this->m_UsePointWeights = true;
  this->Modified();
}

template<typename TInputImage, typename TInputPointSet, typename TOutputImage>
void
DisplacementFieldToBSplineImageFilter<TInputImage, TInputPointSet, TOutputImage>
::GenerateData()
{
  const InputFieldType * inputField = this->GetInput();

  if( inputField && this->m_UseInputFieldToDefineTheBSplineDomain )
    {
    this->m_BSplineDomainOrigin = inputField->GetOrigin();
    this->m_BSplineDomainSpacing = inputField->GetSpacing();
    this->m_BSplineDomainSize = inputField->GetRequestedRegion().GetSize();
    this->m_BSplineDomainDirection = inputField->GetDirection();

    this->m_BSplineDomainIsDefined = true;
    }

  const RealImageType * confidenceImage = this->GetConfidenceImage();

  const InputPointSetType * inputPointSet = this->GetPointSet();
  if( inputPointSet && this->m_UsePointWeights && ( this->m_PointWeights->Size() != inputPointSet->GetNumberOfPoints() ) )
    {
    itkExceptionMacro( "The number of input points does not match the number of weight elements." );
    }

  typename InputPointSetType::Pointer fieldPoints = InputPointSetType::New();
  fieldPoints->Initialize();

  typename WeightsContainerType::Pointer weights = WeightsContainerType::New();

  IdentifierType numberOfPoints = NumericTraits< IdentifierType >::ZeroValue();

  const typename WeightsContainerType::Element boundaryWeight = 1.0e10;

  if( this->m_BSplineDomainIsDefined == false )
    {
    itkExceptionMacro( "Output (B-spline) domain is undefined." )
    }

  typedef ContinuousIndex<typename InputFieldPointType::CoordRepType, ImageDimension> ContinuousIndexType;

  // Create an output field based on the b-spline domain to determine boundary
  // points and whether or not specified points are inside or outside the domain.

  typename InputFieldType::DirectionType identity;
  identity.SetIdentity();

  typename OutputFieldType::Pointer bsplinePhysicalDomainField = OutputFieldType::New();
  bsplinePhysicalDomainField->SetOrigin( this->m_BSplineDomainOrigin );
  bsplinePhysicalDomainField->SetSpacing( this->m_BSplineDomainSpacing );
  bsplinePhysicalDomainField->SetRegions( this->m_BSplineDomainSize );
  bsplinePhysicalDomainField->SetDirection( this->m_BSplineDomainDirection );
  // bsplinePhysicalDomainField->Allocate();

  typename OutputFieldType::Pointer bsplineParametricDomainField = OutputFieldType::New();
  bsplineParametricDomainField->SetOrigin( this->m_BSplineDomainOrigin );
  bsplineParametricDomainField->SetSpacing( this->m_BSplineDomainSpacing );
  bsplineParametricDomainField->SetRegions( this->m_BSplineDomainSize );
  bsplineParametricDomainField->SetDirection( identity );
  bsplineParametricDomainField->Allocate();

  typename OutputFieldType::IndexType startIndex = bsplineParametricDomainField->GetBufferedRegion().GetIndex();

  // Add the boundary points here if we have a b-spline domain not defined by the
  // input field.  This more general case doesn't take advantage of the speed up
  // within the B-spline scattered data fitting filter when consecutive points
  // are arranged consecutively on the grid.

  if( this->m_EnforceStationaryBoundary && ! this->m_UseInputFieldToDefineTheBSplineDomain )
    {
    ImageRegionConstIteratorWithIndex<OutputFieldType> ItB(
      bsplineParametricDomainField, bsplineParametricDomainField->GetBufferedRegion() );

    for ( ItB.GoToBegin(); !ItB.IsAtEnd(); ++ItB )
      {
      typename OutputFieldType::IndexType index = ItB.GetIndex();

      bool isOnStationaryBoundary = false;
      for( unsigned int d = 0; d < ImageDimension; d++ )
        {
        if( index[d] == startIndex[d] || index[d] == startIndex[d] + static_cast<int>( this->m_BSplineDomainSize[d] ) - 1 )
          {
          isOnStationaryBoundary = true;
          break;
          }
        }
      if( isOnStationaryBoundary )
        {
        VectorType data( 0.0 );
        typename InputPointSetType::PointType point;

        bsplineParametricDomainField->TransformIndexToPhysicalPoint( index, point );

        fieldPoints->SetPoint( numberOfPoints, point );
        fieldPoints->SetPointData( numberOfPoints, data );
        weights->InsertElement( numberOfPoints, boundaryWeight );
        numberOfPoints++;
        }
      }
    }

  if( inputField )
    {
    itkDebugMacro( "Gathering information from the input displacement field. " );

    ImageRegionConstIteratorWithIndex<InputFieldType> It( inputField, inputField->GetBufferedRegion() );

    itkDebugMacro( "Extracting points from input displacement field." )

    for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      typename DisplacementFieldType::IndexType index = It.GetIndex();

      bool isOnStationaryBoundary = false;
      if( this->m_EnforceStationaryBoundary && this->m_UseInputFieldToDefineTheBSplineDomain )
        {
        for( unsigned int d = 0; d < ImageDimension; d++ )
          {
          if( index[d] == startIndex[d] || index[d] == startIndex[d] + static_cast<int>( this->m_BSplineDomainSize[d] ) - 1 )
            {
            isOnStationaryBoundary = true;
            break;
            }
          }
        }

      if( confidenceImage && confidenceImage->GetPixel( index ) <= 0.0 && ! isOnStationaryBoundary )
        {
        continue;
        }

      typename WeightsContainerType::Element weight = 1.0;
      if( confidenceImage && confidenceImage->GetPixel( index ) > 0.0 )
        {
        weight = static_cast<typename WeightsContainerType::Element>( confidenceImage->GetPixel( index ) );
        }

      ContinuousIndexType cidx;
      PointType parametricPoint;
      typename InputPointSetType::PointType physicalPoint;

      inputField->TransformIndexToPhysicalPoint( index, physicalPoint );
      bsplinePhysicalDomainField->TransformPhysicalPointToContinuousIndex( physicalPoint, cidx );
      bsplineParametricDomainField->TransformContinuousIndexToPhysicalPoint( cidx, parametricPoint );

      bool isInside = true;

      VectorType data = It.Get();

      if( isOnStationaryBoundary )
        {
        data.Fill( 0.0 );
        weight = boundaryWeight;
        }
      else if( this->m_EstimateInverse || ! this->m_UseInputFieldToDefineTheBSplineDomain )
        {
        if( this->m_EstimateInverse )
          {
          for( unsigned int d = 0; d < ImageDimension; d++ )
            {
            physicalPoint[d] += data[d];
            }
          data *= -1.0;
          }

        InputFieldPointType imagePoint;
        imagePoint.CastFrom( physicalPoint );

        ContinuousIndexType cidx2;
        isInside = bsplinePhysicalDomainField->TransformPhysicalPointToContinuousIndex( imagePoint, cidx2 );
        if( isInside )
          {
          bsplineParametricDomainField->TransformContinuousIndexToPhysicalPoint( cidx2, parametricPoint );
          }
        }

      if( isInside )
        {
        fieldPoints->SetPoint( numberOfPoints, parametricPoint );
        fieldPoints->SetPointData( numberOfPoints, data );
        weights->InsertElement( numberOfPoints, weight );
        numberOfPoints++;
        }
      }
    }

  if( inputPointSet )
    {
    itkDebugMacro( "Gathering information from the input point set. " );

    typename PointsContainerType::ConstIterator ItP = inputPointSet->GetPoints()->Begin();
    typename PointDataContainerType::ConstIterator ItD = inputPointSet->GetPointData()->Begin();

    while( ItP != inputPointSet->GetPoints()->End() )
      {

      PointType parametricPoint;

      PointType physicalPoint = ItP.Value();
      VectorType data = ItD.Value();

      typename WeightsContainerType::Element weight = 1.0;
      if( this->m_UsePointWeights )
        {
        weight = this->m_PointWeights->GetElement( ItP.Index() );
        }

      bool isInside = true;

      if( this->m_EstimateInverse )
        {
        for( unsigned int d = 0; d < ImageDimension; d++ )
          {
          physicalPoint[d] += data[d];
          }
        data *= -1.0;
        }

      InputFieldPointType imagePoint;
      imagePoint.CastFrom( physicalPoint );

      ContinuousIndexType cidx;
      isInside = bsplinePhysicalDomainField->TransformPhysicalPointToContinuousIndex( imagePoint, cidx );

      if( isInside && this->m_EnforceStationaryBoundary )
        {
        // If we enforce the stationary and the point is on the boundary (or really close
        // to the boundary), we can ignore it.
        for( unsigned int d = 0; d < ImageDimension; d++ )
          {
          if( cidx[d] < static_cast<typename ContinuousIndexType::CoordRepType>( startIndex[d] ) + 0.5 ||
              cidx[d] > static_cast<typename ContinuousIndexType::CoordRepType>( startIndex[d] + static_cast<int>( this->m_BSplineDomainSize[d] ) - 1 ) - 0.5 )
            {
            isInside = false;
            break;
            }
          }
        }

      if( isInside )
        {
        bsplineParametricDomainField->TransformContinuousIndexToPhysicalPoint( cidx, parametricPoint );

        fieldPoints->SetPoint( numberOfPoints, parametricPoint );
        fieldPoints->SetPointData( numberOfPoints, data );
        weights->InsertElement( numberOfPoints, weight );
        numberOfPoints++;
        }

      ++ItP;
      ++ItD;
      }
    }

  if( numberOfPoints == 0 )
    {
    itkExceptionMacro( "No points were found.  Check that one or both inputs (displacement field/point set) are set." );
    }

  itkDebugMacro( "Calculating the B-spline displacement field. " );

  ArrayType close;
  close.Fill( false );

  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetOrigin( this->m_BSplineDomainOrigin );
  bspliner->SetSpacing( this->m_BSplineDomainSpacing );
  bspliner->SetSize( this->m_BSplineDomainSize );
  bspliner->SetDirection( this->m_BSplineDomainDirection );
  bspliner->SetNumberOfLevels( this->m_NumberOfFittingLevels );
  bspliner->SetSplineOrder( this->m_SplineOrder );
  bspliner->SetNumberOfControlPoints( this->m_NumberOfControlPoints );
  bspliner->SetCloseDimension( close );
  bspliner->SetInput( fieldPoints );
  bspliner->SetPointWeights( weights );
  bspliner->SetGenerateOutputImage( true );
  bspliner->Update();

  this->SetNthOutput( 0, bspliner->GetOutput() );
  this->SetNthOutput( 1, bspliner->GetPhiLattice() );
}

template<typename TInputImage, typename TInputPointSet, typename TOutputImage>
void
DisplacementFieldToBSplineImageFilter<TInputImage, TInputPointSet, TOutputImage>
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

  os << indent << "B-spline domain" << std::endl;
  os << indent << "  Origin: " << this->m_BSplineDomainOrigin << std::endl;
  os << indent << "  Spacing: " << this->m_BSplineDomainSpacing << std::endl;
  os << indent << "  Size: " << this->m_BSplineDomainSize << std::endl;
  os << indent << "  Direction: " << this->m_BSplineDomainDirection << std::endl;
}

}  //end namespace itk

#endif
