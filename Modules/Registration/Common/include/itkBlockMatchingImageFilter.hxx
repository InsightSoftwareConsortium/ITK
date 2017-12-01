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
#ifndef itkBlockMatchingImageFilter_hxx
#define itkBlockMatchingImageFilter_hxx

#include "itkBlockMatchingImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include <limits>


namespace itk
{
template< typename TFixedImage, typename TMovingImage, typename TFeatures, typename TDisplacements, typename TSimilarities >
BlockMatchingImageFilter< TFixedImage, TMovingImage, TFeatures, TDisplacements, TSimilarities >
::BlockMatchingImageFilter()
{
  // defaults
  this->m_BlockRadius.Fill( 2 );
  this->m_SearchRadius.Fill( 3 );

  // make the outputs
  this->ProcessObject::SetNumberOfRequiredOutputs( 2 );
  typename DisplacementsType::Pointer displacements = static_cast< DisplacementsType * >( this->MakeOutput( 0 ).GetPointer() );
  this->SetNthOutput( 0, displacements.GetPointer() );
  typename SimilaritiesType::Pointer similarities = static_cast< SimilaritiesType * >( this->MakeOutput( 1 ).GetPointer() );
  this->SetNthOutput( 1, similarities.GetPointer() );

  // all inputs are required
  this->AddRequiredInputName( "FeaturePoints" );
  this->SetPrimaryInputName( "FeaturePoints" );
  this->AddRequiredInputName( "FixedImage" );
  this->AddRequiredInputName( "MovingImage" );
}

template< typename TFixedImage, typename TMovingImage, typename TFeatures, typename TDisplacements, typename TSimilarities >
BlockMatchingImageFilter< TFixedImage, TMovingImage, TFeatures, TDisplacements, TSimilarities >
::~BlockMatchingImageFilter()
{
}

template< typename TFixedImage, typename TMovingImage, typename TFeatures, typename TDisplacements, typename TSimilarities >
void
BlockMatchingImageFilter< TFixedImage, TMovingImage, TFeatures, TDisplacements, TSimilarities >
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Number of threads: " << this->GetNumberOfThreads() << std::endl
     << indent << "m_BlockRadius: " << m_BlockRadius << std::endl
     << indent << "m_SearchRadius: " << m_SearchRadius << std::endl;
}

template< typename TFixedImage, typename TMovingImage, typename TFeatures, typename TDisplacements, typename TSimilarities >
void
BlockMatchingImageFilter< TFixedImage, TMovingImage, TFeatures, TDisplacements, TSimilarities >
::GenerateOutputInformation()
{
  // We use the constructor defaults for all regions.
}

template< typename TFixedImage, typename TMovingImage, typename TFeatures, typename TDisplacements, typename TSimilarities >
void
BlockMatchingImageFilter< TFixedImage, TMovingImage, TFeatures, TDisplacements, TSimilarities >
::EnlargeOutputRequestedRegion(DataObject * output)
{
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TFixedImage, typename TMovingImage, typename TFeatures, typename TDisplacements, typename TSimilarities >
void
BlockMatchingImageFilter< TFixedImage, TMovingImage, TFeatures, TDisplacements, TSimilarities >
::GenerateData()
{
  // Call a method that can be overridden by a subclass to perform
  // some calculations prior to splitting the main computations into
  // separate threads
  this->BeforeThreadedGenerateData();

  // Set up the multithreaded processing
  ThreadStruct str;
  str.Filter = this;

  this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->ThreaderCallback, &str);

  // multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();

  // Call a method that can be overridden by a subclass to perform
  // some calculations after all the threads have completed
  this->AfterThreadedGenerateData();
}

template< typename TFixedImage, typename TMovingImage, typename TFeatures, typename TDisplacements, typename TSimilarities >
DataObject::Pointer
BlockMatchingImageFilter< TFixedImage, TMovingImage, TFeatures, TDisplacements, TSimilarities >
::MakeOutput( ProcessObject::DataObjectPointerArraySizeType idx )
{
  switch ( idx )
    {
    case 0:
      {
      return DisplacementsType::New().GetPointer();
      }
      break;

    case 1:
      {
      return SimilaritiesType::New().GetPointer();
      }
      break;
    }
  itkExceptionMacro(<< "Bad output index " << idx );
  return ITK_NULLPTR;
}


template< typename TFixedImage, typename TMovingImage, typename TFeatures, typename TDisplacements, typename TSimilarities >
void
BlockMatchingImageFilter< TFixedImage, TMovingImage, TFeatures, TDisplacements, TSimilarities >
::BeforeThreadedGenerateData()
{
  this->m_PointsCount = itk::NumericTraits< SizeValueType >::ZeroValue();
  FeaturePointsConstPointer featurePoints = this->GetFeaturePoints();
  if ( featurePoints )
    {
    this->m_PointsCount = featurePoints->GetNumberOfPoints();
    }

  if ( this->m_PointsCount < 1 )
    {
    itkExceptionMacro( "Invalid number of feature points: " << this->m_PointsCount << "." );
    }

  this->m_DisplacementsVectorsArray = new DisplacementsVector[ this->m_PointsCount ];
  this->m_SimilaritiesValuesArray = new SimilaritiesValue[ this->m_PointsCount ];
}

template< typename TFixedImage, typename TMovingImage, typename TFeatures, typename TDisplacements, typename TSimilarities >
void
BlockMatchingImageFilter< TFixedImage, TMovingImage, TFeatures, TDisplacements, TSimilarities >
::AfterThreadedGenerateData()
{
  FeaturePointsConstPointer featurePoints = this->GetFeaturePoints();
  const typename FeaturePointsType::PointsContainer *points;
  if ( featurePoints )
    {
    points = featurePoints->GetPoints();

    DisplacementsPointer displacements = this->GetDisplacements();

    typedef typename DisplacementsType::PointsContainerPointer  DisplacementsPointsContainerPointerType;
    typedef typename DisplacementsType::PointsContainer         DisplacementsPointsContainerType;
    DisplacementsPointsContainerPointerType displacementsPoints = DisplacementsPointsContainerType::New();

    typedef typename DisplacementsType::PointDataContainerPointer  DisplacementsPointDataContainerPointerType;
    typedef typename DisplacementsType::PointDataContainer         DisplacementsPointDataContainerType;
    DisplacementsPointDataContainerPointerType displacementsData = DisplacementsPointDataContainerType::New();

    SimilaritiesPointer similarities = this->GetSimilarities();

    typedef typename SimilaritiesType::PointsContainerPointer  SimilaritiesPointsContainerPointerType;
    typedef typename SimilaritiesType::PointsContainer         SimilaritiesPointsContainerType;
    SimilaritiesPointsContainerPointerType similaritiesPoints = SimilaritiesPointsContainerType::New();

    typedef typename SimilaritiesType::PointDataContainerPointer  SimilaritiesPointDataContainerPointerType;
    typedef typename SimilaritiesType::PointDataContainer         SimilaritiesPointDataContainerType;
    SimilaritiesPointDataContainerPointerType similaritiesData = SimilaritiesPointDataContainerType::New();

    // insert displacements and similarities
    for ( SizeValueType i = 0; i < this->m_PointsCount; i++ )
      {
      displacementsPoints->InsertElement( i, points->GetElement( i ) );
      similaritiesPoints->InsertElement( i, points->GetElement( i ) );
      displacementsData->InsertElement( i, this->m_DisplacementsVectorsArray[ i ] );
      similaritiesData->InsertElement( i, this->m_SimilaritiesValuesArray[ i ] );
      }

    displacements->SetPoints( displacementsPoints );
    displacements->SetPointData( displacementsData );
    similarities->SetPoints( similaritiesPoints );
    similarities->SetPointData( similaritiesData );
    }

  // clean up
  delete[] m_DisplacementsVectorsArray;
  delete[] m_SimilaritiesValuesArray;
}

// Callback routine used by the threading library. This routine just calls
// the ThreadedGenerateData method after setting the correct region for this
// thread.
template< typename TFixedImage, typename TMovingImage, typename TFeatures, typename TDisplacements, typename TSimilarities >
ITK_THREAD_RETURN_TYPE
BlockMatchingImageFilter< TFixedImage, TMovingImage, TFeatures, TDisplacements, TSimilarities >
::ThreaderCallback(void *arg)
{
  ThreadStruct *str = (ThreadStruct *)( ( (MultiThreader::ThreadInfoStruct *)( arg ) )->UserData );
  ThreadIdType threadId = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->ThreadID;

  str->Filter->ThreadedGenerateData( threadId );

  return ITK_THREAD_RETURN_VALUE;
}

template< typename TFixedImage, typename TMovingImage, typename TFeatures, typename TDisplacements, typename TSimilarities >
void
BlockMatchingImageFilter< TFixedImage, TMovingImage, TFeatures, TDisplacements, TSimilarities >
::ThreadedGenerateData( ThreadIdType threadId )
{
  FixedImageConstPointer fixedImage = this->GetFixedImage();
  MovingImageConstPointer movingImage = this->GetMovingImage();
  FeaturePointsConstPointer featurePoints = this->GetFeaturePoints();

  SizeValueType threadCount = this->GetNumberOfThreads();

  // compute first point and number of points (count) for this thread
  SizeValueType count = m_PointsCount / threadCount;
  SizeValueType first = threadId * count;
  if ( threadId + 1 == threadCount ) // last thread
    {
    count += this->m_PointsCount % threadCount;
    }

  // start constructing window region and center region (single voxel)
  ImageRegionType window;
  ImageRegionType center;
  ImageSizeType windowSize;
  windowSize.Fill( 1 );
  center.SetSize( windowSize ); // size of center region is 1
  windowSize += m_SearchRadius + m_SearchRadius;
  window.SetSize( windowSize ); // size of window region is 1+2*m_BlockHalfWindow

  // start constructing block iterator
  SizeValueType numberOfVoxelInBlock = 1;
  for ( unsigned i = 0; i < ImageSizeType::Dimension; i++ )
    {
    numberOfVoxelInBlock *= m_BlockRadius[ i ] + 1 + m_BlockRadius[ i ];
    }

  // loop thru feature points
  for ( SizeValueType idx = first, last = first + count; idx < last; idx++ )
    {
    FeaturePointsPhysicalCoordinates originalLocation = featurePoints->GetPoint( idx );
    ImageIndexType fixedIndex;
    fixedImage->TransformPhysicalPointToIndex(    originalLocation, fixedIndex );
    ImageIndexType movingIndex;
    movingImage->TransformPhysicalPointToIndex( originalLocation, movingIndex );

    // the block is selected for a minimum similarity metric
    SimilaritiesValue  similarity = NumericTraits< SimilaritiesValue >::ZeroValue();

    // New point location
    DisplacementsVector displacement;

    // set centers of window and center regions to current location
    ImageIndexType start = fixedIndex - this->m_SearchRadius;
    window.SetIndex( start );
    center.SetIndex( movingIndex );

    // iterate over neighborhoods in region window, for each neighborhood: iterate over voxels in blockRadius
    ConstNeighborhoodIterator< FixedImageType > windowIterator( m_BlockRadius, fixedImage, window );

    // iterate over voxels in neighborhood of current feature point
    ConstNeighborhoodIterator< MovingImageType > centerIterator( m_BlockRadius, movingImage, center );
    centerIterator.GoToBegin();

    // iterate over neighborhoods in region window
    for ( windowIterator.GoToBegin(); !windowIterator.IsAtEnd(); ++windowIterator )
      {
      SimilaritiesValue fixedSum = NumericTraits< SimilaritiesValue >::ZeroValue();
      SimilaritiesValue fixedSumOfSquares = NumericTraits< SimilaritiesValue >::ZeroValue();
      SimilaritiesValue movingSum = NumericTraits< SimilaritiesValue >::ZeroValue();
      SimilaritiesValue movingSumOfSquares = NumericTraits< SimilaritiesValue >::ZeroValue();
      SimilaritiesValue covariance = NumericTraits< SimilaritiesValue >::ZeroValue();

      // iterate over voxels in blockRadius
      for ( SizeValueType i = 0; i < numberOfVoxelInBlock; i++ ) // windowIterator.Size() == numberOfVoxelInBlock
        {
        const SimilaritiesValue fixedValue = windowIterator.GetPixel( i );
        const SimilaritiesValue movingValue = centerIterator.GetPixel( i );
        movingSum += movingValue;
        fixedSum += fixedValue;
        movingSumOfSquares += movingValue * movingValue;
        fixedSumOfSquares += fixedValue * fixedValue;
        covariance += fixedValue * movingValue;
        }
      const SimilaritiesValue fixedMean = fixedSum / numberOfVoxelInBlock;
      const SimilaritiesValue movingMean = movingSum / numberOfVoxelInBlock;
      const SimilaritiesValue fixedVariance = fixedSumOfSquares - numberOfVoxelInBlock * fixedMean * fixedMean;
      const SimilaritiesValue movingVariance = movingSumOfSquares - numberOfVoxelInBlock * movingMean * movingMean;
      covariance -= numberOfVoxelInBlock * fixedMean * movingMean;

      SimilaritiesValue sim = NumericTraits< SimilaritiesValue >::ZeroValue();
      if ( fixedVariance * movingVariance )
        {
        sim = ( covariance * covariance ) / ( fixedVariance * movingVariance );
        }

      if ( sim >= similarity )
        {
        FeaturePointsPhysicalCoordinates newLocation;
        fixedImage->TransformIndexToPhysicalPoint( windowIterator.GetIndex(), newLocation );
        displacement = newLocation - originalLocation;
        similarity = sim;
        }
      }
    this->m_DisplacementsVectorsArray[ idx ] = displacement;
    this->m_SimilaritiesValuesArray[ idx ] = similarity;
    }
}

} // end namespace itk

#endif
