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
#ifndef itkContourDirectedMeanDistanceImageFilter_hxx
#define itkContourDirectedMeanDistanceImageFilter_hxx

#include "itkContourDirectedMeanDistanceImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkOffset.h"
#include "itkImageRegionIterator.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkProgressReporter.h"
#include "itkMacro.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage1, typename TInputImage2 >
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::ContourDirectedMeanDistanceImageFilter():m_MeanDistance(1), m_Count(1)
{
  // this filter requires two input images
  this->SetNumberOfRequiredInputs(2);

  m_UseImageSpacing = true;
  m_DistanceMap = ITK_NULLPTR;
  m_ContourDirectedMeanDistance = NumericTraits< RealType >::ZeroValue();
}

template< typename TInputImage1, typename TInputImage2 >
void
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::SetInput1(const InputImage1Type *image)
{
  this->SetInput(image);
}

template< typename TInputImage1, typename TInputImage2 >
void
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::SetInput2(const TInputImage2 *image)
{
  this->SetNthInput( 1, const_cast< TInputImage2 * >( image ) );
}

template< typename TInputImage1, typename TInputImage2 >
const typename ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::InputImage1Type *
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::GetInput1(void)
{
  return this->GetInput();
}

template< typename TInputImage1, typename TInputImage2 >
const typename ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::InputImage2Type *
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::GetInput2()
{
  return itkDynamicCastInDebugMode< const TInputImage2 * >
    ( this->ProcessObject::GetInput(1) );
}

template< typename TInputImage1, typename TInputImage2 >
void
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  // this filter requires:
  // - the largeset possible region of the first image
  // - the corresponding region of the second image
  if ( this->GetInput1() )
    {
    InputImage1Pointer image1 =
      const_cast< InputImage1Type * >( this->GetInput1() );
    image1->SetRequestedRegionToLargestPossibleRegion();

    if ( this->GetInput2() )
      {
      InputImage2Pointer image2 =
        const_cast< InputImage2Type * >( this->GetInput2() );
      image2->SetRequestedRegion(
        this->GetInput1()->GetRequestedRegion() );
      }
    }
}

template< typename TInputImage1, typename TInputImage2 >
void
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage1, typename TInputImage2 >
void
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::AllocateOutputs()
{
  // Pass the first input through as the output
  InputImage1Pointer image =
    const_cast< TInputImage1 * >( this->GetInput1() );

  this->GraftOutput(image);
}

template< typename TInputImage1, typename TInputImage2 >
void
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::BeforeThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  // Resize the thread temporaries
  m_MeanDistance.SetSize(numberOfThreads);
  m_Count.SetSize(numberOfThreads);

  // Initialize the temporaries
  m_MeanDistance.Fill(NumericTraits< RealType >::ZeroValue());
  m_Count.Fill(0);

  // Compute Signed distance from non-zero pixels in the second image
  typedef SignedMaurerDistanceMapImageFilter< InputImage2Type, DistanceMapType >
  FilterType;

  typename FilterType::Pointer filter = FilterType::New();

  filter->SetInput( this->GetInput2() );
  filter->SetSquaredDistance(false);
  filter->SetUseImageSpacing(m_UseImageSpacing);
  filter->Update();

  m_DistanceMap = filter->GetOutput();
}

template< typename TInputImage1, typename TInputImage2 >
void
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::AfterThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  // find mean over all threads
  IdentifierType  count = 0;
  RealType        sum = NumericTraits< RealType >::ZeroValue();

  for ( ThreadIdType i = 0; i < numberOfThreads; i++ )
    {
    sum += m_MeanDistance[i];
    count += m_Count[i];
    }
  if ( count != 0 )
    {
    m_ContourDirectedMeanDistance = sum / static_cast< RealType >( count );
    }
  else
    {
    m_ContourDirectedMeanDistance = NumericTraits< RealType >::ZeroValue();
    }
}

template< typename TInputImage1, typename TInputImage2 >
void
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  ZeroFluxNeumannBoundaryCondition< InputImage1Type > nbc;

  ConstNeighborhoodIterator< InputImage1Type > bit;

  InputImage1ConstPointer input  = this->GetInput();

  // Find the data-set boundary "faces"
  SizeType radius;
  radius.Fill(1);

  typedef typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImage1Type >::FaceListType
    FaceListType;

  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImage1Type > bC;
  FaceListType faceList = bC(input, outputRegionForThread, radius);

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for ( typename FaceListType::iterator fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    ImageRegionConstIterator< DistanceMapType > it2 (m_DistanceMap, *fit);
    bit = ConstNeighborhoodIterator< InputImage1Type >(radius, input, *fit);
    unsigned int neighborhoodSize = bit.Size();

    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    while ( !bit.IsAtEnd() )
      {
      // first test
      // if current pixel is not on, let's continue
      if ( Math::NotExactlyEquals(bit.GetCenterPixel(), NumericTraits< InputImage1PixelType >::ZeroValue()) )
        {
        bool bIsOnContour = false;

        for ( unsigned int i = 0; i < neighborhoodSize; ++i )
          {
          // second test if at least one neighbour pixel is off
          // the center pixel belongs to contour
          if ( Math::ExactlyEquals(bit.GetPixel(i), NumericTraits< InputImage1PixelType >::ZeroValue()) )
            {
            bIsOnContour = true;
            break;
            }
          }

        // set pixel center pixel value whether it is or not on contour
        if ( bIsOnContour )
          {
          const RealType value = it2.Get();
          m_MeanDistance[threadId] += itk::Math::abs(value);
          m_Count[threadId]++;
          }
        }
      ++bit;
      ++it2;
      progress.CompletedPixel();
      }
    }
}

template< typename TInputImage1, typename TInputImage2 >
void
ContourDirectedMeanDistanceImageFilter< TInputImage1, TInputImage2 >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "UseImageSpacing: "
     << m_UseImageSpacing << std::endl;
  os << indent << "ContourDirectedMeanDistance: "
     << m_ContourDirectedMeanDistance << std::endl;
}
} // end namespace itk
#endif
