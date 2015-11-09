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
#ifndef itkDirectedHausdorffDistanceImageFilter_hxx
#define itkDirectedHausdorffDistanceImageFilter_hxx

#include "itkDirectedHausdorffDistanceImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkProgressReporter.h"
#include "itkMacro.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage1, typename TInputImage2 >
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::DirectedHausdorffDistanceImageFilter():m_MaxDistance(1)
{
  // this filter requires two input images
  this->SetNumberOfRequiredInputs(2);

  m_DistanceMap = ITK_NULLPTR;
  m_DirectedHausdorffDistance = NumericTraits< RealType >::ZeroValue();
  m_AverageHausdorffDistance = NumericTraits< RealType >::ZeroValue();
  m_UseImageSpacing     = true;
}

template< typename TInputImage1, typename TInputImage2 >
void
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::SetInput1(const TInputImage1 *image)
{
  this->SetInput( image );
}

template< typename TInputImage1, typename TInputImage2 >
void
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::SetInput2(const TInputImage2 *image)
{
  this->SetNthInput( 1, const_cast< TInputImage2 * >( image ) );
}

template< typename TInputImage1, typename TInputImage2 >
const typename DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::InputImage1Type *
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::GetInput1()
{
  return this->GetInput();
}

template< typename TInputImage1, typename TInputImage2 >
const typename DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::InputImage2Type *
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::GetInput2()
{
  return itkDynamicCastInDebugMode< const TInputImage2 * >
    ( this->ProcessObject::GetInput(1) );
}

template< typename TInputImage1, typename TInputImage2 >
void
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
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
      RegionType region = image1->GetRequestedRegion();
      image2->SetRequestedRegion( region );
      }
    }
}

template< typename TInputImage1, typename TInputImage2 >
void
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage1, typename TInputImage2 >
void
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::AllocateOutputs()
{
  // Pass the first input through as the output
  InputImage1Pointer image =
    const_cast< TInputImage1 * >( this->GetInput1() );

  this->GraftOutput(image);
}

template< typename TInputImage1, typename TInputImage2 >
void
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::BeforeThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  // Resize the thread temporaries
  m_MaxDistance.SetSize(numberOfThreads);
  m_PixelCount.SetSize(numberOfThreads);
  m_Sum.resize(numberOfThreads);

  // Initialize the temporaries
  m_MaxDistance.Fill(NumericTraits< RealType >::ZeroValue());
  m_PixelCount.Fill(0);

  // Compute distance from non-zero pixels in the second image
  typedef itk::SignedMaurerDistanceMapImageFilter< InputImage2Type, DistanceMapType >
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
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::AfterThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  m_DirectedHausdorffDistance = NumericTraits< RealType >::ZeroValue();

  RealType        sum = NumericTraits< RealType >::ZeroValue();
  IdentifierType  pixelcount = 0;

  // find max over all threads
  for ( ThreadIdType i = 0; i < numberOfThreads; i++ )
    {
    if ( m_MaxDistance[i] > m_DirectedHausdorffDistance )
      {
      m_DirectedHausdorffDistance = m_MaxDistance[i];
      }
    pixelcount += m_PixelCount[i];
    sum += m_Sum[i].GetSum();
    }

  if( pixelcount != 0 )
    {
    m_AverageHausdorffDistance = sum / static_cast< RealType >( pixelcount );
    }
  else
    {
    itkGenericExceptionMacro( <<"pixelcount is equal to 0" );
    }

  // clean up
  m_DistanceMap = ITK_NULLPTR;
}

template< typename TInputImage1, typename TInputImage2 >
void
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::ThreadedGenerateData(const RegionType & regionForThread,
                       ThreadIdType threadId)
{
  ImageRegionConstIterator< TInputImage1 >    it1 (this->GetInput1(), regionForThread);
  ImageRegionConstIterator< DistanceMapType > it2 (m_DistanceMap, regionForThread);

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, regionForThread.GetNumberOfPixels() );

  // do the work
  while ( !it1.IsAtEnd() )
    {
    if ( Math::NotExactlyEquals(it1.Get(), NumericTraits< InputImage1PixelType >::ZeroValue()) )
      {
      // The signed distance map is calculated, but we want the calculation based on the
      // unsigned distance map.  Therefore, we set all distance map values less than 0 to 0.
      const RealType val2 = (static_cast< RealType >( it2.Get() ) < NumericTraits< RealType >::ZeroValue() ) ?
                             NumericTraits< RealType >::ZeroValue() : static_cast< RealType >( it2.Get() );
      if ( val2 > m_MaxDistance[threadId] )
        {
        m_MaxDistance[threadId] = val2;
        }
      m_PixelCount[threadId]++;
      m_Sum[threadId].AddElement(val2);
      }

    ++it1;
    ++it2;

    progress.CompletedPixel();
    }
}

template< typename TInputImage1, typename TInputImage2 >
void
DirectedHausdorffDistanceImageFilter< TInputImage1, TInputImage2 >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "DirectedHausdorffDistance: "
     << m_DirectedHausdorffDistance << std::endl;
  os << indent << "AverageHausdorffDistance: "
     << m_AverageHausdorffDistance << std::endl;
  os << indent << "Use Image Spacing : "
     << m_UseImageSpacing << std::endl;
}
} // end namespace itk
#endif
