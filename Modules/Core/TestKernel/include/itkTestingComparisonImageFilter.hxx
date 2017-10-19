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
#ifndef itkTestingComparisonImageFilter_hxx
#define itkTestingComparisonImageFilter_hxx

#include "itkTestingComparisonImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkProgressReporter.h"

namespace itk
{
namespace Testing
{
//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
ComparisonImageFilter< TInputImage, TOutputImage >
::ComparisonImageFilter()
{
  // Set the default DifferenceThreshold.
  m_DifferenceThreshold = NumericTraits< OutputPixelType >::ZeroValue();

  // Set the default ToleranceRadius.
  m_ToleranceRadius = 0;

  // Initialize statistics about difference image.
  m_MinimumDifference  = NumericTraits< OutputPixelType >::max();
  m_MaximumDifference = NumericTraits< OutputPixelType >::NonpositiveMin();
  m_MeanDifference = NumericTraits< RealType >::ZeroValue();
  m_TotalDifference = NumericTraits< AccumulateType >::ZeroValue();
  m_NumberOfPixelsWithDifferences = 0;
  m_IgnoreBoundaryPixels = false;
  m_VerifyInputInformation = true;

  // #0 "Valid" required
  Self::SetPrimaryInputName("ValidInput");

  // #1 "Test" required
  Self::AddRequiredInputName("TestInput", 1);
}

//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
ComparisonImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "ToleranceRadius: " << m_ToleranceRadius << "\n";
  os << indent << "DifferenceThreshold: " << m_DifferenceThreshold << "\n";
  os << indent << "MinimumDifference: " << m_MinimumDifference << "\n";
  os << indent << "MaximumDifference: " << m_MaximumDifference << "\n";
  os << indent << "MeanDifference: " << m_MeanDifference << "\n";
  os << indent << "TotalDifference: " << m_TotalDifference << "\n";
  os << indent << "NumberOfPixelsWithDifferences: "
     << m_NumberOfPixelsWithDifferences << "\n";
  os << indent << "IgnoreBoundaryPixels: "
     << m_IgnoreBoundaryPixels << "\n";
}

//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
ComparisonImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  // Initialize statistics about difference image.
  m_MinimumDifference = NumericTraits< OutputPixelType >::max();
  m_MaximumDifference = NumericTraits< OutputPixelType >::NonpositiveMin();
  m_MeanDifference = NumericTraits< RealType >::ZeroValue();
  m_TotalDifference = NumericTraits< AccumulateType >::ZeroValue();
  m_NumberOfPixelsWithDifferences = 0;

  // Resize the thread temporaries
  m_ThreadDifferenceSum.SetSize(numberOfThreads);
  m_ThreadMinimumDifference.SetSize(numberOfThreads);
  m_ThreadMaximumDifference.SetSize(numberOfThreads);
  m_ThreadNumberOfPixels.SetSize(numberOfThreads);

  // Initialize the temporaries
  m_ThreadMinimumDifference.Fill(NumericTraits< OutputPixelType >::max());
  m_ThreadMaximumDifference.Fill(NumericTraits< OutputPixelType >::NonpositiveMin());
  m_ThreadDifferenceSum.Fill(NumericTraits< AccumulateType >::ZeroValue());
  m_ThreadNumberOfPixels.Fill(0);
}


//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
ComparisonImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & threadRegion, ThreadIdType threadId)
{
  typedef ConstNeighborhoodIterator< InputImageType >                           SmartIterator;
  typedef ImageRegionConstIterator< InputImageType >                            InputIterator;
  typedef ImageRegionIterator< OutputImageType >                                OutputIterator;
  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType > FacesCalculator;
  typedef typename FacesCalculator::RadiusType                                  RadiusType;
  typedef typename FacesCalculator::FaceListType                                FaceListType;
  typedef typename FaceListType::iterator                                       FaceListIterator;

  // Prepare standard boundary condition.
  ZeroFluxNeumannBoundaryCondition< InputImageType > nbc;

  // Get a pointer to each image.
  const InputImageType *validImage = this->GetInput(0);
  const InputImageType *testImage = this->GetInput(1);
  OutputImageType *     outputPtr = this->GetOutput();

  if( validImage->GetBufferedRegion() != testImage->GetBufferedRegion() )
    {
    itkExceptionMacro( << "Input images have different Buffered Regions." )
    }

  // Create a radius of pixels.
  RadiusType radius;
  const unsigned int minVoxelsNeeded = m_ToleranceRadius*2+1;
  const typename TInputImage::SizeType imageSize = validImage->GetBufferedRegion().GetSize();
  for( unsigned int d=0; d < TInputImage::ImageDimension; ++d )
    {
    if( minVoxelsNeeded < imageSize[d] )
      {
      radius[d] = m_ToleranceRadius;
      }
    else
      {
        radius[d] = ( (imageSize[d]-1)/2 );
      }
    }

  // Find the data-set boundary faces.
  FacesCalculator boundaryCalculator;
  FaceListType    faceList = boundaryCalculator(testImage, threadRegion, radius);

  // Support progress methods/callbacks.
  ProgressReporter progress( this, threadId, threadRegion.GetNumberOfPixels() );

  // Process the internal face and each of the boundary faces.
  for ( FaceListIterator face = faceList.begin(); face != faceList.end(); ++face )
    {
    SmartIterator  test(radius, testImage, *face); // Iterate over test image.
    InputIterator  valid(validImage, *face);       // Iterate over valid image.
    OutputIterator out(outputPtr, *face);          // Iterate over output image.
    if ( !test.GetNeedToUseBoundaryCondition() || !m_IgnoreBoundaryPixels )
      {
      test.OverrideBoundaryCondition(&nbc);

      for ( valid.GoToBegin(), test.GoToBegin(), out.GoToBegin();
            !valid.IsAtEnd();
            ++valid, ++test, ++out )
        {
        // Get the current valid pixel.
        InputPixelType t = valid.Get();

        //  Assume a good match - so test center pixel first, for speed
        RealType difference = static_cast< RealType >( t ) - test.GetCenterPixel();
        if ( NumericTraits< RealType >::IsNegative(difference) )
          {
          difference = -difference;
          }
        OutputPixelType minimumDifference = static_cast< OutputPixelType >( difference );

        // If center pixel isn't good enough, then test the neighborhood
        if ( minimumDifference > m_DifferenceThreshold )
          {
          unsigned int neighborhoodSize = test.Size();
          // Find the closest-valued pixel in the neighborhood of the test
          // image.
          for ( unsigned int i = 0; i < neighborhoodSize; ++i )
            {
            // Use the RealType for the difference to make sure we get the
            // sign.
            RealType differenceReal = static_cast< RealType >( t ) - test.GetPixel(i);
            if ( NumericTraits< RealType >::IsNegative(differenceReal) )
              {
              differenceReal = -differenceReal;
              }
            OutputPixelType d = static_cast< OutputPixelType >( differenceReal );
            if ( d < minimumDifference )
              {
              minimumDifference = d;
              if ( minimumDifference <= m_DifferenceThreshold )
                {
                break;
                }
              }
            }
          }

        // Check if difference is above threshold.
        if ( minimumDifference > m_DifferenceThreshold )
          {
          // Store the minimum difference value in the output image.
          out.Set(minimumDifference);

          // Update difference image statistics.
          m_ThreadDifferenceSum[threadId] += minimumDifference;
          m_ThreadNumberOfPixels[threadId]++;

          m_ThreadMinimumDifference[threadId] = std::min( m_ThreadMinimumDifference[threadId], minimumDifference );
          m_ThreadMaximumDifference[threadId] = std::max( m_ThreadMaximumDifference[threadId], minimumDifference );

          }
        else
          {
          // Difference is below threshold.
          out.Set(NumericTraits< OutputPixelType >::ZeroValue());
          }

        // Update progress.
        progress.CompletedPixel();
        }
      }
    else
      {
      for ( out.GoToBegin(); !out.IsAtEnd(); ++out )
        {
        out.Set(NumericTraits< OutputPixelType >::ZeroValue());
        progress.CompletedPixel();
        }
      }
    }
}

//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
ComparisonImageFilter< TInputImage, TOutputImage >
::AfterThreadedGenerateData()
{
  // Set statistics about difference image.
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  for ( ThreadIdType i = 0; i < numberOfThreads; ++i )
    {
    m_TotalDifference += m_ThreadDifferenceSum[i];
    m_NumberOfPixelsWithDifferences += m_ThreadNumberOfPixels[i];

    m_MinimumDifference = std::min( m_ThreadMinimumDifference[i], m_MinimumDifference );
    m_MaximumDifference = std::max( m_ThreadMaximumDifference[i], m_MaximumDifference );
    }

  // The TotalDifference is an accumulation of values of pixels which
  // don't meet the difference threshold with the radius
  // option. Therefore this value is averaged over the number of pixel
  // actually accumulated.

  // Calculate the mean difference.
  m_MeanDifference = 0.0;
  if ( m_NumberOfPixelsWithDifferences > 0 )
    {
    m_MeanDifference = m_TotalDifference / m_NumberOfPixelsWithDifferences;
    }
}

template< typename TInputImage, typename TOutputImage >
void
ComparisonImageFilter< TInputImage, TOutputImage >
::VerifyInputInformation()
{
  if(m_VerifyInputInformation)
    {
    this->Superclass::VerifyInputInformation();
    }
}


} // end namespace Testing
} // end namespace itk

#endif
