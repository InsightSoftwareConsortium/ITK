/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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


#include "itkConstNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkTotalProgressReporter.h"

#include <cmath> // For abs.

namespace itk
{
namespace Testing
{
//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
ComparisonImageFilter<TInputImage, TOutputImage>::ComparisonImageFilter()
{
  this->DynamicMultiThreadingOn();

  // #0 "Valid" required
  Self::SetPrimaryInputName("ValidInput");

  // #1 "Test" required
  Self::AddRequiredInputName("TestInput", 1);
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
ComparisonImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "ToleranceRadius: " << m_ToleranceRadius << '\n';
  os << indent << "DifferenceThreshold: " << m_DifferenceThreshold << '\n';
  os << indent << "MinimumDifference: " << m_MinimumDifference << '\n';
  os << indent << "MaximumDifference: " << m_MaximumDifference << '\n';
  os << indent << "MeanDifference: " << m_MeanDifference << '\n';
  os << indent << "TotalDifference: " << m_TotalDifference << '\n';
  os << indent << "NumberOfPixelsWithDifferences: " << m_NumberOfPixelsWithDifferences << '\n';
  os << indent << "IgnoreBoundaryPixels: " << m_IgnoreBoundaryPixels << '\n';
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
ComparisonImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  // Initialize statistics about difference image.
  m_MinimumDifference = NumericTraits<OutputPixelType>::max();
  m_MaximumDifference = NumericTraits<OutputPixelType>::NonpositiveMin();
  m_MeanDifference = RealType{};
  m_TotalDifference = AccumulateType{};
  m_NumberOfPixelsWithDifferences = 0;
}


//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
ComparisonImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & threadRegion)
{
  using SmartIterator = ConstNeighborhoodIterator<InputImageType>;
  using InputIterator = ImageRegionConstIterator<InputImageType>;
  using OutputIterator = ImageRegionIterator<OutputImageType>;
  using FacesCalculator = NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>;
  using RadiusType = typename FacesCalculator::RadiusType;
  using FaceListType = typename FacesCalculator::FaceListType;

  // Prepare standard boundary condition.
  ZeroFluxNeumannBoundaryCondition<InputImageType> nbc;

  // Get a pointer to each image.
  const InputImageType * validImage = this->GetInput(0);
  const InputImageType * testImage = this->GetInput(1);
  OutputImageType *      outputPtr = this->GetOutput();

  if (validImage->GetBufferedRegion() != testImage->GetBufferedRegion())
  {
    itkExceptionMacro("Input images have different Buffered Regions.");
  }

  // Create a radius of pixels.
  RadiusType                           radius;
  const unsigned int                   minVoxelsNeeded = m_ToleranceRadius * 2 + 1;
  const typename TInputImage::SizeType imageSize = validImage->GetBufferedRegion().GetSize();
  for (unsigned int d = 0; d < TInputImage::ImageDimension; ++d)
  {
    if (minVoxelsNeeded < imageSize[d])
    {
      radius[d] = m_ToleranceRadius;
    }
    else
    {
      radius[d] = ((imageSize[d] - 1) / 2);
    }
  }


  // Initialize the thread local variables
  OutputPixelType threadMinimumDifference{ NumericTraits<OutputPixelType>::max() };
  OutputPixelType threadMaximumDifference{ NumericTraits<OutputPixelType>::NonpositiveMin() };
  AccumulateType  threadDifferenceSum{};
  SizeValueType   threadNumberOfPixels{ 0 };

  // Find the data-set boundary faces.
  FacesCalculator boundaryCalculator;
  FaceListType    faceList = boundaryCalculator(testImage, threadRegion, radius);

  // Support progress methods/callbacks.
  TotalProgressReporter progress(this, threadRegion.GetNumberOfPixels());

  // Process the internal face and each of the boundary faces.
  for (auto face = faceList.begin(); face != faceList.end(); ++face)
  {
    SmartIterator  test(radius, testImage, *face); // Iterate over test image.
    InputIterator  valid(validImage, *face);       // Iterate over valid image.
    OutputIterator out(outputPtr, *face);          // Iterate over output image.
    if (!test.GetNeedToUseBoundaryCondition() || !m_IgnoreBoundaryPixels)
    {
      test.OverrideBoundaryCondition(&nbc);

      for (valid.GoToBegin(), test.GoToBegin(), out.GoToBegin(); !valid.IsAtEnd(); ++valid, ++test, ++out)
      {
        // Get the current valid pixel.
        const InputPixelType t = valid.Get();

        //  Assume a good match - so test center pixel first, for speed
        const RealType difference = std::abs(static_cast<RealType>(t) - test.GetCenterPixel());
        auto           minimumDifference = static_cast<OutputPixelType>(difference);

        // If center pixel isn't good enough, then test the neighborhood
        if (minimumDifference > m_DifferenceThreshold)
        {
          const unsigned int neighborhoodSize = test.Size();
          // Find the closest-valued pixel in the neighborhood of the test
          // image.
          for (unsigned int i = 0; i < neighborhoodSize; ++i)
          {
            // Use the RealType for the difference to make sure we get the
            // sign.
            const RealType differenceReal = std::abs(static_cast<RealType>(t) - test.GetPixel(i));
            auto           d = static_cast<OutputPixelType>(differenceReal);
            if (d < minimumDifference)
            {
              minimumDifference = d;
              if (minimumDifference <= m_DifferenceThreshold)
              {
                break;
              }
            }
          }
        }

        // Check if difference is above threshold.
        if (minimumDifference > m_DifferenceThreshold)
        {
          // Store the minimum difference value in the output image.
          out.Set(minimumDifference);

          // Update difference image statistics.
          threadDifferenceSum += minimumDifference;
          ++threadNumberOfPixels;

          threadMinimumDifference = std::min(threadMinimumDifference, minimumDifference);
          threadMaximumDifference = std::max(threadMaximumDifference, minimumDifference);
        }
        else
        {
          // Difference is below threshold.
          out.Set(OutputPixelType{});
        }

        // Update progress.
        progress.CompletedPixel();
      }
    }
    else
    {
      for (out.GoToBegin(); !out.IsAtEnd(); ++out)
      {
        out.Set(OutputPixelType{});
        progress.CompletedPixel();
      }
    }
  }

  const std::lock_guard<std::mutex> lock(m_Mutex);
  m_TotalDifference += threadDifferenceSum;
  m_NumberOfPixelsWithDifferences += threadNumberOfPixels;

  m_MinimumDifference = std::min(threadMinimumDifference, m_MinimumDifference);
  m_MaximumDifference = std::max(threadMaximumDifference, m_MaximumDifference);
}

//----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
ComparisonImageFilter<TInputImage, TOutputImage>::AfterThreadedGenerateData()
{
  // Calculate the mean difference.
  m_MeanDifference = 0.0;
  if (m_NumberOfPixelsWithDifferences > 0)
  {
    m_MeanDifference = m_TotalDifference / m_NumberOfPixelsWithDifferences;
  }
}

template <typename TInputImage, typename TOutputImage>
void
ComparisonImageFilter<TInputImage, TOutputImage>::VerifyInputInformation() const
{
  if (m_VerifyInputInformation)
  {
    this->Superclass::VerifyInputInformation();
  }
}


} // end namespace Testing
} // end namespace itk

#endif
