/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkFrequencyFFTLayoutImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"

#include <iostream>
#include <complex>
#include <itkMath.h>

template <typename TImage>
class itkFrequencyFFTLayoutImageRegionIteratorWithIndexTester
{
public:
  using ImageType = TImage;
  using IndexType = typename ImageType::IndexType;
  using FrequencyType = typename ImageType::SpacingType;

  using IteratorType = itk::FrequencyFFTLayoutImageRegionIteratorWithIndex<ImageType>;

  explicit itkFrequencyFFTLayoutImageRegionIteratorWithIndexTester(size_t inputImageSize)
  {
    m_Image = ImageType::New();

    typename ImageType::SizeType size;
    size.Fill(inputImageSize);

    typename ImageType::IndexType start;
    start.Fill(0);

    typename ImageType::RegionType region;
    region.SetSize(size);
    region.SetIndex(start);

    m_Image->SetRegions(region);

    // Setup the half, positive frequencies region.
    size.Fill(inputImageSize / 2);
    start.Fill(1);
    m_PositiveHalfRegion.SetSize(size);
    m_PositiveHalfRegion.SetIndex(start);

    m_ImageIsOdd = inputImageSize % 2 == 1 ? true : false;
    // Setup the half, negative frequencies region.
    unsigned int isImageSizeOdd = m_ImageIsOdd ? 1 : 0;
    size.Fill(inputImageSize / 2);
    start.Fill(inputImageSize / 2 + isImageSizeOdd);
    m_NegativeHalfRegion.SetSize(size);
    m_NegativeHalfRegion.SetIndex(start);
    //
    // With default frequency_spacing = 1 ( = sampling frequency)
    // Nyquist_first = fs * (N/2) / N where N is the size of that dim.
    // If image is Even, there is only one Nyquist at index = N/2, shared between + and - frequencies.
    // If Odd, Nyquist Freq  = fs/2 is not represented, but there is still
    // a largest frequency at half index with value = fs/2 * (N-1)/N
    //
    for (unsigned int dim = 0; dim < ImageType::ImageDimension; dim++)
    {
      m_LargestFrequency[dim] = 0.5; // = fs/2 with default spacing = 1.0
      if (m_ImageIsOdd)
      {
        m_LargestFrequency[dim] = 0.5 * (inputImageSize - 1) * (1.0 / inputImageSize);
      }
    }
  }

  bool
  TestIterator()
  {
    typename ImageType::RegionType region = m_Image->GetLargestPossibleRegion();
    std::cout << "LargestRegion:" << region << std::endl;
    if (TestLargestRegion() == false)
    {
      std::cout << "Failed testing largest region." << std::endl;
      return false;
    }

    if (TestFrequenciesHaveHermitianSimmetry() == false)
    {
      std::cout << "Failed testing Hermitian simmetry." << std::endl;
      return false;
    }

    region = m_PositiveHalfRegion;
    std::cout << "Positive Half Region (excluding 0 freqs in all dim):" << region << std::endl;

    region = m_NegativeHalfRegion;
    std::cout << "Negative Half Region: (including Nyquist freq if N even)" << region << std::endl;

    if (TestNegativeRegion(region) == false)
    {
      std::cout << "Failed testing negative frequencies region." << std::endl;
      return false;
    }

    return true;
  }

  bool
  TestFrequenciesHaveHermitianSimmetry()
  {
    IteratorType it(m_Image, m_PositiveHalfRegion);
    IteratorType reverseIt(m_Image, m_NegativeHalfRegion);

    it.GoToBegin();
    reverseIt.GoToReverseBegin();
    while (!it.IsAtEnd())
    {
      for (unsigned int dim = 0; dim < ImageType::ImageDimension; dim++)
      {
        if (itk::Math::NotAlmostEquals(it.GetFrequency()[dim], itk::Math::abs(reverseIt.GetFrequency()[dim])))
        {
          std::cout << "Failed testing Hermitian property at index:" << it.GetIndex() << " freq: " << it.GetFrequency()
                    << "\n"
                    << " reverseIt: " << reverseIt.GetIndex() << " freq: " << reverseIt.GetFrequency() << std::endl;
          return false;
        }
      }
      --reverseIt;
      ++it;
    }

    return true;
  }

  // If N is even, the Nyquist frequency is stored in the positive region,
  // but shared with the negative region.
  // If N is odd, the largest frequency has a positive and negative component.
  bool
  TestNegativeRegion(typename ImageType::RegionType & region)
  {
    IteratorType it(m_Image, region);

    it.GoToBegin();
    IndexType halfIndexPlusOne;
    for (unsigned int dim = 0; dim < ImageType::ImageDimension; dim++)
    {
      halfIndexPlusOne[dim] = it.GetLargestPositiveFrequencyIndex()[dim] + 1;
    }
    IndexType firstNegativeIndex = m_ImageIsOdd ? halfIndexPlusOne : it.GetLargestPositiveFrequencyIndex();
    IndexType smallestNegativeFreqIndex;
    for (unsigned int dim = 0; dim < ImageType::ImageDimension; dim++)
    {
      smallestNegativeFreqIndex[dim] = m_ImageIsOdd ? -firstNegativeIndex[dim] + 1 : firstNegativeIndex[dim];
    }
    while (!it.IsAtEnd())
    {
      if (it.GetIndex() == firstNegativeIndex)
      {
        // abs value should be equal to largest freq for odd images
        if (m_ImageIsOdd == true && m_LargestFrequency != it.GetFrequency() && -m_LargestFrequency != it.GetFrequency())
        {
          std::cout << " Frequency value is wrong." << it.GetFrequency() << " should be: " << m_LargestFrequency
                    << std::endl;
          return false;
        }
        if (it.GetFrequencyBin() != smallestNegativeFreqIndex)
        {
          std::cout << " Smallest negative frequency bin is wrong." << it.GetFrequencyBin()
                    << " should be: " << smallestNegativeFreqIndex << ".iterator index: " << it.GetIndex() << std::endl;
          return false;
        }
      }
      ++it;
    }

    return true;
  }

  bool
  TestLargestRegion()
  {
    IteratorType it(m_Image, m_Image->GetLargestPossibleRegion());

    typename ImageType::IndexType truthHalfIndex;
    for (unsigned int dim = 0; dim < ImageType::ImageDimension; dim++)
    {
      truthHalfIndex[dim] =
        m_Image->GetLargestPossibleRegion().GetIndex()[dim] + m_Image->GetLargestPossibleRegion().GetSize()[dim] / 2;
      if (it.GetLargestPositiveFrequencyIndex()[dim] != truthHalfIndex[dim])
      {
        std::cerr << "Test failed! " << std::endl;
        std::cerr << "Error in GetLargestPositiveFrequencyIndex()[" << dim << "]" << std::endl;
        std::cerr << "Expected: " << truthHalfIndex << ", but got " << it.GetLargestPositiveFrequencyIndex()
                  << std::endl;
        return false;
      }
    }

    IndexType zero_freq_index;
    zero_freq_index.Fill(0);
    it.GoToBegin();

    if (it.GetIndex() == m_Image->GetLargestPossibleRegion().GetIndex() && it.GetFrequencyBin() != zero_freq_index)
    {
      std::cerr << "Test failed! " << std::endl;
      std::cerr << "Error: Zero frequency is not at the minimum index!" << std::endl;
      return false;
    }

    while (!it.IsAtEnd())
    {
      IndexType index = it.GetIndex();
      // Check to see if the index is within allowed bounds
      bool isInside = m_Image->GetLargestPossibleRegion().IsInside(index);
      if (!isInside)
      {
        std::cerr << "Test failed! " << std::endl;
        std::cerr << "Index is not inside region!: " << index << std::endl;
        return false;
      }
      // Check repeatibility
      if (index != it.GetIndex())
      {
        std::cerr << "Test failed! " << std::endl;
        std::cerr << "Failed to repeat GetIndex()" << std::endl;
        return false;
      }

      if (index == it.GetLargestPositiveFrequencyIndex() && it.GetFrequency() != m_LargestFrequency &&
          it.GetFrequencyBin() != it.GetLargestPositiveFrequencyIndex())
      {
        std::cerr << "Test failed! " << std::endl;
        std::cerr << "Error in largest frequency bin" << std::endl;
        std::cerr << "Expected: " << m_LargestFrequency << ", but got " << it.GetFrequency() << std::endl;
        return false;
      }
      // increment and test index
      ++it;
    }

    return true;
  }

private:
  typename ImageType::Pointer    m_Image;
  typename ImageType::RegionType m_PositiveHalfRegion;
  typename ImageType::RegionType m_NegativeHalfRegion;
  FrequencyType                  m_LargestFrequency;
  bool                           m_ImageIsOdd;
};

int
itkFrequencyFFTLayoutImageRegionIteratorWithIndexTest(int, char *[])
{
  bool testPassed = true; // let's be optimistic

  constexpr unsigned int Dimension = 3;

  using CharPixelType = char;
  using FloatPixelType = char;

  // Even input image size test
  {
    size_t inputImageSize(8);
    std::cout << "Testing with EVEN Image< std::complex<float>, 3 > with size: " << inputImageSize << std::endl;
    itkFrequencyFFTLayoutImageRegionIteratorWithIndexTester<itk::Image<std::complex<FloatPixelType>, Dimension>> Tester(
      inputImageSize);
    if (Tester.TestIterator() == false)
    {
      testPassed = false;
    }
  }

  // Even input image size test
  {
    size_t inputImageSize(10);
    std::cout << "Testing with EVEN Image< char, 3 > with size: " << inputImageSize << std::endl;
    itkFrequencyFFTLayoutImageRegionIteratorWithIndexTester<itk::Image<CharPixelType, Dimension>> Tester(
      inputImageSize);
    if (Tester.TestIterator() == false)
    {
      testPassed = false;
    }
  }

  // Odd input image size test
  {
    size_t inputImageSize(9);
    std::cout << "Testing with ODD Image< char, 3 > with size: " << inputImageSize << std::endl;
    itkFrequencyFFTLayoutImageRegionIteratorWithIndexTester<itk::Image<CharPixelType, Dimension>> Tester(
      inputImageSize);
    if (Tester.TestIterator() == false)
    {
      testPassed = false;
    }
  }

  if (!testPassed)
  {
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
