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

#include <iostream>
#include "itkFrequencyImageRegionIteratorWithIndex.h"
#include <complex>
#include <itkMath.h>
using namespace std;
using namespace itk;

template <typename TImage>
class itkFrequencyImageRegionIteratorWithIndexTester
{
public:
  typedef TImage                          ImageType;
  typedef typename ImageType::IndexType   IndexType;
  typedef typename ImageType::SpacingType FrequencyType;

  typedef itk::FrequencyImageRegionIteratorWithIndex<ImageType> IteratorType;

  explicit itkFrequencyImageRegionIteratorWithIndexTester(size_t input_size)
  {
    m_Image = ImageType::New();

    typename ImageType::SizeType size;
    size.Fill(input_size);

    typename ImageType::IndexType start;
    start.Fill(0);

    typename ImageType::RegionType region;
    region.SetSize(size);
    region.SetIndex(start);

    m_Image->SetRegions(region);

    // Setup the half, positive frequencies region.
    size.Fill(input_size / 2);
    start.Fill(1);
    m_PositiveHalfRegion.SetSize(size);
    m_PositiveHalfRegion.SetIndex(start);

    m_ImageIsOdd = input_size % 2 == 1 ? 1 : 0;
    // Setup the half, negative frequencies region.
    unsigned int one_if_odd = m_ImageIsOdd ? 1 : 0;
    size.Fill(input_size / 2);
    start.Fill(input_size / 2 + one_if_odd);
    m_NegativeHalfRegion.SetSize(size);
    m_NegativeHalfRegion.SetIndex(start);

    /* With default frequency_spacing = 1 ( = sampling frequency)
     * Nyquist_first = fs * (N/2) / N where N is the size of that dim.
     * If image is Even, there is only one Nyquist at index = N/2, shared between + and - frequencies.
     * If Odd, Nyquist Freq  = fs/2 is not represented, but there is still
     * a largest frequency at half index with value = fs/2 * (N-1)/N
     */

    for (unsigned int dim = 0; dim < ImageType::ImageDimension; dim++)
    {
      m_LargestFrequency[dim] = 0.5; // = fs/2 with default spacing = 1.0
      if (m_ImageIsOdd)
        m_LargestFrequency[dim] = 0.5 * (input_size - 1) * (1.0 / input_size);
    }
  }

  bool
  TestIterator()
  {
    typename ImageType::RegionType region = m_Image->GetLargestPossibleRegion();
    std::cout << "---" << std::endl;
    std::cout << "LargestRegion:" << region << std::endl;
    if (TestLargestRegion() == false)
    {
      std::cout << "Failed testing largest region." << std::endl;
      return false;
    }

    if (TestFrequenciesHaveHermitianSimmetry() == false)
    {
      std::cout << "Failed testing hermitian simmetry." << std::endl;
      return false;
    }

    region = m_PositiveHalfRegion;
    std::cout << "---" << std::endl;
    std::cout << "Positive Half Region (excluding 0 freqs in all dim):" << region << std::endl;

    region = m_NegativeHalfRegion;
    std::cout << "---" << std::endl;
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
        if (it.GetFrequency()[dim] != itk::Math::abs(reverseIt.GetFrequency()[dim]))
        {
          std::cout << "Failed testing hermitian property at index:" << it.GetIndex() << " freq: " << it.GetFrequency()
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

  // If N is even, the nyquist frequency is stored in the positive region,
  // but shared with the negative region.
  // If N is odd, the largest frequency has a positive and negative component.
  bool
  TestNegativeRegion(typename ImageType::RegionType & region)
  {
    IteratorType it(m_Image, region);

    it.GoToBegin();
    IndexType half_index_plus_one;
    for (unsigned int dim = 0; dim < ImageType::ImageDimension; dim++)
    {
      half_index_plus_one[dim] = it.GetHalfIndex()[dim] + 1;
    }
    IndexType first_negative_index = m_ImageIsOdd ? half_index_plus_one : it.GetHalfIndex();
    IndexType smallest_negative_freq_index;
    for (unsigned int dim = 0; dim < ImageType::ImageDimension; dim++)
    {
      smallest_negative_freq_index[dim] = m_ImageIsOdd ? -first_negative_index[dim] + 1 : first_negative_index[dim];
    }
    while (!it.IsAtEnd())
    {
      if (it.GetIndex() == first_negative_index)
      {
        // abs value should be equal to largest freq for odd images
        if (m_ImageIsOdd == true && m_LargestFrequency != it.GetFrequency() && -m_LargestFrequency != it.GetFrequency())
        {
          std::cout << " Frequency value is wrong." << it.GetFrequency() << " should be: " << m_LargestFrequency
                    << std::endl;
          return false;
        }
        if (it.GetFrequencyBin() != smallest_negative_freq_index)
        {
          std::cout << " Smallest negative frequency bin is wrong." << it.GetFrequencyBin()
                    << " should be: " << smallest_negative_freq_index << ".iterator index: " << it.GetIndex()
                    << std::endl;
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
      if (it.GetHalfIndex()[dim] != truthHalfIndex[dim])
      {
        std::cout << "HalfIndex is wrong! - " << it.GetHalfIndex() << ". It should be: " << truthHalfIndex << std::endl;
        return false;
      }
    }

    IndexType zero_freq_index;
    zero_freq_index.Fill(0);
    it.GoToBegin();

    if (it.GetIndex() == m_Image->GetLargestPossibleRegion().GetIndex() && it.GetFrequencyBin() != zero_freq_index)
    {
      std::cout << "Error: Zero frequency is not at the minimum index!." << std::endl;
      return false;
    }

    while (!it.IsAtEnd())
    {
      IndexType index = it.GetIndex();
      // Check to see if the index is within allowed bounds
      bool isInside = m_Image->GetLargestPossibleRegion().IsInside(index);
      if (!isInside)
      {
        std::cout << "    Index is not inside region! - " << index << std::endl;
        return false;
      }
      // check repeatibility
      if (index != it.GetIndex())
      {
        std::cout << "    Failed to repeat GetIndex." << std::endl;
        return false;
      }

      if (index == it.GetHalfIndex() && it.GetFrequency() != m_LargestFrequency &&
          it.GetFrequencyBin() != it.GetHalfIndex())
      {
        std::cout << "    Failed! Largest freq bin is wrong. " << index << ". Frequency Index: " << it.GetFrequency()
                  << ". It should be: " << m_LargestFrequency << std::endl;
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
itkFrequencyImageRegionIteratorWithIndexTest(int, char **)
{
  bool testPassed = true; // let's be optimistic

  {
    size_t input_size(8);
    std::cout << "Testing with EVEN Image< std::complex<float>, 3 > with size: " << input_size << std::endl;
    itkFrequencyImageRegionIteratorWithIndexTester<itk::Image<std::complex<float>, 3>> Tester(input_size);
    if (Tester.TestIterator() == false)
    {
      testPassed = false;
    }
  }
  {
    std::cout << "\n------------------------" << std::endl;
    size_t input_size(10);
    std::cout << "Testing with EVEN Image< char, 3 > with size: " << input_size << std::endl;
    itkFrequencyImageRegionIteratorWithIndexTester<itk::Image<char, 3>> Tester(input_size);
    if (Tester.TestIterator() == false)
    {
      testPassed = false;
    }
  }

  {
    std::cout << "\n------------------------" << std::endl;
    size_t input_size(9);
    std::cout << "Testing with ODD Image< char, 3 > with size: " << input_size << std::endl;
    itkFrequencyImageRegionIteratorWithIndexTester<itk::Image<char, 3>> Tester(input_size);
    if (Tester.TestIterator() == false)
    {
      testPassed = false;
    }
  }

  if (!testPassed)
  {
    std::cout << "\nFailed" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "\nSuccess" << std::endl;
  return EXIT_SUCCESS;
}
