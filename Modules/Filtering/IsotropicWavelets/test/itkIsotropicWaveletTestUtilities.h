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

#ifndef itkIsotropicWaveletTestUtilities_h
#define itkIsotropicWaveletTestUtilities_h
#include <complex>
#include <itkMathDetail.h>
#include <iomanip>
namespace itk
{
namespace Testing
{
/** Check if Image is Hermitian f(x) = std:conj(f(-x)).
 * @param cImg ComplexImage
 * @param maxUlp Set ulps, error tolerance for the comparison of floats/doubles. We are only interested to detect clear
 * errors
 */
template <typename TValueType, unsigned int N>
bool
ComplexImageIsHermitian(Image<std::complex<TValueType>, N> *                       cImg,
                        typename itk::Math::Detail::FloatIEEE<TValueType>::IntType maxUlp = 10000000)
{
  bool isHermitian = true;

  using ComplexImageType = Image<std::complex<TValueType>, N>;
  itk::ImageRegionConstIteratorWithIndex<ComplexImageType> complexIt(cImg, cImg->GetLargestPossibleRegion());
  complexIt.GoToBegin();
  typename ComplexImageType::IndexType originIndex = cImg->GetLargestPossibleRegion().GetIndex();
  typename ComplexImageType::IndexType upperIndex = cImg->GetLargestPossibleRegion().GetUpperIndex();
  for (; !complexIt.IsAtEnd(); ++complexIt)
  {
    typename ComplexImageType::IndexType index = complexIt.GetIndex();
    typename ComplexImageType::IndexType indexOpp = originIndex + (upperIndex - index);
    bool                                 isInNyquistBand(false);
    bool                                 isInZeroBand(false);
    // DC: 0 ---> 0; Nyq: N/2 -> N/2;  x ---> N - x
    for (unsigned int i = 0; i < N; ++i)
    {
      if (indexOpp[i] == upperIndex[i]) // 0 case
      {
        indexOpp[i] = originIndex[i];
        isInZeroBand = true;
      }
      else // regular case (including Nyq)
      {
        indexOpp[i] += 1;
        if (indexOpp[i] == index[i])
        {
          isInNyquistBand = true;
        }
      }
    }
    typename ComplexImageType::PixelType conjugateOpp;
    if (isInNyquistBand && isInZeroBand)
    {
      conjugateOpp = complexIt.Get();
    }
    else
    {
      conjugateOpp = std::conj(cImg->GetPixel(indexOpp));
    }

    bool equal = itk::Math::FloatAlmostEqual<typename ComplexImageType::PixelType::value_type>(
                   conjugateOpp.real(), complexIt.Get().real(), maxUlp) &&
                 itk::Math::FloatAlmostEqual<typename ComplexImageType::PixelType::value_type>(
                   conjugateOpp.imag(), complexIt.Get().imag(), maxUlp);
    if (!equal == true)
    {
      std::cerr << std::setprecision(20) << "index: " << index << " : " << complexIt.Get() << " sym: " << indexOpp
                << " : " << cImg->GetPixel(indexOpp) << ", conjugate: " << conjugateOpp << std::endl;
      isHermitian = false;
    }
  }
  return isHermitian;
}
} // namespace Testing
} // namespace itk
#endif
