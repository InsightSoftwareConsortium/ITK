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

#if !defined(ITK_LEGACY_REMOVE)
// Suppress MSVC warnings from VS2022, saying: "warning C4996: 'std::complex<T>::complex': warning STL4037: The effect
// of instantiating the template std::complex for any type other than float, double, or long double is unspecified."
#  define _SILENCE_NONFLOATING_COMPLEX_DEPRECATION_WARNING
#endif

#include "itkNumericTraits.h"

namespace itk
{

#if !defined(ITK_LEGACY_REMOVE)
template <>
const std::complex<char> NumericTraits<std::complex<char>>::Zero = std::complex<char>(0, 0);
template <>
const std::complex<char> NumericTraits<std::complex<char>>::One = std::complex<char>(1, 0);

template <>
const std::complex<unsigned char> NumericTraits<std::complex<unsigned char>>::Zero = std::complex<unsigned char>(0, 0);
template <>
const std::complex<unsigned char> NumericTraits<std::complex<unsigned char>>::One = std::complex<unsigned char>(1, 0);

template <>
const std::complex<short> NumericTraits<std::complex<short>>::Zero = std::complex<short>(0, 0);
template <>
const std::complex<short> NumericTraits<std::complex<short>>::One = std::complex<short>(1, 0);

template <>
const std::complex<unsigned short> NumericTraits<std::complex<unsigned short>>::Zero = std::complex<unsigned short>(0,
                                                                                                                    0);
template <>
const std::complex<unsigned short> NumericTraits<std::complex<unsigned short>>::One = std::complex<unsigned short>(1,
                                                                                                                   0);

template <>
const std::complex<int> NumericTraits<std::complex<int>>::Zero = std::complex<int>(0, 0);
template <>
const std::complex<int> NumericTraits<std::complex<int>>::One = std::complex<int>(1, 0);

template <>
const std::complex<unsigned int> NumericTraits<std::complex<unsigned int>>::Zero = std::complex<unsigned int>(0, 0);
template <>
const std::complex<unsigned int> NumericTraits<std::complex<unsigned int>>::One = std::complex<unsigned int>(1, 0);

template <>
const std::complex<long> NumericTraits<std::complex<long>>::Zero = std::complex<long>(0L, 0L);
template <>
const std::complex<long> NumericTraits<std::complex<long>>::One = std::complex<long>(1L, 0L);

template <>
const std::complex<unsigned long> NumericTraits<std::complex<unsigned long>>::Zero = std::complex<unsigned long>(0UL,
                                                                                                                 0UL);
template <>
const std::complex<unsigned long> NumericTraits<std::complex<unsigned long>>::One = std::complex<unsigned long>(1UL,
                                                                                                                0UL);
#endif // !defined(ITK_LEGACY_REMOVE)

template <>
const std::complex<float> NumericTraits<std::complex<float>>::Zero = std::complex<float>(0.0f, 0.0f);
template <>
const std::complex<float> NumericTraits<std::complex<float>>::One = std::complex<float>(1.0f, 0.0f);

template <>
const std::complex<double> NumericTraits<std::complex<double>>::Zero = std::complex<double>(0.0, 0.0);
template <>
const std::complex<double> NumericTraits<std::complex<double>>::One = std::complex<double>(1.0, 0.0);

template <>
const std::complex<long double> NumericTraits<std::complex<long double>>::Zero = std::complex<long double>(0.0, 0.0);
template <>
const std::complex<long double> NumericTraits<std::complex<long double>>::One = std::complex<long double>(1.0, 0.0);

} // end namespace itk
