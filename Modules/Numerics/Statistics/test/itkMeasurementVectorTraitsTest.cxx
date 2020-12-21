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

#include "itkMeasurementVectorTraits.h"

#define itkSetGetLengthVerificationMacro(measure, type, len1, len2)                                                    \
  itk::NumericTraits<type>::SetLength((measure), len1);                                                                \
  if (itk::NumericTraits<type>::GetLength((measure)) != len2)                                                          \
  {                                                                                                                    \
    std::cerr << "Set/GetLength() failed in measure " << std::endl;                                                    \
  }                                                                                                                    \
  ITK_MACROEND_NOOP_STATEMENT

#define itkSetLengthExceptionMacro(measure, type, len)                                                                 \
  try                                                                                                                  \
  {                                                                                                                    \
    itk::NumericTraits<type>::SetLength((measure), len);                                                               \
    std::cerr << "Failed to get expected exception for SetLength() ";                                                  \
    std::cerr << std::endl;                                                                                            \
    return EXIT_FAILURE;                                                                                               \
  }                                                                                                                    \
  catch (itk::ExceptionObject &)                                                                                       \
  {}                                                                                                                   \
  ITK_MACROEND_NOOP_STATEMENT

#define itkAssertLengthExceptionMacro(m1, m2)                                                                          \
  try                                                                                                                  \
  {                                                                                                                    \
    itk::Statistics::MeasurementVectorTraits::Assert((m1), (m2));                                                      \
    std::cerr << "Failed to get expected exception for Assert() ";                                                     \
    std::cerr << std::endl;                                                                                            \
    return EXIT_FAILURE;                                                                                               \
  }                                                                                                                    \
  catch (itk::ExceptionObject &)                                                                                       \
  {}                                                                                                                   \
  ITK_MACROEND_NOOP_STATEMENT

#define itkAssertLengthSameValueReturn(m1, type1, m2)                                                                  \
  if (itk::Statistics::MeasurementVectorTraits::Assert((m1), (m2)) != itk::NumericTraits<type1>::GetLength((m1)))      \
  {                                                                                                                    \
    std::cerr << "Failed to get expected VLenght for Assert() ";                                                       \
    std::cerr << std::endl;                                                                                            \
    return EXIT_FAILURE;                                                                                               \
  }                                                                                                                    \
  ITK_MACROEND_NOOP_STATEMENT

#define itkAssertSameLengthTest(m1, m2)                                                                                \
  if (itk::Statistics::MeasurementVectorTraits::Assert((m1), (m2)) != 0)                                               \
  {                                                                                                                    \
    std::cerr << "Failed to recognize same length in Assert() ";                                                       \
    std::cerr << std::endl;                                                                                            \
    return EXIT_FAILURE;                                                                                               \
  }                                                                                                                    \
  ITK_MACROEND_NOOP_STATEMENT


int
itkMeasurementVectorTraitsTest(int, char *[])
{
  std::cout << "MeasurementVectorTraits Test" << std::endl;

  constexpr unsigned int length1 = 7;

  using MeasurementVectorType1 = itk::FixedArray<float, length1>;
  using MeasurementVectorType2 = itk::Array<float>;
  using MeasurementVectorType3 = itk::VariableLengthVector<float>;
  using MeasurementVectorType4 = std::vector<float>;
  using MeasurementVectorType5 = itk::NumericTraits<float>::MeasurementVectorType;

  constexpr unsigned int length2 = 9;

  using MeasurementVectorType1b = itk::FixedArray<float, length2>;
  using MeasurementVectorType2b = itk::Array<float>;
  using MeasurementVectorType3b = itk::VariableLengthVector<float>;
  using MeasurementVectorType4b = std::vector<float>;


  MeasurementVectorType1 measure1;
  MeasurementVectorType2 measure2;
  MeasurementVectorType3 measure3;
  MeasurementVectorType4 measure4;
  MeasurementVectorType5 measure5;

  itkSetGetLengthVerificationMacro(measure1, MeasurementVectorType1, length1, length1);
  itkSetLengthExceptionMacro(measure1, MeasurementVectorType1, length2);

  itkSetGetLengthVerificationMacro(measure2, MeasurementVectorType2, length1, length1);
  itkSetGetLengthVerificationMacro(measure2, MeasurementVectorType2, length2, length2);

  itkSetGetLengthVerificationMacro(measure3, MeasurementVectorType3, length1, length1);
  itkSetGetLengthVerificationMacro(measure3, MeasurementVectorType3, length2, length2);

  itkSetGetLengthVerificationMacro(measure4, MeasurementVectorType4, length1, length1);
  itkSetGetLengthVerificationMacro(measure4, MeasurementVectorType4, length2, length2);

  itkSetGetLengthVerificationMacro(measure5, MeasurementVectorType5, 1, 1);
  itkSetLengthExceptionMacro(measure5, MeasurementVectorType5, length2);

  //
  // Test the Assert() methods
  //
  MeasurementVectorType1b measure1b;
  MeasurementVectorType2b measure2b;
  MeasurementVectorType3b measure3b;
  MeasurementVectorType4b measure4b;

  itk::NumericTraits<MeasurementVectorType1b>::SetLength(measure1b, length2);
  itk::NumericTraits<MeasurementVectorType2b>::SetLength(measure2b, length2);
  itk::NumericTraits<MeasurementVectorType3b>::SetLength(measure3b, length2);
  itk::NumericTraits<MeasurementVectorType4b>::SetLength(measure4b, length2);

  // against each other
  itkAssertSameLengthTest(measure1b, measure1b);
  itkAssertSameLengthTest(measure2b, measure2b);
  itkAssertSameLengthTest(measure3b, measure3b);
  itkAssertSameLengthTest(measure4b, measure4b);

  // against same type with different length
  MeasurementVectorType1  measure1bb;
  MeasurementVectorType2b measure2bb;
  MeasurementVectorType3b measure3bb;
  MeasurementVectorType4b measure4bb;

  itk::NumericTraits<MeasurementVectorType1>::SetLength(measure1bb, length1);
  itk::NumericTraits<MeasurementVectorType2b>::SetLength(measure2bb, length1);
  itk::NumericTraits<MeasurementVectorType3b>::SetLength(measure3bb, length1);
  itk::NumericTraits<MeasurementVectorType4b>::SetLength(measure4bb, length1);


  // against each other
  itkAssertLengthExceptionMacro(measure1b, measure1bb);
  itkAssertLengthExceptionMacro(measure2b, measure2bb);
  itkAssertLengthExceptionMacro(measure3b, measure3bb);
  itkAssertLengthExceptionMacro(measure4b, measure4bb);

  // against other arrays
  itkAssertSameLengthTest(measure1b, measure2b);
  itkAssertSameLengthTest(measure1b, measure3b);
  itkAssertSameLengthTest(measure1b, measure4b);

  // against scalar length
  itkAssertSameLengthTest(measure1b, length2);
  itkAssertSameLengthTest(measure2b, length2);
  itkAssertSameLengthTest(measure3b, length2);
  itkAssertSameLengthTest(measure4b, length2);

  itkAssertLengthExceptionMacro(measure1b, length1);
  itkAssertLengthExceptionMacro(measure2b, length1);
  itkAssertLengthExceptionMacro(measure3b, length1);
  itkAssertLengthExceptionMacro(measure4b, length1);

  itkAssertLengthExceptionMacro(measure1, measure2b);
  itkAssertLengthExceptionMacro(measure1, measure3b);
  itkAssertLengthExceptionMacro(measure1, measure4b);

  constexpr unsigned int zeroLength = 0;

  itkAssertLengthSameValueReturn(measure1b, MeasurementVectorType1b, zeroLength);
  itkAssertLengthSameValueReturn(measure2b, MeasurementVectorType2b, zeroLength);
  itkAssertLengthSameValueReturn(measure3b, MeasurementVectorType3b, zeroLength);
  itkAssertLengthSameValueReturn(measure4b, MeasurementVectorType4b, zeroLength);

  itk::NumericTraits<MeasurementVectorType2b>::SetLength(measure2b, zeroLength);
  itk::NumericTraits<MeasurementVectorType3b>::SetLength(measure3b, zeroLength);
  itk::NumericTraits<MeasurementVectorType4b>::SetLength(measure4b, zeroLength);

  itkAssertLengthSameValueReturn(measure1b, MeasurementVectorType1b, measure2b);
  itkAssertLengthSameValueReturn(measure1b, MeasurementVectorType1b, measure3b);
  itkAssertLengthSameValueReturn(measure1b, MeasurementVectorType1b, measure4b);

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
