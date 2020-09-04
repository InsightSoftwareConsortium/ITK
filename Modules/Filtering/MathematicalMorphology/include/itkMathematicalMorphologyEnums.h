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
#ifndef itkMathematicalMorphologyEnums_h
#define itkMathematicalMorphologyEnums_h

#include <iostream>
#include "ITKMathematicalMorphologyExport.h"


namespace itk
{

/**
 * \class MathematicalMorphologyEnums
 * \brief Mathematical Morphology enum classes.
 * \ingroup ITKMathematicalMorphology
 */

class MathematicalMorphologyEnums
{
public:
  /**\class AlgorithmType
   * \brief Algorithm or implementation used in the dilation/erosion operations.
   * \ingroup ITKMathematicalMorphology
   */
  enum class Algorithm : uint8_t
  {
    BASIC = 0,
    HISTO = 1,
    ANCHOR = 2,
    VHGW = 3
  };
};

/** Define how to print enumeration values. */
extern ITKMathematicalMorphology_EXPORT std::ostream &
                                        operator<<(std::ostream & out, const MathematicalMorphologyEnums::Algorithm value);

} // end namespace itk

#endif
