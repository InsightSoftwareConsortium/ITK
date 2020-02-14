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
#ifndef itkAddPixelAccessor_h
#define itkAddPixelAccessor_h

#include "itkNumericTraits.h"

namespace itk
{
namespace Accessor
{
/**
 *\class AddPixelAccessor
 * \brief Simulates the effect of adding a constant value to all pixels
 *
 * This class is intended to be used as parameter of
 * an ImageAdaptor to make an image appear as having
 * pixels with intensity values increased by a constant amount.
 *
 * \sa ImageAdaptor
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 *
 * \sphinx
 * \sphinxexample{Core/ImageAdaptors/AddConstantToPixelsWithoutDuplicatingImage,Add Constant To Every Pixel Without
 * Duplicating Memory} \endsphinx
 */

template <typename TPixel>
class AddPixelAccessor
{
public:
  /** Standard class type aliases. */
  using Self = AddPixelAccessor;

  /** External type alias. It defines the external aspect
   * that this class will exhibit */
  using ExternalType = TPixel;

  /** Internal type alias. It defines the internal real
   * representation of data */
  using InternalType = TPixel;

  /** Write access to the pixel */
  inline void
  Set(InternalType & output, const ExternalType & input) const
  {
    output = static_cast<InternalType>(input - m_Value);
  }

  /** Read access to the pixel */
  inline ExternalType
  Get(const InternalType & input) const
  {
    return static_cast<ExternalType>(input + m_Value);
  }

  /** Set the value to be added to pixels */
  void
  SetValue(const TPixel & newvalue)
  {
    m_Value = newvalue;
  }

  /** Get the value to be added to pixels */
  TPixel
  GetValue() const
  {
    return m_Value;
  }

  /** Assignment Operator */
  Self &
  operator=(const Self & apa)
  {
    this->m_Value = apa.m_Value;
    return *this;
  }

  /** Constructors */
  AddPixelAccessor()
    : m_Value(NumericTraits<TPixel>::ZeroValue())
  {}
  AddPixelAccessor(const Self & apa)
    : m_Value(apa.m_Value)
  {}

private:
  TPixel m_Value;
};
} // end namespace Accessor
} // end namespace itk

#endif
