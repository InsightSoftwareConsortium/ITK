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
#ifndef itkRGBToVectorPixelAccessor_h
#define itkRGBToVectorPixelAccessor_h

#include "itkRGBPixel.h"
#include "itkVector.h"

namespace itk
{
namespace Accessor
{
/**
 * \class RGBToVectorPixelAccessor
 * \brief Give access to a RGBPixel as if it were a Vector type.
 *
 * This class is intended to be used as parameter of
 * an ImageAdaptor to make an RGBPixel image appear as being
 * an image of Vector pixel type.
 *
 * \sa ImageAdaptor
 * \ingroup ImageAdaptors
 *
 * \ingroup ITKImageAdaptors
 */

template <typename T>
class RGBToVectorPixelAccessor
{
public:
  /** Standard class type aliases. */
  using Self = RGBToVectorPixelAccessor;

  /** External type alias. It defines the external aspect
   * that this class will exhibit */
  using ExternalType = Vector<T, 3>;

  /** Internal type alias. It defines the internal real
   * representation of data */
  using InternalType = RGBPixel<T>;

  /** Write access to the RGBToVector component */
  inline void
  Set(InternalType & output, const ExternalType & input) const
  {
    output[0] = input[0];
    output[1] = input[1];
    output[2] = input[2];
  }

  /** Read access to the RGBToVector component */
  inline ExternalType
  Get(const InternalType & input) const
  {
    ExternalType v(input.GetDataPointer());
    return v;
  }

private:
};
} // end namespace Accessor
} // end namespace itk

#endif
