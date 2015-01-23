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
#ifndef itkVectorToRGBPixelAccessor_h
#define itkVectorToRGBPixelAccessor_h

#include "itkRGBPixel.h"
#include "itkVector.h"

namespace itk
{
namespace Accessor
{
/**
 * \class VectorToRGBPixelAccessor
 * \brief Give access to a Vector pixel type as if it were a RGBPixel type.
 *
 * This class is intended to be used as parameter of
 * an ImageAdaptor to make an Vector image appear as being
 * an image of RGB pixel type.
 *
 * \sa ImageAdaptor
 * \ingroup ImageAdaptors
 *
 * \ingroup ITKImageAdaptors
 */

template< typename T >
class VectorToRGBPixelAccessor
{
public:
  /** Standard class typedefs. */
  typedef   VectorToRGBPixelAccessor Self;

  /** External typedef. It defines the external aspect
    * that this class will exhibit */
  typedef   RGBPixel< T > ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data */
  typedef  Vector< T, 3 > InternalType;

  /** Write access to the VectorToRGB component */
  inline void Set(InternalType & output, const ExternalType & input) const
  {
    output[0] = input[0];
    output[1] = input[1];
    output[2] = input[2];
  }

  /** Read access to the VectorToRGB component */
  inline ExternalType Get(const InternalType & input) const
  {
    ExternalType rgb( input.GetDataPointer() );

    return rgb;
  }

private:
};
}  // end namespace Accessor
}  // end namespace itk

#endif
