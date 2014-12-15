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
#ifndef itkGreenPixelAccessor_h
#define itkGreenPixelAccessor_h

#include "itkRGBPixel.h"

namespace itk
{
/**
 * \class GreenPixelAccessor
 * \brief Give access to the Green component of a RGBPixel type
 *
 * This class is intended to be used as parameter of
 * an ImageAdaptor to make an RGBPixel image appear as being
 * of scalar type T, showing only the Green component.
 *
 * \sa ImageAdaptor
 *
 * \ingroup ImageAdaptors
 *
 * \ingroup ITKImageAdaptors
 */

template< typename T >
class GreenPixelAccessor
{
public:
  /** Standard class typedefs. */
  typedef   GreenPixelAccessor Self;

  /** External typedef. It defines the external aspect
   * that this class will exhibit */
  typedef T ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data */
  typedef     RGBPixel< T > InternalType;

  /** Write access to the Green component */
  inline void Set(InternalType & output, const ExternalType & input) const
  { output.SetGreen(input); }

  /** Read access to the Green component */
  inline const ExternalType & Get(const InternalType & input) const
  { return input.GetGreen(); }

  bool operator!=(const Self & ) const
  {
    return false;
  }

  bool operator==(const Self & other) const
  {
    return !( *this != other );
  }
};
} // end namespace itk

#endif
