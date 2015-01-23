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
#ifndef itkPixelAccessor_h
#define itkPixelAccessor_h

#include "itkMacro.h"
namespace itk
{
/** \class PixelAccessor
 * \brief Give access to partial aspects of a type
 *
 * PixelAccessor is templated over an internal type and an
 * external type representation. This class encapsulates a
 * customized conversion between the internal and external
 * type representations.
 *
 * PixelAccessor is designed to be used in conjuntion with
 * ImageAdaptors. An ImageAdaptor take an image and present it
 * as another image in which the pixels are a pixel-to-pixel
 * modification of the original image.
 *
 * ImageAdaptors are intended to perform task similar to ImageFilters,
 * but reducing the overhead in memory allocation, at the price of some
 * reduction in performance.
 *
 * ImageAdaptors are templated over a PixelAccessor class that
 * will define what kind of transformation is applied to the
 * pixel data.  Typical uses of PixelAccessor include basic type
 * casting, (e.g. make a float image looks like a unsigned int image).
 *
 * Every Image has a default PixelAccessor that performs an identity
 * operation. ImageIterators use the PixelAccessor defined by the image
 * in order to get and set the values of pixels.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template< typename TInternalType, typename TExternalType >
class PixelAccessor
{
public:
  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef TExternalType ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data. */
  typedef TInternalType InternalType;

  inline void Set(TInternalType & output, const TExternalType & input) const
  { output = (TInternalType)input; }

  inline TExternalType Get(const TInternalType & input) const
  { return (TExternalType)input; }
};
} // end namespace itk

#endif
