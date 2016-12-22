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
#ifndef itkDefaultPixelAccessor_h
#define itkDefaultPixelAccessor_h

#include "itkMacro.h"

namespace itk
{
/**
 * \class DefaultPixelAccessor
 * \brief Give access to partial aspects a type
 *
 * DefaultPixelAccessor is templated over an internal type and an
 * external type representation. This class encapsulates a
 * customized conversion between the internal and external
 * type representations.
 *
 * PixelAccessor is the class responsible for pixel-to-pixel
 * transformation during image data access. The DefaultPixelAccessor
 * is an identity operation on the pixel value. It only exist in
 * order to standarize the way in which pixels are accessed.
 *
 * PixelAccessor are used by ImageAdaptors in order to present
 * an Image as being of a different type. The usual application
 * of ImageAccessors is Image casting, by avoiding the overhead
 * of an ImageFilter that performs the complete transformation
 *
 * \sa ImageAdaptor
 * \sa PixelAccessor
 * \sa Image
 *
 * \ingroup ImageAdaptors
 *
 * \ingroup ITKCommon
 */

template< typename TType >
class ITK_TEMPLATE_EXPORT DefaultPixelAccessor
{
public:

  DefaultPixelAccessor() {}
  virtual ~DefaultPixelAccessor() {}

  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef TType ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data. */
  typedef TType InternalType;

  /** Set the pixel. */
  inline void Set(TType & output, const TType & input) const
  { output = input; }

  /** Get the pixel. */
  inline TType & Get(TType & input) const
  { return input; }

  /** Get a const reference to the pixel. */
  inline const TType & Get(const TType & input) const
  { return input; }
};
} // end namespace itk

#endif
