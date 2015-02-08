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
#ifndef itkDefaultImageTraits_h
#define itkDefaultImageTraits_h
#if !defined( ITK_LEGACY_REMOVE )

#include "itkImageRegion.h"
#include "itkValarrayImageContainer.h"

namespace itk
{
/** \class DefaultImageTraits
 *
 * \brief Default ImageTraits for any PixelType.
 *
 * \sa Image
 * \deprecated
 * \ingroup ITKDeprecated
 * \ingroup ImageObjects
 */
template< typename TPixelType,
          unsigned int VImageDimension,
          typename TPixelContainer = ValarrayImageContainer< SizeValueType, TPixelType > >
class DefaultImageTraits
{
public:
  /** The pixel type of the image. */
  typedef TPixelType PixelType;

  /** The dimension of the image. */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  /** The container of Pixels for the image. */
  typedef TPixelContainer PixelContainer;

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index< VImageDimension > IndexType;

  /** Offset typedef support. An offset is used to access pixel values. */
  typedef Offset< VImageDimension > OffsetType;

  /** Size typedef support. A size is used to define region bounds. */
  typedef Size< VImageDimension > SizeType;

  /** Region typedef support. A region is used to specify a subset of an image.
    */
  typedef ImageRegion< VImageDimension > RegionType;
};
} // namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
