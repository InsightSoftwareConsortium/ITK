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
#ifndef itkImageRegionIteratorWithIndex_h
#define itkImageRegionIteratorWithIndex_h

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageIteratorWithIndex.h"

namespace itk
{
/** \class ImageRegionIteratorWithIndex
 * \brief A multi-dimensional iterator templated over image type that walks
 * pixels within a region and is specialized to keep track of its image index
 * location.
 *
 * This class is a specialization of ImageRegionConstIteratorWithIndex that
 * adds write-access (the Set() method).  Please see
 * ImageRegionConstIteratorWithIndex for more information.
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * \ingroup ImageIterators
 *
 * \sa ImageConstIterator \sa ConditionalConstIterator
 * \sa ConstNeighborhoodIterator \sa ConstShapedNeighborhoodIterator
 * \sa ConstSliceIterator  \sa CorrespondenceDataStructureIterator
 * \sa FloodFilledFunctionConditionalConstIterator
 * \sa FloodFilledImageFunctionConditionalConstIterator
 * \sa FloodFilledImageFunctionConditionalIterator
 * \sa FloodFilledSpatialFunctionConditionalConstIterator
 * \sa FloodFilledSpatialFunctionConditionalIterator
 * \sa ImageConstIterator \sa ImageConstIteratorWithIndex
 * \sa ImageIterator \sa ImageIteratorWithIndex
 * \sa ImageLinearConstIteratorWithIndex  \sa ImageLinearIteratorWithIndex
 * \sa ImageRandomConstIteratorWithIndex  \sa ImageRandomIteratorWithIndex
 * \sa ImageRegionConstIterator \sa ImageRegionConstIteratorWithIndex
 * \sa ImageRegionExclusionConstIteratorWithIndex
 * \sa ImageRegionExclusionIteratorWithIndex
 * \sa ImageRegionIterator  \sa ImageRegionIteratorWithIndex
 * \sa ImageRegionReverseConstIterator  \sa ImageRegionReverseIterator
 * \sa ImageReverseConstIterator  \sa ImageReverseIterator
 * \sa ImageSliceConstIteratorWithIndex  \sa ImageSliceIteratorWithIndex
 * \sa NeighborhoodIterator \sa PathConstIterator  \sa PathIterator
 * \sa ShapedNeighborhoodIterator  \sa SliceIterator
 * \sa ImageConstIteratorWithIndex
 * \sa ImageRegionRange
 * \sa ImageRegionIndexRange
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/IterateRegionWithAccessToIndexWithWriteAccess,Iterate Region In Image With Access To
 * Current Index With Write Access} \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageRegionIteratorWithIndex
  : public ImageRegionIteratorWithIndexBase<TImage, /*VIsConst=*/false>
{
public:
  using Superclass = ImageRegionIteratorWithIndexBase<TImage, /*VIsConst=*/false>;
  using Superclass::Superclass;
};

template <typename TImage>
ImageRegionIteratorWithIndex(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageRegionIteratorWithIndex<std::remove_const_t<TImage>>;

template <typename TImage>
ImageRegionIteratorWithIndex(TImage *, const typename TImage::RegionType &) -> ImageRegionIteratorWithIndex<TImage>;

template <typename TImage>
ImageRegionIteratorWithIndex(const TImage *, const typename TImage::RegionType &)
  -> ImageRegionIteratorWithIndex<TImage>;

} // end namespace itk

#endif
