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
#ifndef itkImageSliceIteratorWithIndex_h
#define itkImageSliceIteratorWithIndex_h

#include "itkImageSliceConstIteratorWithIndex.h"
#include "itkImageIteratorWithIndex.h"

namespace itk
{
/** \class ImageSliceIteratorWithIndex
 * \brief A multi-dimensional image iterator that extends the
 * ImageLinearIteratorWithIndex from iteration along lines in an image to
 * iteration along both lines and planes (slices) within an image.
 *
 * A slice is defined as a 2D plane spanned by two vectors pointing along
 * orthogonal coordinate axes. Most of the functionality is inherited from
 * the ImageSliceConstIteratorWithIndex. The current class only adds
 * write access to image pixels. See ImageSliceConstIteratorWithIndex
 * for details.
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
 * \ingroup ITKCommon
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageSliceIteratorWithIndex
  : public ImageSliceIteratorWithIndexBase<TImage, /*VIsConst=*/false>
{
public:
  using Superclass = ImageSliceIteratorWithIndexBase<TImage, /*VIsConst=*/false>;
  using Superclass::Superclass;
};

template <typename TImage>
ImageSliceIteratorWithIndex(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageSliceIteratorWithIndex<std::remove_const_t<TImage>>;

template <typename TImage>
ImageSliceIteratorWithIndex(TImage *, const typename TImage::RegionType &) -> ImageSliceIteratorWithIndex<TImage>;

template <typename TImage>
ImageSliceIteratorWithIndex(const TImage *, const typename TImage::RegionType &) -> ImageSliceIteratorWithIndex<TImage>;

} // end namespace itk

#endif
