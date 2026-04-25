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
#ifndef itkImageRandomNonRepeatingIteratorWithIndex_h
#define itkImageRandomNonRepeatingIteratorWithIndex_h

#include "itkImageRandomNonRepeatingConstIteratorWithIndex.h"
#include "itkImageIteratorWithIndex.h"

namespace itk
{
/** \class ImageRandomNonRepeatingIteratorWithIndex
 * \brief A multi-dimensional image iterator that visits image pixels within a
 * region in a random order, without repeating.
 *
 *  This class was contributed by Rupert Brooks, McGill Centre for Intelligent
 *  Machines, Montreal, Canada.  It is heavily based on the
 *  ImageRandomIterator class.
 *
 *  This iterator is a subclass of
 *  itk::ImageRandomNonRepeatingConstIteratorWithIndex that
 *  adds write-access functionality.  Please see
 *  itk::ImageRandomNonRepeatingConstIteratorWithIndex for more information.
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * \author Rupert Brooks, McGill Centre for Intelligent Machines. Canada
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
 * \sa ImageRandomNonRepeatingConstIteratorWithIndex  \sa ImageRandomNonRepeatingIteratorWithIndex
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
 *
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/AddNoiseToBinaryImage,Add Noise To Binary Image}
 * \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageRandomNonRepeatingIteratorWithIndex
  : public ImageRandomNonRepeatingIteratorWithIndexBase<TImage, /*VIsConst=*/false>
{
public:
  using Superclass = ImageRandomNonRepeatingIteratorWithIndexBase<TImage, /*VIsConst=*/false>;
  using Superclass::Superclass;
};

template <typename TImage>
ImageRandomNonRepeatingIteratorWithIndex(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageRandomNonRepeatingIteratorWithIndex<std::remove_const_t<TImage>>;

template <typename TImage>
ImageRandomNonRepeatingIteratorWithIndex(TImage *, const typename TImage::RegionType &)
  -> ImageRandomNonRepeatingIteratorWithIndex<TImage>;

template <typename TImage>
ImageRandomNonRepeatingIteratorWithIndex(const TImage *, const typename TImage::RegionType &)
  -> ImageRandomNonRepeatingIteratorWithIndex<TImage>;

} // end namespace itk

#endif
