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
#ifndef itkShapedNeighborhoodIterator_h
#define itkShapedNeighborhoodIterator_h

#include <vector>
#include <list>
#include "itkConstShapedNeighborhoodIterator.h"

namespace itk
{
/** \class ShapedNeighborhoodIterator
 *
 * \brief  A neighborhood iterator which can take on an arbitrary shape.
 *
 * \par General Information
 * The ShapedNeighborhoodIterator is a refinement of NeighborhoodIterator which
 * allows the user to specify which of the neighborhood elements are active and
 * which will be ignored.  This is useful for applications which only need to
 * work with a subset of the neighborhood around a pixel such as morphological
 * operations or cellular automata.  This iterator can also be used, for
 * example, to specify "cross-shaped" neighborhood where only elements along a
 * spatial axes are significant.
 *
 * \par Constructing a shaped neighborhood iterator
 * A shaped neighborhood iterator is constructed by constructing a list of
 * active neighbor locations.  The list is called the ActiveIndexList.  The
 * methods ActivateOffset, DeactivateOffset, and ClearActiveList are used to
 * construct the ActiveIndexList.  The argument to Activate/DeactivateOffset is
 * an itk::Offset which represents the ND spatial offset from the center of the
 * neighborhood.  For example, to activate the center pixel in the neighborhood,
 * you would do the following:
 *
   \code
   using ImageType = Image<float, 3>;
   ShapedNeighborhoodIterator<ImageType> it(radius, image, region);
   ShapedNeighborhoodIterator<ImageType>::OffsetType offset = {{0,0,0}};
   it.ActivateOffset(offset);
   \endcode
 *
 * where radius, image, and region are as described in NeighborhoodIterator.
 *
 * Once a neighborhood location has been activated, iteration (operator++,
 * operator--, operator+=, operator-=) will update the value at the active
 * location.  Note that values at inactive locations will NOT be valid if
 * queried.
 *
 * \par Accessing elements in a shaped neighborhood.
 * To access the value at an active neighborhood location, you can use the
 * standard GetPixel, SetPixel methods.  SetPixel is not defined for
 * ConstShapedNeighborhoodIterator.   The class will not complain if you
 * attempt to access a value at a non-active location, but be aware that the
 * result will be undefined.  Error checking is not done in this case for the
 * sake of efficiency.
 *
 * A second way to access active shaped neighborhood values is through a
 * ShapedNeighborhoodIterator::Iterator or
 * ConstShapedNeighborhoodIterator::ConstIterator.  The following example
 * demonstrates the use of these iterators.
 *
   \code
   using ImageType = Image<float, 3>;
   ShapedNeighborhoodIterator<ImageType> it(radius, image, region);
   ...
   it.ActivateOffset(offset1);
   it.ActivateOffset(offset2);
   it.ActivateOffset(offset3);
   // etc..
   ...
   ShapedNeighborhoodIterator<ImageType>::Iterator i;
   for (i = it.Begin(); ! i.IsAtEnd(); i++)
   { i.Set(i.Get() + 1.0); }
   // you may also use i != i.End(), but IsAtEnd() may be slightly faster.
   \endcode
 *
 * You can also iterate backward through the neighborhood active list.
 *
   \code
   i = it.End();
   i--;
   while (i != it.Begin())
   {
     i.Set(i.Get() + 1.0);
     i--;
   }
    i.Set(i.Get() + 1.0);
   \endcode
 *
 * The Get() Set() syntax was chosen versus defining operator* for these
 * iterators because lvalue vs. rvalue context information is needed to
 * determine whether bounds checking must take place.
 *
 * \sa Neighborhood
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
 * \sa ShapedImageNeighborhoodRange
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/IterateOverARegionWithAShapedNeighborhoodIterator,Iterate Over A Region With A Shaped
 Neighborhood Iterator Manually}
 * \sphinxexample{Core/Common/IterateOverARegionWithAShapedNeighborhoodIterator,Iterate Over A Region With A Shaped
 Neighborhood Iterator}
 * \endsphinx
 */
template <typename TImage, typename TBoundaryCondition = ZeroFluxNeumannBoundaryCondition<TImage>>
class ITK_TEMPLATE_EXPORT ShapedNeighborhoodIterator
  : public ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition>
{
public:
  /** Extract image type information. */
  using InternalPixelType = typename TImage::InternalPixelType;
  using PixelType = typename TImage::PixelType;

  /** Save the image dimension. */
  static constexpr unsigned int Dimension = TImage::ImageDimension;

  /** Standard class type aliases. */
  using Self = ShapedNeighborhoodIterator;
  using Superclass = ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition>;

  /** Inherit type alias from superclass */
  using OffsetType = typename Superclass::OffsetType;
  using OffsetValueType = typename OffsetType::OffsetValueType;
  using RadiusType = typename Superclass::RadiusType;
  using SizeType = typename Superclass::SizeType;
  using SizeValueType = typename Superclass::SizeValueType;
  using ConstIterator = typename Superclass::ConstIterator;
  using IndexListType = typename Superclass::IndexListType;
  using BoundaryConditionType = typename Superclass::BoundaryConditionType;
  using ImageBoundaryConditionPointerType = typename Superclass::ImageBoundaryConditionPointerType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using IndexType = typename Superclass::IndexType;
  using ImageType = typename Superclass::ImageType;
  using RegionType = typename Superclass::RegionType;
  using IndexValueType = typename Superclass::IndexValueType;

  /** An  iterator for the ShapedNeighborhood classes. */
  struct Iterator : public ConstIterator
  {
    Iterator() = default;
    Iterator(Self * s)
      : ConstIterator(s)
    {}

    ~Iterator() ITK_ITERATOR_OVERRIDE = default;
    Iterator &
    operator=(const Iterator & o)
    {
      ConstIterator::operator=(o);
      return *this;
    }

    // Promote to public
    void
    Set(const PixelType & v) const
    {
      ConstIterator::ProtectedSet(v);
    }

  protected:
    friend Self;

    Iterator(const Self * s, const typename IndexListType::const_iterator & li)
      : ConstIterator(s, li)
    {}
  };

  /** Default constructor */
  ShapedNeighborhoodIterator() = default;

  /** Virtual destructor */
  ~ShapedNeighborhoodIterator() override = default;

  /** Copy constructor */
  ShapedNeighborhoodIterator(const ShapedNeighborhoodIterator & o) = delete;

  /** Constructor which establishes the region size, neighborhood, and image
   * over which to walk. */
  ShapedNeighborhoodIterator(const SizeType & radius, const ImageType * ptr, const RegionType & region)
    : Superclass(radius, const_cast<ImageType *>(ptr), region)
  {}

  // Expose the following methods from the superclass.  This is a restricted
  // subset of the methods available for NeighborhoodIterator.
  using Superclass::SetPixel;
  using Superclass::SetCenterPixel;


  /** Assignment operator */
  Self &
  operator=(const Self & orig)
  {
    Superclass::operator=(orig);
    return *this;
  }

  /** Standard itk print method */
  void
  PrintSelf(std::ostream &, Indent) const override;

  /** Returns a const iterator for the neighborhood which points to the first
   * pixel in the neighborhood. */
  Iterator
  Begin()
  {
    return Iterator(this, this->m_ActiveIndexList.begin());
  }
  Iterator
  End()
  {
    return Iterator(this, this->m_ActiveIndexList.end());
  }

  using Superclass::Begin;
  using Superclass::End;

protected:
  friend Superclass;

  using NeighborIndexType = typename Superclass::NeighborIndexType;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShapedNeighborhoodIterator.hxx"
#endif

#endif
