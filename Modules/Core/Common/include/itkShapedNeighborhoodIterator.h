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
 * \code
 * typedef Image<float, 3> ImageType;
 * ShapedNeighborhoodIterator<ImageType> it(radius, image, region);
 * ShapedNeighborhoodIterator<ImageType>::OffsetType offset = {{0,0,0}};
 * it.ActivateOffset(offset);
 * \endcode
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
 * \code
 * typedef Image<float, 3> ImageType;
 * ShapedNeighborhoodIterator<ImageType> it(radius, image, region);
 * .
 * .
 * .
 * it.ActivateOffset(offset1);
 * it.ActivateOffset(offset2);
 * it.ActivateOffset(offset3);
 * etc..
 * .
 * .
 * .
 * ShapedNeighborhoodIterator<ImageType>::Iterator i;
 * for (i = it.Begin(); ! i.IsAtEnd(); i++)
 * { i.Set(i.Get() + 1.0); }
 * \\ you may also use i != i.End(), but IsAtEnd() may be slightly faster.
 * \endcode
 *
 * You can also iterate backward through the neighbohood active list.
 *
 * \code
 * i = it.End();
 * i--;
 * while (i != it.Begin())
 * {
 *   i.Set(i.Get() + 1.0);
 *   i--;
 * }
 *  i.Set(i.Get() + 1.0);
 * \endcode
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
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Iterators/ShapedNeighborhoodIterator_Manual,Iterate over a region of an image with a shaped neighborhood}
 * \wikiexample{Iterators/ShapedNeighborhoodIterator,Iterate over a region of an image with a shaped neighborhood}
 * \endwiki
 */
template< typename TImage,  typename TBoundaryCondition =
            ZeroFluxNeumannBoundaryCondition< TImage > >
class ITK_TEMPLATE_EXPORT ShapedNeighborhoodIterator:
  public ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition >
{
public:
  /** Extract image type information. */
  typedef typename TImage::InternalPixelType InternalPixelType;
  typedef typename TImage::PixelType         PixelType;

  /** Save the image dimension. */
  itkStaticConstMacro(Dimension, unsigned int, TImage::ImageDimension);

  /** Standard class typedefs. */
  typedef ShapedNeighborhoodIterator Self;
  typedef ConstShapedNeighborhoodIterator< TImage, TBoundaryCondition >
  Superclass;

  /** Inherit typedefs from superclass */
  typedef typename Superclass::OffsetType                        OffsetType;
  typedef typename OffsetType::OffsetValueType                   OffsetValueType;
  typedef typename Superclass::RadiusType                        RadiusType;
  typedef typename Superclass::SizeType                          SizeType;
  typedef typename Superclass::SizeValueType                     SizeValueType;
  typedef typename Superclass::ConstIterator                     ConstIterator;
  typedef typename Superclass::IndexListType                     IndexListType;
  typedef typename Superclass::BoundaryConditionType             BoundaryConditionType;
  typedef typename Superclass::ImageBoundaryConditionPointerType ImageBoundaryConditionPointerType;
  typedef typename Superclass::NeighborhoodType                  NeighborhoodType;
  typedef typename Superclass::IndexType                         IndexType;
  typedef typename Superclass::ImageType                         ImageType;
  typedef typename Superclass::RegionType                        RegionType;
  typedef typename Superclass::IndexValueType                    IndexValueType;

  /** An  iterator for the ShapedNeighborhood classes. */
  struct Iterator:public ConstIterator {
    Iterator() {}
    Iterator(Self *s):ConstIterator(s) {}

    ~Iterator() {}
    Iterator & operator=(const Iterator & o)
    {
      ConstIterator::operator=(o);
      return *this;
    }

    // Promote to public
    void Set(const PixelType & v) const
    { ConstIterator::ProtectedSet(v); }
  };

  /** Default constructor */
  ShapedNeighborhoodIterator()
  {
    m_BeginIterator = Iterator(this);
    m_EndIterator = Iterator(this);
    m_EndIterator.GoToEnd();
  }

  /** Virtual destructor */
  virtual ~ShapedNeighborhoodIterator() {}

  /** Constructor which establishes the region size, neighborhood, and image
   * over which to walk. */
  ShapedNeighborhoodIterator(const SizeType & radius,
                             const ImageType *ptr,
                             const RegionType & region
                             ):Superclass(radius, const_cast< ImageType * >( ptr ),
                                          region)
  {
    m_BeginIterator = Iterator(this);
    m_EndIterator = Iterator(this);
    m_EndIterator.GoToEnd();
  }

  // Expose the following methods from the superclass.  This is a restricted
  // subset of the methods available for NeighborhoodIterator.
  using Superclass::SetPixel;
  using Superclass::SetCenterPixel;


  /** Assignment operator */
  Self & operator=(const Self & orig)
  {
    Superclass::operator=(orig);

    // Reset begin and end pointer locations
    m_BeginIterator.GoToBegin();
    m_EndIterator.GoToEnd();
    return *this;
  }

  /** Standard itk print method */
  virtual void PrintSelf(std::ostream &, Indent) const;

  /** Returns a const iterator for the neighborhood which points to the first
   * pixel in the neighborhood. */
  Iterator & Begin() {    return m_BeginIterator;  }
  Iterator & End()   {   return m_EndIterator;     }

  /** Returns a const iterator for the neighborhood which points to the last
   * pixel in the neighborhood. */
  const ConstIterator & End() const
  { return this->m_ConstEndIterator; }

  void ClearActiveList()
  {
    Superclass::ClearActiveList();
    m_EndIterator.GoToEnd();
    m_BeginIterator.GoToBegin();
  }

protected:

  /** Copy constructor */
  ShapedNeighborhoodIterator(const ShapedNeighborhoodIterator & o) ITK_DELETED_FUNCTION;

  typedef typename Superclass::NeighborIndexType NeighborIndexType;

  void ActivateIndex(NeighborIndexType n)
  {
    Superclass::ActivateIndex(n);
    m_EndIterator.GoToEnd();
    m_BeginIterator.GoToBegin();
  }

  void DeactivateIndex(NeighborIndexType n)
  {
    Superclass::DeactivateIndex(n);
    m_EndIterator.GoToEnd();
    m_BeginIterator.GoToBegin();
  }

  Iterator m_EndIterator;
  Iterator m_BeginIterator;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapedNeighborhoodIterator.hxx"
#endif

#endif
