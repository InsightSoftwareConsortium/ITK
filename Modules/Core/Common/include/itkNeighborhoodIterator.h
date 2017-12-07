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
#ifndef itkNeighborhoodIterator_h
#define itkNeighborhoodIterator_h

#include <vector>
#include <cstring>
#include <iostream>
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
/**
 * \class NeighborhoodIterator
 *
 * \brief Defines iteration of a local N-dimensional neighborhood of pixels
 * across an itk::Image.
 *
 * This class is a loose extension of the Standard Template Library (STL)
 * bi-directional iterator concept to \em masks of pixel neighborhoods within
 * itk::Image objects.  This NeighborhoodIterator base class defines simple
 * forward and reverse iteration of an N-dimensional neighborhood mask
 * across an image.  Elements within the mask can be accessed like elements
 * within an array.
 *
 * NeighborhoodIterators are designed to encapsulate some of the complexity of
 * working with image neighborhoods, complexity that would otherwise have to be
 * managed at the algorithmic level.  Use NeighborhoodIterators to simplify
 * writing algorithms that perform geometrically localized operations on images
 * (for example, convolution and morphological operations).
 *
 * To motivate the discussion of NeighborhoodIterators and their use in
 * Itk, consider the following code that takes directional derivatives at each
 * point in an image.
 *
\code
itk::NeighborhoodInnerProduct<ImageType> innerProduct;

itk::DerivativeOperator<ImageType> operator;
 operator->SetOrder(1);
 operator->SetDirection(0);
 operator->CreateDirectional();

itk::NeighborhoodIterator<ImageType>
  iterator(operator->GetRadius(), myImage, myImage->GetRequestedRegion());

iterator.SetToBegin();
while ( ! iterator.IsAtEnd() )
{
  std::cout << "Derivative at index " << iterator.GetIndex() << is <<
    innerProduct(iterator, operator) << std::endl;
  ++iterator;
}
\endcode
 *
 * Most of the work for the programmer in the code above is in setting up for
 * the iteration.  There are three steps.  First an inner product function
 * object is created which will be used to effect convolution with the
 * derivative kernel.  Setting up the derivative kernel, DerivativeOperator,
 * involves setting the order and direction of the derivative.  Finally, we
 * create an iterator over the RequestedRegion of the itk::Image (see Image)
 * using the radius of the derivative kernel as the size.
 *
 * Itk iterators only loosely follow STL conventions.  Notice that instead of
 * asking myImage for myImage.begin() and myImage.end(), iterator.SetToBegin()
 * and iterator.IsAtEnd() are called.  Itk iterators are typically more complex
 * objects than traditional, pointer-style STL iterators, and the increased
 * overhead required to conform to the complete STL API is not always
 * justified.
 *
 * The API for creating and manipulating a NeighborhoodIterator mimics
 * that of the itk::ImageIterators.  Like the itk::ImageIterator, a
 * ConstNeighborhoodIterator is defined on a region of interest in an itk::Image.
 * Iteration is constrained within that region of interest.
 *
 * A NeighborhoodIterator is constructed as a container of pointers (offsets)
 * to a geometric neighborhood of image pixels.  As the central pixel position
 * in the mask is moved around the image, the neighboring pixel pointers
 * (offsets) are moved accordingly.
 *
 * A \em pixel \em neighborhood is defined as a central pixel location and an
 * N-dimensional radius extending outward from that location.
 *
 * Pixels in a neighborhood can be accessed through a NeighborhoodIterator
 * like elements in an array.  For example, a 2D neighborhood with radius 2x1
 * has indices:
 *
\code
0  1  2  3  4
5  6  7  8  9
10 11 12 13 14
\endcode
 *
 * Now suppose a NeighborhoodIterator with the above dimensions is constructed
 * and positioned over a neighborhood of values in an Image:
 *
\code
1.2 1.3 1.8 1.4 1.1
1.8 1.1 0.7 1.0 1.0
2.1 1.9 1.7 1.4 2.0
\endcode
 *
 * Shown below is some sample pixel access code and the values that it returns.
 *
\code
SizeValueType c = (SizeValueType) (iterator.Size() / 2); // get offset of center pixel
SizeValueType s = iterator.GetStride(1);            // y-dimension step size

std::cout << iterator.GetPixel(7)      << std::endl;
std::cout << iterator.GetCenterPixel() << std::endl;
std::cout << iterator.GetPixel(c)      << std::endl;
std::cout << iterator.GetPixel(c-1)    << std::endl;
std::cout << iterator.GetPixel(c-s)    << std::endl;
std::cout << iterator.GetPixel(c-s-1)  << std::endl;
std::cout << *iterator[c]              << std::endl;
\endcode
 *
 * Results:
 *
\code
0.7
0.7
0.7
1.1
1.8
1.3
0.7
\endcode
 *
 * Use of GetPixel() is preferred over the *iterator[] form, and can be used
 * without loss of efficiency in most cases. Some variations (subclasses) of
 * NeighborhoodIterators may exist which do not support the latter
 * API. Corresponding SetPixel() methods exist to modify pixel values in
 * non-const NeighborhoodIterators.
 *
 * NeighborhoodIterators are "bidirectional iterators". They move only in two
 * directions through the data set.  These directions correspond to the layout
 * of the image data in memory and not to spatial directions of the
 * N-dimensional itk::Image.  Iteration always proceeds along the fastest
 * increasing dimension (as defined by the layout of the image data).  For
 * itk::Image this is the first dimension specified (i.e. for 3-dimensional
 * (x,y,z) NeighborhoodIterator proceeds along the x-dimension) (For random
 * access iteration through N-dimensional indices, use
 * RandomAccessNeighborhoodIterator).
 *
 * Each subclass of a ConstNeighborhoodIterator may also define its own
 * mechanism for iteration through an image.  In general, the Iterator does not
 * directly keep track of its spatial location in the image, but uses a set of
 * internal loop variables and offsets to trigger wraps at itk::Image region
 * boundaries, and to identify the end of the itk::Image region.
 *
 * \todo Better support for regions with negative indices.
 * \todo Add Begin() and End() methods?
 *
 * \sa DerivativeOperator \sa NeighborhoodInnerProduct
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
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Iterators/NeighborhoodIterator,Iterate over a region of an image with a neighborhood (with write access)}
 * \wikiexample{VectorImages/NeighborhoodIterator,NeighborhoodIterator on a VectorImage}
 * \endwiki
 */
template< typename TImage, typename TBoundaryCondition =
            ZeroFluxNeumannBoundaryCondition< TImage > >
class ITK_TEMPLATE_EXPORT NeighborhoodIterator:
  public ConstNeighborhoodIterator< TImage, TBoundaryCondition >
{
public:
  /** Standard class typedefs. */
  typedef NeighborhoodIterator                                    Self;
  typedef ConstNeighborhoodIterator< TImage, TBoundaryCondition > Superclass;

  /** Extract typedefs from superclass. */
  typedef typename Superclass::InternalPixelType InternalPixelType;
  typedef typename Superclass::PixelType         PixelType;
  typedef typename Superclass::SizeType          SizeType;
  typedef typename Superclass::ImageType         ImageType;
  typedef typename Superclass::RegionType        RegionType;
  typedef typename Superclass::IndexType         IndexType;
  typedef typename Superclass::OffsetType        OffsetType;
  typedef typename Superclass::RadiusType        RadiusType;
  typedef typename Superclass::NeighborhoodType  NeighborhoodType;
  typedef typename Superclass::Iterator          Iterator;
  typedef typename Superclass::ConstIterator     ConstIterator;
  typedef typename Superclass::ImageBoundaryConditionPointerType
  ImageBoundaryConditionPointerType;

  /** Default constructor. */
  NeighborhoodIterator():Superclass() {}

  /** Copy constructor */
  NeighborhoodIterator(const NeighborhoodIterator & n):Superclass(n) {}

  /** Assignment operator */
  Self & operator=(const Self & orig)
  {
    Superclass::operator=(orig);
    return *this;
  }

  /** Constructor which establishes the region size, neighborhood, and image
   * over which to walk. */
  NeighborhoodIterator(const SizeType & radius, ImageType *ptr,
                       const RegionType & region):
    Superclass(radius, ptr, region) {}

  /** Standard print method */
  virtual void PrintSelf(std::ostream &, Indent) const;

  /** Returns the central memory pointer of the neighborhood. */
  InternalPixelType * GetCenterPointer()
  { return ( this->operator[]( ( this->Size() ) >> 1 ) ); }

  /** Returns the central pixel of the neighborhood. */
  virtual void SetCenterPixel(const PixelType & p)
  { this->m_NeighborhoodAccessorFunctor.Set(this->operator[]( ( this->Size() ) >> 1 ), p); }

  /** Virtual function that replaces the pixel values in the image
   * neighborhood that are pointed to by this NeighborhoodIterator with
   * the pixel values contained in a Neighborhood. */
  virtual void SetNeighborhood(const NeighborhoodType &);

  /** Special SetPixel method which quietly ignores out-of-bounds attempts.
   *  Sets status TRUE if pixel has been set, FALSE otherwise.  */
  virtual void SetPixel(const unsigned i, const PixelType & v,
                        bool  & status);

  /** Set the pixel at the ith location. */
  virtual void SetPixel(const unsigned i, const PixelType & v);

  //  { *(this->operator[](i)) = v; }

  /** Set the pixel at offset o from the neighborhood center */
  virtual void SetPixel(const OffsetType o, const PixelType & v)
  { this->SetPixel(this->GetNeighborhoodIndex(o), v); }
  //  { *(this->operator[](o)) = v; }

  /** Sets the pixel value located i pixels distant from the neighborhood center in
      the positive specified "axis" direction. No bounds checking is done on
      the size of the neighborhood. */
  virtual void SetNext(const unsigned axis, const unsigned i,
                       const PixelType & v)
  {
    this->SetPixel(this->GetCenterNeighborhoodIndex()
                   + ( i * this->GetStride(axis) ), v);
  }

  /** Sets the pixel value located one pixel distant from the neighborhood center in
      the specifed positive axis direction. No bounds checking is done on the
      size of the neighborhood. */
  virtual void SetNext(const unsigned axis, const PixelType & v)
  {
    this->SetPixel(this->GetCenterNeighborhoodIndex()
                   + this->GetStride(axis), v);
  }

  /** Sets the pixel value located i pixels distant from the neighborhood center in
      the negative specified "axis" direction. No bounds checking is done on
      the size of the neighborhood. */
  virtual void SetPrevious(const unsigned axis, const unsigned i,
                           const PixelType & v)
  {
    this->SetPixel(this->GetCenterNeighborhoodIndex()
                   - ( i * this->GetStride(axis) ), v);
  }

  /** Sets the pixel value located one pixel distant from the neighborhood center in
      the specifed negative axis direction. No bounds checking is done on the
      size of the neighborhood. */
  virtual void SetPrevious(const unsigned axis,
                           const PixelType & v)
  {
    this->SetPixel(this->GetCenterNeighborhoodIndex()
                   - this->GetStride(axis), v);
  }
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodIterator.hxx"
#endif

#endif
