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
#ifndef itkConditionalConstIterator_h
#define itkConditionalConstIterator_h

#include "itkIndex.h"

namespace itk
{
/** \class ConditionalConstIterator
 *  \brief A base class for other iterators where membership in the set
 *         of output pixels is conditional upon some property, calculation,
 *         etc. For example, a threshold iterator might walk a region and
 *         return only those pixels which meet a minimum intensity condition.
 *
 * This class is the const version of the ConditionalIterator.
 * For this reason, it doesn't support the Set() method.
 *
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ConditionalConstIterator
{
public:
  /** Standard class typedefs. */
  typedef ConditionalConstIterator Self;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * that functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(NDimension, unsigned int, TImage::ImageDimension);

  /** Index typedef support. */
  typedef typename TImage::IndexType IndexType;

  /** Size typedef support. */
  typedef typename TImage::SizeType SizeType;

  /** Region typedef support. */
  typedef typename TImage::RegionType RegionType;

  /** Image typedef support. */
  typedef TImage ImageType;

  /** Internal Pixel Type */
  typedef typename TImage::InternalPixelType InternalPixelType;

  /** External Pixel Type */
  typedef typename TImage::PixelType PixelType;

  /** Compute whether the index of interest should be included in the flood */
  virtual bool IsPixelIncluded(const IndexType & index) const = 0;

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self & operator=(const Self & it)
  {
    m_IsAtEnd = it.m_IsAtEnd; // copy the end flag
    m_Image = it.m_Image;     // copy the smart pointer
    m_Region = it.m_Region;   // copy the region
    return *this;
  }

  /** Get the dimension (size) of the index. */
  static unsigned int GetIteratorDimension(void)
  {
    return Self::NDimension;
  }

  /** Get the index at the current iterator location. */
  virtual const IndexType GetIndex() = 0;

  /** Get the pixel value at the current iterator location. */
  virtual const PixelType Get(void) const = 0;

  /** Is the iterator at the end of the region? */
  virtual bool IsAtEnd() = 0;

  /** Walk forward one index. */
  virtual void operator++() = 0;

  /** Constructor */
  ConditionalConstIterator();

  /** Destructor */
  virtual ~ConditionalConstIterator();

protected: //made protected so other iterators can access
  /** Smart pointer to the source image. */
  //SmartPointer<const ImageType> m_Image;
  typename ImageType::ConstWeakPointer m_Image;

  /** Region type to iterate over. */
  RegionType m_Region;

  /** Is the iterator at the end of its walk? */
  bool m_IsAtEnd;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConditionalConstIterator.hxx"
#endif

#endif
