/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFloodFilledSpatialFunctionConditionalIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkFloodFilledSpatialFunctionConditionalIterator_h
#define __itkFloodFilledSpatialFunctionConditionalIterator_h

#include <stack>

#include "itkIndex.h"
#include "itkSize.h"
#include "itkConditionalIterator.h"
#include "itkImage.h"

namespace itk
{

/**
 * \class FloodFilledSpatialFunctionConditionalIterator
 * \brief Iterates over a flood-filled spatial function. 
 *
 * \ingroup ImageIterators
 *
 */
template<class TImage, class TFunction>
class FloodFilledSpatialFunctionConditionalIterator: public ConditionalIterator<TImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FloodFilledSpatialFunctionConditionalIterator Self;

  /**
   * Type of function
   */
  typedef TFunction FunctionType;

  /**
   * Type of vector used to store location info in the spatial function
   */
  typedef typename TFunction::TPositionType PositionType;

  /** 
   * Index typedef support.
   */
  typedef typename TImage::IndexType  IndexType;

  /** 
   * Size typedef support.
   */
  typedef typename TImage::SizeType    SizeType;

  /*
   * Region typedef support
   */
  typedef typename TImage::RegionType    RegionType;

  /**
   * Image typedef support.
   */
  typedef TImage   ImageType;

  /**
   * Internal Pixel Type
   */
  typedef typename TImage::InternalPixelType   InternalPixelType;

  /**
   * External Pixel Type
   */
  typedef typename TImage::PixelType   PixelType;

  /**
   * Dimension of the image the iterator walks.  This enum is needed so that
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks.
   */
  enum { NDimensions = TImage::ImageDimension };

  /**
   * Constructor establishes an iterator to walk a particular image and a
   * particular region of that image.
   */
  FloodFilledSpatialFunctionConditionalIterator(ImageType *imagePtr,
                                     FunctionType *fnPtr,
                                     IndexType startIndex);
  /**
   * Default Destructor.
   */
  virtual ~FloodFilledSpatialFunctionConditionalIterator() {};

  /**
   * Compute whether the index of interest should be included in the flood
   */
  bool IsPixelIncluded(IndexType index);
  
  /**
   * operator= is provided to make sure the handle to the image is properly
   * reference counted.
   */
  Self &operator=(const Self& it)
  {
    m_Image = it.m_Image;     // copy the smart pointer
    m_Region = it.m_Region;
  }
  
  /**
   * Get the dimension (size) of the index.
   */
  static unsigned int GetFloodFilledSpatialFunctionConditionalIteratorDimension() 
    {return TImage::ImageDimension;}

  /**
   * Get the index. This provides a read only reference to the index.
   * This causes the index to be calculated from pointer arithmetic and is
   * therefore an expensive operation.
   * \sa SetIndex
   */
  const IndexType GetIndex()
    { return m_IndexStack.top();}

  /**
   * Get the pixel value
   */
  PixelType & Get(void)
  { return m_Image->GetPixel(m_IndexStack.top() ); }
  
  /**
   * Set the pixel value
   */
  void Set( const PixelType & value)
  { m_Image->GetPixel(m_IndexStack.top() ) = value; }

  /**
   * Is the iterator at the end of the region?
   */
  bool IsAtEnd()
  { return m_IsAtEnd; };

  /**
   * Walk forward one index
   */
  void
  operator++()
  { this->DoFloodStep(); }

  void DoFloodStep();
  
protected: //made protected so other iterators can access 

  /**
   * Smart pointer to the function we're evaluating
   */
  SmartPointer<FunctionType> m_Function;

  /**
   * A temporary image used for storing info about indices
   * 0 = pixel has not yet been processed
   * 1 = pixel is not inside the function
   * 2 = pixel is inside the function, neighbor check incomplete
   * 3 = pixel is inside the function, neighbor check complete
   */
  typedef Image<unsigned char, NDimensions> TTempImage;
  typename TTempImage::Pointer tempPtr;

  /**
   * A known seed location to start the recursive fill
   */
  IndexType m_StartIndex;

  /**
   * The origin of the source image
   */
  const double* m_ImageOrigin;
  
  /**
   * The spacing of the source image
   */
  const double* m_ImageSpacing;

  /**
   * Size of the source image
   */
  const unsigned long int* m_ImageSize;

  /**
   * Stack used to hold the path of the iterator through the image
   */
  std::stack<IndexType> m_IndexStack;

  /**
   * Location vector used in the flood algorithm
   */
  PositionType m_LocationVector;


  bool m_FoundUncheckedNeighbor;
  bool m_IsValidIndex;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFloodFilledSpatialFunctionConditionalIterator.txx"
#endif

#endif 
