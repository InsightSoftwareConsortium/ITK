/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConditionalIterator.h
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
#ifndef __itkConditionalIterator_h
#define __itkConditionalIterator_h

#include "itkIndex.h"

namespace itk
{

/**
 * \class ConditionalIterator
 * \brief ConditionalIterator is a base class for other iterators where
 * membership in the set of output pixels is "conditional" upon some
 * property, calculation, etc. For example, a threshold iterator might
 * walk a region and return only those pixels which meet a minimum
 * intensity condition.
 */

template<class TImage>
class ConditionalIterator {
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ConditionalIterator Self;
  
  /**
   * Dimension of the image the iterator walks.  This enum is needed so that
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks.
   */
  enum { NDimensions = TImage::ImageDimension };

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
   * Compute whether the index of interest should be included in the flood
   */
  virtual bool IsPixelIncluded(IndexType index) = 0;
  
  /**
   * operator= is provided to make sure the handle to the image is properly
   * reference counted.
   */
  Self &operator=(const Self& it)
  {
    m_Image = it.m_Image;     // copy the smart pointer
    m_Region = it.m_Region;   // copy the region
  }
  
  /**
   * Get the dimension (size) of the index.
   */
  static unsigned int GetIteratorDimension() 
    {return TImage::ImageDimension;}

  /**
   * Get the index at the current iterator location.
   */
  virtual const IndexType GetIndex() = 0;

  /**
   * Get the pixel value at the current iterator location.
   */
  virtual PixelType & Get(void) = 0;
  
  /**
   * Set the pixel value at the current iterator location.
   */
  virtual void Set( const PixelType & value) = 0;

  /**
   * Is the iterator at the end of the region?
   */
  virtual bool IsAtEnd() = 0;

  /**
   * Walk forward one index
   */
  virtual void operator++() = 0;

  
protected: //made protected so other iterators can access 

  /**
   * Smart pointer to the source image
   */
  SmartPointer<ImageType> m_Image;

  /*
   * Region type to iterate over
   */
  RegionType m_Region;

  /*
   * Is the iterator at the end of its walk?
   */
  bool m_IsAtEnd;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConditionalIterator.txx"
#endif

#endif 
