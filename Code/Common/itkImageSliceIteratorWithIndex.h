/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSliceIteratorWithIndex.h
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
#ifndef __itkImageSliceIteratorWithIndex_h
#define __itkImageSliceIteratorWithIndex_h

#include "itkImageIteratorWithIndex.h"

namespace itk
{

/**
 * \class ImageSliceIteratorWithIndex
 * \brief Multi-dimensional image iterator which walks a region Slice by Slice.
  *
 * This is the typical use of this iterator in a loop:
 *
 * \code
 *  
 * ImageSliceIteratorWithIndex<ImageType> it( image, image->GetRequestedRegion() );
 * 
 * it.SetFirstDirection(2);
 * it.SetSecondDirection(0);
 *
 * it.GoToBegin();
 * while( !it.IsAtEnd() )
 * {
 *   while( !it.IsAtEndOfSlice() )
 *   {
 *     while( !it.IsAtEndOfLine() )
 *     {
 *        value = it.Get();  
 *        it.Set( value * 2 );
 *        ++it;
 *     }
 *     it.NextLine();
 *   }
 *   it.NextSlice();
 *  } 
 *
 *  \endcode
 *
 * \example  Common/itkImageSliceIteratorWithIndex.cxx
 *
 *
 * 
 *
 */
template<typename TImage>
class ImageSliceIteratorWithIndex : public ImageIteratorWithIndex<TImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageSliceIteratorWithIndex Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageIteratorWithIndex<TImage>  Superclass;

  /** 
   * Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index.
   */
  typedef typename TImage::IndexType IndexType;

  /**
   * Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index.
   */
  typedef TImage ImageType;

  /** 
   * Region typedef support.
   */
  typedef typename TImage::RegionType   RegionType;

  /** 
   * PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef typename TImage::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /**
   * Default constructor. Needed since we provide a cast constructor.
   */
  ImageSliceIteratorWithIndex() : ImageIteratorWithIndex<TImage>() {}
  
  /**
   * Constructor establishes an iterator to walk a particular image and a
   * particular region of that image.
   */
  ImageSliceIteratorWithIndex( ImageType *ptr,
                      const RegionType & region)
    : ImageIteratorWithIndex<TImage>(ptr, region) 
    {
      m_Direction_A = 0;
      m_Direction_B = 1;
    }



  /**
   * Constructor that can be used to cast from an ImageIterator to an
   * ImageSliceIteratorWithIndex. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageSliceIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageSliceIteratorWithIndex.
   */
  ImageSliceIteratorWithIndex( const ImageIteratorWithIndex<TImage> &it)
    { this->ImageIteratorWithIndex<TImage>::operator=(it); }



  /**
   * Go to the next line
   * \sa operator++
   * \sa operator--
   * \sa PreviousLine
   * \sa EndOfLine
   * \sa End
   * \sa NextSlice
   */
  void NextLine(void);

  
  /**
   * Go to the next slice
   * \sa operator++
   * \sa operator--
   * \sa PreviousSlice
   * \sa EndOfLine
   * \sa End
   */
  void NextSlice(void);



  /**
   * Go to the previous line
   * \sa operator++
   * \sa operator--
   * \sa NextLine
   * \sa EndOfLine
   * \sa End
   * \sa NextSlice
   */
  void PreviousLine(void);

  
  /**
   * Go to the previous slice
   * \sa operator++
   * \sa operator--
   * \sa NextSlice
   * \sa EndOfLine
   * \sa End
   */
  void PreviousSlice(void);



  /**
   * Test if the index is at the end of line
   */
  bool IsAtEndOfLine(void);


   /**
   * Test if the index is at the end of the slice
   */
  bool IsAtEndOfSlice(void);

  /**
   * Test if the index is at the begining of line
   */
  bool IsAtBeginOfLine(void);


   /**
   * Test if the index is at the begining of the slice
   */
  bool IsAtBeginOfSlice(void);



  /**
   * Set the fastest direction of movement
   */
  void SetFirstDirection(unsigned int direction);


  /**
   * Set the second fastest direction of movement
   */
  void SetSecondDirection(unsigned int direction);
  

  /**
   * Increment (prefix) the selected dimension.
   * No bounds checking is performed. 
   * \sa GetIndex
   * \sa operator--
   */
  Self & operator++();


  /**
   * Decrement (prefix) the selected dimension.
   * No bounds checking is performed. 
   * \sa GetIndex
   * \sa operator++
   */
  Self & operator--();


private:
    unsigned long  m_Jump_A;
    unsigned long  m_Jump_B;
    unsigned int   m_Direction_A;
    unsigned int   m_Direction_B;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSliceIteratorWithIndex.txx"
#endif

#endif 
