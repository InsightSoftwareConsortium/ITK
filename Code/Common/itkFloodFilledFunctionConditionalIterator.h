/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFloodFilledFunctionConditionalIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFloodFilledFunctionConditionalIterator_h
#define __itkFloodFilledFunctionConditionalIterator_h

#include <stack>

#include "itkImage.h"
#include "itkFloodFilledFunctionConditionalConstIterator.h"

namespace itk
{

/**
 * \class FloodFilledFunctionConditionalIterator
 * \brief Base class for non-const flood-filled function iterators. 
 *
 * \ingroup ImageIterators
 *
 */
template<class TImage, class TFunction>
class ITK_EXPORT FloodFilledFunctionConditionalIterator: public FloodFilledFunctionConditionalConstIterator<TImage, TFunction>
{
public:
  /** Standard class typedefs. */
  typedef FloodFilledFunctionConditionalIterator Self;
  typedef FloodFilledFunctionConditionalConstIterator<TImage, TFunction> Superclass;

  /** Type of function */
  typedef TFunction FunctionType;

  /** Type of vector used to store location info in the spatial function */
  typedef typename TFunction::FunctionInputType FunctionInputType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType  IndexType;

  /** Size typedef support. */
  typedef typename Superclass::SizeType    SizeType;

  /** Region typedef support */
  typedef typename Superclass::RegionType    RegionType;

  /** Image typedef support. */
  typedef typename Superclass::ImageType   ImageType;

  /** Internal Pixel Type */
  typedef typename Superclass::InternalPixelType   InternalPixelType;

  /** External Pixel Type */
  typedef typename Superclass::PixelType   PixelType;

  /** Dimension of the image the iterator walks.  This constant is needed so 
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(NDimensions, unsigned int, TImage::ImageDimension);

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * an explicit seed pixel for the flood fill, the "startIndex" */
  FloodFilledFunctionConditionalIterator(ImageType *imagePtr,
                                         FunctionType *fnPtr,
                                         IndexType startIndex) :
                                      Superclass(imagePtr, fnPtr, startIndex){};

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * an explicit list of seeds for the flood fill, the "startIndex" */
  FloodFilledFunctionConditionalIterator(ImageType *imagePtr,
                                         FunctionType *fnPtr,
                                         std::vector<IndexType>& startIndex) :
                                      Superclass(imagePtr, fnPtr, startIndex){};

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor
   * should be used when the seed pixel is unknown. */
  FloodFilledFunctionConditionalIterator(ImageType *imagePtr,
                                         FunctionType *fnPtr) :
                                      Superclass(imagePtr, fnPtr){}; 

  /** Default Destructor. */
  virtual ~FloodFilledFunctionConditionalIterator() {};
  
  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &operator=(const Self& it)
  {
    m_Image = it.m_Image;     // copy the smart pointer
    m_Region = it.m_Region;
  }

  /** Get the pixel value */
  PixelType & Get(void)
    { return const_cast<ImageType *>(m_Image.GetPointer())->GetPixel(m_IndexStack.front() ); }

  /** Set the pixel value */
  void Set( const PixelType & value)
    { const_cast<ImageType *>(m_Image.GetPointer())->GetPixel(m_IndexStack.front() ) = value; }

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFloodFilledFunctionConditionalIterator.txx"
#endif

#endif 
