/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFloodFilledImageFunctionConditionalIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFloodFilledImageFunctionConditionalIterator_h
#define __itkFloodFilledImageFunctionConditionalIterator_h

#include "itkFloodFilledImageFunctionConditionalConstIterator.h"

namespace itk
{

/**
 * \class FloodFilledImageFunctionConditionalIterator
 * \brief Iterates over a flood-filled image function. 
 *
 * \ingroup ImageIterators
 *
 */
template<class TImage, class TFunction>
class ITK_EXPORT FloodFilledImageFunctionConditionalIterator: public FloodFilledImageFunctionConditionalConstIterator<TImage, TFunction>
{
public:
  /** Standard class typedefs. */
  typedef FloodFilledImageFunctionConditionalIterator Self;
  typedef FloodFilledImageFunctionConditionalConstIterator<TImage, TFunction> Superclass;

  /** Type of function */
  typedef typename Superclass::FunctionType FunctionType;

  /** Type of vector used to store location info in the spatial function */
  typedef typename Superclass::FunctionInputType FunctionInputType;

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
  itkStaticConstMacro(NDimensions, unsigned int, Superclass::NDimensions);

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * an explicit seed pixel for the flood fill, the "startIndex" */
  FloodFilledImageFunctionConditionalIterator(ImageType *imagePtr,
                                     FunctionType *fnPtr,
                                     IndexType startIndex): Superclass(imagePtr, fnPtr, startIndex) {};

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * an explicit list of seed pixels for the flood fill, the "startIndex" */
  FloodFilledImageFunctionConditionalIterator(ImageType *imagePtr,
                                     FunctionType *fnPtr,
                                     std::vector<IndexType>& startIndex): Superclass(imagePtr, fnPtr, startIndex) {};

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor
   * should be used when the seed pixel is unknown. */
  FloodFilledImageFunctionConditionalIterator(ImageType *imagePtr,
                                     FunctionType *fnPtr): Superclass(imagePtr, fnPtr) {};

  /** Get the pixel value */
  const PixelType & Get(void) const
    { return const_cast<ImageType *>(m_Image.GetPointer())->GetPixel(m_IndexStack.top() ); }

  /** Set the pixel value */
  void Set( const PixelType & value)
    { const_cast<ImageType *>(m_Image.GetPointer())->GetPixel(m_IndexStack.top() ) = value; }

  /** Default Destructor. */
  virtual ~FloodFilledImageFunctionConditionalIterator() {};
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFloodFilledImageFunctionConditionalIterator.txx"
#endif

#endif 
