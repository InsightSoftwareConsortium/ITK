/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFloodFilledSpatialFunctionConditionalIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFloodFilledSpatialFunctionConditionalIterator_h
#define __itkFloodFilledSpatialFunctionConditionalIterator_h

#include "itkFloodFilledSpatialFunctionConditionalConstIterator.h"

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
class ITK_EXPORT FloodFilledSpatialFunctionConditionalIterator: public FloodFilledSpatialFunctionConditionalConstIterator<TImage, TFunction>
{
public:
  /** Standard class typedefs. */
  typedef FloodFilledSpatialFunctionConditionalIterator Self;
  typedef FloodFilledSpatialFunctionConditionalConstIterator<TImage, TFunction> Superclass;

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

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * an explicit seed pixel for the flood fill, the "startIndex" */
  FloodFilledSpatialFunctionConditionalIterator(ImageType *imagePtr,
                                     FunctionType *fnPtr,
                                     IndexType startIndex): Superclass(imagePtr, fnPtr, startIndex) {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor
   * should be used when the seed pixel is unknown. */
  FloodFilledSpatialFunctionConditionalIterator(ImageType *imagePtr,
                                     FunctionType *fnPtr): Superclass(imagePtr, fnPtr) {}

  /** Get the pixel value, const version to avoid overload warnings */
  const PixelType & Get(void) const
    { return const_cast<ImageType *>(m_Image.GetPointer())->GetPixel(m_IndexStack.front() ); }

  /** Get the pixel value, non-const version is sometimes useful*/
  PixelType & Get(void)
    { return const_cast<ImageType *>(m_Image.GetPointer())->GetPixel(m_IndexStack.front() ); }

  /** Set the pixel value */
  void Set( const PixelType & value)
    { const_cast<ImageType *>(m_Image.GetPointer())->GetPixel(m_IndexStack.front() ) = value; }

  /** Default Destructor. */
  virtual ~FloodFilledSpatialFunctionConditionalIterator() {};
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFloodFilledSpatialFunctionConditionalIterator.txx"
#endif

#endif 
