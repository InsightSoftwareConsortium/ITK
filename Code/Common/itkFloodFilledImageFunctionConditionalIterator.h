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

#include "itkFloodFilledFunctionConditionalIterator.h"

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
class FloodFilledImageFunctionConditionalIterator: public FloodFilledFunctionConditionalIterator<TImage, TFunction>
{
public:
  /** Standard class typedefs. */
  typedef FloodFilledImageFunctionConditionalIterator Self;
  typedef FloodFilledFunctionConditionalIterator<TImage, TFunction> Superclass;
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

  /** Dimension of the image the iterator walks.  This enum is needed so that
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  enum { NDimensions = Superclass::ImageDimension };

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  FloodFilledImageFunctionConditionalIterator(ImageType *imagePtr,
                                     FunctionType *fnPtr,
                                     IndexType startIndex): Superclass(imagePtr, fnPtr, startIndex) {};
  /** Default Destructor. */
  virtual ~FloodFilledImageFunctionConditionalIterator() {};

  /** Compute whether the index of interest should be included in the flood */
  bool IsPixelIncluded(IndexType index);
  
protected: //made protected so other iterators can access 

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFloodFilledImageFunctionConditionalIterator.txx"
#endif

#endif 
