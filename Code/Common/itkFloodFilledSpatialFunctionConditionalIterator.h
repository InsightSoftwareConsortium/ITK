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

#include "itkFloodFilledFunctionConditionalIterator.h"

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
class FloodFilledSpatialFunctionConditionalIterator: public FloodFilledFunctionConditionalIterator<TImage, TFunction>
{
public:
  /** Standard class typedefs. */
  typedef FloodFilledSpatialFunctionConditionalIterator Self;
  typedef FloodFilledFunctionConditionalIterator<TImage, TFunction> Superclass;
  /** Type of function */
  typedef Superclass::FunctionType FunctionType;

  /** Type of vector used to store location info in the spatial function */
  typedef Superclass::FunctionInputType FunctionInputType;

  /** Index typedef support. */
  typedef Superclass::IndexType  IndexType;

  /** Size typedef support. */
  typedef Superclass::SizeType    SizeType;

  /** Region typedef support */
  typedef Superclass::RegionType    RegionType;

  /** Image typedef support. */
  typedef Superclass::ImageType   ImageType;

  /** Internal Pixel Type */
  typedef Superclass::InternalPixelType   InternalPixelType;

  /** External Pixel Type */
  typedef Superclass::PixelType   PixelType;

  /** Dimension of the image the iterator walks.  This enum is needed so that
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  enum { NDimensions = TImage::ImageDimension };

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  FloodFilledSpatialFunctionConditionalIterator(ImageType *imagePtr,
                                     FunctionType *fnPtr,
                                     IndexType startIndex): Superclass(imagePtr, fnPtr, startIndex) {};
  /** Default Destructor. */
  virtual ~FloodFilledSpatialFunctionConditionalIterator() {};

  /** Compute whether the index of interest should be included in the flood */
  bool IsPixelIncluded(IndexType index);
  
protected: //made protected so other iterators can access 

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFloodFilledSpatialFunctionConditionalIterator.txx"
#endif

#endif 
