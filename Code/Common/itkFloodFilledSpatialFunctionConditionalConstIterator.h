/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFloodFilledSpatialFunctionConditionalConstIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFloodFilledSpatialFunctionConditionalConstIterator_h
#define __itkFloodFilledSpatialFunctionConditionalConstIterator_h

#include "itkFloodFilledFunctionConditionalConstIterator.h"

namespace itk
{

/**
 * \class FloodFilledSpatialFunctionConditionalConstIterator
 * \brief Iterates over a flood-filled spatial function. 
 *
 * \ingroup ImageIterators
 *
 */
template<class TImage, class TFunction>
class FloodFilledSpatialFunctionConditionalConstIterator: public FloodFilledFunctionConditionalConstIterator<TImage, TFunction>
{
public:
  /** Standard class typedefs. */
  typedef FloodFilledSpatialFunctionConditionalConstIterator Self;
  typedef FloodFilledFunctionConditionalConstIterator<TImage, TFunction> Superclass;
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
   * particular region of that image. */
  FloodFilledSpatialFunctionConditionalConstIterator(const ImageType *imagePtr,
                                     FunctionType *fnPtr,
                                     IndexType startIndex): Superclass(imagePtr, fnPtr, startIndex) {};
  /** Default Destructor. */
  virtual ~FloodFilledSpatialFunctionConditionalConstIterator() {};

  /** Compute whether the index of interest should be included in the flood */
  bool IsPixelIncluded(const IndexType & index) const;
  
protected: //made protected so other iterators can access 

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFloodFilledSpatialFunctionConditionalConstIterator.txx"
#endif

#endif 
