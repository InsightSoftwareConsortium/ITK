/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageBoundaryCondition.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef __itkImageBoundaryCondition_h_
#define __itkImageBoundaryCondition_h_

#include "itkImage.h"
#include "itkNeighborhood.h"
#include "itkImageTraits.h"

namespace itk
{

/**
 *\class ImageBoundaryCondition
 * \brief A virtual base object that defines an interface to a class of
 * boundary condition objects for use by neighborhood iterators.
 *
 * A boundary condition object supplies a phantom pixel value when
 * given a neighborhood of (pointers to) image values, the (ND) index of
 * the phantom pixel, and its (ND) offset from the boundary.  The index
 * of the phantom pixel is relative to the "upper left-hand corner" of
 * the neighborhood (as opposed to its center). 
 *
 * CONCEPT: ImageBoundaryCondition
 * 
 * Associated Types                 Description
 * ----------------                 -----------
 * PixelType                         The data type of the return value.
 * PixelPointerType                  A pointer to PixelType.
 * PixelPointerTypeNeighborhood      A neighborhood of PixelPointerTypes
 *                                   that points to the pixel values in
 *                                   an image neighborhood.
 */
template <class TImageType,
  class TNeighborhoodType
    = Neighborhood<ITK_TYPENAME ImageTraits<TImageType>::PixelType *,
                             ImageTraits<TImageType>::ImageDimension > >
class ITK_EXPORT ImageBoundaryCondition
{
public:
  /**
   * Extract information from the image type
   */
  typedef typename TImageType::PixelType PixelType;
  typedef typename TImageType::InternalPixelType *PixelPointerType;
  enum { ImageDimension = TImageType::ImageDimension };

  /**
   * Default constructor.
   */
  ImageBoundaryCondition() {}
  
  /**
   * Returns a value for a given out-of-bounds pixel.  The arguments are the
   * phantom pixel (ND) index within the neighborhood, the pixel's offset from
   * the nearest image border pixel, and a neighborhood of pointers to pixel
   * values in the image. 
   */
  virtual PixelType operator()(const int *point_index,
                               const int *boundary_offset,
                                 const TNeighborhoodType *data) const = 0;
};
  
}// end namespace itk


#endif
