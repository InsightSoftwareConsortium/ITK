/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageBoundaryCondition.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageBoundaryCondition_h_
#define __itkImageBoundaryCondition_h_

#include "itkIndex.h"
#include "itkOffset.h"
#include "itkNeighborhood.h"

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
 * 
 * Associated Types                 Description
 * ----------------                 -----------
 * PixelType                         The data type of the return value.
 * PixelPointerType                  A pointer to PixelType.
 * PixelPointerTypeNeighborhood      A neighborhood of PixelPointerTypes
 *                                   that points to the pixel values in
 *                                   an image neighborhood.
 * 
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 */
template <class TImageType>
class ITK_EXPORT ImageBoundaryCondition
{
public:
  /** Extract information from the image type */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImageType::ImageDimension);

  /** Standard typedefs. */
  typedef ImageBoundaryCondition           Self;

  /** Extract information from the image type */
  typedef typename TImageType::PixelType PixelType;
  typedef typename TImageType::InternalPixelType *PixelPointerType;
  typedef Index<itkGetStaticConstMacro(ImageDimension)> IndexType;
  typedef Offset<itkGetStaticConstMacro(ImageDimension)> OffsetType;
  
  /** Type of the data container passed to this function object. */
  typedef Neighborhood<PixelPointerType,
                      itkGetStaticConstMacro(ImageDimension)> NeighborhoodType;

  /** Functor used to access pixels from a neighborhood of pixel pointers */
  typedef typename TImageType::NeighborhoodAccessorFunctorType 
                                 NeighborhoodAccessorFunctorType;
  
  /** Default constructor. */
  ImageBoundaryCondition() {}
  
  /** Returns a value for a given out-of-bounds pixel.  The arguments are the
   * phantom pixel (ND) index within the neighborhood, the pixel's offset from
   * the nearest image border pixel, and a neighborhood of pointers to pixel
   * values in the image.  */
  virtual PixelType operator()(const OffsetType& point_index,
                               const OffsetType &boundary_offset,
                               const NeighborhoodType *data) const = 0;
  
  /** Computes and returns the appropriate pixel value from
   * neighborhood iterator data, using the functor. */
  virtual PixelType operator()(
      const OffsetType& point_index,
      const OffsetType& boundary_offset,
      const NeighborhoodType *data,
      const NeighborhoodAccessorFunctorType &neighborhoodAccessorFunctor) const = 0;
  
  virtual ~ImageBoundaryCondition() {}

  /** Tell if the boundary condition can index to any location within
    * the associated iterator's neighborhood or if it has some limited
    * subset (such as none) that it relies upon.
    * Subclasses should override this method if they can safely limit
    * indexes to active pixels (or no pixels).
    */
  virtual bool RequiresCompleteNeighborhood() { return true; }
};
  
}// end namespace itk


#endif
