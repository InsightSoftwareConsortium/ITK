/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroFluxNeumannBoundaryCondition.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkZeroFluxNeumannBoundaryCondition_h
#define __itkZeroFluxNeumannBoundaryCondition_h
#include "itkNeighborhood.h"

namespace itk
{

/** \class ZeroFluxNeumannBoundaryCondition
 * \brief
 * A function object that determines a neighborhood of values at an
 * image boundary according to a Neumann boundary condition where first,
 * upwind derivatives on the boundary are zero.  This is a useful condition 
 * in solving some classes of differential equations. 
 *
 * For example, invoking this function object on a 7x5 iterator that masks
 * a region at an image corner (iterator is centered on the 2):
 *
 *               * * * * * * * 
 *               * * * * * * *
 *               * * 1 2 3 4 5  (where * denotes pixels that lie 
 *               * * 3 3 5 5 6          outside of the image boundary)
 *               * * 4 4 6 7 8
 *
 * returns the following neighborhood of values:
 *
 *               1 1 1 2 3 4 5
 *               1 1 1 2 3 4 5
 *               1 1 1 2 3 4 5
 *               3 3 3 3 5 5 6   (note the corner values)
 *               4 4 4 4 6 7 8
 *
 *
 * The input to this function object is a neighborhood iterator.  This boundary
 * condition object is designed to be given as a template argument to a
 * SmartNeighborhoodIterator or any of the SmartNeighborhoodIterator
 * subclasses.  It can also be used to override a default boundary condition
 * type in a SmartNeighborhoodIterator or any of the SmartNeighborhoodIterator
 * subclasses.
 * 
 *
 */

template<class TImage,
         class TNeighborhoodType = Neighborhood<ITK_TYPENAME TImage::PixelType*,
                                                TImage::ImageDimension > >
class  ZeroFluxNeumannBoundaryCondition
  : public ImageBoundaryCondition<TImage, TNeighborhoodType>
{
public:
  /**
   * Self & superclass typedefs
   */ 
  typedef ZeroFluxNeumannBoundaryCondition Self;
  typedef ImageBoundaryCondition<TImage, TNeighborhoodType> Superclass;

  /**
   * Extract information from the image type
   */
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::PixelPointerType PixelPointerType;
  enum { ImageDimension = Superclass::ImageDimension };
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::OffsetType OffsetType;

  /**
   * Default constructor.
   */
  ZeroFluxNeumannBoundaryCondition() {}

  /**
   * Computes and returns a neighborhood of appropriate values from
   * neighborhood iterator data..
   */
  virtual PixelType operator()(const OffsetType& point_index,
                               const OffsetType& boundary_offset,
                               const TNeighborhoodType *data) const; 
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroFluxNeumannBoundaryCondition.txx"
#endif

#endif
