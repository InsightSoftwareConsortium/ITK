/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroFluxNeumannBoundaryCondition.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkZeroFluxNeumannBoundaryCondition_h
#define __itkZeroFluxNeumannBoundaryCondition_h
#include "itkNeighborhood.h"
#include "itkImageBoundaryCondition.h"

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
 * The input to this function object is a neighborhood iterator.  This boundary
 * condition object is designed to be given as a template argument to a
 * NeighborhoodIterator or any of the NeighborhoodIterator
 * subclasses.
 * 
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 */
template<class TImage>
class ITKCommon_EXPORT  ZeroFluxNeumannBoundaryCondition
  : public ImageBoundaryCondition<TImage>
{
public:
  /** Standard class typedefs. */ 
  typedef ZeroFluxNeumannBoundaryCondition Self;
  typedef ImageBoundaryCondition<TImage> Superclass;
  
  /** Extract information from the image type. */
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::PixelPointerType PixelPointerType;
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::OffsetType OffsetType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  
  /** Extract information from the image type. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);
  
  /** Default constructor. */
  ZeroFluxNeumannBoundaryCondition() {}

  /** Computes and returns a neighborhood of appropriate values from
   * neighborhood iterator data.. */
  virtual PixelType operator()(const OffsetType& point_index,
                               const OffsetType& boundary_offset,
                               const NeighborhoodType *data) const; 
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroFluxNeumannBoundaryCondition.txx"
#endif

#endif
