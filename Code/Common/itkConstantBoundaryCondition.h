/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstantBoundaryCondition.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef __itkConstantBoundaryCondition_h
#define __itkConstantBoundaryCondition_h
#include "itkNeighborhood.h"
#include "itkNumericTraits.h"
#include "itkPixelTraits.h"
#include "itkImageBoundaryCondition.h"

namespace itk
{

/** \class ConstantBoundaryCondition
 * \brief This boundary condition returns a constant value for out-of-bounds
 * image pixels.
 * 
 * For example, invoking this function object with a constant value of zero
 * (the default) on each out-of-bounds element of a 7x5 iterator that masks a
 * region at an image corner 
 * (iterator is centered on the 2): 
 *
 *               * * * * * * * 
 *               * * * * * * *
 *               * * 1 2 3 4 5  (where * denotes pixels that lie 
 *               * * 3 3 5 5 6          outside of the image boundary)
 *               * * 4 4 6 7 8
 *
 * would produce the following neighborhood of values:
 *
 *               0 0 0 0 0 0 0
 *               0 0 0 0 0 0 0
 *               0 0 1 2 3 4 5
 *               0 0 3 3 5 5 6
 *               0 0 4 4 6 7 8
 *
 *
 * 
 * \sa ImageBoundaryCondition
 */
template<class TImage, class TNeighborhoodType
    = Neighborhood<ITK_TYPENAME ImageTraits<TImage>::PixelType *,
                             ImageTraits<TImage>::ImageDimension > >
class ConstantBoundaryCondition
  : public ImageBoundaryCondition<TImage, TNeighborhoodType>
{
public:
  /**
   * Self & superclass typedefs
   */ 
  typedef ConstantBoundaryCondition Self;
  typedef ImageBoundaryCondition<TImage, TNeighborhoodType> Superclass;

  /**
   * Extract information from the image type
   */
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::PixelPointerType PixelPointerType;
  enum { ImageDimension = Superclass::ImageDimension };

  /**
   * Default constructor.
   */
  ConstantBoundaryCondition()
  {  m_Constant = NumericTraits<
       typename ScalarTraits<PixelType>::ValueType>::Zero; }

  /**
   * Computes and returns a neighborhood of appropriate values from
   * neighborhood iterator data..
   */
  virtual PixelType operator()(const int *point_index,
                               const int *boundary_offset,
                               const TNeighborhoodType *data) const
  { return m_Constant; }

  void SetConstant(const PixelType &c)
  {  m_Constant = c; }

  const PixelType &GetConstant() const
  {  return m_Constant;  }
  
private:
  PixelType m_Constant;
};

} // end namespace itk

#endif
