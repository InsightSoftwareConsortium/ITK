/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAccessorFunctor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNeighborhoodAccessorFunctor_h
#define __itkNeighborhoodAccessorFunctor_h

#include "itkImageBoundaryCondition.h"
#include "itkNeighborhood.h"
#include "itkImageBase.h" 

namespace itk
{
 
/** \class NeighborhoodAccessorFunctor
 * \brief Provides accessor interfaces to Get pixels and is meant to be
 * used on pointers contained within Neighborhoods. A typical user should
 * not need to use this class directly. This class is used by the 
 * neighborhood iterators to get pixels from pixel pointers or assign 
 * a pixel to an address. 
 *
 * 
 * \note
 * This work is part of the National Alliance for Medical Image Computing 
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
  */
template< class TImage >
class NeighborhoodAccessorFunctor
{
public:
  typedef TImage                                ImageType;
  typedef typename ImageType::PixelType         PixelType;
  typedef typename ImageType::InternalPixelType InternalPixelType;
  typedef unsigned int                          VectorLengthType;
  typedef typename ImageType::OffsetType        OffsetType;

  typedef Neighborhood< InternalPixelType *,
          ::itk::GetImageDimension< TImage >::ImageDimension > NeighborhoodType;
  
  typedef ImageBoundaryCondition< ImageType > const *
                          ImageBoundaryConditionConstPointerType;
  
  /** Set the pointer index to the start of the buffer. */
  inline void SetBegin( const InternalPixelType *) {}
  
  /** Method to dereference a pixel pointer. This is used from the 
   * ConstNeighborhoodIterator as the equivalent operation to (*it).
   * This method should be preferred over the former (*it) notation. 
   * The reason is that dereferencing a pointer to a location of 
   * VectorImage pixel involves a different operation that simply
   * dereferencing the pointer.  */
  inline PixelType Get( const InternalPixelType *pixelPointer ) const
    { 
    return (*pixelPointer);
    }

  /** Method to set the pixel value at a certain pixel pointer */
  inline void Set( InternalPixelType* &pixelPointer, const PixelType &p ) const
    {
    *pixelPointer = p;
    }
  
  inline PixelType BoundaryCondition( 
      const OffsetType& point_index,
      const OffsetType &boundary_offset, 
      const NeighborhoodType *data,
      const ImageBoundaryConditionConstPointerType boundaryCondition) const
    {
    return boundaryCondition->operator()(point_index, boundary_offset, data);
    }
  
  void SetVectorLength( VectorLengthType length ) {};
  VectorLengthType SetVectorLength() { return 0; };
  
};

} // end namespace itk
#endif
