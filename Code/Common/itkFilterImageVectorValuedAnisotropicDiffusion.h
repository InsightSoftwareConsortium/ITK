/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageVectorValuedAnisotropicDiffusion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageVectorValuedAnisotropicDiffusion_h
#define __itkFilterImageVectorValuedAnisotropicDiffusion_h


#include "itkNeighborhoodOperator.h"
#include "itkImage.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkFilterImageAnisotropicDiffusionBase.h"
#include "itkVector.h"

namespace itk
{

// TEMPORARY PATCH UNTIL I CAN REDESIGN THE NEIGHBORHOOD OBJECTS TO
// CLEANLY SUPPORT BOTH VECTOR AND SCALAR TYPES  --jc 12/01/00
// These functions are never called, yet the compiler may
// try to instantiate them.
template <class TPixel, unsigned int TVectorDimension>  
inline Vector<TPixel, TVectorDimension>
operator* ( const Vector<TPixel, TVectorDimension> &a,
            const Vector<TPixel, TVectorDimension> &b ) {};

template <class TPixel, unsigned int TVectorDimension>  
inline Vector<TPixel, TVectorDimension>
operator/ ( const Vector<TPixel, TVectorDimension> &a,
            const Vector<TPixel, TVectorDimension> &b ) {};

template <class TPixel, unsigned int TVectorDimension>
inline typename ScalarTraits<Vector<TPixel, TVectorDimension>
>::ScalarValueType & 
operator+=(typename ScalarTraits<Vector<TPixel, TVectorDimension>
           >::ScalarValueType a, Vector<TPixel, TVectorDimension> b) {};


// END PATCH


  
/**
 * \class FilterImageVectorValuedAnisotropicDiffusion
 * \brief Performs anisotropic diffusion on an N-dimensional vector-valued
 * image region. 
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 */
template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT FilterImageVectorValuedAnisotropicDiffusion :
    public FilterImageAnisotropicDiffusionBase< TPixel, VDimension>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageVectorValuedAnisotropicDiffusion Self;

  /**
   * Standard Superclass typedef support.
   */
  typedef FilterImageAnisotropicDiffusionBase<TPixel, VDimension> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(FilterImageVectorValuedAnisotropicDiffusion,
               FilterImageAnisotropicDiffusionBase);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Computes the output.
   */
  void GenerateData();

protected:
  FilterImageVectorValuedAnisotropicDiffusion() {}
  virtual ~FilterImageVectorValuedAnisotropicDiffusion() {}

  FilterImageVectorValuedAnisotropicDiffusion(const Self&) {}
  void operator=(const Self&) {}

  /**
   * Single iteration of the diffusion algorithm for 2D images.
   */
  template < class TNeighborhoodIterator >
  void VectorValuedAnisotropicDiffuse2D(TNeighborhoodIterator &, const float);

  /**
   * Single iteration of the diffusion algorithm for ND images.
   */
  template < class TNeighborhoodIterator >
  void VectorValuedAnisotropicDiffuseND(TNeighborhoodIterator &, const float);

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageVectorValuedAnisotropicDiffusion.txx"
#endif

#endif
