/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageAnisotropicDiffusion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageAnisotropicDiffusion_h
#define __itkFilterImageAnisotropicDiffusion_h

#include "itkNeighborhoodOperator.h"
#include "itkImage.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkFilterImageAnisotropicDiffusionBase.h"

namespace itk
{
/**
 * \class FilterImageAnisotropicDiffusion
 * \brief Performs anisotropic diffusion on the scalar portion of an
 *  itk::Image. 
 *
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 */
template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT FilterImageAnisotropicDiffusion :
    public FilterImageAnisotropicDiffusionBase< TPixel, VDimension>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageAnisotropicDiffusion Self;

  /**
   * Standard Superclass typedef support.
   */
  typedef FilterImageAnisotropicDiffusionBase<TPixel, VDimension> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(FilterImageAnisotropicDiffusion,
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
  FilterImageAnisotropicDiffusion() {}
  virtual ~FilterImageAnisotropicDiffusion() {}

  FilterImageAnisotropicDiffusion(const Self&) {}
  void operator=(const Self&) {}

  /**
   * Single iteration of the diffusion algorithm for 2D images.
   */
  template < class TNeighborhoodIterator >
  void AnisotropicDiffuse2D(TNeighborhoodIterator, const float);

  /**
   * Single iteration of the diffusion algorithm for 3D images.
   */
  template < class TNeighborhoodIterator >
  void AnisotropicDiffuse3D(TNeighborhoodIterator, const float);

  /**
   * Single iteration of the diffusion algorithm for images of arbitrary
   * dimension.
   */
  template < class TNeighborhoodIterator >
  void AnisotropicDiffuseND(TNeighborhoodIterator, const float);

  /**
   * Single iteration of the curvature-based diffusion algorithm for 2D
   * images.
   */
  template < class TNeighborhoodIterator >
  void CurvatureDiffuse2D(TNeighborhoodIterator, const float);

  

private:

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageAnisotropicDiffusion.txx"
#endif

#endif
