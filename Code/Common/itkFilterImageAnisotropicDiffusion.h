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

#include "itkFilterImageToImage.h"
#include "itkNeighborhoodOperator.h"
#include "itkImage.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"

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
    public FilterImageToImage< Image<TPixel, VDimension>,
                               Image<TPixel, VDimension> > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageAnisotropicDiffusion Self;

  /**
   * Standard Superclass typedef support.
   */
  typedef FilterImageToImage< Image<TPixel, VDimension>,
    Image<TPixel, VDimension> > Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;

  /**
   * Image typedef support
   */
  typedef Image<TPixel, VDimension> ImageType;

  /**
   * Scalar value type typedef support
   */
  typedef typename ScalarTraits<TPixel>::ScalarValueType TPixelScalarValueType;

  
  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(FilterImageAnisotropicDiffusion, FilterImageToImage);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Computes the output.
   */
  void GenerateData();

  /**
   * Sets the total number of times the filter will cycle on the image.
   */
  itkSetMacro(Iterations, unsigned long);

  /**
   * Returns the total number of times the filter will cycle on the image.
   */
  itkGetMacro(Iterations, unsigned long);

  /**
   * Sets the free conductance parameter used in the conductance function.
   */
  itkSetMacro(ConductanceParameter, TPixelScalarValueType);

  /**
   * Returns the free conductance parameter used in the conductance function.
   */
  itkGetMacro(ConductanceParameter, TPixelScalarValueType);

  /**
   * Sets the size of the time step for each iteration.
   */
  itkSetMacro(TimeStep, TPixelScalarValueType);

  /**
   * Returns the size of the time step for each iteration.
   */
  itkGetMacro(TimeStep, TPixelScalarValueType);

protected:
  FilterImageAnisotropicDiffusion() {}
  virtual ~FilterImageAnisotropicDiffusion() {}

  FilterImageAnisotropicDiffusion(const Self&) {}
  void operator=(const Self&) {}

  /**
   *  Returns the average scalar gradient magnitude at all non-boundary pixels
   *  in an image.
   */
  TPixelScalarValueType AverageGradientMagnitude(ImageType *,const
                                                 ImageRegion<VDimension>&); 

  /**
   *  Copies pixels from the input image to the output image.
   */
  void CopyInputToOutput();

  /**
   *  Adds a multiple of the scalar portion of an image to the scalar portion
   *  of the output image.
   */
  void UpdateOutput(ImageType *, const TPixelScalarValueType);

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


private:
  /**
   * Free parameter in the conductance function.
   */
  TPixelScalarValueType m_ConductanceParameter;

  /**
   * Total number of times the filter will cycle on the image.
   */
  unsigned int m_Iterations;

  /**
   * The size of the time step for each iteration. 
   */
  TPixelScalarValueType m_TimeStep;

  //*****************************
  /**
   *TESTING CODE
   */
  void PrintSlice(SliceIterator<TPixel, Neighborhood<TPixel, VDimension> > s)
  {
    std::cout << "[" ;
    for (s=s.Begin(); s < s.End(); s++)
      {
      std::cout << ScalarTraits<TPixel>::GetScalar(*s) << " ";
      }
    std::cout << "]" << std::endl;
  }

  
    //****************************

    


};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageAnisotropicDiffusion.txx"
#endif

#endif
