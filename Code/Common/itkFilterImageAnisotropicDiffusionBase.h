/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageAnisotropicDiffusionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageAnisotropicDiffusionBase_h
#define __itkFilterImageAnisotropicDiffusionBase_h

#include "itkFilterImageToImage.h"
#include "itkNeighborhoodOperator.h"
#include "itkImage.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkVectorComponentDataAccessor.h"

namespace itk
{
/**
 * \class FilterImageAnisotropicDiffusionBase
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
class ITK_EXPORT FilterImageAnisotropicDiffusionBase :
    public FilterImageToImage< Image<TPixel, VDimension>,
                               Image<TPixel, VDimension> > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageAnisotropicDiffusionBase Self;

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
  typedef typename ScalarTraits<TPixel>::ScalarValueType ScalarValueType;

  /**
   * Scalar value type typedef support
   */
  typedef typename VectorTraits<TPixel>::VectorValueType VectorValueType;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(FilterImageAnisotropicDiffusionBase, FilterImageToImage);
  
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
  itkSetMacro(ConductanceParameter, ScalarValueType);

  /**
   * Returns the free conductance parameter used in the conductance function.
   */
  itkGetMacro(ConductanceParameter, ScalarValueType);

  /**
   * Sets the size of the time step for each iteration.
   */
  itkSetMacro(TimeStep, ScalarValueType);

  /**
   * Returns the size of the time step for each iteration.
   */
  itkGetMacro(TimeStep, ScalarValueType);

protected:
  FilterImageAnisotropicDiffusionBase() {}
  virtual ~FilterImageAnisotropicDiffusionBase() {}

  FilterImageAnisotropicDiffusionBase(const Self&) {}
  void operator=(const Self&) {}

  /**
   *  Copies pixels from the input image to the output image.
   */
  void CopyInputToOutput();

   /**
   *  Adds a multiple of the scalar portion of an image to the scalar portion
   *  of the output image.
   */
  void UpdateOutputScalar(ImageType *, const ScalarValueType);
  void UpdateOutputScalar(ImageType *, const ScalarValueType, const
                          VectorComponentDataAccessor<TPixel,
                          VectorValueType> &);
  
  /**
   *  Returns the average scalar gradient magnitude at all non-boundary pixels
   *  in an image.
   */
  ScalarValueType AverageGradientMagnitudeScalar(ImageType *,const
                                               ImageRegion<VDimension>&); 


private:
  /**
   * Free parameter in the conductance function.
   */
  ScalarValueType m_ConductanceParameter;

  /**
   * Total number of times the filter will cycle on the image.
   */
  unsigned int m_Iterations;

  /**
   * The size of the time step for each iteration. 
   */
  ScalarValueType m_TimeStep;

  //*****************************
  /**
   *TESTING CODE
   */
  void PrintSlice(SliceIterator<TPixel, Neighborhood<TPixel, VDimension> > s)
  {
    std::cout << "[" ;
    for (s=s.Begin(); s < s.End(); s++)
      {
        std::cout << *s << " ";
      }
    std::cout << "]" << std::endl;
  }

  
    //****************************

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageAnisotropicDiffusionBase.txx"
#endif

#endif
