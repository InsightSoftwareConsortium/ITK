/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveGaussianImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRecursiveGaussianImageFilter_h
#define __itkRecursiveGaussianImageFilter_h

#include "itkRecursiveSeparableImageFilter.h"

namespace itk
{
  
/** \class RecursiveGaussianImageFilter
 * \brief Base class for recursive convolution with Gaussian kernel.
 *
 * RecursiveGaussianImageFilter is the base class for recursive filters that
 * approximate convolution with the Gaussian kernel.
 * This class implements the recursive filtering
 * method proposed by R.Deriche in IEEE-PAMI
 * Vol.12, No.1, January 1990, pp 78-87.
 */

template <class TInputImage, class TOutputImage, class TComputation>
class ITK_EXPORT RecursiveGaussianImageFilter :
    public RecursiveSeparableImageFilter<TInputImage,TOutputImage,TComputation> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RecursiveGaussianImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef RecursiveSeparableImageFilter<
              TInputImage,TOutputImage,TComputation> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Type macro that defines a name for this class
   */
  itkTypeMacro( RecursiveGaussianImageFilter, RecursiveSeparableImageFilter );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Get the Sigma of the Gaussian kernel.
   */   
  itkGetMacro( Sigma, TComputation );


  /**
   * Set the Sigma of the Gaussian kernel.
   */   
  itkSetMacro( Sigma, TComputation );


protected:
  RecursiveGaussianImageFilter();
  
  virtual ~RecursiveGaussianImageFilter() {};
  
  RecursiveGaussianImageFilter(const Self&) {}
  
  void operator=(const Self&) {}

  /**
   * Set up the coefficients of the filter to approximate a specific kernel.
   * typically it can be used to approximate a gaussian or one of its
   * derivatives.
   */
  virtual void SetUp(void);

   /**
   * Compute Recursive Filter Coefficients this method prepares the values of
   * the coefficients used for filtering the image. The symmetric flag is
   * used to enforce that the filter will be symmetric or antisymmetric. For
   * example, the Gaussian kernel is symmetric, while its first derivative is
   * antisymmetric.
   */
  void ComputeFilterCoefficients(bool symmetric);


private:  

  /**
   * Sigma of the gaussian kernel
   */   
  TComputation m_Sigma;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveGaussianImageFilter.txx"
#endif

#endif
