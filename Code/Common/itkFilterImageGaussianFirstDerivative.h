/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageGaussianFirstDerivative.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkFilterImageGaussianFirstDerivative convolves an image with
 * a kernel approximating the first derivative of a gaussian.
 * 
 * This class implements the recursive filtering
 * method proposed by R.Deriche in IEEE-PAMI
 * Vol.12, No.1, January 1990, pp 78-87.
 */
#ifndef __itkFilterImageGaussianFirstDerivative_h
#define __itkFilterImageGaussianFirstDerivative_h

#include "itkFilterImageGaussian.h"

template <class TInputImage, class TOutputImage, class TComputation>
class ITK_EXPORT itkFilterImageGaussianFirstDerivative : public
                  itkFilterImageGaussian<TInputImage,TOutputImage,TComputation> 

{
  /** 
   * Smart pointer typedef support 
   */
  typedef itkSmartPointer<
    itkFilterImageGaussianFirstDerivative<TInputImage,TOutputImage,TComputation> >
    Pointer;

  /** 
   * Create the source with one output initially 
   */
  static Pointer New();


protected:

  /**
   * Set up the coefficients of the filter to approximate a specific kernel.
   * typically it can be used to approximate a gaussian or one of its
   * derivatives.
   */
	virtual void SetUp(TComputation dd);

public:
	itkFilterImageGaussianFirstDerivative() {};
	virtual ~itkFilterImageGaussianFirstDerivative() {};

private:  

};



#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageGaussianFirstDerivative.txx"
#endif

#endif




