/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageGaussian.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkFilterImageGaussian is the base class for recursive filters that
 * approximate convolution with the gaussian kernel.
 * This class implements the recursive filtering
 * method proposed by R.Deriche in IEEE-PAMI
 * Vol.12, No.1, January 1990, pp 78-87.
 */
#ifndef __itkFilterImageGaussian_h
#define __itkFilterImageGaussian_h

#include "itkFilterImageToImage.h"
#include <cmath>

template <class TInputImage, class TOutputImage, class TComputation>
class ITK_EXPORT itkImageFilterGaussian : public
                  itkFilterImageToImage<TInputImage,TOutputImage> 

{
  /** 
   * Smart pointer typedef support 
   */
  typedef itkSmartPointer< itkFilterImageGaussian<TInputImage,TOutputImage> >
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

  /**
   * Apply the recursive filtre along one of the dimensions of the image.
   * This allow to filter each one of the dimensions of an image using a
   * different sigma. Which usually is needed because of the anisotropy of the
   * data
   */
	void ApplyRecursiveFilter(unsigned int dimension);

  /**
   * Compute Recursive Filter Coefficients 
   * this method prepares the values of the coefficients used for filtering the
   * image. The symmetric flag is used to enforce that the filter will be
   * symmetric or antisymmetric. For example, the Gaussian kernel is symmetric,
   * while its first derivative is antisymmetric
   */ 
	void ComputeFilterCoefficients(bool symmetric);

  /**
   * Apply the Recursive Filter in an array of data.
   * this method is called for each line of the volume from
   * ApplyRecursiveFilter
   * \sa ApplyRecursiveFilter
   */ 
	void FilterDataArray(TComputation *outs,const TComputation *data,unsigned int ln);

public:
	itkImageFilterGaussian();
	virtual ~itkImageFilterGaussian() {};
  itkGetMacro(Sigma,TComputation);
  itkSetMacro(Sigma,TComputation);

private:  

  /**
   * Sigma of the gaussian kernel
   */   
	TComputation m_Sigma;

	// Constants from R.Deriche for approximation
	// of Gaussian with IIR filter
protected:
	TComputation a0,a1,b0,b1,c0,c1,w0,w1; // Parameter of exponential serie
	TComputation K;                       // Normalization factor

private:
	TComputation n00,n11,n22,n33;	// Causal coefficients
	TComputation d11,d22,d33,d44;	// Causal coefficients == Anticausal coeff.
	TComputation m11,m22,m33,m44;	// AntiCausal coefficients (for symmetrical case)

};



#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageGaussian.cxx"
#endif

#endif




