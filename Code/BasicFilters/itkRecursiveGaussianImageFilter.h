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

#include "itkImageToImageFilter.h"

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
    public ImageSource<TOutputImage> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RecursiveGaussianImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>   Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Smart pointer typedef support 
   */
  typedef typename TInputImage::Pointer  InputImagePointer;

  /**
   * Type macro that defines a name for this class
   */
  itkTypeMacro( RecursiveGaussianImageFilter, ImageToImageFilter );

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

  /**
   * Get the direction in which the filter is to be applied
   */   
  itkGetMacro(Direction, unsigned int);

  /**
   * Set the direction in which the filter is to be applied
   */   
  itkSetMacro(Direction, unsigned int);

  /**
   * Set Input Image
   */
  void SetInputImage( InputImagePointer );
    
  /**
   * Get Input Image
   */
//  typename TInputImage::Pointer GetInputImage( void );
  TInputImage * GetInputImage( void );

  /**
   * GenerateData (apply) the filter
   */   
  void GenerateData(void);


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
   * Apply the recursive filter along one of the dimensions of the image.
   * This allow to filter each one of the dimensions of an image separately.
   * Sigma is given in length units so the spacing between pixels is taken 
   * into account. This is relevant for anisotropic images
   */
  void ApplyRecursiveFilter(unsigned int dimension);

  /**
   * Compute Recursive Filter Coefficients this method prepares the values of
   * the coefficients used for filtering the image. The symmetric flag is
   * used to enforce that the filter will be symmetric or antisymmetric. For
   * example, the Gaussian kernel is symmetric, while its first derivative is
   * antisymmetric.
   */
  void ComputeFilterCoefficients(bool symmetric);

  /**
   * Apply the Recursive Filter in an array of data.  this method is called
   * for each line of the volume from ApplyRecursiveFilter.
   * \sa ApplyRecursiveFilter 
   */
  void FilterDataArray(TComputation *outs,
                       const TComputation *data, unsigned int ln);

  TComputation a0,a1,b0,b1,c0,c1,w0,w1; // Parameter of exponential serie

  TComputation K;                       // Normalization factor

  /**
   * Sigma of the gaussian kernel
   */   
  TComputation m_Sigma;

  /**
   * Spacing along the direction of filtering
   */   
  TComputation m_Spacing;

private:  

  /**
   * Direction in which the filter is to be applied
   * this shoul in the range [0,ImageDimension-1]
   */ 
  unsigned int m_Direction;

  TComputation n00,n11,n22,n33; // Causal coefficients
  TComputation d11,d22,d33,d44; // Causal coefficients == Anticausal coeff.
  TComputation m11,m22,m33,m44; // AntiCausal coefficients (symmetric case)

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveGaussianImageFilter.txx"
#endif

#endif
