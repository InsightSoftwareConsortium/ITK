/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveGaussianImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * 
 * \ingroup ImageEnhancement  Singlethreaded
 */
template <class TInputImage, class TOutputImage, class TComputation>
class ITK_EXPORT RecursiveGaussianImageFilter :
   public RecursiveSeparableImageFilter<TInputImage,TOutputImage,TComputation> 
{
public:
  /** Standard class typedefs. */
  typedef RecursiveGaussianImageFilter  Self;
  typedef RecursiveSeparableImageFilter<
              TInputImage,TOutputImage,TComputation> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Type macro that defines a name for this class */
  itkTypeMacro( RecursiveGaussianImageFilter, RecursiveSeparableImageFilter );

  /** Get the Sigma of the Gaussian kernel. */   
  itkGetMacro( Sigma, TComputation );

  /** Set the Sigma of the Gaussian kernel. */   
  itkSetMacro( Sigma, TComputation );

protected:
  RecursiveGaussianImageFilter();
  virtual ~RecursiveGaussianImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Set up the coefficients of the filter to approximate a specific kernel.
   * typically it can be used to approximate a gaussian or one of its
   * derivatives. */
  virtual void SetUp(void);

   /** Compute Recursive Filter Coefficients this method prepares the values of
   * the coefficients used for filtering the image. The symmetric flag is
   * used to enforce that the filter will be symmetric or antisymmetric. For
   * example, the Gaussian kernel is symmetric, while its first derivative is
   * antisymmetric. */
  void ComputeFilterCoefficients(bool symmetric);

private:  
  RecursiveGaussianImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Sigma of the gaussian kernel. */   
  TComputation m_Sigma;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveGaussianImageFilter.txx"
#endif

#endif
