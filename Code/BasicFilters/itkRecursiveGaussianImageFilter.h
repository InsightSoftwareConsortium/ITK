/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveGaussianImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
