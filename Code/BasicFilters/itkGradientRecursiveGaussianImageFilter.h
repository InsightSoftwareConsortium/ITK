/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientRecursiveGaussianImageFilter.h
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
#ifndef __itkGradientRecursiveGaussianImageFilter_h
#define __itkGradientRecursiveGaussianImageFilter_h

#include "itkFirstDerivativeRecursiveGaussianImageFilter.h"
#include "itkNthElementImageAdaptor.h"

namespace itk
{

/** \class GradientRecursiveGaussianImageFilter
 * \brief Computes the gradient of an image by convolution
 *        with the first derivative of a Gaussian.
 * 
 * This filter is implemented using the recursive gaussian
 * filters
 *
 */
template <class TInputImage, class TOutputImage, class TComputation>
class ITK_EXPORT GradientRecursiveGaussianImageFilter:
  public ImageToImageFilter<TInputImage,TOutputImage>
{

public:

  /**
   * Standard "Self" typedef.
   */
  typedef GradientRecursiveGaussianImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  
  /** 
   *  Output Image Nth Element Adaptor
   *  This adaptor allows to use conventional scalar 
   *  smoothing filters to compute each one of the 
   *  components of the gradient image pixels.
   */
  typedef NthElementImageAdaptor< TOutputImage,
                                  typename TInputImage::PixelType >  
                                                     OutputAdaptorType;
 

  /** 
   *  Smoothing filter type
   */
  typedef RecursiveGaussianImageFilter<
                                  TInputImage,
                                  TInputImage,
                                  TComputation>    SmoothingFilterType;


  /** 
   *  Derivative along one dimension filter type
   */
  typedef FirstDerivativeRecursiveGaussianImageFilter<
                                  TInputImage,
                                  OutputAdaptorType,
                                  TComputation>    DerivativeFilterType;

  /** 
   *  Pointer to a smoothing filter 
   */
  typedef typename SmoothingFilterType::Pointer    SmoothingFilterPointer;


  /** 
   *  Pointer to a derivative filter 
   */
  typedef typename DerivativeFilterType::Pointer   DerivativeFilterPointer;                                  
                                  
  /** 
   *  Pointer to the Output Image Adaptor
   */
  typedef typename OutputAdaptorType::Pointer      OutputAdaptorPointer;                                  
   /** 
   *  Pointer to the Output Image Adaptor
   */
  typedef typename TOutputImage::Pointer          OutputImagePointer;                                  
 
  /** 
   * Image Dimension
   */
  enum { ImageDimension = TInputImage::ImageDimension };

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Set Sigma value
   */
  void SetSigma( TComputation sigma );


  /**
   * Generate Data
   */
  void GenerateData( void );



protected:

  GradientRecursiveGaussianImageFilter();
  
  virtual ~GradientRecursiveGaussianImageFilter() {};
  
  GradientRecursiveGaussianImageFilter(const Self&) {}
  
  void operator=(const Self&) {}
  
private:
  
  SmoothingFilterPointer     m_SmoothingFilters[ImageDimension-1];
  DerivativeFilterPointer    m_DerivativeFilter;
  OutputAdaptorPointer       m_OutputAdaptor;
  OutputImagePointer         m_OutputImage;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientRecursiveGaussianImageFilter.txx"
#endif

#endif




