/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientRecursiveGaussianImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * 
 * \ingroup GradientFilters   
 * \ingroup Singlethreaded
 */
template <class TInputImage, class TOutputImage, class TComputation>
class ITK_EXPORT GradientRecursiveGaussianImageFilter:
  public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef GradientRecursiveGaussianImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>        ConstPointer;
  
  /**  Output Image Nth Element Adaptor
   *  This adaptor allows to use conventional scalar 
   *  smoothing filters to compute each one of the 
   *  components of the gradient image pixels. */
  typedef NthElementImageAdaptor< TOutputImage,
                                  typename TInputImage::PixelType >  
                                                     OutputAdaptorType;

  /**  Smoothing filter type */
  typedef RecursiveGaussianImageFilter<
                                  TInputImage,
                                  TInputImage,
                                  TComputation>    SmoothingFilterType;

  /**  Derivative along one dimension filter type. */
  typedef FirstDerivativeRecursiveGaussianImageFilter<
                                  TInputImage,
                                  OutputAdaptorType,
                                  TComputation>    DerivativeFilterType;

  /**  Pointer to a smoothing filter.  */
  typedef typename SmoothingFilterType::Pointer    SmoothingFilterPointer;

  /**  Pointer to a derivative filter.  */
  typedef typename DerivativeFilterType::Pointer   DerivativeFilterPointer;                                  
  /**  Pointer to the output image adaptor. */
  typedef typename OutputAdaptorType::Pointer      OutputAdaptorPointer;                                  
  /**  Pointer to the Output Image Adaptor */
  typedef typename TOutputImage::Pointer          OutputImagePointer;                                  
  /** Image dimension. */
  enum { ImageDimension = TInputImage::ImageDimension };

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set Sigma value */
  void SetSigma( TComputation sigma );

protected:
  GradientRecursiveGaussianImageFilter();
  virtual ~GradientRecursiveGaussianImageFilter() {};
  
  /** Generate Data */
  void GenerateData( void );

private:
  GradientRecursiveGaussianImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  SmoothingFilterPointer     m_SmoothingFilters[ImageDimension-1];
  DerivativeFilterPointer    m_DerivativeFilter;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientRecursiveGaussianImageFilter.txx"
#endif

#endif




