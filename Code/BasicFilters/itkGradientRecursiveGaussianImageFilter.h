/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientRecursiveGaussianImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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




