/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInterpolateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkInterpolateImageFilter_h
#define __itkInterpolateImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"

namespace itk
{

/** \class InterpolateImageFilter
 * \brief Interpolate an image from two N-D images.
 *
 * Interpolates an image from two input images of the same type 
 * and same dimension (N). In particular, this filter forms an intermediate
 * (N+1)D image by concatenating the two input images and interpolating
 * an image a distance \f$ d \in [0,1] \f$ away from the first image.
 *
 * The interpolation is delegated to a user specified InterpolateImageFunction.
 * By default, linear interpolation is used.
 *
 * The filter is templated over the input image type and output image type.
 * It assumes that the input and output have the same number of dimensions.
 *
 * \ingroup Multithreaded
 */
template < class TInputImage, class TOutputImage >
class ITK_EXPORT InterpolateImageFilter:
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef InterpolateImageFilter         Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(InterpolateImageFilter, ImageToImageFilter);

  /** Inherit typedefs from Superclass */
  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::InputImagePointer InputImagePointer;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Number of dimensions. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(IntermediateImageDimension, unsigned int,
                      TOutputImage::ImageDimension + 1);

  /** Interpolator typedef. */
  typedef typename TInputImage::PixelType InputPixelType;
  typedef Image<InputPixelType,itkGetStaticConstMacro(IntermediateImageDimension)> IntermediateImageType;
  typedef InterpolateImageFunction<IntermediateImageType> InterpolatorType;

  /** Input image type. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Set/Get the first image */
   void SetInput1( const InputImageType * image)
    { this->SetInput( image ); }
   const InputImageType * GetInput1()
    { return this->GetInput(); }

  /** Set/Get the second image */
   void SetInput2( const InputImageType * image);
   const InputImageType * GetInput2();

  /** Set/Get the distance from the first image from which to generate
   * interpolated image. The default value is 0.5 */
  itkSetClampMacro( Distance, double, 0.0, 1.0 );
  itkGetMacro( Distance, double );

  /** Set the interpolator function */
  itkSetObjectMacro( Interpolator, InterpolatorType ) 

  /** Get a pointer to the interpolator function. */
  itkGetObjectMacro( Interpolator, InterpolatorType );

  /** This method is used to set the state of the filter before 
   * multi-threading. */
  void BeforeThreadedGenerateData();

  /** This method is used to run after multi-threading. */
  void AfterThreadedGenerateData();

protected:
  InterpolateImageFilter();
  ~InterpolateImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** InterpolateImageFilter can be implemented as a multithreaded filter. */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  InterpolateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  typename InterpolatorType::Pointer      m_Interpolator;
  typename IntermediateImageType::Pointer m_IntermediateImage;
  double                                  m_Distance;

};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkInterpolateImageFilter.txx"
#endif
  
#endif
