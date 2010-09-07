/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConvolutionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright ( c ) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConvolutionImageFilter_h
#define __itkConvolutionImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class ConvolutionImageFilter
 * \brief Convolve a given image with an arbitrary image kernel
 *
 * http://hdl.handle.net/1926/1323
 *
 * \author Nicholas J. Tustison
 * \author James C. Gee
 */
template< class TInputImage, class TOutputImage = TInputImage >
class ITK_EXPORT ConvolutionImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  typedef ConvolutionImageFilter                          Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information ( and related methods ) */
  itkTypeMacro(ConvolutionImageFilter, ImageToImageFilter);

  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  typedef TInputImage                          InputImageType;
  typedef TOutputImage                         OutputImageType;
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef typename OutputImageType::PixelType  OutputPixelType;
  typedef typename OutputImageType::RegionType OutputRegionType;

  itkSetInputMacro(ImageKernel, InputImageType, 1);
  itkGetInputMacro(ImageKernel, InputImageType, 1);

  /**
   * Normalize the output image by the sum of the kernel components
   */
  itkSetMacro(Normalize, bool);
  itkGetConstMacro(Normalize, bool);
  itkBooleanMacro(Normalize);

  /** ConvolutionImageFilter needs a smaller 2nd input (the image kernel)
   * requested region than output requested region.  As such, this filter
   * needs to provide an implementation for GenerateInputRequestedRegion() in
   * order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion()  */
  virtual void GenerateInputRequestedRegion();

protected:
  /** de/constructor */
  ConvolutionImageFilter();
  ~ConvolutionImageFilter();

  void PrintSelf(std::ostream & os, Indent indent) const;

  void ThreadedGenerateData(const OutputRegionType & outputRegionForThread, int threadId);

private:
  ConvolutionImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented

private:
  bool m_Normalize;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConvolutionImageFilter.txx"
#endif

#endif
