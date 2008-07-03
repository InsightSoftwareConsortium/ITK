/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTComplexToComplexImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFFTComplexToComplexImageFilter_h
#define __itkFFTComplexToComplexImageFilter_h


#include <itkImageToImageFilter.h>
#include <itkImage.h>
#include <complex>

namespace itk
{
/** \class FFTComplexToComplexImageFilter
 *
 * \brief Implements an API to enable the Fourier transform or the inverse
 * Fourier transform of images with complex valued voxels to be computed.
 *
 * \ingroup FourierTransform
 *
 * \author Simon K. Warfield simon.warfield@childrens.harvard.edu
 *
 * \note Attribution Notice. This research work was made possible by
 * Grant Number R01 RR021885 (PI Simon K. Warfield, Ph.D.) from
 * the National Center for Research Resources (NCRR), a component of the
 * National Institutes of Health (NIH).  Its contents are solely the
 * responsibility of the authors and do not necessarily represent the
 * official view of NCRR or NIH.
 *
 * This class was taken from the Insight Journal paper:
 * http://insight-journal.org/midas/handle.php?handle=1926/326
 *
 */
#define FFT_FORWARD -1
#define FFT_BACKWARD 1

/*
 * FIXME: This API must be converted to take an input image type instead of pixel and dimension.
 *        Otherwise it won't be usable by customized image types such as the OrientedImage.
 */
template < class TPixel, unsigned int NDimension = 3, int NDirection = FFT_FORWARD >
class FFTComplexToComplexImageFilter :
    public ImageToImageFilter< Image< std::complex< TPixel > , NDimension >,
                               Image< std::complex< TPixel > , NDimension > >
{
public:
  /** Input and output image types. FIXME: These should be the template parameters */
  typedef Image< std::complex< TPixel > , NDimension > TInputImageType;
  typedef Image< std::complex< TPixel > , NDimension > TOutputImageType;

  /** Standard class typedefs. */
  typedef FFTComplexToComplexImageFilter                            Self;
  typedef ImageToImageFilter< TInputImageType, TOutputImageType >   Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  constPointer;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImageType::ImageDimension );

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTComplexToComplexImageFilter, ImageToImageFilter);


  /** Image type typedef support. */
  typedef TInputImageType                         ImageType;
  typedef typename ImageType::SizeType            ImageSizeType;

protected:
  FFTComplexToComplexImageFilter() {}
  virtual ~FFTComplexToComplexImageFilter(){}

  /** methods needed for the image filter pipeline */
  virtual void GenerateOutputInformation(); // figure out allocation for output image
  virtual void GenerateInputRequestedRegion();
  virtual bool FullMatrix() = 0; // must be implemented in child


private:
  FFTComplexToComplexImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTComplexToComplexImageFilter.txx"
#endif

#endif
