/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinomialBlurImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinomialBlurImageFilter_h
#define __itkBinomialBlurImageFilter_h

#include "itkImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkSize.h"

namespace itk
{

/** \class BinomialBlurImageFilter
 * \brief Performs a separable blur on each dimension of an image
 *
 * The binomial blur consists of a nearest neighbor average along each
 * image dimension. The net result after n-iterations approaches
 * convultion with a gaussian.
 * 
 * \ingroup ImageEnhancement
 */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT BinomialBlurImageFilter :
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef BinomialBlurImageFilter Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( BinomialBlurImageFilter, ImageToImageFilter );

  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, TInputImage::ImageDimension);

  /** typedef for images */
  typedef TInputImage                             InputImageType;
  typedef TOutputImage                            OutputImageType;
  typedef typename OutputImageType::Pointer       OutputImagePointer;
  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;

  /** Image size typedef */
  typedef Size<itkGetStaticConstMacro(NDimensions)> SizeType;

  /** Image index typedef */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image pixel value typedef */
  typedef typename TOutputImage::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Get and set the number of times to repeat the filter. */
  itkSetMacro(Repetitions, unsigned int);
  itkGetMacro(Repetitions, unsigned int);

  /** This filter needs to request a larger input than its requested output.
   * If this filter runs "Repetitions" iterations, then it needs an input
   * that is 2*Repetitions larger than the output. In other words, this
   * filter needs a border of "Repetitions" pixels. */
  void GenerateInputRequestedRegion();

protected:
  BinomialBlurImageFilter();
  virtual ~BinomialBlurImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Method for evaluating the implicit function over the image. */
  void GenerateData();

private:
  BinomialBlurImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** How many times should we apply the blur? */
  unsigned int m_Repetitions;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinomialBlurImageFilter.txx"
#endif

#endif
