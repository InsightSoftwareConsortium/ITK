/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTobogganImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTobogganImageFilter_h
#define __itkTobogganImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkConstSmartNeighborhoodIterator.h"

namespace itk {

/** \class TobogganImageFilter
 * \brief toboggan image segmentation
 * The Toboggan segmentation takes a gradient magnitude image
 * as input and produces an (over-)segmentation of the image based
 * on connecting each pixel to a local minimum of gradient.  It is
 * roughly equivalent to a watershed segmentation of the lowest level.
 *
 * The output is a 4 connected labeled map of the image.
 * \ingroup Segmentation
 */
  
template<class TInputImage>
class ITK_EXPORT TobogganImageFilter : 
    public ImageToImageFilter<
            TInputImage,
            Image<unsigned long, ::itk::GetImageDimension<TInputImage>::ImageDimension> >
{
public:
  /** Standard "Self" typedef.   */
  typedef TobogganImageFilter Self;

  /** The type of input image.   */
  typedef TInputImage InputImageType;

  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int, TInputImage::ImageDimension);

  /** The type of output image.   */
  typedef Image<unsigned long, itkGetStaticConstMacro(NDimensions)> OutputImageType;

  /** Output image pixel type. */
  typedef typename OutputImageType::PixelType OutputImagePixelType;

  /** Input image pixel type. */
  typedef typename InputImageType::PixelType InputImagePixelType;

  /** Dimension of the input and output images. */
  enum {ImageDimension = InputImageType::ImageDimension };
  
  /** Other convenient typedefs   */
  typedef typename InputImageType::RegionType RegionType;
  typedef typename InputImageType::SizeType   SizeType;
  typedef typename InputImageType::IndexType  IndexType;
  typedef typename InputImageType::Pointer    InputImagePointer;
  typedef typename OutputImageType::Pointer   OutputImagePointer;
  
  /** Standard super class typedef support. */
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;

  /** Typedef support for the input image scalar value type. */
  typedef typename InputImageType::PixelType ScalarType;

  /** Smart pointer typedef support  */
  typedef SmartPointer<Self> Pointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(TobogganImageFilter, ImageToImageFilter);
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard process object method.  This filter is not multithreaded. */
  void GenerateData();
  void GenerateInputRequestedRegion();
  void EnlargeOutputRequestedRegion ( DataObject* );
  /** Neighborhood iterator type */
  typedef ConstNeighborhoodIterator<TInputImage> 
    NeighborhoodIteratorType ;
  typedef ConstSmartNeighborhoodIterator<TInputImage> 
    SmartNeighborhoodIteratorType ;

protected:
  TobogganImageFilter();
  ~TobogganImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  TobogganImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTobogganImageFilter.txx"
#endif

#endif


