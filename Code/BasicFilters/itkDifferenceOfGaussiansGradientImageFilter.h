/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDifferenceOfGaussiansGradientImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDifferenceOfGaussiansGradientImageFilter_h
#define __itkDifferenceOfGaussiansGradientImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkCovariantVector.h"

namespace itk
{

/** \class DifferenceOfGaussiansGradientImageFilter
 * \brief Performs difference-of-gaussians gradient detection
 *
 * \ingroup ImageEnhancement 
 * \ingroup GradientFilters 
 *
 */
template<typename TInputImage, typename TDataType>
class ITK_EXPORT DifferenceOfGaussiansGradientImageFilter :
    public ImageToImageFilter<TInputImage,
                              Image< CovariantVector<TDataType, ::itk::GetImageDimension<TInputImage>::ImageDimension>, 
                                     ::itk::GetImageDimension<TInputImage>::ImageDimension> >
{
public:
  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int, TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef DifferenceOfGaussiansGradientImageFilter Self;

  /** Output image typedef. The output image is always an n-dimensional
   * image of n-dimensional vectors of doubles. */
  typedef Image<CovariantVector<TDataType, itkGetStaticConstMacro(NDimensions)>, itkGetStaticConstMacro(NDimensions)>
  TOutputImage;

  /** Standard class typedefs. */
  typedef ImageToImageFilter<TInputImage, TOutputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro( DifferenceOfGaussiansGradientImageFilter, ImageToImageFilter );

  /** Image size typedef. */
  typedef Size<itkGetStaticConstMacro(NDimensions)> SizeType;

  /** Image index typedef. */
  typedef typename TInputImage::IndexType IndexType;

  /** Image pixel value typedef. */
  typedef typename TInputImage::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TInputImage::RegionType OutputImageRegionType;

  /** Set/Get the member variables. */
  itkGetMacro(Width, unsigned int);
  itkSetMacro(Width, unsigned int);

protected:
  DifferenceOfGaussiansGradientImageFilter();
  virtual ~DifferenceOfGaussiansGradientImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Method for evaluating the implicit function over the image. */
  void GenerateData();

private:
  DifferenceOfGaussiansGradientImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned int m_Width;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDifferenceOfGaussiansGradientImageFilter.txx"
#endif

#endif
