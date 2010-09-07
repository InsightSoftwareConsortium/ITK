/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMiniPipelineSeparableImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMiniPipelineSeparableImageFilter_h
#define __itkMiniPipelineSeparableImageFilter_h

#include "itkBoxImageFilter.h"
#include "itkCastImageFilter.h"

namespace itk
{
/**
 * \class MiniPipelineSeparableImageFilter
 * \brief A separable filter for filter which are using radius
 *
 * This filter takes a non separable implementation of a neighborhood
 * filter, and run it several times (one per dimension) to implement
 * the same separable transform.
 * This filter can be used with the filter for which the neighborhood is
 * defined by the SetRadius() method, like the BoxImageFilter and its
 * subcalsses.
 *
 * \author Gaetan Lehmann
 * \author Richard Beare
 */

template< class TInputImage, class TOutputImage, class TFilter >
class ITK_EXPORT MiniPipelineSeparableImageFilter:
  public BoxImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef MiniPipelineSeparableImageFilter            Self;
  typedef BoxImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MiniPipelineSeparableImageFilter,
               BoxImageFilter);

  /** Image related typedefs. */
  typedef TInputImage                      InputImageType;
  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::PixelType  PixelType;
  typedef typename TInputImage::OffsetType OffsetType;

  typedef TOutputImage                     OutputImageType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  typedef TFilter                                            FilterType;
  typedef CastImageFilter< InputImageType, OutputImageType > CastType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);
  /** n-dimensional Kernel radius. */
  typedef typename TInputImage::SizeType RadiusType;

  virtual void SetRadius(const RadiusType &);

  virtual void SetRadius(const unsigned long & radius)
  {
    // needed because of the overloading of the method
    Superclass::SetRadius(radius);
  }

  virtual void Modified() const;

  virtual void SetNumberOfThreads(int nb);

protected:
  MiniPipelineSeparableImageFilter();
  ~MiniPipelineSeparableImageFilter() {}

  void GenerateData();

  typename FilterType::Pointer m_Filters[ImageDimension];
  typename CastType::Pointer m_Cast;
private:
  MiniPipelineSeparableImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                   //purposely not implemented
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMiniPipelineSeparableImageFilter.txx"
#endif

#endif
