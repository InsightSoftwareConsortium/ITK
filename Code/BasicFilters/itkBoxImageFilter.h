/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoxImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBoxImageFilter_h
#define __itkBoxImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkCastImageFilter.h"

namespace itk
{
/**
 * \class BoxImageFilter
 * \brief A base class for all the filters working on a box neighborhood
 *
 * This filter provides the code to store the radius information about the
 * neighborhood used in the subclasses.
 * It also conveniently reimplement the GenerateInputRequestedRegion() so
 * that region is well defined for the provided radius.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 */

template< class TInputImage, class TOutputImage >
class ITK_EXPORT BoxImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BoxImageFilter                                  Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BoxImageFilter,
               ImageToImageFilter);

  /** Image related typedefs. */
  typedef TInputImage                      InputImageType;
  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::OffsetType OffsetType;

  typedef typename TInputImage::PixelType InputPixelType;

  typedef TOutputImage                     OutputImageType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  /** n-dimensional Kernel radius. */
  typedef typename TInputImage::SizeType RadiusType;

  virtual void SetRadius(const RadiusType & radius);

  virtual void SetRadius(const unsigned long & radius);

  itkGetConstReferenceMacro(Radius, RadiusType);

  void GenerateInputRequestedRegion();

protected:
  BoxImageFilter();
  ~BoxImageFilter() {}

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  BoxImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  RadiusType m_Radius;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBoxImageFilter.txx"
#endif

#endif
