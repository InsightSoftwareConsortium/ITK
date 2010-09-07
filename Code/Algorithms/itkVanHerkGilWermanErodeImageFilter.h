/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVanHerkGilWermanErodeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkVanHerkGilWermanErodeImageFilter_h
#define __itkVanHerkGilWermanErodeImageFilter_h

#include "itkVanHerkGilWermanErodeDilateImageFilter.h"

namespace itk
{
template< class TPixel >
class MinFunctor
{
public:
  MinFunctor(){}
  ~MinFunctor(){}
  inline TPixel operator()(const TPixel & A, const TPixel & B) const
  {
    return vnl_math_min(A, B);
  }
};

template< class TImage, class TKernel >
class ITK_EXPORT VanHerkGilWermanErodeImageFilter:
  public VanHerkGilWermanErodeDilateImageFilter< TImage, TKernel, MinFunctor< typename TImage::PixelType > >

{
public:
  typedef VanHerkGilWermanErodeImageFilter Self;
  typedef VanHerkGilWermanErodeDilateImageFilter< TImage, TKernel,
                                                  MinFunctor< typename TImage::PixelType > > Superclass;

  /** Runtime information support. */
  itkTypeMacro(VanHerkGilWermanErodeImageFilter,
               VanHerkGilWermanErodeDilateImageFilter);

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  virtual ~VanHerkGilWermanErodeImageFilter() {}
protected:

  typedef typename TImage::PixelType PixelType;

  VanHerkGilWermanErodeImageFilter()
  {
    this->m_Boundary = NumericTraits< PixelType >::max();
  }

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    os << indent << "VanHerkGilWerman erosion: " << std::endl;
  }

private:

  VanHerkGilWermanErodeImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                   //purposely not implemented
};
} // namespace itk

#endif
