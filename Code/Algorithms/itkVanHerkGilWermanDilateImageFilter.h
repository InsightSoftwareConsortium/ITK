/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVanHerkGilWermanDilateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkVanHerkGilWermanDilateImageFilter_h
#define __itkVanHerkGilWermanDilateImageFilter_h

#include "itkVanHerkGilWermanErodeDilateImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
template< class TPixel >
class MaxFunctor
{
public:
  MaxFunctor(){}
  ~MaxFunctor(){}
  inline TPixel operator()(const TPixel & A, const TPixel & B) const
  {
    return vnl_math_max(A, B);
  }
};

template< class TImage, class TKernel >
class ITK_EXPORT VanHerkGilWermanDilateImageFilter:
  public VanHerkGilWermanErodeDilateImageFilter< TImage, TKernel, MaxFunctor< typename TImage::PixelType > >

{
public:
  typedef VanHerkGilWermanDilateImageFilter Self;
  typedef VanHerkGilWermanErodeDilateImageFilter< TImage, TKernel,
                                                  MaxFunctor< typename TImage::PixelType > > Superclass;

  /** Runtime information support. */
  itkTypeMacro(VanHerkGilWermanDilateImageFilter,
               VanHerkGilWermanErodeDilateImageFilter);

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  virtual ~VanHerkGilWermanDilateImageFilter() {}
protected:

  typedef typename TImage::PixelType PixelType;

  VanHerkGilWermanDilateImageFilter()
  {
    this->m_Boundary = NumericTraits< PixelType >::NonpositiveMin();
  }

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    os << indent << "VanHerkGilWerman erosion: " << std::endl;
  }

private:

  VanHerkGilWermanDilateImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                    //purposely not implemented
};
} // namespace itk

#endif
