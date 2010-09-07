/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnchorErodeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnchorErodeImageFilter_h
#define __itkAnchorErodeImageFilter_h

#include "itkAnchorErodeDilateImageFilter.h"

namespace itk
{
template< class TImage, class TKernel >
class ITK_EXPORT AnchorErodeImageFilter:
  public AnchorErodeDilateImageFilter< TImage, TKernel, std::less< typename TImage::PixelType >,
                                       std::less_equal< typename TImage::PixelType > >

{
public:
  typedef AnchorErodeImageFilter
                                                                                        Self;
  typedef AnchorErodeDilateImageFilter< TImage, TKernel, std::less< typename TImage::PixelType >,
                                        std::less_equal< typename TImage::PixelType > > Superclass;

  /** Runtime information support. */
  itkTypeMacro(AnchorErodeImageFilter,
               AnchorErodeDilateImageFilter);

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  virtual ~AnchorErodeImageFilter() {}
protected:

  typedef typename TImage::PixelType PixelType;

  AnchorErodeImageFilter()
  {
    this->m_Boundary = NumericTraits< PixelType >::max();
  }

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    os << indent << "Anchor erosion: " << std::endl;
  }

private:

  AnchorErodeImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented
};
} // namespace itk

#endif
