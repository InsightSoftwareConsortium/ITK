/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnchorCloseImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnchorCloseImageFilter_h
#define __itkAnchorCloseImageFilter_h

#include "itkAnchorOpenCloseImageFilter.h"

namespace itk
{
template< class TImage, class TKernel >
class ITK_EXPORT AnchorCloseImageFilter:
  public AnchorOpenCloseImageFilter< TImage, TKernel, std::greater< typename TImage::PixelType >,
                                     std::less< typename TImage::PixelType >,
                                     std::greater_equal< typename TImage::PixelType >,
                                     std::less_equal< typename TImage::PixelType > >

{
public:
  typedef AnchorCloseImageFilter Self;
  typedef AnchorOpenCloseImageFilter< TImage, TKernel, std::greater< typename TImage::PixelType >,
                                      std::less< typename TImage::PixelType >,
                                      std::greater_equal< typename TImage::PixelType >,
                                      std::less_equal< typename TImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  virtual ~AnchorCloseImageFilter() {}
protected:
  AnchorCloseImageFilter()
  {
    this->m_Boundary1 = NumericTraits< ITK_TYPENAME TImage::PixelType >::NonpositiveMin();
    this->m_Boundary2 = NumericTraits< ITK_TYPENAME TImage::PixelType >::max();
  }

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    os << indent << "Anchor closing: " << std::endl;
  }

private:

  AnchorCloseImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented
};
} // namespace itk

#endif
