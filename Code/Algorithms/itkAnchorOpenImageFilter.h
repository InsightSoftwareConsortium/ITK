/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnchorOpenImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnchorOpenImageFilter_h
#define __itkAnchorOpenImageFilter_h

#include "itkAnchorOpenCloseImageFilter.h"

namespace itk
{
template< class TImage, class TKernel >
class ITK_EXPORT AnchorOpenImageFilter:
  public AnchorOpenCloseImageFilter< TImage, TKernel, std::less< typename TImage::PixelType >,
                                     std::greater< typename TImage::PixelType >,
                                     std::less_equal< typename TImage::PixelType >,
                                     std::greater_equal< typename TImage::PixelType > >

{
public:
  typedef AnchorOpenImageFilter Self;
  typedef AnchorOpenCloseImageFilter< TImage, TKernel, std::less< typename TImage::PixelType >,
                                      std::greater< typename TImage::PixelType >,
                                      std::less_equal< typename TImage::PixelType >,
                                      std::greater_equal< typename TImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  virtual ~AnchorOpenImageFilter() {}
protected:
  AnchorOpenImageFilter()
  {
    this->m_Boundary1 = NumericTraits< ITK_TYPENAME TImage::PixelType >::max();
    this->m_Boundary2 = NumericTraits< ITK_TYPENAME TImage::PixelType >::NonpositiveMin();
  }

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    os << indent << "Anchor opening: " << std::endl;
  }

private:

  AnchorOpenImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);        //purposely not implemented
};
} // namespace itk

#endif
