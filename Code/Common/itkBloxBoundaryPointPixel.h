/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryPointPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxBoundaryPointPixel_h
#define __itkBloxBoundaryPointPixel_h

#include "itkBloxBoundaryPointItem.h"
#include "itkBloxPixel.h"

namespace itk
{

/**
 * \class BloxBoundaryPointPixel
 * \brief Holds a linked list of itk::BloxBoundaryPointItem's.
 *
 * \ingroup ImageObjects 
 * */

template <unsigned int NDimensions>
class ITK_EXPORT BloxBoundaryPointPixel : public BloxPixel< BloxBoundaryPointItem<NDimensions> >
{
public:
  BloxBoundaryPointPixel();
  ~BloxBoundaryPointPixel();
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxBoundaryPointPixel.txx"
#endif

#endif
