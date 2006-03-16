/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryProfilePixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxBoundaryProfilePixel_h
#define __itkBloxBoundaryProfilePixel_h

#include "itkBloxBoundaryProfileItem.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkPoint.h"
#include "itkBloxPixel.h"

namespace itk
{

template <unsigned int NDimensions>
class ITK_EXPORT BloxBoundaryProfilePixel : public BloxPixel< 
                                        BloxBoundaryProfileItem<NDimensions> >
{
public:

  /** Run-time type information (and related methods). */
  itkTypeMacro( BloxBoundaryProfilePixel, BloxPixel );

  /** The type of boundary profile item we process. */
  typedef BloxBoundaryProfilePixel<NDimensions> BoundaryProfileItemType;

  /** The type of boundary point item we process. */
  typedef BloxBoundaryPointItem<NDimensions> BPItemType;

  /** The type used to store the position of the boundary point item. */
  typedef Point<double, NDimensions> PositionType;
  
  BloxBoundaryProfilePixel();
  virtual ~BloxBoundaryProfilePixel();
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxBoundaryProfilePixel.txx"
#endif

#endif
