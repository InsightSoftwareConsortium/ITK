/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxItem.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxItem_h
#define __itkBloxItem_h

#include "itkWin32Header.h"

namespace itk
{

/**
 * \class BloxItem
 * \brief An entry in the BloxPixel linked list
 *
 * This class is do-nothing virtual class, designed to avoid
 * the necessity of templating BloxPixel over item type.
 * \ingroup ImageObjects
 * */

class ITKCommon_EXPORT BloxItem
{
public:
  BloxItem();
  virtual ~BloxItem();

};

} // end namespace itk

#endif
