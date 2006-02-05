/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBrains2IPLHeaderInfo.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBrains2IPLHeaderInfo_h
#define __itkBrains2IPLHeaderInfo_h

#include "itkBrains2HeaderBase.h"

namespace itk {
class Brains2IPLHeaderInfo: public Brains2HeaderBase
{
public:
  Brains2IPLHeaderInfo();
  ~Brains2IPLHeaderInfo();
  virtual std::string GetHeaderBeginTag(void) const;
  virtual std::string GetHeaderEndTag(void) const;
protected:
private:
};

} // end namespace itk
#endif // __itkBrains2IPLHeaderInfo_h
