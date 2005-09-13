/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrientationAdapter.h
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


#ifndef __itkOrientationAdapter_h
#define __itkOrientationAdapter_h
#include "itkImageBase.h"

namespace itk
{
/** \class OrientationAdapterBase
 *  \brief base class that converts Orientation representations to direction cosines.
 *
 * OrientationAdapterBase is a pure virtual base class that defines the
 * member function signatures for any subclass that concretely defines the
 * conversion relation between a method of representing orientation, and the
 * direction cosines managed in itk::ImageBase.
 */
template<class OrientationType, unsigned int Dimension = 3>
class OrientationAdapterBase
{
public:
  /** typedef for matching ImageBase*/
  typedef typename itk::ImageBase<Dimension> ImageType;
  
  /** typedef for matching Direction Cosines type */
  typedef typename ImageType::DirectionType DirectionType;

  /** Convert direction cosines to the Orientation type */
  virtual OrientationType FromDirectionCosines(const DirectionType &Dir) = 0;
  /** Convert Orientation type direction cosines */
  virtual DirectionType ToDirectionCosines(const OrientationType &Orient) = 0;
protected:
  /** destructor, to silence "virtual class has non-virtual destructor()" warnings */
  virtual ~OrientationAdapterBase() {}
};

} // namespace itk
#endif // __itkOrientationAdapter_h
