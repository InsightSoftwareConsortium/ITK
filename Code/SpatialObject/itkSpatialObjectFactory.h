/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectFactory.h
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
#ifndef __itkSpatialObjectFactory_h
#define __itkSpatialObjectFactory_h

#include "itkSpatialObjectFactoryBase.h"

namespace itk
{
/** \class SpatialObjectFactory
 * \brief Create instances of SpatialObjects
 */

template <class T>
class SpatialObjectFactory : public SpatialObjectFactoryBase
{
public:

  static void RegisterSpatialObject()
    {
    typename T::Pointer t = T::New();
    SpatialObjectFactoryBase::Pointer f = SpatialObjectFactoryBase::GetFactory();
    f->RegisterSpatialObject ( t->GetSpatialObjectTypeAsString().c_str(),
                               t->GetSpatialObjectTypeAsString().c_str(),
                               t->GetSpatialObjectTypeAsString().c_str(),
                               1,
                               CreateObjectFunction<T>::New() );
    };
};
} // end namespace itk

#endif
