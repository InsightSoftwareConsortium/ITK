/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkTransformFactoryBase.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTransformFactoryBase.h"
#include "itkTransformFactory.h"
#include "itkVersion.h"
#include "itkCenteredRigid2DTransform.h"
#include "itkAffineTransform.h"

namespace itk
{
  TransformFactoryBase* TransformFactoryBase::m_Factory = 0;




TransformFactoryBase::TransformFactoryBase()
{
}

TransformFactoryBase::~TransformFactoryBase()
{
}

void TransformFactoryBase::RegisterDefaultTransforms()
{
  if ( !m_Factory )
    {
    TransformFactory<CenteredRigid2DTransform < float > >::RegisterTransform();
    TransformFactory<AffineTransform<double,3> >::RegisterTransform ();
    }
}

const char*
TransformFactoryBase::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char*
TransformFactoryBase::GetDescription() const
{
  return "Transform FactoryBase";
}

} // end namespace itk

