/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkTransformFactory.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTransformFactory.h"
#include "itkVersion.h"
#include "itkCenteredRigid2DTransform.h"
#include "itkAffineTransform.h"

namespace itk
{
  TransformFactory* TransformFactory::m_Factory = 0;

TransformFactory::TransformFactory()
{
}

TransformFactory::~TransformFactory()
{
}

void TransformFactory::RegisterDefaultTransforms()
{
  if ( !m_Factory )
    {
    TransformFactory::RegisterTransform<CenteredRigid2DTransform < float > >();
    TransformFactory::RegisterTransform<AffineTransform<double,3> > ();
    }
}

const char*
TransformFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char*
TransformFactory::GetDescription() const
{
  return "Transform Factory";
}

} // end namespace itk

