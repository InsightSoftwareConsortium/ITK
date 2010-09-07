/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPhilipsRECImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkPhilipsRECImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkPhilipsRECImageIO.h"
#include "itkVersion.h"

/**
 * \author Don C. Bigler
 *         The Pennsylvania State University 2005
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/1381
 *
 */

namespace itk
{
PhilipsRECImageIOFactory::PhilipsRECImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkPhilipsRECImageIO",
                          "Philips REC Image IO",
                          1,
                          CreateObjectFunction< PhilipsRECImageIO >::New() );
}

PhilipsRECImageIOFactory::~PhilipsRECImageIOFactory()
{}

const char *
PhilipsRECImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
PhilipsRECImageIOFactory::GetDescription(void) const
{
  return "Philips REC ImageIO Factory, allows the loading of Philips REC images"
         " into Insight";
}
} // end namespace itk
