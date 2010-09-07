/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBruker2DSEQImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBruker2DSEQImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkBruker2DSEQImageIO.h"
#include "itkVersion.h"

/*
* \author Don C. Bigler
*         The Pennsylvania State University 2005
*
* This implementation was contributed as a paper to the Insight Journal
* http://insight-journal.org/midas/handle.php?handle=1926/1381
*
*/

namespace itk
{
Bruker2DSEQImageIOFactory::Bruker2DSEQImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkBruker2DSEQImageIO",
                          "Bruker 2DSEQ Image IO",
                          1,
                          CreateObjectFunction< Bruker2DSEQImageIO >::New() );
}

Bruker2DSEQImageIOFactory::~Bruker2DSEQImageIOFactory()
{}

const char *
Bruker2DSEQImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
Bruker2DSEQImageIOFactory::GetDescription(void) const
{
  return "Bruker 2DSEQ ImageIO Factory, allows the loading of most Bruker 2DSEQ"
         " images into Insight";
}
} // end namespace itk
