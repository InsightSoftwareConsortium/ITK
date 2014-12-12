/*  Copyright (C) 2004 Glenn Pierce.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include "itkFDFImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkFDFImageIO.h"
#include "itkVersion.h"


namespace itk
{
FDFImageIOFactory::FDFImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase", "itkFDFImageIO", "FDF Image IO", 1, CreateObjectFunction<FDFImageIO>::New());
}

FDFImageIOFactory::~FDFImageIOFactory() {}

const char *
FDFImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
FDFImageIOFactory::GetDescription() const
{
  return "FDF ImageIO Factory, allows the loading of Varian FDF images into Insight";
}


// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool FDFImageIOFactoryHasBeenRegistered;

void IOFDF_EXPORT
FDFImageIOFactoryRegister__Private(void)
{
  if (!FDFImageIOFactoryHasBeenRegistered)
  {
    FDFImageIOFactoryHasBeenRegistered = true;
    FDFImageIOFactory::RegisterOneFactory();
  }
}


} // end namespace itk
