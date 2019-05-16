/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMDICOMDIR_H
#define GDCMDICOMDIR_H

#include <utility>
#include "gdcmFileSet.h"

namespace gdcm
{
/**
 * \brief DICOMDIR class
 *
 * \details Structured for handling DICOMDIR
 */
class GDCM_EXPORT DICOMDIR
{
public:
  DICOMDIR() = default;
  DICOMDIR(FileSet  fs):_FS(std::move(std::move(fs))) {}

private:
  FileSet _FS;
  //13 sept 2010 mmr-- added the underscore to FS to compile under Sunos gcc
};

} // end namespace gdcm

#endif //GDCMDICOMDIR_H
