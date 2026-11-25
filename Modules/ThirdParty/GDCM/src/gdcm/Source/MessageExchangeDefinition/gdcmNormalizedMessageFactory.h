/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2014 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMNORMALIZEDMESSAGEFACTORY_H
#define GDCMNORMALIZEDMESSAGEFACTORY_H

#include "gdcmPresentationDataValue.h"
#include "gdcmULConnection.h"

namespace gdcm {
  class BaseQuery;
  class File;
  namespace network {
    class BasePDU;

class NormalizedMessageFactory
{
    public:
      static  std::vector<PresentationDataValue> ConstructNEventReport	(const ULConnection& inConnection, const BaseQuery* inQuery);
      static  std::vector<PresentationDataValue> ConstructNGet			(const ULConnection& inConnection, const BaseQuery* inQuery);
      static  std::vector<PresentationDataValue> ConstructNSet			(const ULConnection& inConnection, const BaseQuery* inQuery);
      static  std::vector<PresentationDataValue> ConstructNAction		(const ULConnection& inConnection, const BaseQuery* inQuery);
      static  std::vector<PresentationDataValue> ConstructNCreate		(const ULConnection& inConnection, const BaseQuery* inQuery);
      static  std::vector<PresentationDataValue> ConstructNDelete		(const ULConnection& inConnection, const BaseQuery* inQuery);


    };
  }
}

#endif // GDCMNORMALIZEDMESSAGEFACTORY_H
