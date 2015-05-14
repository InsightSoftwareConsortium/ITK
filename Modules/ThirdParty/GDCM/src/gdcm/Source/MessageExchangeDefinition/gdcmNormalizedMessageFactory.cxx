/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2014 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
/*
This class constructs PDataPDUs, but that have been specifically constructed for the
Normalized DICOM services (N-EventReport, N-Get, N-Set, N-Action, N-Create, N-Delete).  It will also handle
parsing the incoming data to determine which of the NormalizedPDUs the incoming data is, and
so therefore allowing the scu to determine what to do with incoming data (if acting as
a storescp server, for instance).

*/

#include "gdcmNormalizedMessageFactory.h"
#include "gdcmBaseQuery.h"

#include "gdcmNActionMessages.h"
#include "gdcmNCreateMessages.h"
#include "gdcmNDeleteMessages.h"
#include "gdcmNEventReportMessages.h"
#include "gdcmNGetMessages.h"
#include "gdcmNSetMessages.h"

namespace gdcm {
  namespace network {
    std::vector<PresentationDataValue> NormalizedMessageFactory::ConstructNEventReport	(const ULConnection& inConnection, const BaseQuery* inQuery)
    {
      NEventReportRQ theNEventReportRQ;
      return theNEventReportRQ.ConstructPDV(inConnection, inQuery);
    }
    std::vector<PresentationDataValue> NormalizedMessageFactory::ConstructNGet			(const ULConnection& inConnection, const BaseQuery* inQuery)
    {
      NGetRQ theNGetRQ;
      return theNGetRQ.ConstructPDV(inConnection, inQuery);
    }
    std::vector<PresentationDataValue> NormalizedMessageFactory::ConstructNSet			(const ULConnection& inConnection, const BaseQuery* inQuery)
    {
      NSetRQ theNSetRQ;
      return theNSetRQ.ConstructPDV(inConnection, inQuery);
    }
    std::vector<PresentationDataValue> NormalizedMessageFactory::ConstructNAction		(const ULConnection& inConnection, const BaseQuery* inQuery)
    {
      NActionRQ theNActionRQ;
      return theNActionRQ.ConstructPDV(inConnection, inQuery);
    }
    std::vector<PresentationDataValue> NormalizedMessageFactory::ConstructNCreate		(const ULConnection& inConnection, const BaseQuery* inQuery)
    {
      NCreateRQ theNCreate;
      return theNCreate.ConstructPDV(inConnection, inQuery);
    }
    std::vector<PresentationDataValue> NormalizedMessageFactory::ConstructNDelete		(const ULConnection& inConnection, const BaseQuery* inQuery)
    {
      NDeleteRQ theNDeleteRQ;
      return theNDeleteRQ.ConstructPDV(inConnection, inQuery);
    }
  }
}
