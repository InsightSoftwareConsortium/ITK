/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2014 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMCNCREATEMESSAGES_H
#define GDCMCNCREATEMESSAGES_H

#include "gdcmBaseNormalizedMessage.h"

namespace gdcm{
  namespace network{

class ULConnection;

/**
 * \brief NCreateRQ
 * \details this file defines the messages for the ncreate action
 */
class NCreateRQ : public BaseNormalizedMessage {
    public:
      std::vector<PresentationDataValue> ConstructPDV(const ULConnection &inConnection,
        const BaseQuery* inQuery);
    };

/**
 * \brief NCreateRSP
 * this file defines the messages for the ncreate action
 */
    class NCreateRSP : public BaseNormalizedMessage {
    public:
      std::vector<PresentationDataValue> ConstructPDVByDataSet(const DataSet* inDataSet);
    };
  }
}
#endif // GDCMCNCREATEMESSAGES_H
