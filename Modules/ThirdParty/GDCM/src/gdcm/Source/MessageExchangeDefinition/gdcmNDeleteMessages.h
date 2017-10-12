/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2014 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMCNDELETEMESSAGES_H
#define GDCMCNDELETEMESSAGES_H

#include "gdcmBaseNormalizedMessage.h"

namespace gdcm{
  namespace network{

class ULConnection;

/**
 * \brief NDeleteRQ
 * \details this file defines the messages for the ndelete action
 */
class NDeleteRQ : public BaseNormalizedMessage {
    public:
      std::vector<PresentationDataValue> ConstructPDV(const ULConnection &inConnection,
        const BaseQuery* inQuery);
    };

/**
 * \brief NDeleteRSP
 * this file defines the messages for the ndelete action
 */
    class NDeleteRSP : public BaseNormalizedMessage {
    public:
      std::vector<PresentationDataValue> ConstructPDVByDataSet(const DataSet* inDataSet);
    };
  }
}
#endif // GDCMCNDELETEMESSAGES_H
