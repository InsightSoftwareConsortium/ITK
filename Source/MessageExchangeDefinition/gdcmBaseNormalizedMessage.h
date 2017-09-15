/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2014 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMBASENORMALIZEDMESSSAGE_H
#define GDCMBASENORMALIZEDMESSSAGE_H

#include "gdcmPresentationDataValue.h"
#include "gdcmBaseQuery.h"

#include <vector>

namespace gdcm
{
  namespace network
    {
class ULConnection;
/**
 * \brief BaseNormalizedMessage
 * \details The Normalized events described in section 3.7-2011 of the DICOM standard all
 * use their own messages.  These messages are constructed using Presentation
 * Data Values, from section 3.8-2011 of the standard, and then fill in
 * appropriate values in their datasets.
 *
 * So, for the five normalized:
 * \li N-ACTION
 * \li N-CREATE
 * \li N-DELETE
 * \li N-EVENT
 * \li N-GET
 * \li N-SET
 * there are a series of messages.  However, all of these messages are obtained
 * as part of a PDataPDU, and all have to be placed there.  Therefore, since
 * they all have shared functionality and construction tropes, that will be put
 * into a base class.  Further, the base class will be then returned by the
 * factory class, gdcmNormalizedMessageFactory.h.
 *
 * This is an abstract class.  It cannot be instantiated on its own.
 */
class BaseNormalizedMessage
{
    public:
      virtual ~BaseNormalizedMessage() {}
      //construct the appropriate pdv and dataset for this message
      //for instance, setting tag 0x0,0x100 to the appropriate value
      //the pdv, as described in Annex E of 3.8-2009, is the first byte
      //of the message (the MessageHeader), and then the subsequent dataset
      //that describes the operation.
      virtual std::vector<PresentationDataValue> ConstructPDV(	const ULConnection &inConnection,
																const BaseQuery * inQuery) = 0;
    };
  }
}
#endif //GDCMBASENORMALIZEDMESSSAGE_H
