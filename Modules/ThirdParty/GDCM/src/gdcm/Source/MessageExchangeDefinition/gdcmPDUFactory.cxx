/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*
basically, given an initial byte, construct the appropriate PDU.
this way, the event loop doesn't have to know about all the different PDU types.

name and date: 25 Sept 2010 mmr

Update on 27 sept 2010 mmr: since this is where all PDUs are included, also use this
class to construct specific instances of PDUs, and return the BasePDU class.
*/
#include "gdcmPDUFactory.h"
#include "gdcmAAbortPDU.h"
#include "gdcmAAssociateACPDU.h"
#include "gdcmAAssociateRJPDU.h"
#include "gdcmAAssociateRQPDU.h"
#include "gdcmAReleaseRPPDU.h"
#include "gdcmAReleaseRQPDU.h"
#include "gdcmPDataTFPDU.h"
#include "gdcmCompositeMessageFactory.h"
#include "gdcmNormalizedMessageFactory.h"
#include "gdcmBaseRootQuery.h"
#include "gdcmBasePDU.h"

namespace gdcm
{
namespace network
{

//eventually needs to be smartpointer'd
BasePDU* PDUFactory::ConstructPDU(uint8_t itemtype)
{
  BasePDU* thePDU = nullptr;
  switch (itemtype)
    {
  case 0x01:
    thePDU = new AAssociateRQPDU;
    break;
  case 0x02:
    thePDU = new AAssociateACPDU;
    break;
  case 0x03:
    thePDU = new AAssociateRJPDU;
    break;
  case 0x04:
    thePDU = new PDataTFPDU;
    break;
  case 0x05:
    thePDU = new AReleaseRQPDU;
    break;
  case 0x06:
    thePDU = new AReleaseRPPDU;
    break;
  case 0x07:
    thePDU = new AAbortPDU;
    break;
    //default is that the PDU remains null
    }
  if( !thePDU )
    {
    gdcmErrorMacro( "Could not construct PDU for itemtype: " << (int)itemtype );
    }
  return thePDU;
}

//determine which event was received by the PDU type
EEventID PDUFactory::DetermineEventByPDU(const BasePDU* inPDU)
{
  if(inPDU)
    {
    const AAssociateRQPDU* theAAssociateRQPDU = dynamic_cast<const AAssociateRQPDU*>(inPDU);
    if (theAAssociateRQPDU != nullptr)
      {
      return eAASSOCIATE_RQPDUreceived;
      }
    const AAssociateACPDU* theAAssociateACPDU = dynamic_cast<const AAssociateACPDU*>(inPDU);
    if (theAAssociateACPDU != nullptr)
      {
      return eASSOCIATE_ACPDUreceived;
      }
    const AAssociateRJPDU* theAAssociateRJPDU = dynamic_cast<const AAssociateRJPDU*>(inPDU);
    if (theAAssociateRJPDU != nullptr)
      {
      return eASSOCIATE_RJPDUreceived;
      }
    const PDataTFPDU* thePDataTFPDU = dynamic_cast<const PDataTFPDU*>(inPDU);
    if (thePDataTFPDU != nullptr)
      {
      ///
      const PresentationDataValue &pdv = thePDataTFPDU->GetPresentationDataValue(0);
      (void)pdv;
#if 0
      int mh = pdv.GetMessageHeader();
      if( mh == 3 )
        {
        // E.2 MESSAGE CONTROL HEADER ENCODING
        // If bit 1 is set to 1, the following fragment shall contain the last
        // fragment of a Message Data Set or of a Message Command.
        std::cout << "This was the last fragment of the message data set" << std::endl;
        }
#endif
      return ePDATATFPDU;
      }
    const AReleaseRQPDU* theAReleaseRQPDU = dynamic_cast<const AReleaseRQPDU*>(inPDU);
    if (theAReleaseRQPDU != nullptr)
      {
      return eARELEASE_RQPDUReceivedOpen;
      }
    const AReleaseRPPDU* theAReleaseRPPDU = dynamic_cast<const AReleaseRPPDU*>(inPDU);
    if (theAReleaseRPPDU != nullptr)
      {
      return eARELEASE_RPPDUReceived;
      }
    const AAbortPDU* theAAbortPDU = dynamic_cast<const AAbortPDU*>(inPDU);
    if (theAAbortPDU != nullptr)
      {
      return eAABORTPDUReceivedOpen;
      }
    }
  return eEventDoesNotExist;
}

BasePDU* PDUFactory::ConstructReleasePDU()
{
  AReleaseRQPDU* theAReleaseRQPDU = new AReleaseRQPDU();

  return theAReleaseRQPDU;
}
BasePDU* PDUFactory::ConstructAbortPDU()
{
  AAbortPDU* theAAbortPDU = new AAbortPDU();

  return theAAbortPDU;
}
std::vector<BasePDU*> PDUFactory::CreateCEchoPDU(const ULConnection& inConnection)
{
  std::vector<PresentationDataValue> pdv =
    CompositeMessageFactory::ConstructCEchoRQ(inConnection);
  std::vector<PresentationDataValue>::iterator pdvItor;
  std::vector<BasePDU*> outVector;
  for (pdvItor = pdv.begin(); pdvItor < pdv.end(); pdvItor++)
    {
    PDataTFPDU* thePDataTFPDU = new PDataTFPDU();
    thePDataTFPDU->AddPresentationDataValue( *pdvItor );
    outVector.push_back(thePDataTFPDU);
    }
  return outVector;
}

std::vector<BasePDU*> PDUFactory::CreateCMovePDU(const ULConnection&
  inConnection, const BaseRootQuery* inRootQuery)
{
  std::vector<PresentationDataValue> pdv =
    CompositeMessageFactory::ConstructCMoveRQ(inConnection, inRootQuery );
  std::vector<PresentationDataValue>::iterator pdvItor;
  std::vector<BasePDU*> outVector;
  for (pdvItor = pdv.begin(); pdvItor < pdv.end(); pdvItor++)
    {
    PDataTFPDU* thePDataTFPDU = new PDataTFPDU();
    thePDataTFPDU->AddPresentationDataValue( *pdvItor );
    outVector.push_back(thePDataTFPDU);
    }
  return outVector;
}

std::vector<BasePDU*> PDUFactory::CreateCStoreRQPDU(const ULConnection& inConnection,
  const File& file, bool writeDataSet /*= true*/ )
{
  std::vector<PresentationDataValue> pdv =
    CompositeMessageFactory::ConstructCStoreRQ(inConnection, file, writeDataSet );
  std::vector<PresentationDataValue>::iterator pdvItor;
  std::vector<BasePDU*> outVector;
  for (pdvItor = pdv.begin(); pdvItor < pdv.end(); pdvItor++)
    {
    PDataTFPDU* thePDataTFPDU = new PDataTFPDU;
    thePDataTFPDU->AddPresentationDataValue( *pdvItor );
    outVector.push_back(thePDataTFPDU);
    }
  return outVector;
}

std::vector<BasePDU*> PDUFactory::CreateCStoreRSPPDU(const DataSet* inDataSet,
  const BasePDU* inPDU)
{
  std::vector<PresentationDataValue> pdv =
    CompositeMessageFactory::ConstructCStoreRSP(inDataSet, inPDU );
  std::vector<PresentationDataValue>::iterator pdvItor;
  std::vector<BasePDU*> outVector;
  for (pdvItor = pdv.begin(); pdvItor < pdv.end(); pdvItor++)
    {
    PDataTFPDU* thePDataTFPDU = new PDataTFPDU;
    thePDataTFPDU->AddPresentationDataValue( *pdvItor );
    outVector.push_back(thePDataTFPDU);
    }
  return outVector;
}

std::vector<BasePDU*> PDUFactory::CreateCFindPDU(const ULConnection& inConnection,
  const BaseRootQuery* inRootQuery)
{
//still have to build this!
  std::vector<PresentationDataValue> pdv =
    CompositeMessageFactory::ConstructCFindRQ(inConnection, inRootQuery );
  std::vector<PresentationDataValue>::iterator pdvItor;
  std::vector<BasePDU*> outVector;
  for (pdvItor = pdv.begin(); pdvItor < pdv.end(); pdvItor++)
    {
    PDataTFPDU* thePDataTFPDU = new PDataTFPDU();
    thePDataTFPDU->AddPresentationDataValue( *pdvItor );
    outVector.push_back(thePDataTFPDU);
    }
  return outVector;


}

std::vector<BasePDU*> PDUFactory::CreateNEventReportPDU		(const ULConnection& inConnection, const BaseQuery *inQuery)
{
//still have to build this!
  std::vector<PresentationDataValue> pdv =
	  NormalizedMessageFactory::ConstructNEventReport(inConnection, inQuery );
  std::vector<PresentationDataValue>::iterator pdvItor;
  std::vector<BasePDU*> outVector;
  for (pdvItor = pdv.begin(); pdvItor < pdv.end(); pdvItor++)
    {
    PDataTFPDU* thePDataTFPDU = new PDataTFPDU();
    thePDataTFPDU->AddPresentationDataValue( *pdvItor );
    outVector.push_back(thePDataTFPDU);
    }
  return outVector;


}
std::vector<BasePDU*> PDUFactory::CreateNGetPDU				(const ULConnection& inConnection, const BaseQuery *inQuery)
{
//still have to build this!
  std::vector<PresentationDataValue> pdv =
    NormalizedMessageFactory::ConstructNGet(inConnection, inQuery );
  std::vector<PresentationDataValue>::iterator pdvItor;
  std::vector<BasePDU*> outVector;
  for (pdvItor = pdv.begin(); pdvItor < pdv.end(); pdvItor++)
    {
    PDataTFPDU* thePDataTFPDU = new PDataTFPDU();
    thePDataTFPDU->AddPresentationDataValue( *pdvItor );
    outVector.push_back(thePDataTFPDU);
    }
  return outVector;


}
std::vector<BasePDU*> PDUFactory::CreateNSetPDU				(const ULConnection& inConnection, const BaseQuery *inQuery)
{
//still have to build this!
  std::vector<PresentationDataValue> pdv =
    NormalizedMessageFactory::ConstructNSet(inConnection, inQuery );
  std::vector<PresentationDataValue>::iterator pdvItor;
  std::vector<BasePDU*> outVector;
  for (pdvItor = pdv.begin(); pdvItor < pdv.end(); pdvItor++)
    {
    PDataTFPDU* thePDataTFPDU = new PDataTFPDU();
    thePDataTFPDU->AddPresentationDataValue( *pdvItor );
    outVector.push_back(thePDataTFPDU);
    }
  return outVector;


}
std::vector<BasePDU*> PDUFactory::CreateNActionPDU			(const ULConnection& inConnection, const BaseQuery *inQuery)
{
//still have to build this!
  std::vector<PresentationDataValue> pdv =
    NormalizedMessageFactory::ConstructNAction(inConnection, inQuery );
  std::vector<PresentationDataValue>::iterator pdvItor;
  std::vector<BasePDU*> outVector;
  for (pdvItor = pdv.begin(); pdvItor < pdv.end(); pdvItor++)
    {
    PDataTFPDU* thePDataTFPDU = new PDataTFPDU();
    thePDataTFPDU->AddPresentationDataValue( *pdvItor );
    outVector.push_back(thePDataTFPDU);
    }
  return outVector;


}
std::vector<BasePDU*> PDUFactory::CreateNCreatePDU			(const ULConnection& inConnection, const BaseQuery *inQuery)
{
//still have to build this!
  std::vector<PresentationDataValue> pdv =
	  NormalizedMessageFactory::ConstructNCreate(inConnection, inQuery );
  std::vector<PresentationDataValue>::iterator pdvItor;
  std::vector<BasePDU*> outVector;
  for (pdvItor = pdv.begin(); pdvItor < pdv.end(); pdvItor++)
    {
    PDataTFPDU* thePDataTFPDU = new PDataTFPDU();
    thePDataTFPDU->AddPresentationDataValue( *pdvItor );
    outVector.push_back(thePDataTFPDU);
    }
  return outVector;


}
std::vector<BasePDU*> PDUFactory::CreateNDeletePDU			(const ULConnection& inConnection, const BaseQuery *inQuery)
{
//still have to build this!
  std::vector<PresentationDataValue> pdv =
    NormalizedMessageFactory::ConstructNDelete(inConnection, inQuery );
  std::vector<PresentationDataValue>::iterator pdvItor;
  std::vector<BasePDU*> outVector;
  for (pdvItor = pdv.begin(); pdvItor < pdv.end(); pdvItor++)
    {
    PDataTFPDU* thePDataTFPDU = new PDataTFPDU();
    thePDataTFPDU->AddPresentationDataValue( *pdvItor );
    outVector.push_back(thePDataTFPDU);
    }
  return outVector;


}


//given data pdus, produce the presentation data values stored within.
//all operations have these as the payload of the data sending operation
//however, echo does not have a dataset in the pdv.
std::vector<PresentationDataValue> PDUFactory::GetPDVs(const std::vector<BasePDU*> & inDataPDUs)
{
  std::vector<BasePDU*>::const_iterator itor;
  std::vector<PresentationDataValue> outPDVs;
  for (itor = inDataPDUs.begin(); itor < inDataPDUs.end(); itor++)
    {
    PDataTFPDU* thePDataTFPDU = dynamic_cast<PDataTFPDU*>(*itor);
    if (thePDataTFPDU == nullptr)
      {
      assert(0); //shouldn't really get here.
      return outPDVs; //just stop now, no longer with data pdus.
      }
    size_t theNumPDVsinPDU = thePDataTFPDU->GetNumberOfPresentationDataValues();
    for (size_t i = 0; i < theNumPDVsinPDU; i++)
      {
      outPDVs.push_back(thePDataTFPDU->GetPresentationDataValue(i));
      }
    }
  return outPDVs;
}
} // end namespace network
} // end namespace gdcm
