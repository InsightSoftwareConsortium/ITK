/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef GDCMULEVENT_H
#define GDCMULEVENT_H

#include "gdcmNetworkStateID.h"
#include "gdcmNetworkEvents.h"
#include "gdcmBasePDU.h"
#include <vector>

namespace gdcm {
  namespace network {

/**
 * \brief ULEvent
 * \details base class for network events.
 *
 * An event consists of the event ID and the data associated with that event.
 *
 * Note that once a PDU is created, it is now the responsibility of the associated event to destroy it!
 */
class ULEvent {
      EEventID mEvent;
      std::vector<BasePDU*> mBasePDU;
	  std::istream * m_pStream ;
	  std::streampos m_posDataSet ;
      void DeletePDUVector(){
        std::vector<BasePDU*>::iterator baseItor;
        for (baseItor = mBasePDU.begin(); baseItor < mBasePDU.end(); baseItor++){
          if (*baseItor != NULL){
            delete *baseItor;
            *baseItor = NULL;
          }
        }
      }

    public:
      ULEvent(const EEventID& inEventID, std::vector<BasePDU*> inBasePDU, std::istream * iStream = nullptr, std::streampos posDataSet = 0 ){
        mEvent = inEventID;
        mBasePDU = inBasePDU;
 		m_pStream = iStream ;
 		m_posDataSet = posDataSet ; 
      }
      ULEvent(const EEventID& inEventID, BasePDU* inBasePDU, std::istream * iStream = nullptr, std::streampos posDataSet = 0 ){
        mEvent = inEventID;
        mBasePDU.push_back(inBasePDU);
 		m_pStream = iStream ;
 		m_posDataSet = posDataSet ; 
      }
      ~ULEvent(){
        DeletePDUVector();
      }

      EEventID GetEvent() const { return mEvent; }
      std::vector<BasePDU*> const & GetPDUs() const { return mBasePDU; }
 	  std::istream * GetIStream() const { return m_pStream; }
 	  std::streampos GetDataSetPos() const { return m_posDataSet; }

      void SetEvent(const EEventID& inEvent) { mEvent = inEvent; }
      void SetPDU(std::vector<BasePDU*> const & inPDU) {
        DeletePDUVector();
        mBasePDU = inPDU;
      }
    };
  }
}

#endif //GDCMULEVENT_H
