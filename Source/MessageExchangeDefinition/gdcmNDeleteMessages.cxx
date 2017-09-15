/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2014 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#include "gdcmNDeleteMessages.h"
#include "gdcmUIDs.h"
#include "gdcmAttribute.h"
#include "gdcmImplicitDataElement.h"
#include "gdcmPresentationContextRQ.h"
#include "gdcmCommandDataSet.h"
#include "gdcmULConnection.h"

namespace gdcm{
  namespace network{

    std::vector<PresentationDataValue> NDeleteRQ::ConstructPDV(
      const ULConnection &inConnection, const BaseQuery* inQuery)
    {
      std::vector<PresentationDataValue> thePDV;
      (void)inConnection; (void)inQuery;
      assert( 0 && "TODO" );
      return thePDV;
    }

    std::vector<PresentationDataValue>  
      NDeleteRSP::ConstructPDVByDataSet(const DataSet* inDataSet){
        std::vector<PresentationDataValue> thePDV;
        (void)inDataSet;
        assert( 0 && "TODO" );
        return thePDV;
    }

  }//namespace network
}//namespace gdcm
