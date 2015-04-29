/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2014 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#include "gdcmNCreateMessages.h"
#include "gdcmUIDs.h"
#include "gdcmAttribute.h"
#include "gdcmImplicitDataElement.h"
#include "gdcmPresentationContextRQ.h"
#include "gdcmCommandDataSet.h"
#include "gdcmULConnection.h"

namespace gdcm{
  namespace network{

    std::vector<PresentationDataValue> NCreateRQ::ConstructPDV(
      const ULConnection &inConnection, const BaseQuery* inQuery)
    {
      std::vector<PresentationDataValue> thePDVs;
      PresentationDataValue thePDV;

      PresentationContextRQ pc( inQuery->GetAbstractSyntaxUID() );
      uint8_t presidx = inConnection.GetPresentationContextIDFromPresentationContext(pc);
      if( !presidx )
      {
        // try harder:
        PresentationContextRQ pc2( inQuery->GetAbstractSyntaxUID(), UIDs::ExplicitVRLittleEndian);
        presidx = inConnection.GetPresentationContextIDFromPresentationContext(pc2);
        if( !presidx )
        {
          gdcmErrorMacro( "Could not find Pres Cont ID" );
          return thePDVs;
        }
      }
      thePDV.SetPresentationContextID( presidx );

      thePDV.SetCommand(true);
      thePDV.SetLastFragment(true);
      //ignore incoming data set, make your own
      ;
      CommandDataSet ds;
      ds.Insert( pc.GetAbstractSyntax().GetAsDataElement() );
      {
        Attribute<0x0,0x100> at = { 0x0140 };
        ds.Insert( at.GetAsDataElement() );
      }

      {
        Attribute<0x0,0x110> at = { 1 };
        ds.Insert( at.GetAsDataElement() );
      }

      {
        Attribute<0x0,0x800> at = { 1 };
        ds.Insert( at.GetAsDataElement() );
      }

      {
        Attribute<0x0,0x1000> at = { inQuery->GetSOPInstanceUID() };
        ds.Insert( at.GetAsDataElement() );
      }

      {
        Attribute<0x0,0x0> at = { 0 };
        unsigned int glen = ds.GetLength<ImplicitDataElement>();
        assert( (glen % 2) == 0 );
        at.SetValue( glen );
        ds.Insert( at.GetAsDataElement() );
      }

      thePDV.SetDataSet(ds);
      thePDVs.push_back(thePDV);
      thePDV.SetDataSet(inQuery->GetQueryDataSet());
      thePDV.SetMessageHeader( 2 );
      thePDVs.push_back(thePDV);
      return thePDVs;

    }

    std::vector<PresentationDataValue>  
      NCreateRSP::ConstructPDVByDataSet(const DataSet* inDataSet){
        std::vector<PresentationDataValue> thePDV;
        (void)inDataSet;
        assert( 0 && "TODO" );
        return thePDV;
    }

  }//namespace network
}//namespace gdcm
