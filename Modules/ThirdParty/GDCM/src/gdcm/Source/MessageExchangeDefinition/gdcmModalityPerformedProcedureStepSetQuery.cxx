/*=========================================================================

Program: GDCM (Grassroots DICOM). A DICOM library

Copyright (c) 2006-2011 Mathieu Malaterre
All rights reserved.
See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmModalityPerformedProcedureStepSetQuery.h"
#include "gdcmAttribute.h"
#include "gdcmSequenceOfItems.h"
#include <algorithm>

namespace gdcm
{

  ModalityPerformedProcedureStepSetQuery::ModalityPerformedProcedureStepSetQuery( const std::string & iSopInstanceUID )
  {
    mSopInstanceUID = iSopInstanceUID ;
  }

  bool ModalityPerformedProcedureStepSetQuery::ValidateQuery(bool inStrict ) const
  {
    //if it's empty, it's not useful
    const DataSet &ds = GetQueryDataSet();
    if (ds.Size() == 0)
    {
      if (inStrict)
        gdcmWarningMacro( "Empty DataSet in ValidateQuery" );
      return false;
    }
	gdcm::DataSet requiredDataSet = GetRequiredDataSet();
	bool theReturn = ValidDataSet( ds, requiredDataSet ) ;
    return theReturn;
  }

  UIDs::TSName 
  ModalityPerformedProcedureStepSetQuery::GetAbstractSyntaxUID() const
  {
    return UIDs::ModalityPerformedProcedureStepSOPClass;
  }

  gdcm::DataSet 
  ModalityPerformedProcedureStepSetQuery::GetRequiredDataSet() const
  {
    gdcm::DataSet validDataSet ;

    gdcm::Attribute<0x0040,0x0252> PerformingPhysiciansName ;
    validDataSet.Insert( PerformingPhysiciansName.GetAsDataElement() );

    gdcm::Attribute<0x0020,0x000E> SeriesInstanceUID ;
    validDataSet.Insert( SeriesInstanceUID.GetAsDataElement() );
	
	gdcm::DataElement ReferencedImageSequence( Tag(0x0008,0x1140), 0xFFFFFFFF, VR::SQ );
    validDataSet.Insert( ReferencedImageSequence );
	
	gdcm::DataElement ReferencedNonImageCompositeSOPInstanceSequence( Tag(0x0040,0x0220), 0xFFFFFFFF, VR::SQ );
    validDataSet.Insert( ReferencedNonImageCompositeSOPInstanceSequence );

    return validDataSet ;
  }      
}
