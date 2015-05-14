/*=========================================================================

Program: GDCM (Grassroots DICOM). A DICOM library

Copyright (c) 2006-2011 Mathieu Malaterre
All rights reserved.
See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmModalityPerformedProcedureStepCreateQuery.h"
#include "gdcmAttribute.h"
#include "gdcmSequenceOfItems.h"
#include <algorithm>

namespace gdcm
{

  ModalityPerformedProcedureStepCreateQuery::ModalityPerformedProcedureStepCreateQuery( const std::string & iSopInstanceUID )
  {
    mSopInstanceUID = iSopInstanceUID ;
  }

  bool ModalityPerformedProcedureStepCreateQuery::ValidateQuery(bool inStrict ) const
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

  UIDs::TSName ModalityPerformedProcedureStepCreateQuery::GetAbstractSyntaxUID() const
  {
    return UIDs::ModalityPerformedProcedureStepSOPClass;
  }

  gdcm::DataSet 
  ModalityPerformedProcedureStepCreateQuery::GetRequiredDataSet() const
  {
		gdcm::DataSet validDataSet ;
		gdcm::DataElement ScheduledStepAttributeSequence( Tag(0x0040,0x0270), 0xFFFFFFFF, VR::SQ );
		gdcm::SmartPointer<gdcm::SequenceOfItems> sqItemList = new gdcm::SequenceOfItems();
		ScheduledStepAttributeSequence.SetValue( *sqItemList );
		validDataSet.Insert( ScheduledStepAttributeSequence );
		gdcm::Item & ScheduledStepAttributeSequenceItem = sqItemList->AddNewUndefinedLengthItem();

		gdcm::Attribute<0x0020,0x000D> StudyInstanceUID;
		ScheduledStepAttributeSequenceItem.GetNestedDataSet().Insert( StudyInstanceUID.GetAsDataElement() );

		sqItemList = new gdcm::SequenceOfItems();
		gdcm::DataElement ReferencedStudySequence( Tag(0x0008,0x1110), 0xFFFFFFFF, VR::SQ );
		ReferencedStudySequence.SetValue( *sqItemList );
		ScheduledStepAttributeSequenceItem.GetNestedDataSet().Insert( ReferencedStudySequence );
		
		gdcm::Attribute<0x0008,0x0050> AccessionNumber;
		ScheduledStepAttributeSequenceItem.GetNestedDataSet().Insert( AccessionNumber.GetAsDataElement() );

		gdcm::Attribute<0x0040,0x1001> RequestedProcedureID;
		ScheduledStepAttributeSequenceItem.GetNestedDataSet().Insert( RequestedProcedureID.GetAsDataElement() );
		
		gdcm::Attribute<0x0032,0x1060> RequestedProcedureDescription;
		ScheduledStepAttributeSequenceItem.GetNestedDataSet().Insert( RequestedProcedureDescription.GetAsDataElement() );

		gdcm::Attribute<0x0040,0x0009> ScheduledProcedureStepID;
		ScheduledStepAttributeSequenceItem.GetNestedDataSet().Insert( ScheduledProcedureStepID.GetAsDataElement() );
		gdcm::Attribute<0x0040,0x0007> ScheduledProcedureStepDescription;
		ScheduledStepAttributeSequenceItem.GetNestedDataSet().Insert( ScheduledProcedureStepDescription.GetAsDataElement() );

		sqItemList = new gdcm::SequenceOfItems();
		gdcm::DataElement ScheduledProtocolCodeSequence( Tag(0x0040,0x0008), 0xFFFFFFFF, VR::SQ );
		ScheduledProtocolCodeSequence.SetValue( *sqItemList );
		ScheduledStepAttributeSequenceItem.GetNestedDataSet().Insert( ScheduledProtocolCodeSequence );

		gdcm::Attribute<0x0010,0x0010> PatientsName;
		validDataSet.Insert( PatientsName.GetAsDataElement() );

		gdcm::Attribute<0x0010,0x0020> PatientID ;
		validDataSet.Insert( PatientID.GetAsDataElement() );

		gdcm::Attribute<0x0010,0x0030> PatientsBirthDate ;
		validDataSet.Insert( PatientsBirthDate.GetAsDataElement() );

		gdcm::Attribute<0x0010,0x0040> PatientsSex ;
		validDataSet.Insert( PatientsSex.GetAsDataElement() );

		gdcm::DataElement ReferencedPatientSequence( Tag(0x0008,0x1120), 0xFFFFFFFF, VR::SQ );
		validDataSet.Insert( ReferencedPatientSequence );

		gdcm::Attribute<0x0040,0x0253> PerformedProcedureStepID ;
		validDataSet.Insert( PerformedProcedureStepID.GetAsDataElement() );

		gdcm::Attribute<0x0040,0x0241> PerformedStationAETitle ;
		validDataSet.Insert( PerformedStationAETitle.GetAsDataElement() );

		gdcm::Attribute<0x0040,0x0242> PerformedStationName ;
		validDataSet.Insert( PerformedStationName.GetAsDataElement() );

		gdcm::Attribute<0x0040,0x0243> PerformedLocation ;
		validDataSet.Insert( PerformedLocation.GetAsDataElement() );

		gdcm::Attribute<0x0040,0x0244> PerformedProcedureStepStartDate ;
		validDataSet.Insert( PerformedProcedureStepStartDate.GetAsDataElement() );

		gdcm::Attribute<0x0040,0x0245> PerformedProcedureStepStartTime ;
		validDataSet.Insert( PerformedProcedureStepStartTime.GetAsDataElement() );

		gdcm::Attribute<0x0040,0x0252> PerformedProcedureStepStatus ;
		validDataSet.Insert( PerformedProcedureStepStatus.GetAsDataElement() );

		gdcm::Attribute<0x0040,0x0254> PerformedProcedureStepDescription ;
		validDataSet.Insert( PerformedProcedureStepDescription.GetAsDataElement() );

		gdcm::Attribute<0x0040,0x0255> PerformedProcedureTypeDescription ;
		validDataSet.Insert( PerformedProcedureTypeDescription.GetAsDataElement() );

		gdcm::DataElement ProcedureCodeSequence( Tag(0x0008,0x1032), 0xFFFFFFFF, VR::SQ );
		validDataSet.Insert( ProcedureCodeSequence );

		gdcm::Attribute<0x0040,0x0250> PerformedProcedureStepEndDate ;
		validDataSet.Insert( PerformedProcedureStepEndDate.GetAsDataElement() );

		gdcm::Attribute<0x0040,0x0251> PerformedProcedureStepEndTime ;
		validDataSet.Insert( PerformedProcedureStepEndTime.GetAsDataElement() );

		gdcm::Attribute<0x0008,0x0060> Modality ;
		validDataSet.Insert( Modality.GetAsDataElement() );

		gdcm::Attribute<0x0020,0x0010> StudyID ;
		validDataSet.Insert( StudyID.GetAsDataElement() );

		gdcm::DataElement PerformedProtocolCodeSequence( Tag(0x0040,0x0260), 0xFFFFFFFF, VR::SQ );
		validDataSet.Insert( PerformedProtocolCodeSequence );

		gdcm::DataElement PerformedSeriesSequence( Tag(0x0040,0x0340), 0xFFFFFFFF, VR::SQ );
		validDataSet.Insert( PerformedSeriesSequence );

		return validDataSet ;
  }      
}
