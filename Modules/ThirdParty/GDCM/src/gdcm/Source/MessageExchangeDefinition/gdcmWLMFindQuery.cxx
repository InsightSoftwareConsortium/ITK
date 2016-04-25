/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmWLMFindQuery.h"
#include "gdcmAttribute.h"
#include "gdcmSequenceOfItems.h"

namespace gdcm
{
WLMFindQuery::WLMFindQuery()
{
  mRootType = ePatientRootType;
  mHelpDescription = "Modality Worklist query";
}

void WLMFindQuery::InitializeDataSet(const EQueryLevel& inQueryLevel)
{
  // no level so no tags to add
  (void)inQueryLevel;
}

  std::vector<Tag> 
WLMFindQuery::GetTagListByLevel(const EQueryLevel& inQueryLevel)
{
  // no level so no tags
  std::vector<Tag> tags ;
  (void)inQueryLevel;
  return tags;
}

bool 
WLMFindQuery::ValidateQuery(bool inStrict) const
{
  //if it's empty, it's not useful
  (void)inStrict;
  const DataSet &ds = GetQueryDataSet();
  if (ds.Size() == 0) return false;

  // in Query somme tags are required other are optional
  // lets check that we have required one's

  bool theReturn = true ;
  DataSet validDs = GetValidDataSet();
  theReturn &= ValidDataSet( ds, validDs );

  return theReturn ;
}

UIDs::TSName WLMFindQuery::GetAbstractSyntaxUID() const
{
  return UIDs::ModalityWorklistInformationModelFIND;
}

DataSet WLMFindQuery::GetValidDataSet() const
{
  DataSet validDataSet ;
  Attribute<0x10,0x10> PatientName;
  validDataSet.Insert( PatientName.GetAsDataElement() );
  Attribute<0x10,0x20> PatientId;
  validDataSet.Insert( PatientId.GetAsDataElement() );

  gdcm::SmartPointer<gdcm::SequenceOfItems> sqItemList = new gdcm::SequenceOfItems();
  gdcm::DataElement scheduledProcedureStepSequence;
  scheduledProcedureStepSequence.SetTag( Tag(0x40,0x0100) );
  scheduledProcedureStepSequence.SetVR( gdcm::VR::SQ );
  scheduledProcedureStepSequence.SetValue( *sqItemList );
  scheduledProcedureStepSequence.SetVLToUndefined();
  validDataSet.Insert( scheduledProcedureStepSequence );


  // Item Separator
  gdcm::Item item; //( Tag(0xfffe,0xe000) );
  item.SetVLToUndefined();

  Attribute<0x8,0x60> Modality ;
  item.GetNestedDataSet().Insert( Modality.GetAsDataElement() );
  Attribute<0x40,0x1, VR::AE, VM::VM1> ScheduledStationAETitle ;
  item.GetNestedDataSet().Insert( ScheduledStationAETitle.GetAsDataElement() );
  Attribute<0x40,0x2> ScheduledProcedureStepStartDate;
  item.GetNestedDataSet().Insert( ScheduledProcedureStepStartDate.GetAsDataElement() );
  Attribute<0x40,0x3> ScheduledProcedureStepStartTime ;
  item.GetNestedDataSet().Insert( ScheduledProcedureStepStartTime.GetAsDataElement() );
  Attribute<0x40,0x6> ScheduledPerformingPhysiciansName ;
  item.GetNestedDataSet().Insert( ScheduledPerformingPhysiciansName.GetAsDataElement() );
  sqItemList->AddItem( item );
  return validDataSet;
}

} // end namespace gdcm
