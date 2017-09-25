/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmSegmentWriter.h"
#include "gdcmAttribute.h"

namespace gdcm
{

SegmentWriter::SegmentWriter()
{
}

SegmentWriter::~SegmentWriter()
{
}

unsigned int SegmentWriter::GetNumberOfSegments() const
{
  return (unsigned int)Segments.size();
}

void SegmentWriter::SetNumberOfSegments(const unsigned int size)
{
  Segments.resize(size);
}

const SegmentWriter::SegmentVector & SegmentWriter::GetSegments() const
{
  return Segments;
}

SegmentWriter::SegmentVector & SegmentWriter::GetSegments()
{
  return Segments;
}

SmartPointer< Segment > SegmentWriter::GetSegment(const unsigned int idx /*= 0*/) const
{
  assert( idx < Segments.size() );
  return Segments[idx];
}

void SegmentWriter::AddSegment(SmartPointer< Segment > segment)
{
  Segments.push_back(segment);
}

void SegmentWriter::SetSegments(SegmentVector & segments)
{
  Segments = segments;
}


void writeCodeSequenceMacroAttributes(const SegmentHelper::BasicCodedEntry & entry,
                                      const Tag & tag,
                                      DataSet & dataset,
                                      bool severalItemsAllowed)
{
  SmartPointer<SequenceOfItems> sequence;

  // If the sequence does not exist, we create it
  if(!dataset.FindDataElement(tag))
  {
    sequence = new SequenceOfItems();
    DataElement dataElement( tag );
    dataElement.SetVR( VR::SQ );
    dataElement.SetValue(*sequence);
    dataElement.SetVLToUndefined();
    dataset.Insert(dataElement);
  }

  // Retrieve the sequence from the dataset
  sequence = dataset.GetDataElement(tag).GetValueAsSQ();

  // Check if an item is already present in the sequence
  if (!severalItemsAllowed && sequence->GetNumberOfItems() > 0)
  {
    // Obviously the user has already added an item to the sequence
    // Let's assume the user knows what he does
    return;
  }

  // Fill the Sequence
  sequence->SetLengthToUndefined();

  Item item;
  item.SetVLToUndefined();
  DataSet & itemDataSet = item.GetNestedDataSet();

  // Code Sequence Macro Attributes
  {
    // Code Value (Type 1C)
    Attribute<0x0008, 0x0100> codeValueAttribute;
    codeValueAttribute.SetValue(entry.CV);
    itemDataSet.Replace(codeValueAttribute.GetAsDataElement());

    // Coding Scheme Designator (Type 1C)
    Attribute<0x0008, 0x0102> codingSchemeDesignatorAttribute;
    codingSchemeDesignatorAttribute.SetValue(entry.CSD);
    itemDataSet.Replace(codingSchemeDesignatorAttribute.GetAsDataElement());

    // Coding Scheme Version (Type 1C)
    if(!entry.CSV.empty())
    {
      Attribute<0x0008, 0x0103> codingSchemeVersionAttribute;
      codingSchemeVersionAttribute.SetValue(entry.CSV);
      itemDataSet.Replace(codingSchemeVersionAttribute.GetAsDataElement());
    }

    // Code Meaning (Type 1)
    Attribute<0x0008, 0x0104> codeMeaningAttribute;
    codeMeaningAttribute.SetValue(entry.CM);
    itemDataSet.Replace(codeMeaningAttribute.GetAsDataElement());
  }

  sequence->AddItem(item);

}

void writeCodeSequenceMacroAttributes(const Segment::BasicCodedEntryVector & entries,
                                      const Tag & tag,
                                      DataSet & dataset)
{
  Segment::BasicCodedEntryVector::const_iterator it = entries.begin();
  for(; it != entries.end(); ++it)
  {
    writeCodeSequenceMacroAttributes(*it, tag, dataset, true);
  }
}

bool SegmentWriter::PrepareWrite()
{
  File &      file    = GetFile();
  DataSet &   ds      = file.GetDataSet();

  // Segment Sequence
  SmartPointer<SequenceOfItems> segmentsSQ;
  if( !ds.FindDataElement( Tag(0x0062, 0x0002) ) )
  {
    segmentsSQ = new SequenceOfItems;
    DataElement detmp( Tag(0x0062, 0x0002) );
    detmp.SetVR( VR::SQ );
    detmp.SetValue( *segmentsSQ );
    detmp.SetVLToUndefined();
    ds.Insert( detmp );
  }
  segmentsSQ = ds.GetDataElement( Tag(0x0062, 0x0002) ).GetValueAsSQ();
  segmentsSQ->SetLengthToUndefined();

  {
    // Fill the Segment Sequence
    const unsigned int              numberOfSegments  = this->GetNumberOfSegments();
    assert( numberOfSegments );
    const size_t nbItems           = segmentsSQ->GetNumberOfItems();
    if (nbItems < numberOfSegments)
    {
      const size_t diff           = numberOfSegments - nbItems;
      const size_t nbOfItemToMake = (diff > 0?diff:0);
      for(unsigned int i = 1; i <= nbOfItemToMake; ++i)
      {
        Item item;
        item.SetVLToUndefined();
        segmentsSQ->AddItem(item);
      }
    }
  }
  // else Should I remove items?

  std::vector< SmartPointer< Segment > >::const_iterator  it0            = Segments.begin();
  std::vector< SmartPointer< Segment > >::const_iterator  it0End         = Segments.end();
  unsigned int                                            itemNumber    = 1;
  unsigned long                                           surfaceNumber = 1;
  for (; it0 != it0End; it0++)
  {
    SmartPointer< Segment > segment = *it0;
    assert( segment );

    Item &    segmentItem = segmentsSQ->GetItem(itemNumber);
    DataSet & segmentDS   = segmentItem.GetNestedDataSet();

    // Segment Number (Type 1)
    Attribute<0x0062, 0x0004> segmentNumberAt;
    unsigned short segmentNumber = segment->GetSegmentNumber();
    if (segmentNumber == 0)
      segmentNumber = (unsigned short)itemNumber;
    segmentNumberAt.SetValue( segmentNumber );
    segmentDS.Replace( segmentNumberAt.GetAsDataElement() );

    // Segment Label (Type 1)
    const char * segmentLabel = segment->GetSegmentLabel();
    if ( strcmp(segmentLabel, "") != 0 )
    {
      gdcmWarningMacro("No segment label specified");
    }
    Attribute<0x0062, 0x0005> segmentLabelAt;
    segmentLabelAt.SetValue( segmentLabel );
    segmentDS.Replace( segmentLabelAt.GetAsDataElement() );

    // Segment Description (Type 3)
    const char * segmentDescription = segment->GetSegmentDescription();
    if ( strcmp(segmentDescription, "") != 0 )
    {
      Attribute<0x0062, 0x0006> segmentDescriptionAt;
      segmentDescriptionAt.SetValue( segmentDescription );
      segmentDS.Replace( segmentDescriptionAt.GetAsDataElement() );
    }

    // Segment Algorithm Type (Type 1)
    const char * segmentAlgorithmType = Segment::GetALGOTypeString( segment->GetSegmentAlgorithmType() );
    if ( segmentAlgorithmType == 0 )
    {
      gdcmWarningMacro("No segment algorithm type specified");
      Attribute<0x0062, 0x0008> segmentAlgorithmTypeAt;
      segmentAlgorithmTypeAt.SetValue( "" );
      segmentDS.Replace( segmentAlgorithmTypeAt.GetAsDataElement() );
    }
    else
    {
      Attribute<0x0062, 0x0008> segmentAlgorithmTypeAt;
      segmentAlgorithmTypeAt.SetValue( segmentAlgorithmType );
      segmentDS.Replace( segmentAlgorithmTypeAt.GetAsDataElement() );
    }


    // General Anatomy Optional Macro Attributes
    {
      // Anatomic Region Sequence (Type 3) - Only a single Item allowed
      const SegmentHelper::BasicCodedEntry & anatomicRegion = segment->GetAnatomicRegion();
      if(!anatomicRegion.IsEmpty())
      {
        writeCodeSequenceMacroAttributes(anatomicRegion, Tag(0x0008, 0x2218), segmentDS, false);

        // Anatomic Region Modifier Sequence (Type 3)
        const Segment::BasicCodedEntryVector & anatomicRegionModifiers = segment->GetAnatomicRegionModifiers();
        if(!anatomicRegionModifiers.empty())
        {
          SmartPointer<SequenceOfItems> sequence = segmentDS.GetDataElement(Tag(0x0008, 0x2218)).GetValueAsSQ();
          Item& item = sequence->GetItem(1);
          DataSet& itemDataSet = item.GetNestedDataSet();

          writeCodeSequenceMacroAttributes(anatomicRegionModifiers, Tag(0x0008, 0x2220), itemDataSet);
        }
      }

    }

    // Segmented Property Category Code Sequence (Type 1) - Only a single Item allowed
    const SegmentHelper::BasicCodedEntry & propertyCategory = segment->GetPropertyCategory();
    if(propertyCategory.IsEmpty())
    {
      gdcmWarningMacro("The property category is not specified or incomplete");
    }
    writeCodeSequenceMacroAttributes(propertyCategory, Tag(0x0062, 0x0003), segmentDS, false);


    // Segmented Property Type Code Sequence (Type 1) - Only a single Item allowed
    const SegmentHelper::BasicCodedEntry & propertyType = segment->GetPropertyType();
    if(propertyType.IsEmpty())
    {
      gdcmWarningMacro("The property type is not specified or incomplete");
    }
    writeCodeSequenceMacroAttributes(propertyType, Tag(0x0062, 0x000F), segmentDS, false);


    // Segmented Property Type Modifier Code Sequence (Type 3)
    const Segment::BasicCodedEntryVector & propertyTypeModifiers = segment->GetPropertyTypeModifiers();
    if(!propertyTypeModifiers.empty())
    {
      SmartPointer<SequenceOfItems> sequence = segmentDS.GetDataElement(Tag(0x0062, 0x000F)).GetValueAsSQ();
      Item& item = sequence->GetItem(1);
      DataSet& itemDataSet = item.GetNestedDataSet();

      writeCodeSequenceMacroAttributes(propertyTypeModifiers, Tag(0x0062, 0x0011), itemDataSet);
    }


    //*****   Surface segmentation    *****//
    const unsigned long surfaceCount = segment->GetSurfaceCount();
    if (surfaceCount > 0)
    {
      // Surface Count
      Attribute<0x0066, 0x002A> surfaceCountAt;
      surfaceCountAt.SetValue( (unsigned int)surfaceCount );
      segmentDS.Replace( surfaceCountAt.GetAsDataElement() );

      //*****   Referenced Surface Sequence   *****//
      SmartPointer<SequenceOfItems> segmentsRefSQ;
      if( !segmentDS.FindDataElement( Tag(0x0066, 0x002B) ) )
      {
        segmentsRefSQ = new SequenceOfItems;
        DataElement detmp( Tag(0x0066, 0x002B) );
        detmp.SetVR( VR::SQ );
        detmp.SetValue( *segmentsRefSQ );
        detmp.SetVLToUndefined();
        segmentDS.Insert( detmp );
      }
      segmentsRefSQ = segmentDS.GetDataElement( Tag(0x0066, 0x002B) ).GetValueAsSQ();
      segmentsRefSQ->SetLengthToUndefined();

      // Fill the Segment Surface Generation Algorithm Identification Sequence
      const size_t nbItems        = segmentsRefSQ->GetNumberOfItems();
      if (nbItems < surfaceCount)
      {
        const size_t diff           = surfaceCount - nbItems;
        const size_t nbOfItemToMake = (diff > 0?diff:0);
        for(unsigned int i = 1; i <= nbOfItemToMake; ++i)
        {
          Item item;
          item.SetVLToUndefined();
          segmentsRefSQ->AddItem(item);
        }
      }
      // else Should I remove items?

      std::vector< SmartPointer< Surface > >                  surfaces          = segment->GetSurfaces();
      std::vector< SmartPointer< Surface > >::const_iterator  it                = surfaces.begin();
      std::vector< SmartPointer< Surface > >::const_iterator  itEnd             = surfaces.end();
      unsigned int                                            itemSurfaceNumber = 1;
      for (; it != itEnd; it++)
      {
        SmartPointer< Surface > surface = *it;

        Item &    segmentsRefItem = segmentsRefSQ->GetItem( itemSurfaceNumber++ );
        DataSet & segmentsRefDS   = segmentsRefItem.GetNestedDataSet();

        // Referenced Surface Number
        Attribute<0x0066, 0x002C> refSurfaceNumberAt;
        unsigned long refSurfaceNumber = surface->GetSurfaceNumber();
        if (refSurfaceNumber == 0)
        {
          refSurfaceNumber = surfaceNumber++;
          surface->SetSurfaceNumber( refSurfaceNumber );
        }
        refSurfaceNumberAt.SetValue( (unsigned int)refSurfaceNumber );
        segmentsRefDS.Replace( refSurfaceNumberAt.GetAsDataElement() );

        //*****   Segment Surface Source Instance Sequence   *****//
        {
//          SmartPointer<SequenceOfItems> surfaceSourceSQ;
//          if( !segmentsRefDS.FindDataElement( Tag(0x0066, 0x002E) ) )
//          {
//            surfaceSourceSQ = new SequenceOfItems;
//            DataElement detmp( Tag(0x0066, 0x002E) );
//            detmp.SetVR( VR::SQ );
//            detmp.SetValue( *surfaceSourceSQ );
//            detmp.SetVLToUndefined();
//            segmentsRefDS.Insert( detmp );
//          }
//          surfaceSourceSQ = segmentsRefDS.GetDataElement( Tag(0x0066, 0x002E) ).GetValueAsSQ();
//          surfaceSourceSQ->SetLengthToUndefined();

          //NOTE: If surfaces are derived from image, include 'Image SOP Instance Reference Macro' PS 3.3 Table C.10-3.
          //      How to know it?
        }
      }
    }
    else
    {
      // Segment Algorithm Name (Type 1)
      const char * segmentAlgorithmName = segment->GetSegmentAlgorithmName();
      if ( strcmp(segmentAlgorithmName, "") != 0 )
      {
        gdcmWarningMacro("No segment algorithm name specified");
      }
      Attribute<0x0062, 0x0009> segmentAlgorithmNameAt;
      segmentAlgorithmNameAt.SetValue( segmentAlgorithmName );
      segmentDS.Replace( segmentAlgorithmNameAt.GetAsDataElement() );
    }

    ++itemNumber;
  }

  return true;
}

bool SegmentWriter::Write()
{
  if( !PrepareWrite() )
  {
    return false;
  }

  assert( Stream );
  if( !Writer::Write() )
  {
    return false;
  }

  return true;
}

}
