/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmSegmentReader.h"
#include "gdcmMediaStorage.h"
#include "gdcmAttribute.h"
#include "gdcmString.h"

namespace gdcm
{

SegmentReader::SegmentReader()
{
}

SegmentReader::~SegmentReader()
{
}

const SegmentReader::SegmentVector SegmentReader::GetSegments() const
{
  return const_cast<SegmentReader*>(this)->GetSegments();
}

SegmentReader::SegmentVector SegmentReader::GetSegments()
{
  SegmentVector res;

  // Make a segment vector from map with no duplicate.
  SegmentMap::const_iterator itMap    = Segments.begin();
  SegmentMap::const_iterator itMapEnd = Segments.end();
  if (itMap != itMapEnd)
  {
    // Add first segment
    res.push_back(itMap->second);
    itMap++;

    // Search and add only different segments
    SegmentVector::const_iterator itVec;
    SegmentVector::const_iterator itVecEnd;
    for (; itMap != itMapEnd; itMap++)
    {
      itVec     = res.begin();
      itVecEnd  = res.end();  // if res is a list, remove this line
      while (itVec != itVecEnd && itMap->second != *itVec)
        itVec++;
      if (itVec == itVecEnd)
        res.push_back(itMap->second);
    }
  }

  return res;
}

//unsigned int SegmentReader::GetNumberOfSegments()
//{
//  return GetSegments().size();
//}

bool SegmentReader::Read()
{
  bool res = false;

  // Read a file
  if( !Reader::Read() )
  {
    return res;
  }

  // Read Segments from file
  const FileMetaInformation & header  = F->GetHeader();
  MediaStorage                ms      = header.GetMediaStorage();

  if( ms == MediaStorage::SegmentationStorage
   || ms == MediaStorage::SurfaceSegmentationStorage )
  {
    res = ReadSegments();
  }
  else
  {
    const char *    modality  = ms.GetModality();
    const DataSet & dsRoot    = F->GetDataSet();
    if (modality != 0)
    { // Check modality
      String<> modalityStr( modality );
      if ( modalityStr.Trim() == "SEG" )
      {
        res = ReadSegments();
      }
      else if (dsRoot.FindDataElement( Tag(0x0062, 0x0002) ))
      { // Try to find Segment Sequence
        res = ReadSegments();
      }
    }
    else if (dsRoot.FindDataElement( Tag(0x0062, 0x0002) ))
    { // Try to find Segment Sequence
      res = ReadSegments();
    }
  }

  return res;
}

bool SegmentReader::ReadSegments()
{
  bool                        res     = false;

  const DataSet &             ds      = F->GetDataSet();

  // Segment Sequence
  const Tag segmentSQTag(0x0062, 0x0002);
  if (ds.FindDataElement(segmentSQTag))
  {
    SmartPointer< SequenceOfItems > segmentSQ = ds.GetDataElement(segmentSQTag).GetValueAsSQ();

    const size_t numberOfSegments = segmentSQ->GetNumberOfItems();
    if ( numberOfSegments == 0)
    {
      gdcmErrorMacro( "No segment found" );
      return false;
    }

    for (unsigned int i = 1; i <= numberOfSegments; ++i)
    {
      if ( !ReadSegment( segmentSQ->GetItem(i), i ) )
      {
        gdcmWarningMacro( "Segment "<<i<<" reading error" );
        // return false?
      }
    }

    res = true;
  }

  return res;
}


Segment::BasicCodedEntryVector readCodeSequenceMacroAttributes(const Tag & tag, const DataSet & dataset)
{
  Segment::BasicCodedEntryVector result;

  if(dataset.FindDataElement(tag))
  {
    SmartPointer<SequenceOfItems> sequence = dataset.GetDataElement(tag).GetValueAsSQ();

    SequenceOfItems::Iterator it = sequence->Begin();
    for(; it != sequence->End(); ++it)
    {
      const Item & item = *it;
      const DataSet & itemDataSet = item.GetNestedDataSet();

      SegmentHelper::BasicCodedEntry entry;

      // Code Value (Type 1C)
      Attribute<0x0008, 0x0100> codeValueAttribute;
      codeValueAttribute.SetFromDataSet(itemDataSet);
      entry.CV = codeValueAttribute.GetValue();

      // Coding Scheme Designator (Type 1C)
      Attribute<0x0008, 0x0102> codingSchemeDesignatorAttribute;
      codingSchemeDesignatorAttribute.SetFromDataSet(itemDataSet);
      entry.CSD = codingSchemeDesignatorAttribute.GetValue();

      // Coding Scheme Version (Type 1C)
      Attribute<0x0008, 0x0103> codingSchemeVersionAttribute;
      codingSchemeVersionAttribute.SetFromDataSet(itemDataSet);
      entry.CSV = codingSchemeVersionAttribute.GetValue();

      // Code Meaning (Type 1)
      Attribute<0x0008, 0x0104> codeMeaningAttribute;
      codeMeaningAttribute.SetFromDataSet(itemDataSet);
      entry.CM = codeMeaningAttribute.GetValue();

      result.push_back(entry);
    }
  }

  return result;
}


bool SegmentReader::ReadSegment(const Item & segmentItem, const unsigned int idx)
{
  SmartPointer< Segment > segment   = new Segment;

  const DataSet &         rootDs    = GetFile().GetDataSet();
  const DataSet &         segmentDS = segmentItem.GetNestedDataSet();

  // Segment Number
  const Tag segmentNumberTag(0x0062, 0x0004);
  if (segmentDS.FindDataElement( segmentNumberTag )
  && !segmentDS.GetDataElement( segmentNumberTag ).IsEmpty() )
  {
    Attribute<0x0062, 0x0004> segmentNumberAt;
    segmentNumberAt.SetFromDataSet( segmentDS );
    segment->SetSegmentNumber( segmentNumberAt.GetValue() );
  }
  else
  {
    segment->SetSegmentNumber( (unsigned short)idx );
  }

  // Segment Label
  Attribute<0x0062, 0x0005> segmentLabelAt;
  segmentLabelAt.SetFromDataSet( segmentDS );
  segment->SetSegmentLabel( segmentLabelAt.GetValue() );

  // Segment Description
  Attribute<0x0062, 0x0006> segmentDescriptionAt;
  segmentDescriptionAt.SetFromDataSet( segmentDS );
  segment->SetSegmentDescription( segmentDescriptionAt.GetValue() );

  // Segment Algorithm Type
  Attribute<0x0062, 0x0008> segmentAlgoType;
  segmentAlgoType.SetFromDataSet( segmentDS );
  segment->SetSegmentAlgorithmType( segmentAlgoType.GetValue() );

  // Surface Count
  Attribute<0x0066, 0x002A> surfaceCountAt;
  surfaceCountAt.SetFromDataSet( segmentDS );
  const unsigned long surfaceCount = surfaceCountAt.GetValue();
  segment->SetSurfaceCount( surfaceCount );

  // Check if there is a Surface Segmentation Module
  if (surfaceCount > 0 || rootDs.FindDataElement(Tag(0x0066, 0x0002)))
  {

    //Basic Coded Entries in each sequences
    Segment::BasicCodedEntryVector basicCodedEntries;

    // Anatomic Region Sequence (Type 3)
    basicCodedEntries = readCodeSequenceMacroAttributes(Tag(0x0008, 0x2218), segmentDS);
    if(!basicCodedEntries.empty())
    {
      segment->SetAnatomicRegion(basicCodedEntries[0]);
      // Only a single Item is permitted in this Sequence
      if(basicCodedEntries.size() > 1)
      {
        gdcmWarningMacro("Only a single Item is permitted in Anatomic Region Sequence, other items will be ignored");
      }

      SmartPointer<SequenceOfItems> sequence = segmentDS.GetDataElement(Tag(0x0008, 0x2218)).GetValueAsSQ();
      Item& item = sequence->GetItem(1);
      DataSet& itemDataSet = item.GetNestedDataSet();

      // Anatomic Region Modifier Sequence (Type 3)
      basicCodedEntries = readCodeSequenceMacroAttributes(Tag(0x0008, 0x2220), itemDataSet);
      if(!basicCodedEntries.empty())
      {
          segment->SetAnatomicRegionModifiers(basicCodedEntries);
      }
    }


    // Segmented Property Category Code Sequence (Type 1)
    basicCodedEntries = readCodeSequenceMacroAttributes(Tag(0x0062, 0x0003), segmentDS);
    if(!basicCodedEntries.empty())
    {
      segment->SetPropertyCategory(basicCodedEntries[0]);
      // Only a single Item shall be included in this Sequence
      if(basicCodedEntries.size() > 1)
      {
        gdcmWarningMacro("Only a single Item shall be included in Segmented Property Category Code Sequence, other items will be ignored");
      }
    }
    else
    {
        gdcmWarningMacro("No Item have been found in Segmented Property Category Code Sequence.");
    }

    // Segmented Property Type Code Sequence (Type 1)
    basicCodedEntries = readCodeSequenceMacroAttributes(Tag(0x0062, 0x000F), segmentDS);
    if(!basicCodedEntries.empty())
    {
      segment->SetPropertyType(basicCodedEntries[0]);
      // Only a single Item shall be included in this Sequence
      if(basicCodedEntries.size() > 1)
      {
        gdcmWarningMacro("Only a single Item shall be included in Segmented Property Type Code Sequence, other items will be ignored");
      }

      SmartPointer<SequenceOfItems> sequence = segmentDS.GetDataElement(Tag(0x0062, 0x000F)).GetValueAsSQ();
      Item& item = sequence->GetItem(1);
      DataSet& itemDataSet = item.GetNestedDataSet();

      // Segmented Property Type Modifier Sequence (Type 3)
      basicCodedEntries = readCodeSequenceMacroAttributes(Tag(0x0062, 0x0011), itemDataSet);
      if(!basicCodedEntries.empty())
      {
          segment->SetPropertyTypeModifiers(basicCodedEntries);
      }
    }
    else
    {
        gdcmWarningMacro("No Item have been found in Segmented Property Type Code Sequence.");
    }



    // Referenced Surface Sequence
    const Tag refSurfaceSQTag(0x0066, 0x002B);
    if (segmentDS.FindDataElement(refSurfaceSQTag))
    {
      SmartPointer< SequenceOfItems > refSurfaceSQ = segmentDS.GetDataElement(refSurfaceSQTag).GetValueAsSQ();

      // Index each surface of a segment
      SequenceOfItems::ConstIterator itRefSurface     = refSurfaceSQ->Begin();
      SequenceOfItems::ConstIterator itEndRefSurface  = refSurfaceSQ->End();
      unsigned long                  numberOfSurfaces = 0;
      for (; itRefSurface != itEndRefSurface; itRefSurface++)
      {
        const DataSet & refSurfaceDS = itRefSurface->GetNestedDataSet();

        // Referenced Surface Number
        Attribute<0x0066, 0x002C> refSurfaceNumberAt;
        refSurfaceNumberAt.SetFromDataSet( refSurfaceDS );
        unsigned long             refSurfaceNumber;
        if ( !refSurfaceNumberAt.GetAsDataElement().IsEmpty() )
        {
          refSurfaceNumber = refSurfaceNumberAt.GetValue();
        }
        else
        {
          refSurfaceNumber = idx;
        }
        // Index the segment with its referenced surface number
        Segments[refSurfaceNumber] = segment;

        // Compute number of items
        // (can not use GetNumberOfItems because of it returns a unsigned int)
        ++numberOfSurfaces;
      }

      // Set surface count corresponding to number of items
      if ( numberOfSurfaces != surfaceCount)
      {
        segment->SetSurfaceCount( numberOfSurfaces ); // Is it the right thing to do?
      }
    }
    else
    {// Index the segment with item number
      Segments[idx] = segment;
    }
  }
  else
  { // If is not a surface segmentation storage

    // Segment Algorithm Name
    Attribute<0x0062, 0x0009> segmentAlgoName;
    segmentAlgoName.SetFromDataSet( segmentDS );
    segment->SetSegmentAlgorithmName( segmentAlgoName.GetValue() );

    // Index the segment with item number
    Segments[idx] = segment;
  }

  return true;
}

}
