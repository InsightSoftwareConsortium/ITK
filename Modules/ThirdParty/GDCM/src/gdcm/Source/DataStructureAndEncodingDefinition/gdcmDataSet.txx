/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMDATASET_TXX
#define GDCMDATASET_TXX

#include "gdcmByteValue.h"
#include "gdcmParseException.h"

namespace gdcm
{
  template <typename TDE, typename TSwap>
  std::istream &DataSet::ReadNested(std::istream &is) {
    DataElement de;
    const Tag itemDelItem(0xfffe,0xe00d);
    assert( de.GetTag() != itemDelItem ); // precondition before while loop
    try
      {
      while( de.Read<TDE,TSwap>(is) && de.GetTag() != itemDelItem  ) // Keep that order please !
        {
        //std::cerr << "DEBUG Nested: " << de << std::endl;
        InsertDataElement( de );
        }
      }
    catch(ParseException &pe)
      {
      if( pe.GetLastElement().GetTag() == Tag(0xfffe,0xe0dd) )
        {
        //  BogusItemStartItemEnd.dcm
        gdcmWarningMacro( "SQ End found but no Item end found" );
        de.SetTag( itemDelItem );
        is.seekg( -4, std::ios::cur );
        }
      else
        {
        // MR_Philips_Intera_PrivateSequenceExplicitVR_in_SQ_2001_e05f_item_wrong_lgt_use_NOSHADOWSEQ.dcm
        // Need to rethrow the exception...sigh
        throw pe;
        }
      }
    assert( de.GetTag() == itemDelItem );
    return is;
  }

  template <typename TDE, typename TSwap>
  std::istream &DataSet::Read(std::istream &is) {
    DataElement de;
    while( !is.eof() && de.Read<TDE,TSwap>(is) )
      {
      //std::cerr << "DEBUG:" << de << std::endl;
      InsertDataElement( de );
      }
    return is;
  }

  template <typename TDE, typename TSwap>
  std::istream &DataSet::ReadUpToTag(std::istream &is, const Tag &t, const std::set<Tag> & skiptags) {
    DataElement de;
    while( !is.eof() && de.template ReadOrSkip<TDE,TSwap>(is, skiptags) )
      {
      // If tag read was in skiptags then we should NOT add it:
      if( skiptags.count( de.GetTag() ) == 0 )
        InsertDataElement( de );
      // tag was found, we can exit the loop:
      if ( t <= de.GetTag() ) break;
      }
    return is;
  }

  template <typename TDE, typename TSwap>
  std::istream &DataSet::ReadUpToTagWithLength(std::istream &is, const Tag &t, VL & length) {
    DataElement de;
    while( !is.eof() && de.ReadWithLength<TDE,TSwap>(is,length) )
      {
      //assert( de.GetTag() != Tag(0,0) );
      InsertDataElement( de );
      // tag was found, we can exit the loop:
      if ( t <= de.GetTag() ) break;
      }
    return is;
  }

  template <typename TDE, typename TSwap>
  std::istream &DataSet::ReadSelectedTags(std::istream &inputStream, const std::set<Tag> & selectedTags) {
    if ( ! selectedTags.empty() )
    {
      const Tag maxTag = *(selectedTags.rbegin());
      std::set<Tag> tags = selectedTags;
      DataElement dataElem;

      // TODO There's an optimization opportunity here:
      // dataElem.Read only needs to read the value if the tag is selected!
      // Niels Dekker, LKEB, Jan 2010.
      while( !inputStream.eof() && dataElem.template Read<TDE,TSwap>(inputStream) )
      {
        const Tag tag = dataElem.GetTag();
        const std::set<Tag>::iterator found = tags.find(tag);

        if ( found != tags.end() )
        {
          InsertDataElement( dataElem );
          tags.erase(found);

          if ( tags.empty() )
          {
            // All selected tags were found, we can exit the loop:
            break;
          }
        }
        if ( ! (tag < maxTag ) )
        {
          // The maximum tag was encountered, and as we assume
          // ascending tag ordering, we can exit the loop:
          break;
        }
      }
    }
    return inputStream;
  }


  template <typename TDE, typename TSwap>
  std::istream &DataSet::ReadSelectedTagsWithLength(std::istream &inputStream, const std::set<Tag> & selectedTags, VL & length) {
    if ( ! selectedTags.empty() )
    {
      const Tag maxTag = *(selectedTags.rbegin());
      std::set<Tag> tags = selectedTags;
      DataElement dataElem;

      // TODO There's an optimization opportunity here:
      // dataElem.ReadWithLength only needs to read the value if the tag is selected!
      // Niels Dekker, LKEB, Jan 2010.
      while( !inputStream.eof() && dataElem.template ReadWithLength<TDE,TSwap>(inputStream, length) )
      {
        const Tag tag = dataElem.GetTag();
        const std::set<Tag>::iterator found = tags.find(tag);

        if ( found != tags.end() )
        {
          InsertDataElement( dataElem );
          tags.erase(found);

          if ( tags.empty() )
          {
            // All selected tags were found, we can exit the loop:
            break;
          }
        }
        if ( ! (tag < maxTag ) )
        {
          // The maximum tag was encountered, and as we assume
          // ascending tag ordering, we can exit the loop:
          break;
        }
      }
    }
    return inputStream;
  }

  template <typename TDE, typename TSwap>
  std::istream &DataSet::ReadWithLength(std::istream &is, VL &length) {
    DataElement de;
    VL l = 0;
    //std::cout << "ReadWithLength Length: " << length << std::endl;
    VL locallength = length;
    try
      {
      while( l != locallength && de.ReadWithLength<TDE,TSwap>(is, locallength))
        {
        //std::cout << "Nested: " << de << std::endl;
#ifndef GDCM_SUPPORT_BROKEN_IMPLEMENTATION
        assert( de.GetTag() != Tag(0xfffe,0xe000) ); // We should not be reading the next item...
#endif
        InsertDataElement( de );
        l += de.GetLength<TDE>();
        //std::cout << "l:" << l << std::endl;
        //assert( !de.GetVL().IsUndefined() );
        //std::cerr << "DEBUG: " << de.GetTag() << " "<< de.GetLength() <<
        //  "," << de.GetVL() << "," << l << std::endl;
        // Bug_Philips_ItemTag_3F3F
        //  (0x2005, 0x1080): for some reason computation of length fails...
        if( l == 70 && locallength == 63 )
          {
          gdcmWarningMacro( "PMS: Super bad hack. Changing length" );
          length = locallength = 140;
          }
        if( l > locallength )
          {
          gdcmDebugMacro( "Out of Range SQ detected: " << l << " while max: " << locallength );
          throw Exception( "Out of Range" );
          }
        }
    }
    catch(ParseException &pe)
      {
      if( pe.GetLastElement().GetTag() == Tag(0xfffe,0xe000) )
        {
        // gdcm-MR-PHILIPS-16-Multi-Seq.dcm
        // Long story short, I think Philips engineer inserted 0xfffe,0x0000 instead of an item start element
        // assert( FindDataElement( Tag(0xfffe,0x0000) ) == false );
        is.seekg(-6, std::ios::cur );
        length = locallength = l;
        }
      else
        {
        // Could be the famous :
        // gdcmDataExtra/gdcmBreakers/BuggedDicomWorksImage_Hopeless.dcm
        // let's just give up:
        throw Exception( "Unhandled" );
        }
      }
    catch(Exception &pe)
      {
      if( strcmp( pe.GetDescription(), "Out of Range" ) == 0 )
        {
        // BogugsItemAndSequenceLength.dcm
        // This is most likely the "Out of Range" one
        // Cautiously read until we find the next item starter and then stop.
        //std::cout << "Length read was:" << l << " should be at most:" <<  locallength ;
        while( de.Read<TDE,TSwap>(is) && de.GetTag() != Tag(0xfffe,0xe000) && de.GetTag().GetElement() != 0x0 )
          {
          //std::cout << "Nested2: " << de << std::endl;
          InsertDataElement( de );
          l += de.GetLength<TDE>();
          //std::cout << l << std::endl;
          }
        // seek back since we read the next item starter:
        int iteml = de.GetLength<TDE>();
        //assert( de.GetTag().GetElement() );
        if( !de.GetTag().GetElement() )
          {
          assert( iteml == 12 ); (void)iteml;
          is.seekg( -12, std::ios::cur );
          }
        else
          {
          //assert( de.GetTag() == Tag(0xfffe,0xe000) );
          is.seekg( -4, std::ios::cur );
          }
        // let's fix the length now:
        length = locallength = l;
        gdcmWarningMacro( "Item length is wrong" );
        throw Exception( "Changed Length" );
        }
      else
        {
        // re throw
        throw pe;
        }
      }

    // technically we could only do this assert if the dataset did not contains duplicate data elements
    // so only do a <= instead:
    //assert( l == locallength );
    assert( l <= locallength );
    return is;
  }

  template <typename TDE, typename TSwap>
  std::ostream const &DataSet::Write(std::ostream &os) const {
    typename DataSet::ConstIterator it = DES.begin();
    for( ; it != DES.end(); ++it)
      {
      const DataElement & de = *it;
      //if( de.GetTag().GetGroup() >= 0x0008 || de.GetTag().GetGroup() == 0x0002 )
        {
        de.Write<TDE,TSwap>(os);
        }
      }
    return os;
  }
} // end namespace gdcm

#endif // GDCMDATASET_TXX
