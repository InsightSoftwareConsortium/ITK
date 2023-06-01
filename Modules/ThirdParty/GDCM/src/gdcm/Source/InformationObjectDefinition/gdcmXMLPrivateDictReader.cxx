/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmXMLPrivateDictReader.h"
#include "gdcmDict.h"

#include <iostream>
#include <stdlib.h> // abort

namespace gdcm
{

XMLPrivateDictReader::XMLPrivateDictReader():ParsingDescription(false),Description()
{
}

void XMLPrivateDictReader::HandleEntry(const char **atts)
{
  assert( !ParsingDescription );
  std::string name;
  std::string owner;
  VR vr;
  VM::VMType vm = VM::VM0;
  bool ret = false;

  PrivateTag &tag = CurrentTag;
  DictEntry &de = CurrentDE;

  int i = 0;
  const char **current = atts;
  // Supported attributes:
  std::string group = "group";
  std::string element = "element";
  std::string strvr = "vr";
  std::string strvm = "vm";
  std::string retired = "retired";
  std::string retiredtrue = "true";
  std::string retiredfalse = "false";
  std::string version = "version";
  std::string strowner = "owner";
  std::string strname = "name";

  while(*current /*&& current+1*/)
    {
    assert( *(current + 1) );
    if( group == *current )
      {
      unsigned int v;
      const char *raw = *(current+1);
      int r = sscanf(raw, "%04x", &v);
      assert( r == 1 );
      assert( v <= 0xFFFF );

      char sv[4+1];
      r = snprintf(sv, sizeof(sv), "%04x", v);
      assert( r == 4 );
      if( strncmp(raw, sv, 4) == 0 ) // GroupXX
        {
        tag.SetGroup( v );
        }
      else
        {
        assert( (raw[0] == '5' && raw[1] == '0') || (raw[0] == '6' && raw[1] == '0') ||
          (raw[0] == '7' && raw[1] == '0') );
        if( raw[0] == '5' ) tag.SetGroup( 0x5000 );
        else if( raw[0] == '6' ) tag.SetGroup( 0x6000 );
        else if( raw[0] == '7' ) tag.SetGroup( 0x7000 );
        else assert(0);
        CurrentDE.SetGroupXX( true );
        }
      }
    else if( element == *current )
      {
      const char *raw = *(current+1);
      assert( (raw[0] == 'x' && raw[1] == 'x' )
           || (raw[2] == 'x' && raw[3] == 'x') );
      if (raw[2] == 'x' && raw[3] == 'x')
        {
        if( raw[0] == '0' && raw[1] == '0' )
          {
          tag.SetElement( 0x0000 );
          CurrentDE.SetElementXX( true );
          }
        else if( raw[0] == '1' && raw[1] == '0' )
          {
          tag.SetElement( 0x1000 );
          CurrentDE.SetElementXX( true );
          }
        else
          {
          assert(0); // FIXME
          }
        }
      else
        {
        unsigned int v;
        int r = sscanf(raw+2, "%02x", &v);
        assert( r == 1 );
        assert( v <= 0xFF );

        char sv[4+1];
        r = snprintf(sv, sizeof(sv), "xx%02x", v);
        assert( r == 4 );
        if( strncmp(raw, sv, 4) == 0 )
          {
          tag.SetElement( v );
          }
        else
          {
          assert(0);
          }
        }
      }
    else if( strvr == *current )
      {
      vr = VR::GetVRTypeFromFile( *(current + 1) );
      //assert( vr != VR::INVALID );
      }
    else if( strvm == *current )
      {
      vm = VM::GetVMType( *(current + 1) );
      //assert( *(current+1) != '\0' );
      //assert( vm != VM::VM0 );
      //assert( vm != VM::VM_END );
      }
    else if( retired == *current )
      {
      if( retiredtrue == *(current+1) )
        {
        ret = true;
        }
      else if( retiredfalse == *(current+1) )
        {
        ret = false;
        }
      else
        {
        assert(0);
        }
      }
    else if( version == *current )
      {
      // ??
      }
    else if( strowner == *current )
      {
      owner = *(current+1);
      }
    else if( strname == *current )
      {
      name = *(current+1);
      }
    else
      {
      assert(0);
      }
    // goes on to the next attribute (need to skip value)
    ++current;
    ++current;
    }
  // Done !
  de = DictEntry(name.c_str(), vr, vm, ret );
  tag.SetOwner( owner.c_str() );
}

void XMLPrivateDictReader::HandleDescription(const char **atts)
{
  assert( ParsingDescription );
  assert( *atts == NULL );
  assert( Description == "" );
#if 0
  DictEntry &de = CurrentDE;

  const char **current = atts;
  std::string description;
  while(*current /*&& current+1*/)
    {
    assert( *(current + 1) );
    ++current;
    }
  // Done !
  //de.SetName( description.c_str() );
#endif
}

void XMLPrivateDictReader::StartElement(const char *name, const char **atts)
{
  std::string dict = "dict";
  std::string entry = "entry";
  std::string description = "description"; // aka name
  Tag currenttag;
  //DictEntry currentde("",VR::INVALID,VM::VM0,true);
  if( dict == name )
    {
    // dict ??
    }
  else if( entry == name )
    {
    HandleEntry(atts);
    }
  else if( description == name )
    {
    ParsingDescription = true; // mark that we are processing description element
    // We need a second pass to fill in the currentde struct:
    HandleDescription(atts);
    }
  else
    {
    std::cerr << name << std::endl;
    assert(0);
    }
}

void XMLPrivateDictReader::EndElement(const char *name)
{
  std::string entry = "entry";
  std::string dict = "dict";
  std::string description = "description"; // free form text usually the raw description of tag
  if( entry == name )
    {
    // Ok currentde is now valid let's insert it into the Dict:
    PDict.AddDictEntry( CurrentTag, CurrentDE);
    }
  else if( description == name )
    {
    assert( ParsingDescription );
    ParsingDescription = false;

    // TODO: do something with description ??
    //
    // Invalidate Description ?
    Description = "";
    }
  else if ( dict == name )
    {
    }
  else
    {
    assert(0);
    }
}

void XMLPrivateDictReader::CharacterDataHandler(const char *data, int length)
{
  if( ParsingDescription )
    {
    std::string name( data, length);
    assert( length == strlen( name.c_str() ) );
    Description.append( name );
    }
}

}
