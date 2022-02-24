/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmDicts.h"

namespace gdcm
{

Dicts::Dicts():PublicDict(),ShadowDict()
{
  //PublicType = DICOMV3_DICT;
  //PublicDicts.resize(3, Dict() );
}

Dicts::~Dicts()
= default;

//void Dicts::AddPublicDict(const Dict& dict)
//{
//  (void)dict;
//  //PublicDicts.push_back( dict );
//}

//void Dicts::SetPublicType(int type)
//{
//  PublicType = type;
//}

const DictEntry &Dicts::GetDictEntry(const Tag& tag, const char *owner) const
{
  if( tag.IsGroupLength() )
    {
    const DictEntry & de = PublicDict.GetDictEntry(tag);
    const char *name = de.GetName();
    if( name && *name )
      {
      return de;
      }
    else
      {
      bool retired = true; // Since DICOM 2008, all (but 0002,0004) group length are retired
      static const DictEntry Dummy("Generic Group Length", "GenericGroupLength", VR::UL, VM::VM1, retired);
      return Dummy;
      }
    }
  else if( tag.IsPublic() )
    {
    assert( owner == nullptr );
    return PublicDict.GetDictEntry(tag);
    }
  else
    {
    assert( tag.IsPrivate() );
    if( owner && *owner )
      {
      size_t len = strlen(owner); (void)len;
      PrivateTag ptag(tag.GetGroup(), (uint16_t)(((uint16_t)(tag.GetElement() << 8)) >> 8), owner);
      const DictEntry &de = GetPrivateDict().GetDictEntry(ptag);
      return de;
      }
    else
      {
      // Check special private element: 0x0000 and [0x1,0xFF] are special cases:
      if( tag.IsIllegal() )
        {
        static const DictEntry Dummy("Illegal Element", "IllegalElement", VR::INVALID, VM::VM0, false);
        return Dummy;
        }
      else if( tag.IsPrivateCreator() )
        {
        assert( !tag.IsIllegal() );
        assert( tag.GetElement() ); // Not a group length !
        assert( tag.IsPrivate() );
        assert( owner == nullptr );
        static const DictEntry Dummy("Private Creator", "PrivateCreator", VR::LO, VM::VM1, false);
        return Dummy;
        }
      else
        {
        static const DictEntry Dummy("Private Element With Empty Private Creator",
                                     "PrivateElementWithEmptyPrivateCreator", VR::INVALID, VM::VM0);
        return Dummy;
        }
      }
  }
}

const DictEntry &Dicts::GetDictEntry(const PrivateTag& tag) const
{
  return GetDictEntry(tag, tag.GetOwner() );
}


const char *Dicts::GetConstructorString(ConstructorType type)
{
  (void)type;
  return "";
}

const Dict &Dicts::GetPublicDict() const
{
  //assert( PublicType < PublicDicts.size() );
  return PublicDict; //[PublicType];
}

const PrivateDict &Dicts::GetPrivateDict() const
{
  return ShadowDict;
}
PrivateDict &Dicts::GetPrivateDict()
{
  return ShadowDict;
}

const CSAHeaderDict &Dicts::GetCSAHeaderDict() const
{
  return CSADict;
}

void Dicts::LoadDefaults()
{
  // TODO: should the user be able to control which dict to load ?
  PublicDict.LoadDefault();
  ShadowDict.LoadDefault();
  CSADict.LoadDefault();
}

} // end namespace gdcm
