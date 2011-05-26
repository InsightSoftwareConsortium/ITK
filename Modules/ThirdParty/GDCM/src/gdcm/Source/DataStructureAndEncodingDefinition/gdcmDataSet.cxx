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
#include "gdcmDataSet.h"
#include "gdcmPrivateTag.h"

namespace gdcm
{
DataElement DataSet::DEEnd = DataElement( Tag(0xffff,0xffff) );

const DataElement& DataSet::GetDEEnd() const
{
  return DEEnd;
}

std::string DataSet::GetPrivateCreator(const Tag &t) const
{
  Tag pc = t.GetPrivateCreator();
  if( pc.GetElement() )
    {
    const DataElement r(pc);
    ConstIterator it = DES.find(r);
    if( it == DES.end() )
      {
      // FIXME, could this happen ?
      return "";
      }
    const DataElement &de = *it;
    if( de.IsEmpty() ) return "";
    const ByteValue *bv = de.GetByteValue();
    assert( bv );
    std::string owner = std::string(bv->GetPointer(),bv->GetLength());
    // There should not be any trailing space character...
    // TODO: tmp.erase(tmp.find_last_not_of(' ') + 1);
    while( owner.size() && owner[owner.size()-1] == ' ' )
      {
      // osirix/AbdominalCT/36382443
      owner.erase(owner.size()-1,1);
      }
    assert( owner.size() == 0 || owner[owner.size()-1] != ' ' );
    return owner;
    }
  return "";
}

Tag DataSet::ComputeDataElement(const PrivateTag & t) const
{
  gdcmDebugMacro( "Entering ComputeDataElement" );
  //assert( t.IsPrivateCreator() ); // No this is wrong to do the assert: eg. (0x07a1,0x000a,"ELSCINT1")
  // is valid because we have not yet done the mapping, so 0xa < 0x10 fails but might not later on
  const Tag start(t.GetGroup(), 0x0010 ); // First possible private creator (0x0 -> 0x9 are reserved...)
  const DataElement r(start);
  ConstIterator it = DES.lower_bound(r);
  const char *refowner = t.GetOwner();
  assert( refowner );
  bool found = false;
  while( it != DES.end() && it->GetTag().GetGroup() == t.GetGroup() && it->GetTag().GetElement() < 0x100 )
    {
    //assert( it->GetTag().GetOwner() );
    const ByteValue * bv = it->GetByteValue();
    assert( bv );
    //std::cout << std::string(bv->GetPointer(), bv->GetLength() ) << std::endl;
    //if( strcmp( bv->GetPointer(), refowner ) == 0 )
    std::string tmp(bv->GetPointer(),bv->GetLength());
    // trim trailing whitespaces:
    tmp.erase(tmp.find_last_not_of(' ') + 1);
    assert( tmp.size() == 0 || tmp[ tmp.size() - 1 ] != ' ' ); // FIXME
    if( System::StrCaseCmp( tmp.c_str(), refowner ) == 0 )
      {
      // found !
      found = true;
      break;
      }
    ++it;
    }
  gdcmDebugMacro( "In compute found is:" << found );
  if (!found) return GetDEEnd().GetTag();
  // else
  // ok we found the Private Creator Data Element, let's construct the proper data element
  Tag copy = t;
  copy.SetPrivateCreator( it->GetTag() );
  gdcmDebugMacro( "Compute found:" << copy );
  return copy;
}

bool DataSet::FindDataElement(const PrivateTag &t) const
{
  return FindDataElement( ComputeDataElement(t) );
}

const DataElement& DataSet::GetDataElement(const PrivateTag &t) const
{
  return GetDataElement( ComputeDataElement(t) );
}

} // end namespace gdcm
