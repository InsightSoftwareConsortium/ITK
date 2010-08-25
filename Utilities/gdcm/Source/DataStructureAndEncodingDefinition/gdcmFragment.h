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
#ifndef GDCMFRAGMENT_H
#define GDCMFRAGMENT_H

#include "gdcmDataElement.h"
#include "gdcmByteValue.h"
#include "gdcmSmartPointer.h"
#include "gdcmParseException.h"

namespace gdcm
{

// Implementation detail:
// I think Fragment should be a protected sublclass of DataElement:
// looking somewhat like this:
/*
class GDCM_EXPORT Fragment : protected DataElement
{
public:
  using DataElement::GetTag;
  using DataElement::GetVL;
  using DataElement::SetByteValue;
  using DataElement::GetByteValue;
  using DataElement::GetValue;
*/
// Instead I am only hiding the SetTag member...

/**
 * \brief Class to represent a Fragment
 */
class GDCM_EXPORT Fragment : public DataElement
{
//protected:
//  void SetTag(const Tag &t);
public:
  Fragment() : DataElement(Tag(0xfffe, 0xe000), 0) {}
  friend std::ostream &operator<<(std::ostream &os, const Fragment &val);

  VL GetLength() const {
    assert( !ValueLengthField.IsUndefined() );
    assert( !ValueField || ValueField->GetLength() == ValueLengthField );
    return TagField.GetLength() + ValueLengthField.GetLength()
      + ValueLengthField;
  }

  template <typename TSwap>
  std::istream &Read(std::istream &is)
    {
    // Superclass
    const Tag itemStart(0xfffe, 0xe000);
    const Tag seqDelItem(0xfffe,0xe0dd);
    if( !TagField.Read<TSwap>(is) )
      {
      //  BogusItemStartItemEnd.dcm
      throw Exception( "Problem" );
      return is;
      }
    if( !ValueLengthField.Read<TSwap>(is) )
      {
      // GENESIS_SIGNA-JPEG-CorruptFrag.dcm
      // JPEG fragment is declared to have 61902, but infact really is only 61901
      // so we end up reading 0xddff,0x00e0, and VL = 0x0 (1 byte)
      throw Exception( "Problem" );
      return is;
      }
#ifdef GDCM_SUPPORT_BROKEN_IMPLEMENTATION
    if( TagField != itemStart && TagField != seqDelItem )
      {
      throw Exception( "Problem" );
      }
#endif
    // Self
    SmartPointer<ByteValue> bv = new ByteValue;
    bv->SetLength(ValueLengthField);
    if( !bv->Read<TSwap>(is) )
      {
      // Fragment is incomplete, but is a itemStart, let's try to push it anyway...
      gdcmWarningMacro( "Fragment could not be read" );
      //bv->SetLength(is.gcount());
      ValueField = bv;
      ParseException pe;
      pe.SetLastElement( *this );
      throw pe;
      return is;
      }
    ValueField = bv;
    return is;
    }


  template <typename TSwap>
  std::ostream &Write(std::ostream &os) const {
    const Tag itemStart(0xfffe, 0xe000);
    const Tag seqDelItem(0xfffe,0xe0dd);
    if( !TagField.Write<TSwap>(os) )
      {
      assert(0 && "Should not happen");
      return os;
      }
    assert( TagField == itemStart
         || TagField == seqDelItem );
    const ByteValue *bv = GetByteValue();
    // VL
    // The following piece of code is hard to read in order to support such broken file as:
    // CompressedLossy.dcm
    if( IsEmpty() )
      {
      //assert( bv );
      VL zero = 0;
      if( !zero.Write<TSwap>(os) )
        {
        assert(0 && "Should not happen");
        return os;
        }
      }
    else
      {
      assert( ValueLengthField );
      if( !ValueLengthField.Write<TSwap>(os) )
        {
        assert(0 && "Should not happen");
        return os;
        }
      }
    // Value
    if( ValueLengthField && bv )
      {
      // Self
      assert( bv );
      assert( bv->GetLength() == ValueLengthField );
      if( !bv->Write<TSwap>(os) )
        {
        assert(0 && "Should not happen");
        return os;
        }
      }
    return os;
    }
};
//-----------------------------------------------------------------------------
inline std::ostream &operator<<(std::ostream &os, const Fragment &val)
{
  os << "Tag: " << val.TagField;
  os << "\tVL: " << val.ValueLengthField;
  if( val.ValueField )
    {
    os << "\t" << *(val.ValueField);
    }

  return os;
}


} // end namespace gdcm

#endif //GDCMFRAGMENT_H
