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
#ifndef GDCMSEQUENCEOFFRAGMENTS_H
#define GDCMSEQUENCEOFFRAGMENTS_H

#include "gdcmValue.h"
#include "gdcmVL.h"
#include "gdcmFragment.h"
#include "gdcmBasicOffsetTable.h"

namespace gdcm
{

  // FIXME gdcmSequenceOfItems qnd gdcmSequenceOfFragments
  // should be rethink (duplicate code)
/**
 * \brief Class to represent a Sequence Of Fragments
 * \todo I do not enforce that Sequence of Fragments ends with a SQ end del
 */
class GDCM_EXPORT SequenceOfFragments : public Value
{
public:
  // Typdefs:
  typedef std::vector<Fragment> FragmentVector;

/// \brief constructor (UndefinedLength by default)
  SequenceOfFragments():Table(),SequenceLengthField(0xFFFFFFFF) { }

  /// \brief Returns the SQ length, as read from disk
  VL GetLength() const {
    return SequenceLengthField; }
  /// \brief Sets the actual SQ length
  void SetLength(VL length) {
    SequenceLengthField = length;
  }
  void Clear();

  /// \brief Appends a Fragment to the already added ones
  void AddFragment(Fragment const &item);

  // Compute the length of all fragments (and framents only!).
  // Basically the size of the PixelData as stored (in bytes).
  unsigned long ComputeByteLength() const;

  // Compute the length of fragments (in bytes)+ length of tag...
  // to be used for computation of Group Length
  VL ComputeLength() const;

  // Get the buffer
  bool GetBuffer(char *buffer, unsigned long length) const;
  bool GetFragBuffer(unsigned int fragNb, char *buffer, unsigned long &length) const;
  unsigned int GetNumberOfFragments() const;
  const Fragment& GetFragment(unsigned int num) const;

  // Write the buffer of each fragment (call WriteBuffer on all Fragments, which are
  // ByteValue). No Table information is written.
  bool WriteBuffer(std::ostream &os) const;

  const BasicOffsetTable &GetTable() const { return Table; }
  BasicOffsetTable &GetTable() { return Table; }


template <typename TSwap>
std::istream& Read(std::istream &is)
{
  assert( SequenceLengthField.IsUndefined() );
  //if( SequenceLengthField.IsUndefined() )
    {
    const Tag seqDelItem(0xfffe,0xe0dd);
    // First item is the basic offset table:
    try
      {
      Table.Read<TSwap>(is);
      gdcmDebugMacro( "Table: " << Table );
      }
    catch(...)
      {
      // Bug_Siemens_PrivateIconNoItem.dcm
      // First thing first let's rewind
      is.seekg(-4, std::ios::cur);
      if ( Table.GetTag() == Tag(0xd8ff,0xe0ff) )
        {
        Fragment frag;
        is.seekg( 8340, std::ios::cur );
        char dummy[8340];
        frag.SetByteValue( dummy, 8340 - Table.GetLength() - 16 );
        Fragments.push_back( frag );
        return is;
        }
      else
        {
        throw "Catch me if you can";
        //assert(0);
        }
      }
    // not used for now...
    Fragment frag;
    try
      {
      while( frag.Read<TSwap>(is) && frag.GetTag() != seqDelItem )
        {
        gdcmDebugMacro( "Frag: " << frag );
        Fragments.push_back( frag );
        }
      assert( frag.GetTag() == seqDelItem && frag.GetVL() == 0 );
      }
    catch(Exception &)
      {
#ifdef GDCM_SUPPORT_BROKEN_IMPLEMENTATION
      // that's ok ! In all cases the whole file was read, because Fragment::Read only fail on eof() reached
      // 1. SIEMENS-JPEG-CorruptFrag.dcm is more difficult to deal with, we have a partial fragment, read
      // we decide to add it anyway to the stack of fragments (eof was reached so we need to clear error bit)
      if( frag.GetTag() == Tag(0xfffe,0xe000)  )
        {
        gdcmWarningMacro( "Pixel Data Fragment could be corrupted. Use file at own risk" );
        Fragments.push_back( frag );
        is.clear(); // clear the error bit
        }
      // 2. GENESIS_SIGNA-JPEG-CorruptFrag.dcm
      else if ( frag.GetTag() == Tag(0xddff,0x00e0) )
        {
        assert( Fragments.size() == 1 );
        const ByteValue *bv = Fragments[0].GetByteValue();
        assert( (unsigned char)bv->GetPointer()[ bv->GetLength() - 1 ] == 0xfe );
        // Yes this is an extra copy, this is a bug anyway, go fix YOUR code
        Fragments[0].SetByteValue( bv->GetPointer(), bv->GetLength() - 1 );
        gdcmWarningMacro( "JPEG Fragment length was declared with an extra byte at the end: stripped !" );
        is.clear(); // clear the error bit
        }
      else
        {
        // 3. gdcm-JPEG-LossLess3a.dcm: easy case, an extra tag was found instead of terminator (eof is the next char)
        gdcmWarningMacro( "Reading failed at Tag:" << frag.GetTag() << ". Use file at own risk." << ex.what() );
        }
#else
      (void)ex;
#endif /* GDCM_SUPPORT_BROKEN_IMPLEMENTATION */
      }
    }

  return is;
}

template <typename TSwap>
std::ostream const &Write(std::ostream &os) const
{
  if( !Table.Write<TSwap>(os) )
    {
    assert(0 && "Should not happen");
    return os;
    }
  SequenceOfFragments::FragmentVector::const_iterator it = Fragments.begin();
  for(;it != Fragments.end(); ++it)
    {
    it->Write<TSwap>(os);
    }
  // seq del item is not stored, write it !
  const Tag seqDelItem(0xfffe,0xe0dd);
  seqDelItem.Write<TSwap>(os);
  VL zero = 0;
  zero.Write<TSwap>(os);

  return os;
}

//#if defined(SWIGPYTHON) || defined(SWIGCSHARP) || defined(SWIGJAVA)
  // For now leave it there, this does not make sense in the C++ layer
  // Create a new object
  static SmartPointer<SequenceOfFragments> New()
  {
     return new SequenceOfFragments();
  }
//#endif

protected:
public:
  void Print(std::ostream &os) const {
    os << "SQ L= " << SequenceLengthField << "\n";
    os << "Table:" << Table << "\n";
    FragmentVector::const_iterator it =
      Fragments.begin();
    for(;it != Fragments.end(); ++it)
      {
      os << "  " << *it << "\n";
      }
    assert( SequenceLengthField.IsUndefined() );
      {
      const Tag seqDelItem(0xfffe,0xe0dd);
      VL zero = 0;
      os << seqDelItem;
      os << "\t" << zero;
      }
  }
  bool operator==(const Value &val) const
    {
    const SequenceOfFragments &sqf = dynamic_cast<const SequenceOfFragments&>(val);
    return Table == sqf.Table &&
      SequenceLengthField == sqf.SequenceLengthField &&
      Fragments == sqf.Fragments;
    }

private:
public:
  BasicOffsetTable Table;
  VL SequenceLengthField;
  /// \brief Vector of Sequence Fragments
  FragmentVector Fragments;
};

/**
 * \example DecompressJPEGFile.cs
 * This is a C# example on how to use gdcm::SequenceOfFragments
 */

} // end namespace gdcm

#endif //GDCMSEQUENCEOFFRAGMENTS_H
