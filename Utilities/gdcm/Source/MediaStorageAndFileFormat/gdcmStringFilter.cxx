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
#include "gdcmStringFilter.h"
#include "gdcmGlobal.h"
#include "gdcmElement.h"
#include "gdcmByteValue.h"
#include "gdcmAttribute.h"
#include "gdcmDataSetHelper.h"

namespace gdcm
{

//-----------------------------------------------------------------------------
StringFilter::StringFilter():F(new File)
{
}
//-----------------------------------------------------------------------------
StringFilter::~StringFilter()
{
}

void StringFilter::SetDicts(const Dicts &dicts)
{
  (void)dicts;
  assert(0); // FIXME
}

std::string StringFilter::ToString(const Tag& t) const
{
  return ToStringPair(t).second;
}

/*
std::string StringFilter::ToMIME64(const Tag& t) const
{
  return ToStringPair(t).second;
          // base64 streams have to be a multiple of 4 bytes long
          int encodedLengthEstimate = 2 * bv->GetLength();
          encodedLengthEstimate = ((encodedLengthEstimate / 4) + 1) * 4;

          char *bin = new char[encodedLengthEstimate];
          unsigned int encodedLengthActual = static_cast<unsigned int>(
            itksysBase64_Encode(
              (const unsigned char *) bv->GetPointer(),
              static_cast< unsigned long>( bv->GetLength() ),
              (unsigned char *) bin,
              static_cast< int >( 0 ) ));
          std::string encodedValue(bin, encodedLengthActual);

}
*/

#define StringFilterCase(type) \
  case VR::type: \
    { \
      Element<VR::type,VM::VM1_n> el; \
      if( !de.IsEmpty() ) { \
      el.Set( de.GetValue() ); \
      if( el.GetLength() ) { \
      os << el.GetValue(); \
      for(unsigned long i = 1; i < el.GetLength(); ++i) os << "\\" << el.GetValue(i); \
      ret.second = os.str(); } } \
    } break

std::pair<std::string, std::string> StringFilter::ToStringPair(const Tag& t) const
{
  if( t.GetGroup() == 0x2 )
    {
    const FileMetaInformation &header = GetFile().GetHeader();
    return ToStringPair(t, header);
    }
  else
    {
    const DataSet &ds = GetFile().GetDataSet();
    return ToStringPair(t, ds);
    }
}

std::pair<std::string, std::string> StringFilter::ToStringPair(const Tag& t, DataSet const &ds) const
{
  std::pair<std::string, std::string> ret;
  const Global &g = GlobalInstance;
  const Dicts &dicts = g.GetDicts();
  if( ds.IsEmpty() || !ds.FindDataElement(t) )
    {
    gdcmDebugMacro( "DataSet is empty or does not contains tag:" );
    return ret;
    }
  const DataElement &de = ds.GetDataElement( t );
  //assert( de.GetTag().IsPublic() );
  std::string strowner;
  const char *owner = 0;
  if( t.IsPrivate() && !t.IsPrivateCreator() )
    {
    strowner = ds.GetPrivateCreator(t);
    owner = strowner.c_str();
    }

  const DictEntry &entry = dicts.GetDictEntry(de.GetTag(), owner);

  const VR &vr_read = de.GetVR();
  const VR &vr_dict = entry.GetVR();

  if( vr_dict == VR::INVALID )
    {
    // FIXME This is a public element we do not support...
    return ret;
    }

  VR vr;
  // always prefer the vr from the file:
  if( vr_read == VR::INVALID )
    {
    vr = vr_dict;
    }
  else if ( vr_read == VR::UN && vr_dict != VR::INVALID ) // File is explicit, but still prefer vr from dict when UN
    {
    vr = vr_dict;
    }
  else // cool the file is Explicit !
    {
    vr = vr_read;
    }
  if( vr.IsDual() ) // This mean vr was read from a dict entry:
    {
    vr = DataSetHelper::ComputeVR(*F,ds, t);
    }

  if( vr == VR::UN )
    {
    // this element is not known...
    return ret;
    }

  assert( vr != VR::UN && vr != VR::INVALID );
  //std::cerr << "Found " << vr << " for " << de.GetTag() << std::endl;
  ret.first = entry.GetName();
  if( VR::IsASCII( vr ) )
    {
    assert( vr & VR::VRASCII );
    const ByteValue *bv = de.GetByteValue();
    if( de.GetVL() )
      {
      assert( bv /*|| bv->IsEmpty()*/ );
      ret.second = std::string( bv->GetPointer(), bv->GetLength() );
      // Let's remove any trailing \0 :
      ret.second.resize( std::min( ret.second.size(), strlen( ret.second.c_str() ) ) ); // strlen is garantee to be lower or equal to ::size()
      }
    else
      {
      //assert( bv == NULL );
      ret.second = ""; // ??
      }
    }
  else
    {
    assert( vr & VR::VRBINARY );
    const ByteValue *bv = de.GetByteValue();
    std::ostringstream os;
    if( bv )
      {
      //VM::VMType vm = entry.GetVM();//!!mmr-- can I remove this, or will it mess with the stream?
      //assert( vm == VM::VM1 );
      if( vr.IsDual() ) // This mean vr was read from a dict entry:
        {
        vr = DataSetHelper::ComputeVR(GetFile(),ds, t);
        }
      std::ostringstream os;
      switch(vr)
        {
        StringFilterCase(AT);
        StringFilterCase(FL);
        StringFilterCase(FD);
        //StringFilterCase(OB);
        StringFilterCase(OF);
        //StringFilterCase(OW);
        StringFilterCase(SL);
        //StringFilterCase(SQ);
        StringFilterCase(SS);
        StringFilterCase(UL);
        //StringFilterCase(UN);
        StringFilterCase(US);
        StringFilterCase(UT);
      case VR::UN:
      case VR::US_SS:
        assert(0);
        break;
      case VR::OB:
      case VR::OW:
      case VR::OB_OW:
      case VR::SQ:
        gdcmWarningMacro( "Unhandled: " << vr << " for tag " << de.GetTag() );
        ret.second = "";
        break;
      default:
        assert(0);
        break;
        }
      }
    }
  return ret;
}

std::string StringFilter::FromString(const Tag&t, const char * value, VL const & vl)
{
  (void)t;
  (void)value;
  (void)vl;
  assert(0 && "TODO");
  return "";
}

#define FromStringFilterCase(type) \
  case VR::type: \
      { \
      Element<VR::type,VM::VM1_n> el; \
      /* el.ReadComputeLength( is ); */ \
      el.SetLength( vl );  \
       for(unsigned int i = 0; i < vm.GetLength(); ++i)  \
        is >> el.GetValue(i);  \
      el.Write(os); \
      } \
    break

size_t count_backslash(const char *s, size_t len)
{
  size_t c = 0;
  for(size_t i = 0; i < len; ++i, ++s)
    {
    if( *s == '\\' )
      {
      ++c;
      }
    }
  return c;
}

std::string StringFilter::FromString(const Tag&t, const char * value, size_t len)
{
  if( !value || !len ) return "";
  const Global &g = GlobalInstance;
  const Dicts &dicts = g.GetDicts();
  std::string strowner;
  const char *owner = 0;
  const DataSet &ds = GetFile().GetDataSet();
  if( t.IsPrivate() && !t.IsPrivateCreator() )
    {
    strowner = ds.GetPrivateCreator(t);
    owner = strowner.c_str();
    }

  const DictEntry &entry = dicts.GetDictEntry(t, owner);
  const VM &vm = entry.GetVM();
  //const VR &vr = entry.GetVR();
  const DataElement &de = ds.GetDataElement( t );
  const VR &vr_read = de.GetVR();
  const VR &vr_dict = entry.GetVR();

  VR vr;
  // always prefer the vr from the file:
  if( vr_read == VR::INVALID )
    {
    vr = vr_dict;
    }
  else if ( vr_read == VR::UN && vr_dict != VR::INVALID ) // File is explicit, but still prefer vr from dict when UN
    {
    vr = vr_dict;
    }
  else // cool the file is Explicit !
    {
    vr = vr_read;
    }
  if( vr.IsDual() ) // This mean vr was read from a dict entry:
    {
    vr = DataSetHelper::ComputeVR(*F,ds, t);
    }

  if( vr == VR::UN )
    {
    // this element is not known...
    //return ret;
    }

  std::string s(value,value+len);
  if( VR::IsASCII( vr ) )
    {
    return s;
    }
  VL::Type castLen = (VL::Type)len;
  VL::Type count = VM::GetNumberOfElementsFromArray(value, castLen);
  VL vl = vm.GetLength() * vr.GetSizeof();
  if( vm.GetLength() == 0 )
    {
    // VM1_n
    vl = count * vr.GetSizeof();
    VM check  = VM::GetVMTypeFromLength(count, 1);
    assert( vm.Compatible( check ) );
    }

  //if( vl != vm.GetLength() * vr.GetSizeof() )
  //  {
  //  assert(0);
  //  }

  std::istringstream is;
  is.str( s );
  std::ostringstream os;
  switch(vr)
    {
    FromStringFilterCase(AT);
    FromStringFilterCase(FL);
    FromStringFilterCase(FD);
    //FromStringFilterCase(OB);
    FromStringFilterCase(OF);
    //FromStringFilterCase(OW);
    FromStringFilterCase(SL);
    //FromStringFilterCase(SQ);
    FromStringFilterCase(SS);
    FromStringFilterCase(UL);
    //FromStringFilterCase(UN);
    FromStringFilterCase(US);
    FromStringFilterCase(UT);
  default:
    gdcmErrorMacro( "Not implemented" );
    assert(0);
    }
  return os.str();
}

}
