/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmMrProtocol.h"

#include <map>
#include <string>

namespace gdcm
{

struct MrProtocol::Element {
};

typedef std::map< std::string, std::string > MyMapType;
struct MrProtocol::Internals {
  MyMapType mymap;
  std::string csastr;
  int version;
};

static inline bool starts_with(const std::string& s1, const std::string& s2)
{
  return s2.size() <= s1.size() && s1.compare(0, s2.size(), s2) == 0;
}

MrProtocol::MrProtocol()
{
  Pimpl = new MrProtocol::Internals;
}

bool MrProtocol::Load( const ByteValue * bv, const char * csastr, int version )
{
  if( bv )
  {
    std::string str(bv->GetPointer(), bv->GetLength());
    std::istringstream is(str);
    std::string s;
    Pimpl->version = version;
    if( csastr ) Pimpl->csastr = csastr;
    else Pimpl->csastr = "";
    MyMapType &mymap = Pimpl->mymap;
    mymap.clear();
    // Need to handle both:
    // ### ASCCONV BEGIN ###
    // as well as:
    // ### ASCCONV BEGIN object=MrProtDataImpl@MrProtocolData version=41310008 converter=%MEASCONST%/ConverterList/Prot_Converter.txt ###
    static const char begin[] = "### ASCCONV BEGIN ";
    static const char end[] = "### ASCCONV END ###";
    bool hasstarted = false;
    while( std::getline(is, s ) )
    {
      if( !hasstarted )
      {
        hasstarted = starts_with(s, begin);
        if( hasstarted ) {
          if( version == -1 ) {
            // find version if not specified:
            static const char vers[] = "version=";
            std::string::size_type p = s.find(vers);
            if ( p != std::string::npos) {
              const char *v = s.c_str() + p + sizeof(vers) - 1;
              Pimpl->version = atoi(v);
            }
          }
          continue; // do not insert ASCCONV begin
        }
      }
      if( !hasstarted ) continue;
      if( starts_with(s, end) ) break;
      std::string::size_type pos = s.find( '=' );
      if( pos != std::string::npos )
      {
        std::string sub1 = s.substr(0, pos);
        sub1.erase( sub1.find_last_not_of(" \t") + 1);
        std::string sub2 = s.substr(pos+1); // skip the '=' char
        sub2.erase( 0, sub2.find_first_not_of(" \t"));
        mymap.insert( MyMapType::value_type(sub1, sub2) );
      }
      else
      {
        // ### ASCCONV BEGIN ###
        // ### ASCCONV END ###
      }
    }
  }
  else
  {
    Pimpl->version = 0;
    Pimpl->csastr = "";
    Pimpl->mymap.clear();
    return false;
  }
  return true;
}

MrProtocol::~MrProtocol()
{
  delete Pimpl;
}

int MrProtocol::GetVersion() const
{
  return Pimpl->version;
}

static inline std::string trim(std::string str)
{
  str.erase(0, str.find_first_not_of('"'));
  str.erase(str.find_last_not_of('"')+1);
  return str;
}

void MrProtocol::Print(std::ostream &os) const
{
  {
    os << Pimpl->csastr << " / Version: " << Pimpl->version << std::endl;
    os << std::endl;
    MyMapType &mymap = Pimpl->mymap;
    for( MyMapType::const_iterator it = mymap.begin();
        it != mymap.end(); ++it)
    {
      os << it->first << " : " << trim(std::string(it->second)) << std::endl;
    }
  }
}

const char * MrProtocol::GetMrProtocolByName(const char *name) const
{
  if( name )
  {
    MyMapType &mymap = Pimpl->mymap;
    MyMapType::const_iterator it = mymap.find ( name );
    if( it == mymap.end() ) return NULL;
    return it->second.c_str();
  }
  return NULL;
}

bool MrProtocol::FindMrProtocolByName(const char *name) const
{
  if( name )
  {
    MyMapType &mymap = Pimpl->mymap;
    MyMapType::const_iterator it = mymap.find ( name );
    if( it != mymap.end() ) return true;
  }
  return false;
}

bool MrProtocol::GetSliceArray( MrProtocol::SliceArray & sa ) const
{
  // Technically this is all boilerplate code, since we could infer the type
  // from the first few leters, eg:
  // us -> unsigned char
  // d -> double
  // ...
  sa.Slices.clear();
  static const char saSize[] = "sSliceArray.lSize";
  const char * sizestr = GetMrProtocolByName(saSize);
  if( sizestr == NULL ) return false;
  const int size = atoi( sizestr );
  sa.Slices.resize( size );

  // sSliceArray.asSlice[*].sPosition.dSag
  // sSliceArray.asSlice[*].sPosition.dCor
  // sSliceArray.asSlice[*].sPosition.dTra
  char buf[512];
  static const char templ1[] = "sSliceArray.asSlice[%d].sPosition.%s";
  static const char templ2[] = "sSliceArray.asSlice[%d].sNormal.%s";
  static const char *dir[3] = { "dSag", "dCor", "dTra"};
  for( int i = 0; i < size; ++i )
  {
    Slice & slice = sa.Slices[i];
    {
      double v[3];
      for( int j = 0; j < 3; ++j )
      {
        sprintf( buf, templ1, i, dir[j] );
        const char * valstr = GetMrProtocolByName(buf);
        // when not present this means 0.0
        double val = 0.0;
        if( valstr ) val = atof( valstr );
        v[j] = val;
      }
      Vector3 & pos = slice.Position;
      pos.dSag = v[0];
      pos.dCor = v[1];
      pos.dTra = v[2];
    }
    {
      double v[3];
      for( int j = 0; j < 3; ++j )
      {
        sprintf( buf, templ2, i, dir[j] );
        const char * valstr = GetMrProtocolByName(buf);
        // when not present this means 0.0
        double val = 0.0;
        if( valstr ) val = atof( valstr );
        v[j] = val;
      }
      Vector3 & pos = slice.Normal;
      pos.dSag = v[0];
      pos.dCor = v[1];
      pos.dTra = v[2];
    }
  }

  return true;
}

} // end namespace gdcm
