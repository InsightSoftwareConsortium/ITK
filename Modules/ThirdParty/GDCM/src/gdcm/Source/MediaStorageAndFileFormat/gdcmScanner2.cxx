/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmScanner2.h"
#include "gdcmReader.h"
#include "gdcmGlobal.h"
#include "gdcmDicts.h"
#include "gdcmDict.h"
#include "gdcmDictEntry.h"
#include "gdcmStringFilter.h"
#include "gdcmProgressEvent.h"
#include "gdcmFileNameEvent.h"

#include <algorithm> // std::find

namespace gdcm
{

Scanner2::~Scanner2() = default;

void Scanner2::ClearPublicTags()
{
  PublicTags.clear();
}

void Scanner2::ClearPrivateTags()
{
  PrivateTags.clear();
}

void Scanner2::ClearSkipTags()
{
  SkipTags.clear();
}

bool Scanner2::AddSkipTag( Tag const & t )
{
  SkipTags.insert( t );
  return true;
}

bool Scanner2::AddPrivateTag( PrivateTag const & t )
{
  if( !( t.IsPrivate() && t.GetOwner() && *t.GetOwner() ) ) return false;
  // Do not check IsIllegal at this point
  static const Global &g = GlobalInstance;
  static const Dicts &dicts = g.GetDicts();
  const DictEntry &entry = dicts.GetDictEntry( t );
  // Is this tag an ASCII on ?
  if( entry.GetVR() & VR::VRASCII )
    {
    PrivateTags.insert( t );
    }
  else if( entry.GetVR() == VR::INVALID )
    {
    gdcmWarningMacro( "Only tag with known VR are allowed. Tag " << t << " will be discarded" );
    }
  else
    {
    assert( entry.GetVR() & VR::VRBINARY );
    //gdcmWarningMacro( "Only ASCII VR are supported for now. Tag " << t << " will be discarded" );
    PrivateTags.insert( t );
    }
  return true;
}

bool Scanner2::AddPublicTag( Tag const & t )
{
  if( !t.IsPublic() && !t.IsPrivateCreator() ) return false;
  if( t.IsIllegal() ) return false;
  static const Global &g = GlobalInstance;
  static const Dicts &dicts = g.GetDicts();
  const DictEntry &entry = dicts.GetDictEntry( t );
  // Is this tag an ASCII on ?
  if( entry.GetVR() & VR::VRASCII )
    {
    PublicTags.insert( t );
    }
  else if( entry.GetVR() == VR::INVALID )
    {
    gdcmWarningMacro( "Only tag with known VR are allowed. Tag " << t << " will be discarded" );
    }
  else
    {
    assert( entry.GetVR() & VR::VRBINARY );
    //gdcmWarningMacro( "Only ASCII VR are supported for now. Tag " << t << " will be discarded" );
    PublicTags.insert( t );
    }
  return true;
}

bool Scanner2::Scan( Directory::FilenamesType const & filenames )
{
  this->InvokeEvent( StartEvent() );

  // Is there at least one tag ?
  if( !PublicTags.empty() || !PrivateTags.empty() )
    {
    // Prepare hash tables:
    PublicMappings.clear();
    PublicMappings[""]; // Create a fake table for dummy file
    PrivateMappings.clear();
    PrivateMappings[""]; // Create a fake table for dummy file

    // Make our own copy:
    Filenames = filenames;

    // Find the tag with the highest value (get the one from the end of the std::set)
    Tag last(0x0,0x0);
    if( !PublicTags.empty() )
      {
      PublicTagsType::const_reverse_iterator it1 = PublicTags.rbegin();
      const Tag & publiclast = *it1;
      last = publiclast;
      }
    if( !PrivateTags.empty() )
      {
      PrivateTagsType::const_reverse_iterator pit1 = PrivateTags.rbegin();
      Tag privatelast = *pit1;
      // at this point we do not know the private creator, since it can go up
      // to 0xff, simply read up to the next public group (simple logic).
      privatelast.SetGroup( privatelast.GetGroup() + 1 );
      privatelast.SetElement( 0x0 );
      if( last < privatelast ) last = privatelast;
      }

    StringFilter sf;
    Directory::FilenamesType::const_iterator it = Filenames.begin();
    const double progresstick = 1. / (double)Filenames.size();
    Progress = 0;
    for(; it != Filenames.end(); ++it)
      {
      Reader reader;
      const char *filename = it->c_str();
      assert( filename );
      reader.SetFileName( filename );
      bool read = false;
      try
        {
        // Start reading all tags, including the 'last' one:
        read = reader.ReadUpToTag(last, SkipTags);
        }
      catch(std::exception & ex)
        {
        (void)ex;
        gdcmWarningMacro( "Failed to read:" << filename << " with ex:" << ex.what() );
        }
      catch(...)
        {
        gdcmWarningMacro( "Failed to read:" << filename  << " with unknown error" );
        }
      if( read )
        {
        // Keep the mapping:
        sf.SetFile( reader.GetFile() );
        Scanner2::ProcessPublicTag(sf, filename);
        Scanner2::ProcessPrivateTag(sf, filename);
        }
      // Update progress
      Progress += progresstick;
      ProgressEvent pe;
      pe.SetProgress( Progress );
      this->InvokeEvent( pe );
      // For outside application tell which file is being processed:
      FileNameEvent fe( filename );
      this->InvokeEvent( fe );
      }
    }

  this->InvokeEvent( EndEvent() );
  return true;
}

void Scanner2::Print( std::ostream & os ) const
{
  os << "Values:\n";
  for(ValuesType::const_iterator it = Values.begin() ; it != Values.end();
    ++it)
    {
    os << *it << "\n";
    }
  os << "Mapping:\n";
  Directory::FilenamesType::const_iterator file = Filenames.begin();
  for(; file != Filenames.end(); ++file)
    {
    const char *filename = file->c_str();
    assert( filename && *filename );
    bool b = IsKey(filename);
    const char *comment = !b ? "could not be read" : "could be read";
    os << "Filename: " << filename << " (" << comment << ")\n";
    if( PublicMappings.find(filename) != PublicMappings.end() )
      {
      const PublicTagToValue &mapping = GetPublicMapping(filename);
      PublicTagToValue::const_iterator it = mapping.begin();
      for( ; it != mapping.end(); ++it)
        {
        const Tag & tag = it->first;
        const char *value = it->second;
        os << tag << " -> [" << value << "]\n";
        }
      }
    if( PrivateMappings.find(filename) != PrivateMappings.end() )
      {
      const PrivateTagToValue &mapping = GetPrivateMapping(filename);
      PrivateTagToValue::const_iterator it = mapping.begin();
      for( ; it != mapping.end(); ++it)
        {
        const PrivateTag & tag = it->first;
        const char *value = it->second;
        os << tag << " -> [" << value << "]\n";
        }
      }
    }
}

static bool IsVRUI(Tag const &tag)
{
  static const Global &g = Global::GetInstance();
  static const Dicts &dicts = g.GetDicts();
  const DictEntry &dictentry = dicts.GetDictEntry(tag);
  if( dictentry.GetVR() == VR::UI ) return true;
  return false;
}

static bool IsVRUI(PrivateTag const &tag)
{
  static const Global &g = Global::GetInstance();
  static const Dicts &dicts = g.GetDicts();
  const DictEntry &dictentry = dicts.GetDictEntry(tag);
  if( dictentry.GetVR() == VR::UI ) return true;
  return false;
}


void Scanner2::PrintTable( std::ostream & os, bool header ) const
{
  if( header ) {
	  os << "\"Filename\"" << "\t";
    {
    PublicTagsType::const_iterator tag = PublicTags.begin();
    for( ; tag != PublicTags.end(); ++tag )
      {
      const Tag &t = *tag;
      os << '"' << t << '"';
      os << "\t";
      }
    }
    {
    PrivateTagsType::const_iterator tag = PrivateTags.begin();
    for( ; tag != PrivateTags.end(); ++tag )
      {
      const PrivateTag &t = *tag;
      os << '"' << t << '"';
      os << "\t";
      }
    }
    os << "\n";
  }
  Directory::FilenamesType::const_iterator file = Filenames.begin();
  for(; file != Filenames.end(); ++file)
    {
    const char *filename = file->c_str();
    assert( filename && *filename );
    os << '"' << filename << '"' << "\t";
    {
    PublicTagsType::const_iterator tag = PublicTags.begin();
    const PublicTagToValue &mapping = GetPublicMapping(filename);
    for( ; tag != PublicTags.end(); ++tag )
      {
      const Tag &t = *tag;
      bool isui = IsVRUI(t);
      const char *value = "";
      if( mapping.find(t) != mapping.end() ) {
        const char * v = mapping.find(t)->second;
        if(v) value = v;
      }
      os << '"' << (isui ? String<>::Trim( value ) : value) << '"';
      os << "\t";
      }
    }
    {
    PrivateTagsType::const_iterator tag = PrivateTags.begin();
    const PrivateTagToValue &mapping = GetPrivateMapping(filename);
    for( ; tag != PrivateTags.end(); ++tag )
      {
      const PrivateTag &t = *tag;
      bool isui = IsVRUI(t);
      const char *value = "";
      if( mapping.find(t) != mapping.end() ) {
        const char * v = mapping.find(t)->second;
        if(v) value = v;
      }
      os << '"' << (isui ? String<>::Trim( value ) : value) << '"';
      os << "\t";
      }
    }
    os << "\n";
    }
}

Scanner2::PublicTagToValue const & Scanner2::GetPublicMapping(const char *filename) const
{
  assert( filename && *filename );
  if( PublicMappings.find(filename) != PublicMappings.end() )
    return PublicMappings.find(filename)->second;
  return PublicMappings.find("")->second; // dummy file could not be found
}

Scanner2::PrivateTagToValue const & Scanner2::GetPrivateMapping(const char *filename) const
{
  assert( filename && *filename );
  if( PrivateMappings.find(filename) != PrivateMappings.end() )
    return PrivateMappings.find(filename)->second;
  return PrivateMappings.find("")->second; // dummy file could not be found
}

bool Scanner2::IsKey( const char * filename ) const
{
  // Look for the file in Mappings tables:
  assert( filename && *filename );
  PublicMappingType::const_iterator it2 = PublicMappings.find(filename);
  PrivateMappingType::const_iterator it3 = PrivateMappings.find(filename);
  return it2 != PublicMappings.end() || it3 != PrivateMappings.end();
}

Directory::FilenamesType Scanner2::GetKeys() const
{
  Directory::FilenamesType keys;

  Directory::FilenamesType::const_iterator file = Filenames.begin();
  for(; file != Filenames.end(); ++file)
    {
    const char *filename = file->c_str();
    if( IsKey( filename ) )
      {
      keys.push_back( filename );
      }
    }
  assert( keys.size() <= Filenames.size() );
  return keys;
}

const char* Scanner2::GetPublicValue(const char *filename, Tag const &t) const
{
  // \precondition
  assert( PublicTags.find( t ) != PublicTags.end() );
  PublicTagToValue const &ftv = GetPublicMapping(filename);
  if( ftv.find(t) != ftv.end() )
    {
    return ftv.find(t)->second;
    }
  return nullptr;
}

const char* Scanner2::GetPrivateValue(const char *filename, PrivateTag const &t) const
{
  // \precondition
  assert( PrivateTags.find( t ) != PrivateTags.end() );
  PrivateTagToValue const &ftv = GetPrivateMapping(filename);
  if( ftv.find(t) != ftv.end() )
    {
    return ftv.find(t)->second;
    }
  return nullptr;
}

const char *Scanner2::GetFilenameFromPublicTagToValue(Tag const &t, const char *valueref) const
{
  const char *filenameref = nullptr;
  if( valueref )
    {
    Directory::FilenamesType::const_iterator file = Filenames.begin();
    const std::string valueref_str = String<>::Trim( valueref );
    for(; file != Filenames.end() && !filenameref; ++file)
      {
      const char *filename = file->c_str();
      const char * value = GetPublicValue(filename, t);
      if( value && valueref == value )
        {
        filenameref = filename;
        }
      }
    }
  return filenameref;
}

const char *Scanner2::GetFilenameFromPrivateTagToValue(PrivateTag const &pt, const char *valueref) const
{
  const char *filenameref = nullptr;
  if( valueref )
    {
    Directory::FilenamesType::const_iterator file = Filenames.begin();
    const std::string valueref_str = String<>::Trim( valueref );
    for(; file != Filenames.end() && !filenameref; ++file)
      {
      const char *filename = file->c_str();
      const char * value = GetPrivateValue(filename, pt);
      if( value && valueref_str == value )
        {
        filenameref = filename;
        }
      }
    }
  return filenameref;
}



/// Will loop over all files and return a vector of std::strings of filenames
/// where value match the reference value 'valueref'
Directory::FilenamesType Scanner2::GetAllFilenamesFromPublicTagToValue(Tag const &t, const char *valueref) const
{
  Directory::FilenamesType theReturn;
  if( valueref )
    {
    const std::string valueref_str = String<>::Trim( valueref );
    Directory::FilenamesType::const_iterator file = Filenames.begin();
    for(; file != Filenames.end(); ++file)
      {
      const char *filename = file->c_str();
      const char * value = GetPublicValue(filename, t);
      if( value && valueref_str == value )
        {
        theReturn.push_back( filename );
        }
      }
    }
  return theReturn;
}

Directory::FilenamesType Scanner2::GetAllFilenamesFromPrivateTagToValue(PrivateTag const &pt, const char *valueref) const
{
  Directory::FilenamesType theReturn;
  if( valueref )
    {
    const std::string valueref_str = String<>::Trim( valueref );
    Directory::FilenamesType::const_iterator file = Filenames.begin();
    for(; file != Filenames.end(); ++file)
      {
      const char *filename = file->c_str();
      const char * value = GetPrivateValue(filename, pt);
      if( value && valueref_str == value )
        {
        theReturn.push_back( filename );
        }
      }
    }
  return theReturn;
}

Scanner2::PublicTagToValue const & Scanner2::GetMappingFromPublicTagToValue(Tag const &t, const char *valueref) const
{
  return GetPublicMapping( GetFilenameFromPublicTagToValue(t, valueref) );
}

Scanner2::PrivateTagToValue const & Scanner2::GetMappingFromPrivateTagToValue(PrivateTag const &pt, const char *valueref) const
{
  return GetPrivateMapping( GetFilenameFromPrivateTagToValue(pt, valueref) );
}

Scanner2::ValuesType Scanner2::GetPublicValues(Tag const &t) const
{
  ValuesType vt;
  Directory::FilenamesType::const_iterator file = Filenames.begin();
  for(; file != Filenames.end(); ++file)
    {
    const char *filename = file->c_str();
    PublicTagToValue const &ttv = GetPublicMapping(filename);
    if( ttv.find(t) != ttv.end() )
      {
      vt.insert( ttv.find(t)->second );
      }
    }
  return vt;
}

Scanner2::ValuesType Scanner2::GetPrivateValues(PrivateTag const &pt) const
{
  ValuesType vt;
  Directory::FilenamesType::const_iterator file = Filenames.begin();
  for(; file != Filenames.end(); ++file)
    {
    const char *filename = file->c_str();
    PrivateTagToValue const &ttv = GetPrivateMapping(filename);
    if( ttv.find(pt) != ttv.end() )
      {
      vt.insert( ttv.find(pt)->second );
      }
    }
  return vt;
}

Directory::FilenamesType Scanner2::GetPublicOrderedValues(Tag const &t) const
{
  Directory::FilenamesType theReturn;
  Directory::FilenamesType::const_iterator file = Filenames.begin();
  for(; file != Filenames.end(); ++file)
    {
    const char *filename = file->c_str();
    PublicTagToValue const &ttv = GetPublicMapping(filename);
    if( ttv.find(t) != ttv.end() )
      {
        std::string theVal = std::string(ttv.find(t)->second);
        if (std::find(theReturn.begin(), theReturn.end(), theVal) == theReturn.end()){
          theReturn.push_back( theVal );//only add new tags to the list
        }
      }
    }
  return theReturn;
}

Directory::FilenamesType Scanner2::GetPrivateOrderedValues(PrivateTag const &pt) const
{
  Directory::FilenamesType theReturn;
  Directory::FilenamesType::const_iterator file = Filenames.begin();
  for(; file != Filenames.end(); ++file)
    {
    const char *filename = file->c_str();
    PrivateTagToValue const &ttv = GetPrivateMapping(filename);
    if( ttv.find(pt) != ttv.end() )
      {
        std::string theVal = std::string(ttv.find(pt)->second);
        if (std::find(theReturn.begin(), theReturn.end(), theVal) == theReturn.end()){
          theReturn.push_back( theVal );//only add new tags to the list
        }
      }
    }
  return theReturn;
}

void Scanner2::ProcessPublicTag(StringFilter &sf, const char *filename)
{
  assert( filename );
  PublicTagToValue &mapping = PublicMappings[filename];
  const File& file = sf.GetFile();

  const FileMetaInformation & header = file.GetHeader();
  const DataSet & ds = file.GetDataSet();
  PublicTagsType::const_iterator tag = PublicTags.begin();
  for( ; tag != PublicTags.end(); ++tag )
    {
    if( tag->GetGroup() == 0x0002 )
      {
      if( header.FindDataElement( *tag ) )
        {
        DataElement const & de = header.GetDataElement( *tag );
        std::string s = sf.ToString(de.GetTag());

        // Store the potentially new value:
        Values.insert( s );
        assert( Values.find( s ) != Values.end() );
        const char *value = Values.find( s )->c_str();
        assert( value );
        mapping.insert(
          PublicTagToValue::value_type(*tag, value));
        }
      }
    else
      {
      if( ds.FindDataElement( *tag ) )
        {
        DataElement const & de = ds.GetDataElement( *tag );
        std::string s = sf.ToString(de.GetTag());

        // Store the potentially new value:
        Values.insert( s );
        assert( Values.find( s ) != Values.end() );
        const char *value = Values.find( s )->c_str();
        assert( value );
        mapping.insert(
          PublicTagToValue::value_type(*tag, value));
        }
      }
    } // end for
}

void Scanner2::ProcessPrivateTag(StringFilter &sf, const char *filename)
{
  assert( filename );
  PrivateTagToValue &mapping = PrivateMappings[filename];
  const File& file = sf.GetFile();
  const DataSet & ds = file.GetDataSet();
  PrivateTagsType::const_iterator ptag = PrivateTags.begin();
  for( ; ptag != PrivateTags.end(); ++ptag )
    {
    if( ds.FindDataElement( *ptag ) )
      {
      DataElement const & de = ds.GetDataElement( *ptag );
      std::string s = sf.ToString(de.GetTag());

      // Store the potentially new value:
      Values.insert( s );
      assert( Values.find( s ) != Values.end() );
      const char *value = Values.find( s )->c_str();
      assert( value );
      mapping.insert(
        PrivateTagToValue::value_type(*ptag, value));
      }
    } // end for
}

} // end namespace gdcm
