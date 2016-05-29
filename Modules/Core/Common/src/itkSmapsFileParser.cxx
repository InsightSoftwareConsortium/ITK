/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkSmapsFileParser.h"

namespace itk
{
bool ITKCommon_EXPORT ci_equal(char a, char b)
{
  return tolower( static_cast< int >( a ) ) == tolower( static_cast< int >( b ) );
}

MapRecord::~MapRecord()
{}

void MapRecord::Reset(void)
{
  m_Tokens.clear();
  m_RecordName = "";
}

/* SmapsRecord implementation */

ITKCommon_EXPORT std::istream &  operator>>(std::istream & in, SmapsRecord & record)
{
  record.Reset();

  try
    {
    // Get Header line
    std::string headerline;
    std::getline(in, headerline);

    if ( headerline.empty() )
      {
      return in;
      }

    // Get name
    std::istringstream stream(headerline);
    std::string        address, perms, offset, device;
    int                inode = -1;
    // the header is defined with the following expression: "address permissions
    // offset device inode [name]"
    stream >> address;
    if ( !stream.good() ) { itkGenericExceptionMacro(<< "bad address: " << address); }
    stream >> perms;
    if ( !stream.good() ) { itkGenericExceptionMacro(<< "bad perms: " << perms); }
    stream >> offset;
    if ( !stream.good() ) { itkGenericExceptionMacro(<< "bad offset: " << offset); }
    stream >> device;
    if ( !stream.good() ) { itkGenericExceptionMacro(<< "bad device: " << device); }
    stream >> inode;
    // name can be empty
    if ( !stream.eof() )
      {
      std::getline(stream, record.m_RecordName);
      }

    std::string    token;
    std::streampos lastPos = in.tellg();
    // a token is defined with the following expression: "token: N kB"
    while ( std::getline(in, token, ':').good() )
      {
      //make sure it is a token and not a new record. A token doesn't contains
      // space character
      if ( token.find(' ') != std::string::npos )
        {
        in.seekg (lastPos, std::ios::beg);
        break;
        }
      //fill the token with the memory usage N in kB
      in >> record.m_Tokens[token];
      std::getline(in, token);
      if ( token != " kB" || !in.good() ) { itkGenericExceptionMacro(<< "bad size: " << record.m_Tokens[token]); }
      lastPos = in.tellg();
      }
    }
  catch ( ExceptionObject & excp )
    {
    record.Reset();
    // propagate the exception
    itkGenericExceptionMacro(<< "The smaps header is corrupted" << excp);
    }
  return in;
}

ITKCommon_EXPORT std::istream & operator>>(std::istream & in, VMMapSummaryRecord & record)
{
  record.Reset();

  try
    {
    // the record name can have spaces.
    in >> record.m_RecordName;

    if ( in.eof() && record.m_RecordName.empty() )
      {
      return in;
      }

    if ( !in.good() )
      {
      itkGenericExceptionMacro(<< "Bad record name: " << record.m_RecordName);
      }

    std::string bracket;

    while ( ( in >> bracket ).good() && bracket.find("[", 0) == std::string::npos )
      {
      record.m_RecordName += std::string(" ") + bracket;
      }

    if ( !in.good() || bracket.find("[", 0) == std::string::npos )
      {
      itkGenericExceptionMacro(<< "For record: " << record.m_RecordName
                               << ", bad left bracket: " << bracket);
      }

    in >> record.m_Tokens["Size"];

    if ( !in.good() )
      {
      itkGenericExceptionMacro(<< "For record: " << record.m_RecordName
                               << ", bad size: " << record.m_Tokens["Size"]);
      }

    in >> bracket;

    if ( !in.good() )
      {
      itkGenericExceptionMacro(<< "For record: " << record.m_RecordName
                               << ", bad right bracket: " << bracket);
      }
    }
  catch ( ExceptionObject & excp )
    {
    record.Reset();
    // propagate the exception
    itkGenericExceptionMacro(<< "The smaps header is corrupted" << excp);
    }
  return in;
}

/* typical output:
 *  __DATA                             c000 [   4K] rw-/rwx SM=ZER ...l/bin/cronwake
 *  VALLOC_USED(DefaultMallocZone_    25000 [   4K] rw-/rwx SM=COW
 *  MALLOC_USED(DefaultMallocZone_    26000 [  44K] rw-/rwx SM=ZER
 *                                   31000 [   4K] rw-/rwx SM=COW 34300000 00000...
 *                                   32000 [  76K] rw-/rwx SM=COW 00001eaa 01001...
 *                                   45000 [   4K] rw-/rwx SM=COW 34300000 00000...
 *                                   46000 [ 344K] rw-/rwx SM=COW 00000000 00000...
 *  Memory tag=7                     100000 [1044K] rw-/rwx SM=COW
 *  MALLOC_USED(DefaultMallocZone_   300000 [   4K] rw-/rwx SM=PRV
 *  Submap                90000000-9fffffff         r--/r-- machine-wide submap
 *  __DATA                         a0000000 [   4K] rw-/rw- SM=COW ...System.B.dylib
 *  __DATA                         a0001000 [   4K] rw-/rw- SM=ZER ...System.B.dylib
 *  __DATA                         a0002000 [   4K] rw-/rw- SM=COW ...System.B.dylib
 *  __DATA                         a0003000 [  20K] rw-/rw- SM=COW ...System.B.dylib
*/

ITKCommon_EXPORT std::istream & operator>>(std::istream & in, VMMapRecord & record)
{
  record.Reset();
  bool submapFound = false;
  bool recordHasNoName = false;
  char line[256];
  try
    {
    std::string address;
    do
      {
      // the record name can have spaces.
      in >> record.m_RecordName;
      if ( in.eof() || record.m_RecordName.empty() )
        {
        return in;
        }

      if ( !in.good() )
        {
        itkGenericExceptionMacro(<< "Bad record name: " << record.m_RecordName);
        }

      //skip Submap entries
      if ( record.m_RecordName == "Submap" )
        {
        in.getline(line, 256);
        submapFound = true;
        }

      // all the records have been parsed, this is a new section
      else if ( record.m_RecordName == "====" )
        {
        record.Reset();
        return in;
        }
      else
        {
        // the name is folowed by an address
        in >> address;

        if ( !in.good() )
          {
          itkGenericExceptionMacro(<< "For record: " << record.m_RecordName
                                   << ", bad address: " << address);
          }
        // If address is "[" then recordName was the address and there is name
        // for
        // the record, skip it.
        if ( address.find("[", 0) != std::string::npos )
          {
          in.getline(line, 256);
          recordHasNoName = true;
          }
        else
          {
          recordHasNoName = false;
          }
        submapFound = false;
        }
      }
    while ( submapFound || recordHasNoName );

    std::string bracket;

    while ( ( in >> bracket ).good() && bracket.find("[", 0) == std::string::npos )
      {
      // the string is not a bracket yet, but probably the address. So the
      // previous
      // address was just the name
      record.m_RecordName += std::string(" ") + address;
      address = bracket;
      }
    if ( !in.good() || bracket.find("[", 0) == std::string::npos )
      {
      itkGenericExceptionMacro(<< "For record: " << record.m_RecordName
                               << ", bad left bracket: " << bracket);
      }
    if ( bracket.length() > 1 )
      { //bracket contains the size, ie "[1024K]"
      record.m_Tokens["Size"] = static_cast<itk::SizeValueType>( atoi( bracket.substr(1, bracket.length() - 3).c_str() ) );
      }
    else
      {
      in >> record.m_Tokens["Size"];
      }
    if ( !in.good() )
      {
      itkGenericExceptionMacro(<< "For record: " << record.m_RecordName
                               << ", bad size: " << record.m_Tokens["Size"]);
      }
    in.getline(line, 256);
    if ( !in.good() )
      {
      itkGenericExceptionMacro(<< "For record: " << record.m_RecordName
                               << ", bad end of line: " << line);
      }
    }
  catch ( ExceptionObject & excp )
    {
    record.Reset();
    // propagate the exception
    itkGenericExceptionMacro(<< "The smaps header is corrupted" << excp);
    }
  return in;
}

//bool ITKCommon_EXPORT ci_equal(char a, char b); // located in
// itkSmapsFileParser.cxx

/** Binary functor to accumulate memory usage in kB
 */
template< typename TFirstType >
struct MapRecordPlusor {
  MapRecordPlusor< TFirstType >(const char *token = "Size"):
    m_Token(token)
           {}

  TFirstType operator()(TFirstType first, const MapRecord *const & second)
  {
    std::map< std::string, MapRecord::MemoryLoadType >::const_iterator it = second->m_Tokens.find(m_Token);
    return first + ( ( it != second->m_Tokens.end() ) ? it->second : 0 );
  }

  const char *m_Token;
};

/** Binary functor to accumulate memory usage in kB
 *  The record must match (insensitively) the filter in order to be taken into account
 */
template< typename TFirstType >
struct MapRecordConditionalPlusor {
  MapRecordConditionalPlusor< TFirstType >(const char *filter, const char *token = "Size"):
    m_Filter(filter), m_Token(token)
         {}
  TFirstType operator()(TFirstType first, const MapRecord *const & second)
  {
    if ( std::search(second->m_RecordName.begin(), second->m_RecordName.end(),
                     m_Filter.begin(), m_Filter.end(), ci_equal) != second->m_RecordName.end() )
      {
      return MapRecordPlusor< TFirstType >(m_Token) (first, second);
      }
    return first;
  }

  std::string m_Filter;
  const char *m_Token;
};

/**              ---             MapData               ---              **/

/** MadData destructor */
MapData::~MapData()
{
  this->Reset();
}

MapData::MemoryLoadType
MapData::GetTotalMemoryUsage()
{
  return std::accumulate( this->m_Records.begin(), this->m_Records.end(),
                          MapData::MemoryLoadType(0),
                          MapRecordPlusor< MemoryLoadType >() );
}

MapData::MemoryLoadType
MapData::GetMemoryUsage(const char *filter, const char *token)
{
  return std::accumulate( this->m_Records.begin(), this->m_Records.end(),
                          MapData::MemoryLoadType(0),
                          MapRecordConditionalPlusor< MemoryLoadType >(filter, token) );
}

/** is the data empty ? */
bool MapData::Empty()
{
  return m_Records.empty();
}

void DeleteMapRecord(MapRecord *const & record)
{
  delete record;
}

void MapData::Reset()
{
  std::for_each(m_Records.begin(), m_Records.end(), DeleteMapRecord);
  m_Records.clear();
}

/**              ---            SmapsData               ---              **/

SmapsData_2_6::~SmapsData_2_6()
{}

std::istream & operator>>(std::istream & smapsStream, SmapsData_2_6 & data)
{
  SmapsRecord *record = ITK_NULLPTR;

  // reset the records from a previous parsing
  data.Reset();
  try
    {
    record = new SmapsRecord;
    // parse each line of the Smaps file and fill the record vector.
    while ( smapsStream >> *record )
      {
      data.m_Records.push_back(record);
      record = new SmapsRecord;
      }
    }
  catch ( ExceptionObject & excp )
    {
    // in case of error, erase the records.
    data.Reset();
    // propagate the exception
    itkGenericExceptionMacro(<< "The Smaps stream contains errors, can't read the memory records." << excp);
    }
  delete record;
  return smapsStream;
}

SmapsData_2_6::MemoryLoadType
SmapsData_2_6::GetHeapUsage()
{
  MemoryLoadType heapUsage = this->GetMemoryUsage("heap", "Size");

  // in some machines, there is no [heap] record;
  if ( heapUsage == 0 )
    {
    //use the unnamed segments instead
    heapUsage = this->GetMemoryUsage("", "Size");
    }
  return heapUsage;
}

SmapsData_2_6::MemoryLoadType
SmapsData_2_6::GetStackUsage()
{
  return this->GetMemoryUsage("stack", "Size");
}

/**              ---            VMMapData               ---              **/

VMMapData_10_2
::VMMapData_10_2():
  m_UsingSummary(false)
{}

VMMapData_10_2
::~VMMapData_10_2()
{}

std::istream & operator>>(std::istream & stream, VMMapData_10_2 & data)
{
  MapRecord *record = ITK_NULLPTR;

  // reset the records from a previous parsing
  data.Reset();
  try
    {
    std::string    line;

    // get to the Summary subsection
    while ( std::getline(stream, line).good() )
      {
      if ( line.find("==== Summary for process", 0) != std::string::npos )
        {
        break;
        }
      }
    // get to the first record
    while ( std::getline(stream, line).good() )
      {
      if ( line.find("REGION TYPE", 0) != std::string::npos )
        {
        break;
        }
      }
    // burn the line "===========             [ =======]"
    if ( !std::getline(stream, line).good() )
      {
      //sometimes, vmmap doesn't have any Region Type summary sections,
      //parse "Writable regions" instead
      //go back to the beginning of the file
      stream.clear();
      stream.seekg(0, std::ios_base::beg);
      // get to the Summary subsection
      while ( std::getline(stream, line).good() )
        {
        if ( line.find("==== Writable regions for process", 0) != std::string::npos )
          {
          break;
          }
        }
      if ( stream.fail() )
        {
        itkGenericExceptionMacro(<< "Can't find the \"Writable regions\" section, can't read the memory records.");
        }
      data.m_UsingSummary = false;
      }
    else
      {
      data.m_UsingSummary = true;
      }
    if ( data.m_UsingSummary )
      {
      record = new VMMapSummaryRecord;
      // parse each line of the Smaps file and fill the record vector.
      while ( stream >> *static_cast< VMMapSummaryRecord * >( record ) )
        {
        if ( record->m_RecordName.empty() )
          {
          break;
          }
        data.m_Records.push_back(record);
        record = new VMMapSummaryRecord;
        }
      }
    else
      {
      record = new VMMapRecord;
      // parse each line of the Smaps file and fill the record vector.
      while ( stream >> *dynamic_cast< VMMapRecord * >( record ) )
        {
        if ( record->m_RecordName.empty() )
          {
          break;
          }
        data.m_Records.push_back(record);
        record = new VMMapRecord;
        }
      }
    }
  catch ( ExceptionObject & excp )
    {
    // in case of error, erase the records.
    data.Reset();
    // propagate the exception
    itkGenericExceptionMacro(<< "The VMMap stream contains errors, can't read the memory records." << excp);
    }
  //last record failed, it hasn't be added into data, delete it.
  delete record;
  return stream;
}

VMMapData_10_2::MemoryLoadType
VMMapData_10_2::GetHeapUsage()
{
  return this->GetMemoryUsage("malloc", "Size");
}

VMMapData_10_2::MemoryLoadType
VMMapData_10_2::GetStackUsage()
{
  return this->GetMemoryUsage("stack", "Size");
}
} // end namespace itk
