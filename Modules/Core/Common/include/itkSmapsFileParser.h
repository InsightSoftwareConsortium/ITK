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
#ifndef itkSmapsFileParser_h
#define itkSmapsFileParser_h

#include "itkMacro.h"
#include "itkIntTypes.h"

#include <string>
#include <vector>
#include <map>
#include <istream>
#include <iostream>

namespace itk
{
/** \class MapRecord
 * \brief MapRecord class.
 *
 * \todo Add documentation
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT MapRecord
{
public:
  typedef SizeValueType  MemoryLoadType;
  virtual ~MapRecord();
  /** Reset the record
  */
  void Reset();

  /** Optional record name
  */
  std::string m_RecordName;

  /** Contains a list of token with the associated memory allocated, tokens
   *  could be typically: Size, Rss, Shared_Clean, Shared_Dirty, Private_Clean,
   *  Private_Dirty, Referenced.
   */
  std::map< std::string, MemoryLoadType > m_Tokens;
};

/** \class SmapsRecord
 *  \brief An entry in a smaps file.
 *
 *  It is filled by operator>>(istream&,SmapsRecord&).
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT SmapsRecord:public MapRecord
{
  /** Input operator to fill a SmapsRecord
   *  The format has to be the following:
   *  "address permissions offset device inode optional_name
   *   Token1:      number kB
   *   Token2:      number kB
   *   ...
   *   TokenN:      number kB"
   *  Example:
   *  00101000-0023b000 r-xp 00000000 fd:00 165671     /lib/libc-2.5.so
   *  Size:              1256 kB
   *  Rss:                640 kB
   *  Shared_Clean:       640 kB
   *  Shared_Dirty:         0 kB
   *  Private_Clean:        0 kB
   *  Private_Dirty:        0 kB
   */
  friend ITKCommon_EXPORT std::istream &  operator>>(std::istream & in, SmapsRecord & record);
};

/** \class VMMapSummaryRecord
 *  \brief Contains an entry in a smaps file.
 *
 *  It is filled by operator>>(istream&,VMMapRecord&).
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT VMMapSummaryRecord:public MapRecord
{
  /** Input operator to fill a VMMapRecord
   *  recordName             [ numberK]
   *  Example
   *  MALLOC                  [  18536K]
   */
  friend ITKCommon_EXPORT std::istream &  operator>>(std::istream & in, VMMapSummaryRecord & record);
};

/** \class VMMapRecord
 *  \brief Contains an entry in a smaps file.
 *
 *  It is filled by operator>>(istream&,SmapsRecord&).
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT VMMapRecord:public MapRecord
{
  /** Input operator to fill a VMMapRecord
   *  recordName address [ numberK] permissions mode
   *  Example
   *  __DATA                         8fe51000 [   4K] rw-/rwx SM=COW /usr/lib/dyld
   */
  friend ITKCommon_EXPORT std::istream &  operator>>(std::istream & in, VMMapRecord & record);
};

/** MAP DATA **/

/** \class MapData
 *  \brief Base class for map data containers.
 *
 *  Inherited classes must implement their own
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT MapData
{
public:
  /** need a large enough type to be able to accumulate the SmapsRecord */
  typedef SizeValueType  MemoryLoadType;
  //todo delete records
  virtual ~MapData();

  /** Returns the heap usage in kB of the process */
  virtual MemoryLoadType GetHeapUsage() = 0;

  /** Returns the stack usage in kB of the process */
  virtual MemoryLoadType GetStackUsage() = 0;

  /** Returns the total memory usage in kB of the process */
  virtual MemoryLoadType GetTotalMemoryUsage();

  /** Returns the memory usage in kB of a process segment */
  virtual MemoryLoadType GetMemoryUsage(const char *filter, const char *token);

  /** Returns true if the data has not been initialized yet */
  bool Empty();

protected:
  /** Clear the content of the container */
  void Reset();

protected:
  typedef std::vector< MapRecord * > MapRecordVectorType;

  /** contains all the segment records */
  MapRecordVectorType m_Records;
};

/** \class SmapsData_2_6
 *  \brief Read a smaps stream and return the memory usage information.
 *  Smaps files have been added since the linux kernel 2.6
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT SmapsData_2_6:public MapData
{
public:
  typedef MapData::MemoryLoadType MemoryLoadType;

  virtual ~SmapsData_2_6() ITK_OVERRIDE;

  /** Returns the heap usage in kB of the process */
  virtual MemoryLoadType GetHeapUsage() ITK_OVERRIDE;

  /** Returns the stack usage in kB of the process */
  virtual MemoryLoadType GetStackUsage() ITK_OVERRIDE;

  /** fill the smaps data */
  friend ITKCommon_EXPORT std::istream &  operator>>(std::istream & smapsStream,
                                                     SmapsData_2_6 & data);

protected:
  bool m_HeapRecordFound;
};

/** \class VMMapData_10_2
 *  \brief TODO
 *
 *  Apparently the vmmap command exists since at least Mac OS X v10.2
 *  On Tiger, /usr/bin/vmmap used to be installed by the Essentials.pkg,
 *  On Panther, /usr/bin/vmmap used to be installed by the DevTools.pkg,
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT VMMapData_10_2:public MapData
{
public:
  typedef MapData::MemoryLoadType MemoryLoadType;
  VMMapData_10_2();
  virtual ~VMMapData_10_2() ITK_OVERRIDE;

  /** Returns the heap usage in kB of the process */
  virtual MemoryLoadType GetHeapUsage() ITK_OVERRIDE;

  /** Returns the stack usage in kB of the process */
  virtual MemoryLoadType GetStackUsage() ITK_OVERRIDE;

  /** fill the smaps data */
  friend ITKCommon_EXPORT std::istream & operator>>(std::istream & stream,
                                                    VMMapData_10_2 & data);

protected:
  bool m_UsingSummary;
};

/** \class MapFileParser
 *  \brief TODO
 *
 * FIXME: Add documentation
 *
 * \ingroup ITKCommon
 */
template< typename TMapData >
class ITK_TEMPLATE_EXPORT MapFileParser
{
public:
  typedef typename TMapData::MemoryLoadType MemoryLoadType;

  virtual ~MapFileParser();
  /** Load and parse a Map file pointed by mapFileLocation.
   *  If mapFileLocation is empty, load the default file
   *  Throw an exception is the file can't be opened.
   */
  virtual void ReadFile(const std::string & mapFileLocation = "") = 0;

  /** ReRead the last parsed file to refresh the memory usage.
   *  Returns true if read from the default location "".
  */
  bool Update();

  /** Returns the heap usage in kB of the process.
   *  If no file has been loaded yet, load a default file.
  */
  MemoryLoadType GetHeapUsage();

  /** Returns the heap usage in kB of the process.
   *  If no file has been loaded yet, load a default file.
  */
  MemoryLoadType GetStackUsage();

  /** Returns the total memory usage in kB of a process.
   *  If no file has been loaded yet, load a default file.
  */
  MemoryLoadType GetTotalMemoryUsage();

  /** Returns the memory usage in kB of a process segment.
   *  If no file has been loaded yet, load a default file.
  */
  MemoryLoadType GetMemoryUsage(const char *filter, const char *token = "Size");

protected:
  std::string m_MapFilePath;    //< location of the last loaded Map file
  TMapData    m_MapData;        //< data of the loaded smap file
};

/** \class SmapsFileParser
 *  \brief Read a smap file (typically located in /proc/PID/smaps) and extract the
 *  memory usage information. Any smaps data reader can be used in template as
 *  long as they implement a operator>>(istream&) and have GetXXXUsage() methods.
 * \ingroup ITKCommon
 */
template< typename TSmapsDataType >
class ITK_TEMPLATE_EXPORT SmapsFileParser:public MapFileParser< TSmapsDataType >
{
public:
  virtual ~SmapsFileParser();
  /** Load and parse the smaps file pointed by smapsFileLocation.
   *  If smapsFileLocation is empty, load the file located at
   *  "/proc/" + PID + "/smaps"
   *  Throw an exception is the file can't be opened.
   */
  virtual void ReadFile(const std::string & mapFileLocation = "");
};

/** \class VMMapFileParser
 *  \brief Read the output of a vmmap command and extract the
 *  memory usage information. Used for MAC OS X machines.
 * \ingroup ITKCommon
 */
template< typename TVMMapDataType >
class ITK_TEMPLATE_EXPORT VMMapFileParser:public MapFileParser< TVMMapDataType >
{
public:
  virtual ~VMMapFileParser();
  /** If no vmmap file, create one using "vmmap pid" command
   *  Throw an exception is the file can't be created/opened.
   */
  virtual void ReadFile(const std::string & mapFileLocation = "");
};
}  // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSmapsFileParser.hxx"
#endif

#endif // itkSmapsFileParser_h
