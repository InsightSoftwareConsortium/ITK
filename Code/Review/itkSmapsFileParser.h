/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmapsFileParser.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSmapsFileParser_h
#define __itkSmapsFileParser_h

#include "itkConfigure.h"
#include "itkWin32Header.h"
#include "itkExceptionObject.h"

#include <string>
#include <vector>
#include <map>
#include <istream>

namespace itk {

/** This struct contains an entry in a smaps file. 
 *  It is filled by operator>>(istream&,SmapsRecord&).
*/
struct ITKCommon_EXPORT SmapsRecord
{
  typedef unsigned int  MemoryLoadType;

  /** Reset the record
  */
  void Reset(void);

  /** Name of the file if any. Can be [heap] or [stack] also.
  */
  std::string  m_HeaderName;

  /** Contains a list of token with the associated memory allocated, tokens 
   *  could be typically: Size, Rss, Shared_Clean, Shared_Dirty, Private_Clean, 
   *  Private_Dirty, Referenced.
  */
  std::map<std::string, MemoryLoadType> m_Tokens;

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
  friend ITKCommon_EXPORT std::istream&  operator>>(std::istream &in, SmapsRecord &record);
};



/** \class SmapsData_2_6 
 *  \brief Read a smaps stream and return the memory usage information.
 *  Smaps files have been added since the linux kernel 2.6 
*/
template <class TSmapsRecord>
class ITK_EXPORT SmapsData_2_6{
public:
  /** need an unsigned long type to be able to accumulate the SmapsRecord */
  typedef unsigned long             MemoryLoadType;
  
  /** Returns true if the data has not been initialized yet.
  */
  bool Empty();

  /** Returns the heap usage in kB of the process */
  MemoryLoadType GetHeapUsage();
  /** Returns the stack usage in kB of the process */
  MemoryLoadType GetStackUsage();
  /** Returns the memory usage in kB of a process segment */
  MemoryLoadType GetMemoryUsage( const char * filter, const char * token = "Size");
  /** Returns the total memory usage in kB of the process */
  MemoryLoadType GetTotalMemoryUsage();

  /** fill the smaps data */
  template<class TSmapsRecordType> friend ITK_EXPORT std::istream& operator>>( std::istream &smapsStream,SmapsData_2_6<TSmapsRecord>&data);

protected:
  void Reset(void);  

protected:
  typedef TSmapsRecord                RecordType;
  typedef std::vector< RecordType >   RecordVectorType;

  bool                m_HeapRecordFound;  

  /** contains all the segment records */
  RecordVectorType    m_Records;
};


/** \class SmapsFileParser 
 *  \brief Read a smap file (typically located in /proc/PID/smaps) and extract the 
 *  memory usage information. Any smaps data reader can be used in template as 
 *  long as they implement a operator>>(istream&) and have GetXXXUsage() methods.
*/
template<class TSmapsData>
class ITK_EXPORT SmapsFileParser
{
public:
  typedef typename TSmapsData::MemoryLoadType MemoryLoadType;

  /** Load and parse the smaps file pointed by smapsFileLocation.
   *  If smapsFileLocation is empty, load the file located at 
   *  "/proc/" + PID + "/smaps" 
   *  Throw an exception is the file can't be opened.
   */
  void ReadFile( const std::string &smapsFileLocation = "");

  /** ReRead the last parsed file to refresh the memory usage.
   *  Returns false if no smaps file has been previously read.
  */
  bool Update(void);
  
  /** Returns the heap usage in kB of the process. 
   *  If no file has been loaded yet, load a default file.
  */
  MemoryLoadType GetHeapUsage();
  /** Returns the heap usage in kB of the process. 
   *  If no file has been loaded yet, load a default file.
  */
  MemoryLoadType GetStackUsage();
  /** Returns the memory usage in kB of a process segment. 
   *  If no file has been loaded yet, load a default file.
  */
  MemoryLoadType GetMemoryUsage( const char* filter , const char * token = "Size" );
  /** Returns the total memory usage in kB of a process. 
   *  If no file has been loaded yet, load a default file.
  */
  MemoryLoadType GetTotalMemoryUsage();

protected:
  std::string   m_SmapsFilePath;  //< location of the last loaded smaps file  
  TSmapsData    m_SmapsData;      //< data of the loaded smap file
};

}  // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSmapsFileParser.txx"
#endif

#endif // __itkSmapsFileParser_h
