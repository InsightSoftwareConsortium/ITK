/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIPLCommonImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/
#include "itkIPLCommonImageIO.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkGEImageHeader.h"
#include "idbm_hdr_def.h"
#include "itkDirectory.h"
#include <cstdio>
#include <iostream>
#include <fstream>
#include <string.h>
#include <limits.h>
#include <stdlib.h>
#include <vector>

//From uiig library "The University of Iowa Imaging Group-UIIG"

namespace itk 
{
  // Default constructor
  IPLCommonImageIO::IPLCommonImageIO()
  {
    m_system_byteOrder = ByteSwapper<int>::SystemIsBigEndian() ? ImageIOBase::BigEndian :
      ImageIOBase::LittleEndian;
    memset(&m_fnlist,0,sizeof(m_fnlist));
  }

  IPLCommonImageIO::~IPLCommonImageIO()
  {
    delete m_ImageHeader;
  }

  void IPLCommonImageIO::PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
  }

  bool IPLCommonImageIO::CanWriteFile(const char * FileNameToWrite)
  {
    
    return false;
  }

  const std::type_info& IPLCommonImageIO::GetPixelType() const
  {
    return typeid(S16);
  }

  unsigned int IPLCommonImageIO::GetComponentSize() const
  {
    return sizeof(S16);
  }
  void IPLCommonImageIO::Read(void* buffer)
  {
    int i;
    S16 *img_buffer = (S16 *)buffer;
    for(i = 0; i < m_fnlist.numImageInfoStructs; i++) 
      {
  std::ifstream f(m_fnlist.Info[i].imageFileName,std::ifstream::binary);
  if(!f.is_open())
    RAISE_EXCEPTION();
  f.seekg (m_fnlist.Info[i].SliceOffset, std::ios::beg);
  f.read((char *)img_buffer, m_fnlist.XDim * m_fnlist.YDim * sizeof(S16));
  IOCHECK();
  f.close();
  // ByteSwapper::SwapRangeFromSystemToBigEndian is set up based on
  // the FILE endian-ness, not as the name would lead you to believe.
  // So, on LittleEndian systems, SwapFromSystemToBigEndian will swap.
  // On BigEndian systems, SwapFromSystemToBigEndian will do nothing.
  itk::ByteSwapper<S16>::SwapRangeFromSystemToBigEndian(img_buffer,m_fnlist.XDim*m_fnlist.YDim);
  img_buffer += m_fnlist.XDim * m_fnlist.YDim;
      }
#if 0 // Debugging
      std::ofstream f2("test.img",std::ofstream::binary);
      f2.write(buffer,(m_fnlist.numImageInfoStructs *
           m_fnlist.XDim * m_fnlist.YDim * sizeof(S16)));
      f2.close();
#endif
  }
  struct GEImageHeader *IPLCommonImageIO::ReadHeader(const char *FileNameToRead)
  {
    //
    // must be redefined in a child class
    return 0;
  }

  bool IPLCommonImageIO::CanReadFile( const char* FileNameToRead )
  {
    //
    // must be redefined in child class or you'll never read anything ;-)
    return false;
  }

  void IPLCommonImageIO::ReadImageInformation()
  {
    const char *FileNameToRead = this->GetFileName();
    InitializeFILENAMELIST(&m_fnlist);
    this->m_ImageHeader = this->ReadHeader(FileNameToRead);
    //
    // if anything fails in the header read, just let
    // exceptions propogate up.
    AddElementToList(&m_fnlist,m_ImageHeader->filename,
         m_ImageHeader->sliceLocation,
         m_ImageHeader->offset,
         m_ImageHeader->imageXsize,
         m_ImageHeader->imageYsize,
         m_ImageHeader->seriesNumber,
         m_ImageHeader->echoNumber);
    
     
    //
    // GE images are stored in separate files per slice.
    char imagePath[MAXPATHLEN+1];
    char imageMask[MAXPATHLEN+1];
    if(realpath(FileNameToRead,imagePath) == NULL)
      RAISE_EXCEPTION();
    strcpy(imageMask,imagePath);
    char *lastslash = strrchr(imagePath,'/');
    if(lastslash == NULL)
      {
      strcpy(imagePath,".");
      }
    else
      {
      *lastslash = '\0';
      }
    itk::Directory::Pointer dir = itk::Directory::New();
    if(dir->Load(imagePath) == 0)
      RAISE_EXCEPTION();
    std::vector<std::string>::size_type i;
    std::vector<std::string>::size_type numfiles;
    
    struct GEImageHeader *curImageHeader;

    for(i = 0, numfiles = dir->GetNumberOfFiles(); i < numfiles; i++) 
      {
  const char *curFname =  dir->GetFile(i);
  if(curFname == 0)
    {
      break;
    }
  else if (strcmp(curFname,FileNameToRead) == 0)
    {
      continue;
    }
  char fullPath[MAXPATHLEN+1];
  sprintf(fullPath,"%s/%s",imagePath,curFname);
  try 
    {
      curImageHeader = this->ReadHeader(fullPath);
    }
  catch (itk::ExceptionObject e)
    {
      // ReadGE4XHeader throws an exception on any error.
      // So if, for example we run into a subdirectory, it would
      // throw an exception, and we'd just want to skip it.
      continue;
    }
  if(curImageHeader->echoNumber == m_fnlist.Key2 &&
     curImageHeader->seriesNumber == m_fnlist.Key1)
    {
      AddElementToList(&m_fnlist,curImageHeader->filename,
           curImageHeader->sliceLocation,
           curImageHeader->offset,
           curImageHeader->imageXsize,
           curImageHeader->imageYsize,
           curImageHeader->seriesNumber,
           curImageHeader->echoNumber);
    }
  delete curImageHeader;
  curImageHeader = 0;
      }
    switch(m_ImageHeader->imagePlane)
      {
      case AXIAL:  //Axial needs to descend
  sortImageListDescend (&m_fnlist);
  break;
      case CORONAL: //Fall through and ascend
      case SAGITTAL:
  sortImageListAscend (&m_fnlist);
  break;
      default:
  assert( 0==1 );
  break;
      }
    //
    //
    // set the image properties
    this->SetNumberOfDimensions(3);
    this->SetDimensions(0,m_ImageHeader->imageXsize);
    this->SetDimensions(1,m_ImageHeader->imageYsize);
    this->SetDimensions(2,m_fnlist.numImageInfoStructs);
    this->SetSpacing(0, m_ImageHeader->imageXres);
    this->SetSpacing(1, m_ImageHeader->imageYres);
    this->SetSpacing(2, m_ImageHeader->sliceThickness);
    
  }



  /**
   *
   */
  void
  IPLCommonImageIO
  ::WriteImageInformation(void)
  {
    RAISE_EXCEPTION();
  }


  /**
   *
   */
  void
  IPLCommonImageIO
  ::Write( const void* buffer)
  {
    RAISE_EXCEPTION();
  }

  int
  IPLCommonImageIO
  ::GetStringAt(std::ifstream &f,
       std::streamoff Offset,
       char *buf,
       size_t amount,bool throw_exception)
  {
    f.seekg(Offset,std::ios::beg);
    if(f.fail()) 
      { 
  if(throw_exception)
    {
      RAISE_EXCEPTION(); 
    }
  else
    {
      return -1;
    }
      }
    f.read(buf,amount);
    if(f.fail()) 
      { 
  if(throw_exception)
    {
      RAISE_EXCEPTION(); 
    }
  else
    {
      return -1;
    }
      }
    return 0;
  }
  int IPLCommonImageIO
  ::GetIntAt(std::ifstream &f,std::streamoff Offset,int *ip,
       bool throw_exception)
  {
    int tmp;
    this->GetStringAt(f,Offset,(char *)&tmp,sizeof(int));
    *ip = this->hdr2Int((char *)&tmp);
    return 0;
  }
  int IPLCommonImageIO
  ::GetShortAt(std::ifstream &f,std::streamoff Offset,short *ip,
       bool throw_exception )
  {
    short tmp;
    this->GetStringAt(f,Offset,(char *)&tmp,sizeof(short));
    *ip = this->hdr2Short((char *)&tmp);
    return 0;
  }
  int IPLCommonImageIO
  ::GetFloatAt(std::ifstream &f,std::streamoff Offset,float *ip,
       bool throw_exception )
  {
    float tmp;
    this->GetStringAt(f,Offset,(char *)&tmp,sizeof(float));
    *ip = this->hdr2Float((char *)&tmp);
    return 0;
  }
  short IPLCommonImageIO
  ::hdr2Short (char *hdr)
  {
    short shortValue;
    memcpy (&shortValue, hdr, sizeof(short));
    ByteSwapper<short int>::SwapFromSystemToBigEndian (&shortValue);
    return (shortValue);
  }

  int IPLCommonImageIO
  ::hdr2Int (char *hdr)
  {
    int intValue;

    memcpy (&intValue, hdr, sizeof(int));
    ByteSwapper< int>::SwapFromSystemToBigEndian (&intValue);
    return (intValue);
  }

  float IPLCommonImageIO
  ::hdr2Float (char *hdr)
  {
    float floatValue;

    memcpy (&floatValue, hdr, 4);
    ByteSwapper<float>::SwapFromSystemToBigEndian (&floatValue);

    return (floatValue);
  }
  void IPLCommonImageIO
  ::InitializeFILENAMELIST( FILENAMELIST * const fnList )
  {
    memset(fnList,0,sizeof(FILENAMELIST));
  }

  int IPLCommonImageIO
  ::AddElementToList(FILENAMELIST * const fnList,char const * const filename, const float sliceLocation, const int offset, const int XDim, const int YDim, const int Key1, const int Key2 )
  {
    if(fnList->numImageInfoStructs == 0)
      {
        fnList->XDim = XDim;
        fnList->YDim = YDim;
        fnList->Key1 = Key1;
        fnList->Key2 = Key2;
      }
    else if(XDim != fnList->XDim || YDim != fnList->YDim  )
      {
        return 0;
      }
    else if (fnList->Key1 != Key1 ||  fnList->Key2 != Key2)
      {
        return 1;  //It is OK for keys to not match,  Just don't add.
      }
    fnList->Info[fnList->numImageInfoStructs].SliceLocation = sliceLocation;
    fnList->Info[fnList->numImageInfoStructs].echoNumber = 0;
    fnList->Info[fnList->numImageInfoStructs].SliceOffset = offset;
    strncpy ( fnList->Info[fnList->numImageInfoStructs].imageFileName,filename, MAXPATHLEN+1);
    fnList->numImageInfoStructs++;
    assert(fnList->numImageInfoStructs< MAX_FILENAMELIST_SIZE);
    return 1;
  }
  /**
   * \author Hans J. Johnson
   * \brief This function is the comparitor to qsort to determine if the slice is greater or less than
   * the desired value, and returns so that the qsort call will sort in ascending order
   * \return See qsort for valid return values.
   */
  static int qsort_FILESORTINFO_ascend_compar( const void * item1, const void * item2 )
  {
    float sliceGap;
    
    if (((IPLCommonImageIO::FILESORTINFO const * )item1)->echoNumber > ((IPLCommonImageIO::FILESORTINFO const * )item2)->echoNumber)
      {
  return 1;
      }
    else if (((IPLCommonImageIO::FILESORTINFO const * )item1)->echoNumber < ((IPLCommonImageIO::FILESORTINFO const * )item2)->echoNumber)
      {
  return -1;
      }
    else
      {
  sliceGap = ((IPLCommonImageIO::FILESORTINFO const * )item1)->SliceLocation - ((IPLCommonImageIO::FILESORTINFO const * )item2)->SliceLocation;
  if (sliceGap < 0.0) 
    {
      return -1;
    }
  else if (sliceGap > 0.0) 
    {
      return 1;
    }
  else 
    {
      return 0;
    }
      }
    
  }

  /**
   * \author Hans J. Johnson
   * \brief This function is the comparitor to qsort to determine if the slice is greater or less than
   * the desired value, and returns so that the qsort call will sort in descending order
   * \return See qsort for valid return values.
   */
  static int qsort_FILESORTINFO_descend_compar( const void * item1, const void * item2 )
  {
    float sliceGap;
    
    if (((IPLCommonImageIO::FILESORTINFO const * )item1)->echoNumber > ((IPLCommonImageIO::FILESORTINFO const * )item2)->echoNumber)
      {
  return 1;
      }
    else if (((IPLCommonImageIO::FILESORTINFO const * )item1)->echoNumber < ((IPLCommonImageIO::FILESORTINFO const * )item2)->echoNumber)
      {
  return -1;
      }
    else
      {
  sliceGap = ((IPLCommonImageIO::FILESORTINFO const * )item1)->SliceLocation - ((IPLCommonImageIO::FILESORTINFO const * )item2)->SliceLocation;
  if (sliceGap < 0.0) 
    {
      return 1;
    }
  else if (sliceGap > 0.0) 
    {
      return -1;
    }
  else 
    {
      return 0;
    }
      }
  }
  void IPLCommonImageIO
  ::sortImageListAscend (FILENAMELIST * const fnList)
  {
    qsort(fnList->Info,fnList->numImageInfoStructs,sizeof(FILESORTINFO),qsort_FILESORTINFO_ascend_compar);
    return;
  }
  void IPLCommonImageIO
  ::sortImageListDescend (FILENAMELIST * const fnList)
  {
    qsort(fnList->Info,fnList->numImageInfoStructs,sizeof(FILESORTINFO),qsort_FILESORTINFO_descend_compar);
    return;
  }
  int IPLCommonImageIO
  ::statTimeToAscii (void *clock, char *timeString)
  {
    char *asciiTime;
    unsigned int i;
#ifdef SGI
    timespec_t *lclock;
#else

#endif

#ifdef SGI
    lclock = (timespec_t *) clock;
    asciiTime = ctime (&(lclock->tv_sec));
#else
    asciiTime = ctime ((time_t *) (clock));
#endif

    strncpy (timeString, asciiTime, 64);

    for (i = 0; i < 26; i++)
      {
  if (timeString[i] == '\n')
    timeString[i] = '\0';
      }

    return 1;

  }


} // end namespace itk
