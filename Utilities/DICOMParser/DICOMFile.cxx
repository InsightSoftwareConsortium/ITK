
#ifdef _MSC_VER
#pragma warning ( disable : 4514 )
#pragma warning ( disable : 4710 )
#pragma warning ( push, 3 )
#endif 

#include <iostream>
#include <fstream>
#include <iomanip>
#include <stdio.h>
#include <string>

#include "DICOMConfig.h"
#include "DICOMFile.h"

DICOMFile::DICOMFile() : InputStream()
{
  /* Are we little or big endian?  From Harbison&Steele.  */
  union
  {
    long l;
    char c[sizeof (long)];
  } u;
  u.l = 1;
  PlatformIsBigEndian = (u.c[sizeof (long) - 1] == 1);
  if (PlatformIsBigEndian)
    {
    PlatformEndian = "BigEndian";
    }
  else
    {
    PlatformEndian = "LittleEndian";
    }
}

DICOMFile::~DICOMFile()
{
  this->Close();
}

DICOMFile::DICOMFile(const DICOMFile& in)
{
  if (strcmp(in.PlatformEndian, "LittleEndian") == 0)
    {
    PlatformEndian = "LittleEndian";
    }
  else
    {
    PlatformEndian = "BigEndian";
    }
  //
  // Some compilers can't handle. Comment out for now.
  //
  // InputStream = in.InputStream;
}

void DICOMFile::operator=(const DICOMFile& in)
{
  if (strcmp(in.PlatformEndian, "LittleEndian") == 0)
    {
    PlatformEndian = "LittleEndian";
    }
  else
    {
    PlatformEndian = "BigEndian";
    }
  //
  // Some compilers can't handle. Comment out for now.
  //
  // InputStream = in.InputStream;
}

bool DICOMFile::Open(const dicomstd::string& filename)
{
  InputStream.open(filename.c_str(), dicomstd::ios::binary | dicomstd::ios::in);

  if (InputStream.is_open())
    {
    return true;
    }
  else
    {
    return false;
    }
}

void DICOMFile::Close()
{
  InputStream.close();
}

long DICOMFile::Tell() 
{
  long loc = InputStream.tellg();
  // dicomstd::cout << "Tell: " << loc << dicomstd::endl;
  return loc;
}

void DICOMFile::SkipToPos(long increment) 
{
  InputStream.seekg(increment, dicomstd::ios::beg);
}

long DICOMFile::GetSize() 
{
  long curpos = this->Tell();

  InputStream.seekg(0,dicomstd::ios::end);

  long size = this->Tell();
  // dicomstd::cout << "Tell says size is: " << size << dicomstd::endl;
  this->SkipToPos(curpos);

  return size;
}

void DICOMFile::Skip(long increment) 
{
  InputStream.seekg(increment, dicomstd::ios::cur);
}

void DICOMFile::SkipToStart() 
{
  InputStream.seekg(0, dicomstd::ios::beg);
}

void DICOMFile::Read(void* ptr, long nbytes) 
{
  InputStream.read((char*)ptr, nbytes);
  // dicomstd::cout << (char*) ptr << dicomstd::endl;
}

doublebyte DICOMFile::ReadDoubleByte() 
{
  doublebyte sh = 0;
  int sz = sizeof(doublebyte);
  this->Read((char*)&(sh),sz); 
  if (PlatformIsBigEndian) 
    {
    sh = swapShort(sh);
    }
  return(sh);
}

doublebyte DICOMFile::ReadDoubleByteAsLittleEndian() 
{
  doublebyte sh = 0;
  int sz = sizeof(doublebyte);
  this->Read((char*)&(sh),sz); 
  if (PlatformIsBigEndian)
    {
    sh = swapShort(sh);
    }
  return(sh);
}

quadbyte DICOMFile::ReadQuadByte() 
{
  quadbyte sh;
  int sz = sizeof(quadbyte);
  this->Read((char*)&(sh),sz);
  if (PlatformIsBigEndian) 
    {
    sh = swapLong(sh);
    }
  return(sh);
}

quadbyte DICOMFile::ReadNBytes(int len) 
{
  quadbyte ret = -1;
  switch (len) 
    {
    case 1:
      char ch;
      this->Read(&ch,1);  //from Image
      ret =(quadbyte) ch;
      break;
    case 2:
      ret =(quadbyte) ReadDoubleByte();
      break;
    case 4:
      ret = ReadQuadByte();
      break;
    default:
      dicomstd::cerr << "Unable to read " << len << " bytes" << dicomstd::endl;
      break;
    }
  return (ret);
}

float DICOMFile::ReadAsciiFloat(int len) 
{
  float ret=0.0;


  char* val = new char[len+1];
  this->Read(val,len);
  val[len] = '\0';

#if 0
  //
  // istrstream destroys the data during formatted input.
  //
  int len2 = static_cast<int> (strlen((char*) val));
  char* val2 = new char[len2];
  strncpy(val2, (char*) val, len2);

  dicomstd::istrstream data(val2);
  data >> ret;
  delete [] val2;
#else
  sscanf(val,"%e",&ret);
#endif

  dicomstd::cout << "Read ASCII float: " << ret << dicomstd::endl;

  delete [] val;
  return (ret);
}

int DICOMFile::ReadAsciiInt(int len) 
{
  int ret=0;

  char* val = new char[len+1];
  this->Read(val,len);
  val[len] = '\0';

#if 0
  //
  // istrstream destroys the data during formatted input.
  //
  int len2 = static_cast<int> (strlen((char*) val));
  char* val2 = new char[len2];
  strncpy(val2, (char*) val, len2);

  dicomstd::istrstream data(val2);
  data >> ret;
  delete [] val2;
#else
  sscanf(val,"%d",&ret);
#endif

  dicomstd::cout << "Read ASCII int: " << ret << dicomstd::endl;

  delete [] val;
  return (ret);
}

char* DICOMFile::ReadAsciiCharArray(int len) 
{
  if (len <= 0)
    {
    return NULL;
    }
  char* val = new char[len + 1];
  this->Read(val, len);
  val[len] = 0; // NULL terminate.
  return val;
}

#ifdef _MSC_VER
#pragma warning ( pop )
#endif
