
#ifdef WIN32
#pragma warning(disable:4786)
#endif


#include <iostream>
#include <fstream>
#include <iomanip>
#include <strstream>
#include <string>

// #include <stdio.h>

#include "DICOMFile.h"


#ifdef WIN32
#define WORDS_LITTLEENDIAN
#endif
#ifdef __CYGWIN__
#define WORDS_LITTLEENDIAN
#endif



DICOMFile::DICOMFile() : inputStream()
{
  Filename = NULL;
#ifdef WORDS_LITTLEENDIAN
  ByteSwap = false;
#else
  ByteSwap = true;
#endif  

}

DICOMFile::~DICOMFile()
{

}

bool DICOMFile::Open(char* filename)
{
  inputStream.open(filename, std::ios::binary | std::ios::in);

  if (inputStream.is_open())
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
  inputStream.close();
}

long DICOMFile::Tell() 
{
  long loc = inputStream.tellg();
  // std::cout << "Tell: " << loc << std::endl;
  return loc;
}

void DICOMFile::SkipToPos(long increment) 
{
  inputStream.seekg(increment, std::ios::beg);
}

long DICOMFile::GetSize() 
{
  long curpos = this->Tell();

  inputStream.seekg(0,std::ios::end);

  long size = this->Tell();
  // std::cout << "Tell says size is: " << size << std::endl;
  this->SkipToPos(curpos);

  return size;
}

void DICOMFile::Skip(long increment) 
{
  inputStream.seekg(increment, std::ios::cur);
}

void DICOMFile::SkipToStart() 
{
  inputStream.seekg(0, std::ios::beg);
}

void DICOMFile::Read(void* ptr, long nbytes) 
{
  inputStream.read((char*)ptr, nbytes);
  // std::cout << (char*) ptr << std::endl;
}

doublebyte DICOMFile::ReadDoubleByte() 
{
  doublebyte sh;
  int sz = sizeof(doublebyte);
  this->Read((char*)&(sh),sz); 
  if (ByteSwap) 
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
  if (ByteSwap) 
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
      std::cerr << "Unable to read " << len << " bytes" << std::endl;
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

  //
  // istrstream destroys the data during formatted input.
  //
  int len2 = strlen((char*) val);
  char* val2 = new char[len2];
  strncpy(val2, (char*) val, len2);

  std::istrstream data(val2);
  data >> ret;

  std::cout << "Read ASCII float: " << ret << std::endl;

  //
  //sscanf(val,"%e",&ret);
  //

  delete [] val;
  return (ret);
}

int DICOMFile::ReadAsciiInt(int len) 
{
  int ret=0;

  char* val = new char[len+1];
  this->Read(val,len);
  val[len] = '\0';

  //
  // istrstream destroys the data during formatted input.
  //
  int len2 = strlen((char*) val);
  char* val2 = new char[len2];
  strncpy(val2, (char*) val, len2);

  std::istrstream data(val2);
  data >> ret;

  std::cout << "Read ASCII int: " << ret << std::endl;

  //
  //sscanf(val,"%d",&ret);
  //

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

