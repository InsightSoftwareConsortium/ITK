/*
 * Copyright 2003,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
#include <log4cxx/helpers/socketinputstream.h>
#include <log4cxx/helpers/socket.h>
#include <log4cxx/helpers/loglog.h>

using namespace log4cxx;
using namespace log4cxx::helpers ;

IMPLEMENT_LOG4CXX_OBJECT(SocketInputStream)

size_t SocketInputStream::DEFAULT_BUFFER_SIZE = 32;

SocketInputStream::SocketInputStream(SocketPtr socket)
: socket(socket), bufferSize(DEFAULT_BUFFER_SIZE),
currentPos(0), maxPos(0)
{
//  memBuffer = new unsigned char[bufferSize];
}

SocketInputStream::SocketInputStream(SocketPtr socket, size_t bufferSize)
: socket(socket), bufferSize(bufferSize),
currentPos(0), maxPos(0)
{
//  memBuffer = new unsigned char[bufferSize];
}

SocketInputStream::~SocketInputStream()
{
//  delete [] memBuffer;
}

void SocketInputStream::read(void * buf, size_t len) const
{
  size_t read = socket->read(buf, len);

  if (read == 0)
  {
    throw EOFException();
  }

/*
//  LOGLOG_DEBUG(_T("SocketInputStream reading ") << len << _T(" bytes"));
  unsigned char * dstBuffer = (unsigned char *)buf;
  
  if (len <= maxPos - currentPos)
  {
//    LOGLOG_DEBUG(_T("SocketInputStream using cache buffer, currentPos=")
//      << currentPos << _T(", maxPos=") << maxPos);
    memcpy(dstBuffer, memBuffer + currentPos, len);
    currentPos += len;
  }
  else
  {
//    LOGLOG_DEBUG(_T("SocketInputStream cache buffer too small"));

//    LOGLOG_DEBUG(_T("tmpBuffer=alloca(")
//      << len - maxPos + currentPos + bufferSize << _T(")"));
      
    unsigned char * tmpBuffer
       = (unsigned char *) alloca(len - maxPos + currentPos + bufferSize);

    size_t read = socket->read(tmpBuffer, len - maxPos + currentPos + bufferSize);

    if (read == 0)
    {
      throw EOFException();
    }

//    LOGLOG_DEBUG(_T("SocketInputStream currentPos:") << currentPos
//      << _T(", maxPos:") << maxPos << _T(", read:") << read);

    if (maxPos - currentPos > 0)
    {
//      LOGLOG_DEBUG(_T("memcpy(dstBuffer, membuffer+") << currentPos
//        << _T(",") << maxPos << _T("-") << currentPos << _T(")"));
      memcpy(dstBuffer, memBuffer + currentPos, maxPos - currentPos);
    }

    if (read <= len - maxPos + currentPos)
    {
//      LOGLOG_DEBUG(_T("SocketInputStream read <= len - maxPos + currentPos"));

//      LOGLOG_DEBUG(_T("memcpy(dstBuffer+") << maxPos - currentPos
//        << _T(",tmpBuffer,") << read << _T(")"));
      memcpy(dstBuffer + maxPos - currentPos, tmpBuffer, read);
      currentPos = 0;
      maxPos = 0;
    }
    else
    {
//      LOGLOG_DEBUG(_T("memcpy(dstBuffer+") << maxPos - currentPos
//        << _T(",tmpBuffer,") << len - maxPos + currentPos << _T(")"));
      memcpy(dstBuffer + maxPos - currentPos, tmpBuffer, len - maxPos + currentPos);

//      LOGLOG_DEBUG(_T("memcpy(memBuffer,tmpBuffer+")
//        << len - maxPos + currentPos
//        << _T(",") << read - len + maxPos - currentPos << _T(")"));
      memcpy(memBuffer,
        tmpBuffer + len - maxPos + currentPos,
        read - len + maxPos - currentPos);

//      LOGLOG_DEBUG(_T("maxPos=") << read - len + maxPos - currentPos);
      maxPos = read - len + maxPos - currentPos;
      currentPos = 0;
    }
  }
*/
}

void SocketInputStream::read(unsigned int& value) const
{
  read(&value, sizeof(value));
//  LOGLOG_DEBUG(_T("unsigned int read:") << value);
}

void SocketInputStream::read(int& value) const
{
  read(&value, sizeof(value));
//  LOGLOG_DEBUG(_T("int read:") << value);
}

void SocketInputStream::read(unsigned long& value) const
{
  read(&value, sizeof(value));
//  LOGLOG_DEBUG(_T("unsigned long read:") << value);
}

void SocketInputStream::read(long& value) const
{
  read(&value, sizeof(value));
//  LOGLOG_DEBUG(_T("long read:") << value);
}

void SocketInputStream::read(String& value) const
{
  String::size_type size = 0;

  read(&size, sizeof(String::size_type));
//  LOGLOG_DEBUG(_T("string size read:") << size);

  if (size > 0)
  {
    if (size > 1024)
    {
      throw SocketException();
    }
    
    TCHAR * buffer;
    buffer = (TCHAR *)alloca((size + 1)* sizeof(TCHAR));
    buffer[size] = _T('\0');
    read(buffer, size * sizeof(TCHAR));
    value = buffer;
  }
  
//  LOGLOG_DEBUG(_T("string read:") << value);
}

void SocketInputStream::close()
{
  // seek to begin
  currentPos = 0;

  // dereference socket
  socket = 0;
}
