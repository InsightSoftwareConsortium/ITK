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
 
#include <log4cxx/helpers/socketoutputstream.h>
#include <log4cxx/helpers/socket.h>
#include <log4cxx/helpers/loglog.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(SocketOutputStream)

#define INCREMENT 512

SocketOutputStream::SocketOutputStream(SocketPtr socket)
: socket(socket), beg(0), cur(0), end(0)
{
}

SocketOutputStream::~SocketOutputStream()
{
  delete [] beg;
}

void SocketOutputStream::write(const void * buffer, size_t len)
{
//  LOGLOG_DEBUG (_T("SocketOutputStream writing ") << len << _T(" bytes."));
  if (cur + len > end)
  {
    if (beg == 0)
    {
      size_t size = ((len > INCREMENT) ? len : INCREMENT);
//      LOGLOG_DEBUG (_T("SocketOutputStream growing ") << size << _T(" bytes."));
      beg = new unsigned char[size];
      end = beg + size;
      cur = beg;
    }
    else
    {
      size_t size = end - beg + ((len > INCREMENT) ? len : INCREMENT);
      unsigned char * old = beg;
//      LOGLOG_DEBUG (_T("SocketOutputStream growing ") <<
//        ((len > INCREMENT) ? len : INCREMENT) << _T(" bytes."));
      beg = new unsigned char[size];
      memcpy(beg, old, cur - old);
      cur = beg + (cur - old);
      end = beg + size;
      delete [] old;
    }    
  }

  memcpy(cur, buffer, len);
  cur+= len;
}

void SocketOutputStream::write(unsigned int value)
{
  write(&value, sizeof(value));
}

void SocketOutputStream::write(int value)
{
  write(&value, sizeof(value));
}

void SocketOutputStream::write(unsigned long value)
{
  write(&value, sizeof(value));
}

void SocketOutputStream::write(long value)
{
  write(&value, sizeof(value));
}

void SocketOutputStream::write(const String& value)
{
  String::size_type size;

  size = value.size();
  write(&size, sizeof(String::size_type));
  if (size > 0)
  {
    if (size > 1024)
    {
      size = 1024;
    }
    
    write(value.c_str(), size * sizeof(TCHAR));
  }
}

void SocketOutputStream::close()
{
  // force flushing
  flush();

  // dereference socket
  socket = 0;
}

void SocketOutputStream::flush()
{
  if (cur != beg)
  {
    // write to socket
    socket->write(beg, cur - beg);

    // seek to begin
    cur = beg;
  }
}
