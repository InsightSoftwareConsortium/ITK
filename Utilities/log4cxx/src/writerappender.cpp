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
 
#include <log4cxx/writerappender.h>
#include <log4cxx/helpers/loglog.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(WriterAppender)

WriterAppender::WriterAppender()
: immediateFlush(true), os(0)
{
}

WriterAppender::WriterAppender(const LayoutPtr& layout, ostream * os)
: immediateFlush(true), os(os)
{
  this->layout = layout;
}

WriterAppender::~WriterAppender()
{
}

void WriterAppender::append(const spi::LoggingEventPtr& event)
{

// Reminder: the nesting of calls is:
//
//    doAppend()
//      - check threshold


//      - filter
//      - append();
//        - checkEntryConditions();

//        - subAppend();

  if(!checkEntryConditions())
  {
    return;
  }

  subAppend(event);
}

bool WriterAppender::checkEntryConditions() const
{
  if(closed)
  {
    LogLog::warn(_T("Not allowed to write to a closed appender."));
    return false;
  }

  if(os == 0)
  {
    errorHandler->error(
      _T("No output stream or file set for the appender named [")
      + name+ _T("]."));
    return false;
  }

  if(layout == 0)
  {
    errorHandler->error(
      _T("No layout set for the appender named [")
      + name+_T("]."));
    return false;
  }

  return true;
}

void WriterAppender::close()
{
  synchronized sync(this);
  
  if(closed)
  {
    return;
  }

  closed = true;
  writeFooter();
  reset();
}

void WriterAppender::subAppend(const spi::LoggingEventPtr& event)
{
  layout->format(*os, event);

  if(immediateFlush)
  {
    os->flush();
  }
}

void WriterAppender::reset()
{
  closeWriter();
  os = 0;
}

void WriterAppender::writeFooter()
{
  if(layout != 0)
  {
    if(os != 0)
    {
      layout->appendFooter(*os);
      os->flush();
    }
  }
}

void WriterAppender::writeHeader()
{
  if(layout != 0)
  {
    if(os != 0)
    {
      layout->appendHeader(*os);
    }
  }
}
