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
 
#include <log4cxx/fileappender.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/optionconverter.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(FileAppender)

FileAppender::FileAppender()
: fileAppend(true), bufferedIO(false), bufferSize(8*1024)
{
}

FileAppender::FileAppender(const LayoutPtr& layout, const String& fileName,
  bool append, bool bufferedIO, int bufferSize)
: fileAppend(true), bufferedIO(false), bufferSize(8*1024)
{
  this->layout = layout;
  this->setFile(fileName, append, bufferedIO, bufferSize);
}

FileAppender::FileAppender(const LayoutPtr& layout, const String& fileName,
  bool append)
: fileAppend(true), bufferedIO(false), bufferSize(8*1024)
{
  this->layout = layout;
  this->setFile(fileName, append, false, bufferSize);
}

FileAppender::FileAppender(const LayoutPtr& layout, const String& fileName)
: fileAppend(true), bufferedIO(false), bufferSize(8*1024)
{
  this->layout = layout;
  this->setFile(fileName, true, false, bufferSize);
}

FileAppender::~FileAppender()
{
  finalize();
}

void FileAppender::setFile(const String& file)
{
  // Trim spaces from both ends. The users probably does not want
  // trailing spaces in file names.
  fileName = StringHelper::trim(file);
}

void FileAppender::setFile(const String& fileName, bool append, 
  bool bufferedIO, int bufferSize)
{
  synchronized sync(this);
  
  LOGLOG_DEBUG(_T("FileAppender::activateOptions called : ")
    << fileName << _T(", ") << append);
  // It does not make sense to have immediate flush and bufferedIO.
  if(bufferedIO)
  {
    setImmediateFlush(false);
  }

  if(ofs.is_open())
  {
    reset();
  }

/*  if (bufferedIO && bufferSize > 0)
  {
    buffer = new char[bufferSize];
    out.rdbuf()->setbuf(buffer, 0);
  }*/

  USES_CONVERSION
  ofs.open(T2A(fileName.c_str()), (append ? std::ios::app :
    std::ios::trunc)|std::ios::out);

  if(!ofs.is_open())
  {
    throw RuntimeException();
  }

  this->os = &ofs;
    this->fileName = fileName;
    this->fileAppend = append;
    this->bufferedIO = bufferedIO;
    this->bufferSize = bufferSize;
  writeHeader();
  LogLog::debug(_T("FileAppender::setFile ended"));
}

void FileAppender::closeWriter()
{
  ofs.close();
  os = 0;
}

void FileAppender::closeFile()
{
  if (os != 0)
  {
    try
    {
      closeWriter();
    }
    catch(Exception& e)
    {
      LogLog::error(_T("Could not close file ") + fileName, e);
    }
  }
}

void FileAppender::setBufferedIO(bool bufferedIO)
{
  this->bufferedIO = bufferedIO;
  if(bufferedIO)
  {
    immediateFlush = false;
  }
}

void FileAppender::setOption(const String& option,
  const String& value)
{
  if (StringHelper::equalsIgnoreCase(option, _T("file"))
    || StringHelper::equalsIgnoreCase(option, _T("filename")))
  {
    fileName = value;
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("append")))
  {
    fileAppend = OptionConverter::toBoolean(value, true);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("bufferedio")))
  {
    bufferedIO = OptionConverter::toBoolean(value, true);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("immediateflush")))
  {
    bufferedIO = !OptionConverter::toBoolean(value, false);
  }
  else if (StringHelper::equalsIgnoreCase(option, _T("buffersize")))
  {
    bufferSize = OptionConverter::toFileSize(value, 8*1024);
  }
  else
  {
    WriterAppender::setOption(name, value);
  }
}

void FileAppender::activateOptions()
{
  if (!fileName.empty())
  {
    try
    {
      setFile(fileName, fileAppend, bufferedIO, bufferSize);
    }
    catch(Exception& e)
    {
      errorHandler->error(_T("Unable to open file: ") + fileName,
      e, ErrorCode::FILE_OPEN_FAILURE);
    }
  }
  else
  {
    LogLog::warn(_T("File option not set for appender [")+name+_T("]."));
    LogLog::warn(_T("Are you using FileAppender instead of ConsoleAppender?"));
  }
}


