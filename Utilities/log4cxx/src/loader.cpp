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
 
#include <log4cxx/helpers/loader.h>
#include <log4cxx/appender.h>
#include <log4cxx/spi/filter.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/spi/loggerfactory.h>
#include <log4cxx/spi/loggerrepository.h>
#include <log4cxx/helpers/object.h>
#include <log4cxx/spi/errorhandler.h>
#include <log4cxx/varia/denyallfilter.h>
#include <log4cxx/spi/repositoryselector.h>
#include <log4cxx/spi/appenderattachable.h>
#include <log4cxx/helpers/xml.h>
#include <log4cxx/spi/triggeringeventevaluator.h>
#include <sys/stat.h>
#include <fstream>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;
using namespace log4cxx::varia;

IMPLEMENT_LOG4CXX_OBJECT(Object)
IMPLEMENT_LOG4CXX_OBJECT(OptionHandler)
IMPLEMENT_LOG4CXX_OBJECT(ErrorHandler)
IMPLEMENT_LOG4CXX_OBJECT(Appender)
IMPLEMENT_LOG4CXX_OBJECT(Filter)
IMPLEMENT_LOG4CXX_OBJECT(AppenderAttachable)
IMPLEMENT_LOG4CXX_OBJECT(LoggerFactory)
IMPLEMENT_LOG4CXX_OBJECT(LoggerRepository)
IMPLEMENT_LOG4CXX_OBJECT(DenyAllFilter)
IMPLEMENT_LOG4CXX_OBJECT(RepositorySelector)
IMPLEMENT_LOG4CXX_OBJECT(XMLDOMNode)
IMPLEMENT_LOG4CXX_OBJECT(XMLDOMDocument)
IMPLEMENT_LOG4CXX_OBJECT(XMLDOMElement)
IMPLEMENT_LOG4CXX_OBJECT(XMLDOMNodeList)
IMPLEMENT_LOG4CXX_OBJECT(TriggeringEventEvaluator)

const Class& Loader::loadClass(const String& clazz)
{
  return Class::forName(clazz);
}

String Loader::getResource(const String& name)
{
  String path;
  struct stat buff;
  USES_CONVERSION;
  if (stat(T2A(name.c_str()), &buff) == 0)
  {
    path = name;
  }

  return path;
}

istream * Loader::getResourceAsStream(const String& name)
{
  String path = getResource(name);
  if (path.empty())
  {
    return 0;
  }

#ifdef UNICODE
    std::wifstream * stream = new std::wifstream();
#else
    std::ifstream * stream = new std::ifstream();
#endif

  USES_CONVERSION;
  stream->open(T2A(name.c_str()));
  if (stream->fail())
  {
    delete stream;
    return 0;
  }

  return stream;
}
