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
 
#ifndef _LOG4CXX_SPI_LOGGERFACTORY_H
#define _LOG4CXX_SPI_LOGGERFACTORY_H

#include <log4cxx/helpers/object.h>
#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/tchar.h>

namespace log4cxx
{
  class Logger;
  typedef helpers::ObjectPtrT<Logger> LoggerPtr;

  namespace spi
  {
    /**
    Implement this interface to create new instances of Logger or
    a sub-class of Logger.
    */
    class LOG4CXX_EXPORT LoggerFactory : public virtual helpers::Object
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(LoggerFactory)
      virtual ~LoggerFactory() {}
      virtual LoggerPtr makeNewLoggerInstance(const String& name) = 0;
    };

    typedef helpers::ObjectPtrT<LoggerFactory> LoggerFactoryPtr;

  }  // namespace spi
}; // namesapce log4cxx

#endif //_LOG4CXX_SPI_LOGGERFACTORY_H
