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
 
#ifndef _LOG4CXX_SPI_CONFIGURATOR_H
#define _LOG4CXX_SPI_CONFIGURATOR_H

#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/object.h>
#include <log4cxx/helpers/objectptr.h>

namespace log4cxx
{
  namespace spi
  {
    class LoggerRepository;
    typedef helpers::ObjectPtrT<LoggerRepository> LoggerRepositoryPtr;

    class Configurator;
    typedef helpers::ObjectPtrT<Configurator> ConfiguratorPtr;

    /**
    Implemented by classes capable of configuring log4j using a URL.
    */
    class LOG4CXX_EXPORT Configurator : virtual public helpers::Object
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(Configurator)
      /**
      Special level value signifying inherited behaviour. The current
      value of this string constant is <b>inherited</b>. #NuLL
      is a synonym.  */
      static String INHERITED /*= "inherited"*/;
      
      /**
      Special level signifying inherited behaviour, same as
      #INHERITED. The current value of this string constant is
      <b>null</b>. */
      static String NuLL /*= "null"*/;
      
      /**
      Interpret a resource pointed by a URL and set up log4j accordingly.

      The configuration is done relative to the <code>hierarchy</code>
      parameter.

      @param configFileName The file to parse
      @param repository The hierarchy to operation upon.
      */
      virtual void doConfigure(const String& configFileName, 
        spi::LoggerRepositoryPtr& repository) = 0;
    };
  } 
} 

#endif // _LOG4CXX_SPI_CONFIGURATOR_H
