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
 
#ifndef _LOG4CXX_APPENDER_H
#define _LOG4CXX_APPENDER_H

#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/object.h>
#include <vector>
#include <log4cxx/spi/optionhandler.h>

namespace log4cxx
{
    // Forward declarations
    namespace spi
  {
        class LoggingEvent;
        typedef helpers::ObjectPtrT<LoggingEvent> LoggingEventPtr;
       
        class Filter;
        typedef helpers::ObjectPtrT<Filter> FilterPtr;

        class ErrorHandler;
    typedef log4cxx::helpers::ObjectPtrT<ErrorHandler> ErrorHandlerPtr;
    }

    class Layout;
    typedef log4cxx::helpers::ObjectPtrT<Layout> LayoutPtr;

    class Appender;
    typedef log4cxx::helpers::ObjectPtrT<Appender> AppenderPtr;
    typedef std::vector<AppenderPtr> AppenderList;

  /**
  Implement this interface for your own strategies for outputting log
  statements.
  */
    class LOG4CXX_EXPORT Appender :
    public virtual spi::OptionHandler
    {
    public:
    DECLARE_ABSTRACT_LOG4CXX_OBJECT(Appender)
    virtual ~Appender() {}
    
        /**
         Add a filter to the end of the filter list.
        */
        virtual void addFilter(const spi::FilterPtr& newFilter) = 0;

        /**
         Returns the head Filter. The Filters are organized in a linked list
         and so all Filters on this Appender are available through the result.

         @return the head Filter or null, if no Filters are present
         */
        virtual const spi::FilterPtr& getFilter() const = 0;

        /**
         Clear the list of filters by removing all the filters in it.
        */
        virtual void clearFilters() = 0;

        /**
         Release any resources allocated within the appender such as file
         handles, network connections, etc.
         <p>It is a programming error to append to a closed appender.
        */
        virtual void close() = 0;

        /**
         Log in <code>Appender</code> specific way. When appropriate,
         Loggers will call the <code>doAppend</code> method of appender
         implementations in order to log.
        */
        virtual void doAppend(const spi::LoggingEventPtr& event) = 0;


        /**
         Get the name of this appender. The name uniquely identifies the
         appender.
        */
        virtual const String& getName() const = 0;


        /**
         Set the {@link spi::ErrorHandler ErrorHandler} for this appender.
        */
        virtual void setErrorHandler(const spi::ErrorHandlerPtr& errorHandler) = 0;

        /**
         Returns the {@link spi::ErrorHandler ErrorHandler} for this appender.
       */
       virtual const spi::ErrorHandlerPtr& getErrorHandler() const = 0;

       /**
        Set the Layout for this appender.
       */
       virtual void setLayout(const LayoutPtr& layout) = 0;

       /**
        Returns this appenders layout.
       */
       virtual const LayoutPtr& getLayout() const = 0;


       /**
        Set the name of this appender. The name is used by other
        components to identify this appender.
       */
       virtual void setName(const String& name) = 0;

       /**
        Configurators call this method to determine if the appender
        requires a layout. If this method returns <code>true</code>,
        meaning that layout is required, then the configurator will
        configure an layout using the configuration information at its
        disposal.  If this method returns <code>false</code>, meaning that
        a layout is not required, then layout configuration will be
        skipped even if there is available layout configuration
        information at the disposal of the configurator..

        <p>In the rather exceptional case, where the appender
        implementation admits a layout but can also work without it, then
        the appender should return <code>true</code>.
       */    
       virtual bool requiresLayout() const = 0;
   };
} 

#endif //_LOG4CXX_APPENDER_H
