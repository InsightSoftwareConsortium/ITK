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
 
#ifndef _LOG4CXX_SPI_ERROR_HANDLER_H
#define _LOG4CXX_SPI_ERROR_HANDLER_H

#include <log4cxx/spi/optionhandler.h>
#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/object.h>
#include <log4cxx/helpers/exception.h>

namespace log4cxx
{
    class Appender;
    typedef log4cxx::helpers::ObjectPtrT<Appender> AppenderPtr;
    
    class Logger;
    typedef helpers::ObjectPtrT<Logger> LoggerPtr;

    namespace spi
  {
    class ErrorCode
    {
    public:
      enum
      {
        GENERIC_FAILURE = 0,
        WRITE_FAILURE = 1,
        FLUSH_FAILURE = 2,
        CLOSE_FAILURE = 3,
        FILE_OPEN_FAILURE = 4,
        MISSING_LAYOUT = 5,
        ADDRESS_PARSE_FAILURE = 6
      };
    };

    class LoggingEvent;
    typedef helpers::ObjectPtrT<LoggingEvent> LoggingEventPtr;

    class ErrorHandler;
    typedef log4cxx::helpers::ObjectPtrT<ErrorHandler> ErrorHandlerPtr;

    /**
    Appenders may delegate their error handling to
    <code>ErrorHandlers</code>.

    <p>Error handling is a particularly tedious to get right because by
    definition errors are hard to predict and to reproduce.


    <p>Please take the time to contact the author in case you discover
    that errors are not properly handled. You are most welcome to
    suggest new error handling policies or criticize existing policies.
    */
    class LOG4CXX_EXPORT ErrorHandler : public virtual OptionHandler
    {
    public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(ErrorHandler)
      virtual ~ErrorHandler() {}
        
        /**
      Add a reference to a logger to which the failing appender might
      be attached to. The failing appender will be searched and
      replaced only in the loggers you add through this method.

      @param logger One of the loggers that will be searched for the failing
      appender in view of replacement.
      */
      virtual void setLogger(const LoggerPtr& logger) = 0;


      /**
      Equivalent to the error(const String&, helpers::Exception&, int,
      spi::LoggingEvent&) with the the event parameteter set to
      null.
      */
      virtual void error(const String& message, helpers::Exception& e,
        int errorCode) const = 0;

      /**
      This method is normally used to just print the error message
      passed as a parameter.
      */
      virtual void error(const String& message) const = 0;

      /**
      This method is invoked to handle the error.

      @param message The message assoicated with the error.
      @param e The Exption that was thrown when the error occured.
      @param errorCode The error code associated with the error.
      @param event The logging event that the failing appender is asked
      to log.
      */
      virtual void error(const String& message, helpers::Exception& e,
        int errorCode, const LoggingEventPtr& event) const = 0;
    
      /**
      Set the appender for which errors are handled. This method is
      usually called when the error handler is configured.
      */
      virtual void setAppender(const AppenderPtr& appender) = 0;

      /**
      Set the appender to fallback upon in case of failure.
      */
      virtual void setBackupAppender(const AppenderPtr& appender) = 0;
    };
  }  //namespace spi
}; //namespace log4cxx

#endif //_LOG4CXX_SPI_ERROR_HANDLER_H
