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
 
#ifndef _LOG4CXX_HELPERS_APPENDER_ATTACHABLE_IMPL_H
#define _LOG4CXX_HELPERS_APPENDER_ATTACHABLE_IMPL_H

#include <log4cxx/spi/appenderattachable.h>
#include <log4cxx/helpers/objectimpl.h>

namespace log4cxx
{
    namespace spi
    {
        class LoggingEvent;
        typedef helpers::ObjectPtrT<LoggingEvent> LoggingEventPtr;
    }
    
    namespace helpers
    {
    class AppenderAttachableImpl;
    typedef log4cxx::helpers::ObjectPtrT<AppenderAttachableImpl>
      AppenderAttachableImplPtr;
  
    class LOG4CXX_EXPORT AppenderAttachableImpl :
      public virtual spi::AppenderAttachable,
      public virtual helpers::ObjectImpl
        {
        protected:
            /** Array of appenders. */
            AppenderList  appenderList;

        public:
      DECLARE_LOG4CXX_OBJECT(AppenderAttachableImpl)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(AppenderAttachableImpl)
        LOG4CXX_CAST_ENTRY(spi::AppenderAttachable)
      END_LOG4CXX_CAST_MAP()

      // Methods
            /**
             * Add an appender.
             */
            virtual void addAppender(const AppenderPtr& newAppender);

            /**
             Call the <code>doAppend</code> method on all attached appenders.
            */
            int appendLoopOnAppenders(const spi::LoggingEventPtr& event);

            /**
             * Get all previously added appenders as an Enumeration.
             */
            virtual AppenderList getAllAppenders() const;

            /**
             * Get an appender by name.
             */
            virtual AppenderPtr getAppender(const String& name) const;

            /**
             Returns <code>true</code> if the specified appender is in the
             list of attached appenders, <code>false</code> otherwise.
            */
            virtual bool isAttached(const AppenderPtr& appender) const;

            /**
             * Remove all previously added appenders.
             */
            virtual void removeAllAppenders();

            /**
             * Remove the appender passed as parameter from the list of appenders.
             */
            virtual void removeAppender(const AppenderPtr& appender);

            /**
             * Remove the appender with the name passed as parameter from the
             * list of appenders.
             */
            virtual void removeAppender(const String& name);
        };
    } 
} 

#endif //_LOG4CXX_HELPERS_APPENDER_ATTACHABLE_IMPL_H
