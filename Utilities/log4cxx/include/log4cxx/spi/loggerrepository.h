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
 
#ifndef _LOG4CXX_SPI_LOG_REPOSITORY_H
#define _LOG4CXX_SPI_LOG_REPOSITORY_H

#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/object.h>
#include <log4cxx/helpers/tchar.h>
#include <vector>

namespace log4cxx
{
  class Level;
  typedef helpers::ObjectPtrT<Level> LevelPtr;

  class Logger;
  typedef helpers::ObjectPtrT<Logger> LoggerPtr;

  typedef std::vector<LoggerPtr> LoggerList;
  
  class Appender;
  typedef log4cxx::helpers::ObjectPtrT<Appender> AppenderPtr;

  namespace spi
  {
    class HierarchyEventListener;
    typedef log4cxx::helpers::ObjectPtrT<HierarchyEventListener>
      HierarchyEventListenerPtr;
      
    class LoggerFactory;
    typedef helpers::ObjectPtrT<LoggerFactory> LoggerFactoryPtr;
    
    class LoggerRepository;
    typedef helpers::ObjectPtrT<LoggerRepository> LoggerRepositoryPtr;
    
    /**
    A <code>LoggerRepository</code> is used to create and retrieve
        <code>Loggers</code>. The relation between loggers in a repository
        depends on the repository but typically loggers are arranged in a
        named hierarchy.

        <p>In addition to the creational methods, a
        <code>LoggerRepository</code> can be queried for existing loggers,
        can act as a point of registry for events related to loggers.
        */
        class LOG4CXX_EXPORT LoggerRepository : public virtual helpers::Object
        {
        public:
      DECLARE_ABSTRACT_LOG4CXX_OBJECT(LoggerRepository)
      virtual ~LoggerRepository() {}

            /**
            Add a {@link spi::HierarchyEventListener HierarchyEventListener}
      event to the repository.
            */
            virtual void addHierarchyEventListener(const HierarchyEventListenerPtr& 
        listener) = 0;
            /**
            Is the repository disabled for a given level? The answer depends
            on the repository threshold and the <code>level</code>
            parameter. See also #setThreshold method.  */
            virtual bool isDisabled(int level) const = 0;

            /**
            Set the repository-wide threshold. All logging requests below the
            threshold are immediately dropped. By default, the threshold is
            set to <code>Level::ALL</code> which has the lowest possible rank.  */
            virtual void setThreshold(const LevelPtr& level) = 0;

            /**
            Another form of {@link #setThreshold(const LevelPtr&) 
      setThreshold} accepting a string
            parameter instead of a <code>Level</code>. */
            virtual void setThreshold(const String& val) = 0;

            virtual void emitNoAppenderWarning(const LoggerPtr& logger) = 0;

            /**
            Get the repository-wide threshold. See {@link
            #setThreshold(const LevelPtr&) setThreshold}
      for an explanation. */
            virtual const LevelPtr& getThreshold() const = 0;

            virtual LoggerPtr getLogger(const String& name) = 0;

            virtual LoggerPtr getLogger(const String& name, spi::LoggerFactoryPtr 
        factory) = 0;
        
            virtual LoggerPtr getRootLogger() const = 0;

            virtual LoggerPtr exists(const String& name) = 0;

            virtual void shutdown() = 0;

            virtual LoggerList getCurrentLoggers() const = 0;

            virtual void fireAddAppenderEvent(const LoggerPtr& logger, 
        const AppenderPtr& appender) = 0;
        
            virtual void resetConfiguration() = 0;
        }; // class LoggerRepository
  }  // namespace spi
}; // namespace log4cxx

#endif //_LOG4CXX_SPI_LOG_REPOSITORY_H
