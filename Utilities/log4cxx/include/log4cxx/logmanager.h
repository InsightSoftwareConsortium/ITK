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
 
#ifndef _LOG4CXX_LOG_MANAGER_H
#define _LOG4CXX_LOG_MANAGER_H

#include <log4cxx/helpers/tchar.h>
#include <vector>
#include <log4cxx/spi/repositoryselector.h>

namespace log4cxx
{
    class Logger;
    typedef helpers::ObjectPtrT<Logger> LoggerPtr;
    typedef std::vector<LoggerPtr> LoggerList;

    namespace spi
    {
    class LoggerFactory;
    typedef helpers::ObjectPtrT<LoggerFactory> LoggerFactoryPtr;
    } 
    
    /**
    * Use the <code>LogManager</code> class to retreive Logger
    * instances or to operate on the current {@link spi::LoggerRepository
  * LoggerRepository}. When the <code>LogManager</code> class is loaded
    * into memory the default initialization procedure is inititated.
  */
    class LOG4CXX_EXPORT LogManager
    {
    private:
        static void * guard;
        static spi::RepositorySelectorPtr repositorySelector;
 
    public:
        /**
        Sets <code>LoggerFactory</code> but only if the correct
        <em>guard</em> is passed as parameter.

        <p>Initally the guard is null.  If the guard is
        <code>null</code>, then invoking this method sets the logger
        factory and the guard. Following invocations will throw a {@link
        helpers::IllegalArgumentException IllegalArgumentException},
    unless the previously set <code>guard</code> is passed as the second
    parameter.

        <p>This allows a high-level component to set the {@link
        spi::RepositorySelector RepositorySelector} used by the 
    <code>LogManager</code>.
    */

        static void setRepositorySelector(spi::RepositorySelectorPtr selector,
      void * guard);

        static spi::LoggerRepositoryPtr& getLoggerRepository();

        /**
        Retrieve the appropriate root logger.
        */
        static LoggerPtr getRootLogger();

        /**
        Retrieve the appropriate Logger instance.
        */
        static LoggerPtr getLogger(const String& name);

        /**
        Retrieve the appropriate Logger instance.
        */
        static LoggerPtr getLogger(const String& name,
      spi::LoggerFactoryPtr factory);

        static LoggerPtr exists(const String& name);

        static LoggerList getCurrentLoggers();

        /**
    Safely close and remove all appenders in all loggers including
    the root logger.
    */
    static void shutdown();

    /**
    Reset all values contained in this current {@link 
    spi::LoggerRepository LoggerRepository}  to their default.
    */
        static void resetConfiguration();
    }; // class LogManager
}  // namespace log4cxx

#endif //_LOG4CXX_LOG_MANAGER_H
