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
 
#ifndef _LOG4CXX_SPI_ROOT_CATEGORY_H
#define _LOG4CXX_SPI_ROOT_CATEGORY_H

#include <log4cxx/logger.h>

namespace log4cxx
{
  namespace spi
  {
        /**
        RootCategory sits at the top of the logger hierachy. It is a
        regular logger except that it provides several guarantees.

        <p>First, it cannot be assigned a null
        level. Second, since root logger cannot have a parent, the
        #getEffectiveLevel method always returns the value of the
        level field without walking the hierarchy.
        */
        class LOG4CXX_EXPORT RootCategory : public Logger
    {
    public:
            /**
            The root logger names itself as "root". However, the root
            logger cannot be retrieved by name.
            */
            RootCategory(const LevelPtr& level);
 
            /**
            Return the assigned level value without walking the category
            hierarchy.
            */
            virtual const LevelPtr& getEffectiveLevel();

            /**
      Setting a null value to the level of the root category may have catastrophic
      results. We prevent this here.
      */
            void setLevel(const LevelPtr& level);
    };
  }  // namespace spi
}; // namespace log4cxx

#endif //_LOG4CXX_SPI_ROOT_CATEGORY_H
