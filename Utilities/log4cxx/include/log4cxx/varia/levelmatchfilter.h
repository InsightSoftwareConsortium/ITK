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
 
#ifndef _LOG4CXX_VARIA_LEVEL_MATCH_FILTER_H
#define _LOG4CXX_VARIA_LEVEL_MATCH_FILTER_H

#include <log4cxx/spi/filter.h>
#include <log4cxx/level.h>

namespace log4cxx
{
  class Level;
  
  namespace varia
  {
    /**
    This is a very simple filter based on level matching.

    <p>The filter admits two options <b>LevelToMatch</b> and
    <b>AcceptOnMatch</b>. If there is an exact match between the value
    of the <b>LevelToMatch</b> option and the level of the {@link
    spi::LoggingEvent LoggingEvent}, then the #decide method returns {@link
    spi::Filter#ACCEPT ACCEPT} in case the <b>AcceptOnMatch</b> 
    option value is set to <code>true</code>, if it is <code>false</code>
    then {@link spi::Filter#DENY DENY} is returned. If there is no match,
    {@link spi::Filter#NEUTRAL NEUTRAL} is returned.
    */
    class LevelMatchFilter;
    typedef helpers::ObjectPtrT<LevelMatchFilter> LevelMatchFilterPtr;

    class LOG4CXX_EXPORT LevelMatchFilter : public spi::Filter
    {
    private:
      static String LEVEL_TO_MATCH_OPTION;
      static String ACCEPT_ON_MATCH_OPTION;

      bool acceptOnMatch;
      LevelPtr levelToMatch;

    public:
      typedef spi::Filter BASE_CLASS;
      DECLARE_LOG4CXX_OBJECT(LevelMatchFilter)
      BEGIN_LOG4CXX_CAST_MAP()
        LOG4CXX_CAST_ENTRY(LevelMatchFilter)
        LOG4CXX_CAST_ENTRY_CHAIN(BASE_CLASS)
      END_LOG4CXX_CAST_MAP()

      LevelMatchFilter();

      /**
      Set options
      */
      virtual void setOption(const String& option,
        const String& value);

      void setLevelToMatch(const String& levelToMatch);

      const String& getLevelToMatch() const;

      inline void setAcceptOnMatch(bool acceptOnMatch)
        { this->acceptOnMatch = acceptOnMatch; }

      inline bool getAcceptOnMatch() const
        { return acceptOnMatch; }

      /**
      Return the decision of this filter.

      Returns {@link spi::Filter#NEUTRAL NEUTRAL} if the 
      <b>LevelToMatch</b> option is not set or if there is not match.
      Otherwise, if there is a match, then the returned decision is 
      {@link spi::Filter#ACCEPT ACCEPT} if the <b>AcceptOnMatch</b>
      property is set to <code>true</code>. The returned decision is 
      {@link spi::Filter#DENY DENY} if the
      <b>AcceptOnMatch</b> property is set to false.
      */
      FilterDecision decide(const spi::LoggingEventPtr& event) const;
    }; // class LevelMatchFilter
  }  // namespace varia
}; // namespace log4cxx

#endif // _LOG4CXX_VARIA_STRING_MATCH_FILTER_H
