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
 
#ifndef _LOG4CXX_HELPERS_LOCALE_H
#define _LOG4CXX_HELPERS_LOCALE_H

#include <log4cxx/helpers/tchar.h>

namespace log4cxx
{
  namespace helpers
  {
    class LOG4CXX_EXPORT Locale
    {
    public:
      Locale(const String& language);
      Locale(const String& language, const String& country);
      Locale(const String& language, const String& country, 
        const String& variant);
        
      static const Locale& getDefault();
      static void setDefault(const Locale& newLocale);
      
      const String& getLanguage() const;
      const String& getCountry() const;
      const String& getVariant() const;
      
    protected:
      String language;
      String country;
      String variant;  
    }; // class Locale
  }  // namespace helpers
}; // namespace log4cxx

#endif // _LOG4CXX_HELPERS_LOCALE_H
