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
 
#include <log4cxx/helpers/locale.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

Locale defaultLocale(_T(""));

Locale::Locale(const String& language)
 : language(language)
{
}

Locale::Locale(const String& language, const String& country)
 : language(language), country(country)
{
}

Locale::Locale(const String& language, const String& country, 
   const String& variant)
: language(language), country(country), variant(variant)
{
}

const Locale& Locale::getDefault()
{
  return defaultLocale;
}

void Locale::setDefault(const Locale& newLocale)
{
  defaultLocale = newLocale;
}

const String& Locale::getLanguage() const
{
  return language;
}

const String& Locale::getCountry() const
{
  return country;
}

const String& Locale::getVariant() const
{
  return variant;
}

