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
 
#include <log4cxx/helpers/resourcebundle.h>
#include <log4cxx/helpers/propertyresourcebundle.h>
#include <log4cxx/helpers/loader.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(ResourceBundle)

ResourceBundlePtr ResourceBundle::getBundle(const String& baseName,
  const Locale& locale)
{
  String bundleName;
  istream * bundleStream;
  PropertyResourceBundlePtr resourceBundle, previous;

  std::vector<String> bundlesNames;

  if (!locale.getVariant().empty())
  {
    bundlesNames.push_back(baseName + _T("_") + 
      locale.getLanguage() + _T("_") + 
      locale.getCountry() + _T("_") +
      locale.getVariant());
  }

  if (!locale.getCountry().empty())
  {
    bundlesNames.push_back(baseName + _T("_") + 
        locale.getLanguage() + _T("_") + 
        locale.getCountry());
  }

  if (!locale.getLanguage().empty())
  {
    bundlesNames.push_back(baseName + _T("_") + 
          locale.getLanguage());
  }

  bundlesNames.push_back(baseName);

  for (std::vector<String>::iterator it = bundlesNames.begin();
    it != bundlesNames.end(); it++)
  {
    bundleName = *it;
    
    PropertyResourceBundlePtr current;
    
    try
    {
      const Class& classObj = Loader::loadClass(bundleName);
      current = classObj.newInstance();
    }
    catch(ClassNotFoundException&)
    {
      current = 0;
    }
    
    if (current == 0)
    {
      bundleStream = 
        Loader::getResourceAsStream(bundleName + _T(".properties"));

      if (bundleStream == 0)
      {
        continue;
      }
    }

    try
    {
      current = new PropertyResourceBundle(*bundleStream);
    }
    catch(Exception&)
    {
      delete bundleStream;
      bundleStream = 0;
      throw;
    }
    
    delete bundleStream;
    bundleStream = 0;

    if (resourceBundle == 0)
    {
      resourceBundle = current;
      previous = current;
    }
    else
    {
      previous->setParent(current);
      previous = current;
    }
  }

  if (resourceBundle == 0)
  {
    throw MissingResourceException();
  }

  return resourceBundle;
}


