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
 
#include <log4cxx/helpers/properties.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

class PropertyParser
{
public:
  void parse(istream& in, Properties& properties)
  {
    StringBuffer key, element;
    LexemType lexemType = BEGIN;
    TCHAR c;
    bool finished = false;

    if (!get(in, c))
    {
      return;
    }

    while (!finished)
    {
      switch(lexemType)
      {
      case BEGIN:
        switch(c)
        {
        case _T(' '):
        case _T('\t'):
        case _T('\n'):
        case _T('\r'):
          if (!get(in, c))
            finished = true;
          break;

        case _T('#'):
        case _T('!'):
          lexemType = COMMENT;
          if (!get(in, c))
            finished = true;
          break;

        default:
          lexemType = KEY;
          break;
        }
        break;

      case KEY:
        switch(c)
        {
        case _T('\\'):
          lexemType = KEY_ESCAPE;
          if (!get(in, c))
            finished = true;
          break;

        case _T('\t'):
        case _T(' '):
        case _T(':'):
        case _T('='):
          lexemType = DELIMITER;
          if (!get(in, c))
            finished = true;
          break;

        case _T('\n'):
        case _T('\r'):
          // key associated with an empty string element
          properties.setProperty(key.str(), _T(""));
          key.str(_T(""));
          lexemType = BEGIN;
          if (!get(in, c))
            finished = true;
          break;

        default:
          key << c;
          if (!get(in, c))
            finished = true;
          break;
        }
        break;

      case KEY_ESCAPE:
        switch(c)
        {
        case _T('\t'):
        case _T(' '):
        case _T(':'):
        case _T('='):
        case _T('\\'):
          key << c;
          lexemType = KEY;
          if (!get(in, c))
            finished = true;
          break;

        case _T('\n'):
          lexemType = KEY_CONTINUE;
          if (!get(in, c))
            finished = true;
          break;

        case _T('\r'):
          lexemType = KEY_CONTINUE2;
          if (!get(in, c))
            finished = true;
          break;
        }
        break;

      case KEY_CONTINUE:
        switch(c)
        {
        case _T(' '):
        case _T('\t'):
          if (!get(in, c))
            finished = true;
          break;

        default:
          lexemType = KEY;
          break;
        }
        break;

      case KEY_CONTINUE2:
        switch(c)
        {
        case _T('\n'):
          if (!get(in, c))
            finished = true;
          lexemType = KEY_CONTINUE;
          break;

        default:
          lexemType = KEY_CONTINUE;
          break;
        }
        break;

      case DELIMITER:
        switch(c)
        {
        case _T('\t'):
        case _T(' '):
        case _T(':'):
        case _T('='):
          if (!get(in, c))
            finished = true;
          break;

        default:
          lexemType = ELEMENT;
          break;
        }
        break;

      case ELEMENT:
        switch(c)
        {
        case _T('\\'):
          lexemType = ELEMENT_ESCAPE;
          if (!get(in, c))
            finished = true;
          break;

        case _T('\n'):
        case _T('\r'):
          // key associated with an empty string element
          properties.setProperty(key.str(), element.str());
          key.str(_T(""));
          element.str(_T(""));
          lexemType = BEGIN;
          if (!get(in, c))
            finished = true;
          break;

        default:
          element << c;
          if (!get(in, c))
            finished = true;
          break;
        }
        break;

      case ELEMENT_ESCAPE:
        switch(c)
        {
        case _T('t'):
        case _T(' '):
        case _T('n'):
        case _T('r'):
        case _T('\''):
        case _T('\\'):
        case _T('\"'):
        case _T(':'):
        default:
          element << c;
          lexemType = ELEMENT;
          if (!get(in, c))
            finished = true;
          break;

        case _T('\n'):
          lexemType = ELEMENT_CONTINUE;
          if (!get(in, c))
            finished = true;
          break;

        case _T('\r'):
          lexemType = ELEMENT_CONTINUE2;
          if (!get(in, c))
            finished = true;
          break;
        }
        break;

      case ELEMENT_CONTINUE:
        switch(c)
        {
        case _T(' '):
        case _T('\t'):
          if (!get(in, c))
            finished = true;
          break;

        default:
          lexemType = ELEMENT;
          break;
        }
        break;

      case ELEMENT_CONTINUE2:
        switch(c)
        {
        case _T('\n'):
          if (!get(in, c))
            finished = true;
          lexemType = ELEMENT_CONTINUE;
          break;

        default:
          lexemType = ELEMENT_CONTINUE;
          break;
        }
        break;

      case COMMENT:
        if (c == _T('\n') || c == _T('\r'))
        {
          lexemType = BEGIN;
        }
        if (!get(in, c))
          finished = true;
        break;
      }
    }

    if (!key.str().empty())
    {
      properties.setProperty(key.str(), element.str());
    }
  }

protected:
  bool get(istream& in, TCHAR& c)
  {
    in.get(c);

    if (in.eof())
    {
      return false;
    }

    if (in.bad())
    {
      throw IOException();
    }

    return true;
  }

  typedef enum
  {
    BEGIN,
    KEY,
    KEY_ESCAPE,
    KEY_CONTINUE,
    KEY_CONTINUE2,
    DELIMITER,
    ELEMENT,
    ELEMENT_ESCAPE,
    ELEMENT_CONTINUE,
    ELEMENT_CONTINUE2,
    COMMENT
  }
  LexemType;
};

String Properties::setProperty(const String& key, const String& value)
{
  String oldValue = properties[key];
  properties[key] = value;
  //tcout << _T("setting property key=") << key << _T(", value=") << value << std::endl;
  return oldValue;
}

String Properties::getProperty(const String& key) const
{
  std::map<String, String>::const_iterator it = properties.find(key);
  return (it != properties.end()) ? it->second : String();
}

void Properties::load(istream& inStream)
{
  properties.clear();
  PropertyParser parser;
  parser.parse(inStream, *this);
}

std::vector<String> Properties::propertyNames() const
{
  std::vector<String> names;
  names.reserve(properties.size());

  std::map<String, String>::const_iterator it;
  for (it = properties.begin(); it != properties.end(); it++)
  {
    const String& key = it->first;
    names.push_back(key);
  }

  return names;
}

