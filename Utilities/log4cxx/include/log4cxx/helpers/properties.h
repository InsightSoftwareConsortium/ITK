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
 
#ifndef _LOG4CXX_HELPER_PROPERTIES_H
#define _LOG4CXX_HELPER_PROPERTIES_H

#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/objectptr.h>
#include <log4cxx/helpers/objectimpl.h>
#include <map>
#include <vector>

namespace log4cxx
{
  namespace helpers
  {
    class LOG4CXX_EXPORT Properties
    {
    private:
      std::map<String, String> properties;

    public:
      /**
      Reads a property list (key and element pairs) from the input stream.
      The stream is assumed to be using the ISO 8859-1 character encoding.

      <p>Every property occupies one line of the input stream.
      Each line is terminated by a line terminator (<code>\\n</code> or
      <code>\r</code> or <code>\r\\n</code>).
      Lines from the input stream are processed until end of file is reached
      on the input stream.

      <p>A line that contains only whitespace or whose first non-whitespace
      character is an ASCII <code>#</code> or <code>!</code> is ignored
      (thus, <code>#</code> or <code>!</code> indicate comment lines).

      <p>Every line other than a blank line or a comment line describes one
      property to be added to the table (except that if a line ends with \,
      then the following line, if it exists, is treated as a continuation
      line, as described below). The key consists of all the characters in
      the line starting with the first non-whitespace character and up to,
      but  not including, the first ASCII <code>=</code>, <code>:</code>,
      or whitespace character. All of the
      key termination characters may be included in the key by preceding them
      with a <code>\\</code>. Any whitespace after the key is skipped;
      if the first
      non-whitespace character after the key is <code>=</code> or
      <code>:</code>, then it is ignored
      and any whitespace characters after it are also skipped. All remaining
      characters on the line become part of the associated element string.
      Within the element string, the ASCII escape sequences <code>\t</code>,
      <code>\\n</code>, <code>\r</code>, <code>\\</code>, <code>\"</code>,
      <code>\'</code>, <code>\\</code> (a backslash and a space), and
      <code>\uxxxx</code> are recognized
      and converted to single characters. Moreover, if the last character on
      the line is <code>\\</code>, then the next line is treated as a
      continuation of the
      current line; the <code>\\</code> and line terminator are simply
      discarded, and any
      leading whitespace characters on the continuation line are also
      discarded and are not part of the element string.

      <p>As an example, each of the following four lines specifies the key
      "Truth" and the associated element value "Beauty":

       <pre>
 Truth = Beauty
  Truth:Beauty
 Truth      :Beauty
      </pre>

        As another example, the following three lines specify a single
      property:
      <pre>
 fruits        apple, banana, pear, \
                                  cantaloupe, watermelon, \
                                  kiwi, mango
      </pre>
           The key is "<code>fruits</code>" and the associated element is:
      <pre>
"apple, banana, pear, cantaloupe, watermelon, kiwi, mango"
      </pre>
      Note that a space appears before each \ so that a space will appear
      after each comma in the final result; the \, line terminator, and
      leading whitespace on the continuation line are merely discarded and are
      not replaced by one or more other characters.

        <p>As a third example, the line:
      <pre>
cheeses
       </pre>
          specifies that the key is "<code>cheeses</code>" and the associated
      element is the empty string.
      
        @param inStream the input stream.
      
      @throw IOException if an error occurred when reading from the input
      stream.
      */
      void load(istream& inStream);

      /**
      Calls the Hashtable method put. Provided for parallelism with the
      getProperty method. Enforces use of strings for property keys and
      values. The value returned is the result of the Hashtable call to
      put.
      @param key the key to be placed into this property list.
          @param value the value corresponding to key.
        @return the previous value of the specified key in this
      property list, or an empty string if it did not have one.
      */
      String setProperty(const String& key, const String& value);


      /**
      Searches for the property with the specified key in this property
      list. If the key is not found in this property list, the default
      property list, and its defaults, recursively, are then checked.
      The method returns an empty string if the property is not found.
        @param key the property key.
        @return the value in this property list with the specified
      key value.
      */
      String getProperty(const String& key) const;

      /**
      Returns an enumeration of all the keys in this property list,
      including distinct keys in the default property list if a key
      of the same name has not already been found from the main
      properties list.
      @return an array of all the keys in this
      property list, including the keys in the default property list.
      */
      std::vector<String> propertyNames() const;
    }; // class Properties
  }  // namespace helpers
}; // namespace log4cxx

#endif //_LOG4CXX_HELPER_PROPERTIES_H
