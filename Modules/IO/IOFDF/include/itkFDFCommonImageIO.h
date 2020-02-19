/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkFDFCommonImageIO_h
#define itkFDFCommonImageIO_h

#include <itkFDFImageIO.h>
#include <itkIndent.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <iterator>
#include <algorithm>

namespace itk
{

std::string
RemoveCharacters(std::string, char);

void
Tokenize(const std::string & str, std::vector<std::string> & tokens, const std::string & delimiters = " ");

std::string
ParseLine(std::string line);

template <typename T>
void
ConvertFromString(std::string s, T & value)
{
  std::stringstream str;
  str << s;
  str >> value;
}

template <typename T>
void
StringToVector(std::string value, std::vector<T> & values)
{
  std::vector<std::string> tokens;

  // value consists of something like {256,256}
  std::string::size_type startBracketPosition = value.find_first_of("{", 0);
  std::string::size_type endBracketPosition = value.find_first_of("}", startBracketPosition);

  if (startBracketPosition != std::string::npos && endBracketPosition != std::string::npos)
  {
    std::string elements = value.substr(startBracketPosition + 1, endBracketPosition - startBracketPosition - 1);


    Tokenize(elements, tokens, ",");
  }

  T element;

  for (auto & token : tokens)
  {
    ConvertFromString(token, element);
    values.push_back(element);
  }
}

template <typename T>
void
PrintVector(std::ostream & os, std::string name, const std::vector<T> & vect)
{
  int size = vect.size();

  os << name << " {";

  for (int i = 0; i < size; i++)
  {
    os << vect[i];

    if (i < size - 1)
      os << ", ";
  }

  os << "}" << std::endl;
}

} // namespace itk

#endif
