/*  Copyright (C) 2004 Glenn Pierce.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef _FDFCommonImageIO_H_
#define _FDFCommonImageIO_H_

#include <itkFDFImageIO.h>
#include <itkIndent.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <iterator>
#include <algorithm>

std::string
RemoveCharacters(std::string, char);

void
Tokenize(const std::string & str, std::vector<std::string> & tokens, const std::string & delimiters = " ");

std::string
ParseLine(std::string line);

template <class T>
void
ConvertFromString(std::string s, T & value)
{
  std::stringstream str;
  str << s;
  str >> value;
}

template <class T>
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

  for (unsigned int i = 0; i < tokens.size(); i++)
  {
    ConvertFromString(tokens[i], element);
    values.push_back(element);
  }
}

template <class T>
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

#endif
