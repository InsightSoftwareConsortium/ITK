/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkFDFCommonImageIO.h"

namespace itk
{

// Remove a particular type of character from a string
std::string
RemoveCharacters(std::string line, char character)
{
  line.erase(std::remove(line.begin(), line.end(), character), line.end());
  return line;
}

void
Tokenize(const std::string & str, std::vector<std::string> & tokens, const std::string & delimiters)
{
  // Skip delimiters at beginning.
  std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  // Find first "non-delimiter".
  std::string::size_type pos = str.find_first_of(delimiters, lastPos);

  while (std::string::npos != pos || std::string::npos != lastPos)
  {
    // Found a token, add it to the vector.
    tokens.push_back(str.substr(lastPos, pos - lastPos));
    // Skip delimiters.  Note the "not_of"
    lastPos = str.find_first_not_of(delimiters, pos);
    // Find next "non-delimiter"
    pos = str.find_first_of(delimiters, lastPos);
  }
}

std::string
ParseLine(std::string line)
{
  // strip *
  line = RemoveCharacters(line, '*');
  line = RemoveCharacters(line, '\"');
  line = RemoveCharacters(line, '[');
  line = RemoveCharacters(line, ']');

  // Need to deal with space between {}
  std::string::size_type startBracketPosition = line.find_first_of("{", 0);
  std::string::size_type endBracketPosition = line.find_first_of("}", startBracketPosition);

  if (startBracketPosition != std::string::npos && endBracketPosition != std::string::npos)
  {
    std::string element = line.substr(startBracketPosition, endBracketPosition - startBracketPosition);

    // Find whitespace within {} and erase
    std::string::size_type whiteSpacePosition = line.find_first_of(" ", startBracketPosition);

    while (whiteSpacePosition != std::string::npos)
    {
      line.erase(whiteSpacePosition, 1);
      whiteSpacePosition = line.find_first_of(" ", whiteSpacePosition);
    }
  }

  return line;
}
} // namespace itk
