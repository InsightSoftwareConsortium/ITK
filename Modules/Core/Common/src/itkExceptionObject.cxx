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

#include "itkIndent.h"
#include "itkMacro.h"

#include <utility>

namespace itk
{
/** \class ExceptionObject::ExceptionData
 * \brief Exception data, used to implement itk::ExceptionObject.
 *
 * Contains the location and description of the error, as well as
 * the text that should be returned by itk::ExceptionObject::what().
 */
class ExceptionObject::ExceptionData
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ExceptionData);

  // Constructor. Might throw an exception.
  ExceptionData(std::string file, unsigned int line, std::string description, std::string location)
    : m_Location(std::move(location))
    , m_Description(std::move(description))
    , m_File(std::move(file))
    , m_Line(line)
  {
    std::ostringstream loc;

    loc << ":" << m_Line << ":\n";
    m_What = m_File;
    m_What += loc.str();
    m_What += m_Description;
  }

private:
  friend class ExceptionObject;

  // The data members should never change after construction of the
  // ExceptionData object,
  // to ensure the consistency of the exception data.
  const std::string  m_Location;
  const std::string  m_Description;
  const std::string  m_File;
  const unsigned int m_Line;
  std::string        m_What;
};


ExceptionObject::ExceptionObject(const char * file, unsigned int lineNumber, const char * desc, const char * loc)
  : m_ExceptionData(std::make_shared<const ExceptionData>(file == nullptr ? "" : file,
                                                          lineNumber,
                                                          desc == nullptr ? "" : desc,
                                                          loc == nullptr ? "" : loc))
{}

ExceptionObject::ExceptionObject(std::string file, unsigned int lineNumber, std::string desc, std::string loc)
  : m_ExceptionData(std::make_shared<const ExceptionData>(std::move(file), lineNumber, std::move(desc), std::move(loc)))
{}


// Note: It appears necessary to define the destructor "out-of-line" for external linkage.
ExceptionObject::~ExceptionObject() = default;


bool
ExceptionObject::operator==(const ExceptionObject & orig) const
{
  // operator== is reimplemented, but it still behaves like the previous
  // version, from ITK 3.6.0.
  const ExceptionData * const thisData = m_ExceptionData.get();
  const ExceptionData * const origData = orig.m_ExceptionData.get();

  if (thisData == origData)
  {
    return true;
  }
  else
  {
    return (thisData != nullptr) && (origData != nullptr) && thisData->m_Location == origData->m_Location &&
           thisData->m_Description == origData->m_Description && thisData->m_File == origData->m_File &&
           thisData->m_Line == origData->m_Line;
  }
}

void
ExceptionObject::SetLocation(const std::string & s)
{
  const bool IsNull = m_ExceptionData == nullptr;

  m_ExceptionData = std::make_shared<const ExceptionData>(IsNull ? "" : m_ExceptionData->m_File.c_str(),
                                                          IsNull ? 0 : m_ExceptionData->m_Line,
                                                          IsNull ? "" : m_ExceptionData->m_Description.c_str(),
                                                          s);
}

void
ExceptionObject::SetDescription(const std::string & s)
{
  const bool IsNull = m_ExceptionData == nullptr;

  m_ExceptionData = std::make_shared<const ExceptionData>(IsNull ? "" : m_ExceptionData->m_File.c_str(),
                                                          IsNull ? 0 : m_ExceptionData->m_Line,
                                                          s,
                                                          IsNull ? "" : m_ExceptionData->m_Location.c_str());
}

void
ExceptionObject::SetLocation(const char * s)
{
  std::string location;

  if (s)
  {
    location = s;
  }
  ExceptionObject::SetLocation(location);
}

void
ExceptionObject::SetDescription(const char * s)
{
  std::string description;

  if (s)
  {
    description = s;
  }
  ExceptionObject::SetDescription(description);
}

const char *
ExceptionObject::GetLocation() const
{
  return (m_ExceptionData == nullptr) ? "" : m_ExceptionData->m_Location.c_str();
}

const char *
ExceptionObject::GetDescription() const
{
  return (m_ExceptionData == nullptr) ? "" : m_ExceptionData->m_Description.c_str();
}

const char *
ExceptionObject::GetFile() const
{
  return (m_ExceptionData == nullptr) ? "" : m_ExceptionData->m_File.c_str();
}

unsigned int
ExceptionObject::GetLine() const
{
  return (m_ExceptionData == nullptr) ? 0 : m_ExceptionData->m_Line;
}

const char *
ExceptionObject::what() const noexcept
{
  return (m_ExceptionData == nullptr) ? "ExceptionObject" : m_ExceptionData->m_What.c_str();
}

void
ExceptionObject::Print(std::ostream & os) const
{
  Indent indent;

  // Print header
  os << std::endl;
  os << indent << "itk::" << this->GetNameOfClass() << " (" << this << ")\n";

  // Print self
  indent.GetNextIndent();

  if (m_ExceptionData != nullptr)
  {
    const ExceptionData & data = *(m_ExceptionData);

    if (!data.m_Location.empty())
    {
      os << indent << "Location: \"" << data.m_Location << "\" " << std::endl;
    }

    if (!data.m_File.empty())
    {
      os << indent << "File: " << data.m_File << std::endl;
      os << indent << "Line: " << data.m_Line << std::endl;
    }

    if (!data.m_Description.empty())
    {
      os << indent << "Description: " << data.m_Description << std::endl;
    }
  }
  // Print trailer
  os << indent << std::endl;
}

} // end namespace itk
