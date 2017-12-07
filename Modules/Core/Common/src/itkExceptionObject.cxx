/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkLightObject.h"


namespace itk
{
/** \class ExceptionObject::ExceptionData
 * \brief Exception data, used to implement itk::ExceptionObject.
 *
 * Contains the location and description of the error, as well as
 * the text that should be returned by itk::ExceptionObject::what().
 */
class ExceptionObject::ExceptionData:public ReferenceCounterInterface
{
protected:
  // Constructor. Might throw an exception.
  ExceptionData(
    const std::string & file, unsigned int line,
    const std::string & description,
    const std::string & location):
    m_Location(location),
    m_Description(description),
    m_File(file),
    m_Line(line)
    {
    std::ostringstream loc;

    loc << ":" << m_Line << ":\n";
    m_What = m_File;
    m_What += loc.str();
    m_What += m_Description;
    m_WhatPointer = m_What.c_str();
    }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExceptionData);

  friend class ExceptionObject;

  // The data members should never change after construction of the
  // ExceptionData object,
  // to ensure the consistency of the exception data.
  const std::string  m_Location;
  const std::string  m_Description;
  const std::string  m_File;
  const unsigned int m_Line;
  std::string        m_What;
  const char *       m_WhatPointer;
};

/** \class ExceptionObject::ReferenceCountedExceptionData
 * \brief Reference counted exception data, used to implement itk::ExceptionObject.
 *
 * Its first base class, ExceptionObject::ExceptionData, holds its data, while its second
 * base class, itk::LightObject, takes care of the reference counting.
 *
 * \note ExceptionData is constructed before LightObject, thereby it is ensured that
 * an exception within the constructor of ExceptionData won't trigger the destruction
 * of LightObject.
 */
class ExceptionObject::ReferenceCountedExceptionData:public ExceptionData, public LightObject
{
public:
  typedef ReferenceCountedExceptionData Self;
  typedef SmartPointer< const Self >    ConstPointer;
  static ConstPointer ConstNew(
    const std::string & file, unsigned int line,
    const std::string & description,
    const std::string & location)
  {
    ConstPointer      smartPtr;
    const Self *const rawPtr = new Self(file, line, description, location);

    smartPtr = rawPtr;
    rawPtr->LightObject::UnRegister();
    return smartPtr;
  }

  /** Increase the reference count (mark as used by another object).
    * Delegates the counting to its LightObject superclass  */
  virtual void Register() const ITK_OVERRIDE
  {
    this->LightObject::Register();
  }

  /** Decrease the reference count (release by another object).
    * Delegates the counting to its LightObject superclass  */
  virtual void UnRegister() const ITK_NOEXCEPT ITK_OVERRIDE
  {
    this->LightObject::UnRegister();
  }

private:
  // Constructor. Might throw an exception.
  ReferenceCountedExceptionData(
    const std::string & file, unsigned int line,
    const std::string & description,
    const std::string & location):
    ExceptionData(file, line, description, location),
    LightObject()
         {}

  // Destructor. Only invoked via LightObject::UnRegister(), when its reference
  // count drops to zero.
  ~ReferenceCountedExceptionData() ITK_OVERRIDE {}

  ITK_DISALLOW_COPY_AND_ASSIGN(ReferenceCountedExceptionData);
};

ExceptionObject::ExceptionObject()
{
  // The default construction never throws an exception.
}

ExceptionObject::ExceptionObject(
  const char *file,
  unsigned int lineNumber,
  const char *desc,
  const char *loc):
  m_ExceptionData( ReferenceCountedExceptionData::ConstNew(file == ITK_NULLPTR ? "":file, lineNumber, desc == ITK_NULLPTR ? "":desc, loc ==
                                                               ITK_NULLPTR ? "":loc) )
{}

ExceptionObject::ExceptionObject(
  const std::string & file,
  unsigned int lineNumber,
  const std::string & desc,
  const std::string & loc):
  m_ExceptionData( ReferenceCountedExceptionData::ConstNew(file, lineNumber, desc, loc) )
{}

ExceptionObject::ExceptionObject(const ExceptionObject & orig):
  Superclass(orig),
  m_ExceptionData(orig.m_ExceptionData)
{
  // This copy construction never throws, because it just copies the smart
  // pointer.
}

ExceptionObject::~ExceptionObject() ITK_NOEXCEPT
{
  // During destruction, the reference count of the
  // ReferenceCountedExceptionData will be decreased
  // automatically, by the destructor of the smart pointer.
}

const ExceptionObject::ExceptionData *
ExceptionObject::GetExceptionData() const
{
  // Note: dynamic_cast does a runtime check if the m_ExceptionData pointer is
  // indeed
  // pointing to an ExceptionData object. In this case, a static_cast could have
  // been
  // used instead, which only does compile time checking. But we expect the
  // runtime overhead of this particular dynamic_cast to be insignificant.
  const ExceptionData *thisData =
    dynamic_cast< const ExceptionData * >( this->m_ExceptionData.GetPointer() );

  return thisData;
}

ExceptionObject &
ExceptionObject::operator=(const ExceptionObject & orig)
{
  // Note: there is no superclass assignment here, because
  // std::exception::operator=
  // appears have a bug on some platforms, including MSVC 2003. The MSVC 2003
  // bug is
  // described at the Usenet newsgroup microsoft.public.vc.language, June 2,
  // 2008,
  // subject "VC7.1 std::exception assignment operator bug (crash) a known
  // issue?"
  //
  //
  //
  // http://groups.google.com/group/microsoft.public.vc.language/msg/15b927c8c1130e88

  // Assigns its smart pointer:
  m_ExceptionData = orig.m_ExceptionData;
  return *this;
}

bool
ExceptionObject::operator==(const ExceptionObject & orig)
{
  // operator== is reimplemented, but it still behaves like the previous
  // version, from ITK 3.6.0.
  const ExceptionData *const thisData = this->GetExceptionData();
  const ExceptionData *const origData = orig.GetExceptionData();

  if ( thisData == origData )
    {
    return true;
    }
  else
    {
    return ( thisData != ITK_NULLPTR ) && ( origData != ITK_NULLPTR )
           && thisData->m_Location == origData->m_Location
           && thisData->m_Description == origData->m_Description
           && thisData->m_File == origData->m_File
           && thisData->m_Line == origData->m_Line;
    }
}

void
ExceptionObject::SetLocation(const std::string & s)
{
  const bool IsNull = m_ExceptionData.IsNull();

  m_ExceptionData = ReferenceCountedExceptionData::ConstNew(
    IsNull ? "" : this->GetExceptionData()->m_File.c_str(),
    IsNull ? 0 : this->GetExceptionData()->m_Line,
    IsNull ? "" : this->GetExceptionData()->m_Description.c_str(),
    s);
}

void
ExceptionObject::SetDescription(const std::string & s)
{
  const bool IsNull = m_ExceptionData.IsNull();

  m_ExceptionData = ReferenceCountedExceptionData::ConstNew(
    IsNull ? "" : this->GetExceptionData()->m_File.c_str(),
    IsNull ? 0 : this->GetExceptionData()->m_Line,
    s,
    IsNull ? "" : this->GetExceptionData()->m_Location.c_str() );
}

void
ExceptionObject::SetLocation(const char *s)
{
  std::string location;

  if ( s )
    {
    location = s;
    }
  ExceptionObject::SetLocation(location);
}

void
ExceptionObject::SetDescription(const char *s)
{
  std::string description;

  if ( s )
    {
    description = s;
    }
  ExceptionObject::SetDescription(description);
}

const char *
ExceptionObject::GetLocation() const
{
  // Note: std::string::c_str() might throw an exception.
  return m_ExceptionData.IsNull() ? "" : this->GetExceptionData()->m_Location.c_str();
}

const char *
ExceptionObject::GetDescription() const
{
  // Note: std::string::c_str() might throw an exception.
  return m_ExceptionData.IsNull() ? "" : this->GetExceptionData()->m_Description.c_str();
}

const char *
ExceptionObject::GetFile() const
{
  // Note: std::string::c_str() might throw an exception.
  return m_ExceptionData.IsNull() ? "" : this->GetExceptionData()->m_File.c_str();
}

unsigned int
ExceptionObject::GetLine() const
{
  return m_ExceptionData.IsNull() ? 0 : this->GetExceptionData()->m_Line;
}

const char *
ExceptionObject::what() const ITK_NOEXCEPT
{
  const ExceptionData *const thisData = this->GetExceptionData();

  // Note: m_What.c_str() wouldn't be safe, because c_str() might throw an
  // exception.
  return thisData ? thisData->m_WhatPointer : "ExceptionObject";
}

void
ExceptionObject
::Print(std::ostream & os) const
{
  Indent indent;

  // Print header
  os << std::endl;
  os << indent << "itk::" << this->GetNameOfClass() << " (" << this << ")\n";

  // Print self
  indent.GetNextIndent();

  if ( m_ExceptionData.IsNotNull() )
    {
    const ExceptionData & data = *( this->GetExceptionData() );

    if ( !data.m_Location.empty() )
      {
      os << indent << "Location: \"" << data.m_Location << "\" " << std::endl;
      }

    if ( !data.m_File.empty() )
      {
      os << indent << "File: " << data.m_File << std::endl;
      os << indent << "Line: " << data.m_Line << std::endl;
      }

    if ( !data.m_Description.empty() )
      {
      os << indent << "Description: " << data.m_Description << std::endl;
      }
    }
  // Print trailer
  os << indent << std::endl;
}

MemoryAllocationError::~MemoryAllocationError() ITK_NOEXCEPT
{
}

RangeError::~RangeError() ITK_NOEXCEPT
{
}

InvalidArgumentError::~InvalidArgumentError() ITK_NOEXCEPT
{
}

IncompatibleOperandsError::~IncompatibleOperandsError() ITK_NOEXCEPT
{
}

ProcessAborted::~ProcessAborted() ITK_NOEXCEPT
{
}

ExceptionObject::ReferenceCounterInterface::ReferenceCounterInterface() {}
ExceptionObject::ReferenceCounterInterface::~ReferenceCounterInterface() {}

} // end namespace itk
