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
#ifndef itkExceptionObject_h
#  error "Do not include itkExceptionObject.h directly,  include itkMacro.h instead."
#else // itkExceptionObject_h

#  include <memory> // For shared_ptr.
#  include <string>
#  include <stdexcept>

namespace itk
{
/** \class ExceptionObject
 * \brief Standard exception handling object.
 *
 * ExceptionObject provides standard methods for throwing
 * and managing exceptions in itk. Specific exceptions should be
 * derived from this class. Note that this class is derived from
 * std::exception, so an application can catch ITK exceptions as
 * std::exception if desired.
 *
 * ExceptionObject maintains two types of information: a location
 * and description (both of which are strings). The location is the
 * point in the code where the exception was thrown; the description
 * is an error message that describes the exception. The ExceptionObject
 * can be thrown explicitly in code, or more conveniently, the
 * itkExceptionMacro (found in Common/itkMacro.h) can be used.
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT ExceptionObject : public std::exception
{
public:
  static constexpr const char * const default_exception_message = "Generic ExceptionObject";
  using Superclass = std::exception;

  /** Explicitly-defaulted default-constructor. Creates an empty exception object. */
  ExceptionObject() noexcept = default;

  explicit ExceptionObject(const char * file,
                           unsigned int lineNumber = 0,
                           const char * desc = "None",
                           const char * loc = "Unknown");
  explicit ExceptionObject(std::string  file,
                           unsigned int lineNumber = 0,
                           std::string  desc = "None",
                           std::string  loc = "Unknown");

  /** Copy-constructor. */
  ExceptionObject(const ExceptionObject &) noexcept = default;

  /** Move-constructor. */
  ExceptionObject(ExceptionObject &&) noexcept = default;

  /** Copy-assignment operator. */
  ExceptionObject &
  operator=(const ExceptionObject &) noexcept = default;

  /** Move-assignment operator. */
  ExceptionObject &
  operator=(ExceptionObject &&) noexcept = default;

  /** Destructor.
   * \note It appears necessary to define the destructor "out-of-line" for external linkage. */
  ~ExceptionObject() override;

  /** Equivalence operator. */
  virtual bool
  operator==(const ExceptionObject & orig) const;

  virtual const char *
  GetNameOfClass() const
  {
    return "ExceptionObject";
  }

  /** Print exception information.  This method can be overridden by
   * specific exception subtypes.  The default is to print out the
   * location where the exception was first thrown and any description
   * provided by the "thrower".   */
  virtual void
  Print(std::ostream & os) const;

  /** Methods to get and set the Location and Description fields. The Set
   * methods are overloaded to support both std::string and const char
   * array types. Get methods return const char arrays. */
  virtual void
  SetLocation(const std::string & s);

  virtual void
  SetDescription(const std::string & s);

  virtual void
  SetLocation(const char * s);

  virtual void
  SetDescription(const char * s);

  virtual const char *
  GetLocation() const;

  virtual const char *
  GetDescription() const;

  /** What file did the exception occur in? */
  virtual const char *
  GetFile() const;

  /** What line did the exception occur in? */
  virtual unsigned int
  GetLine() const;

  /** Provide std::exception::what() implementation. */
  const char *
  what() const noexcept override;

private:
  class ExceptionData;

  std::shared_ptr<const ExceptionData> m_ExceptionData;
};

/** Generic inserter operator for ExceptionObject and its subclasses. */
inline std::ostream &
operator<<(std::ostream & os, const ExceptionObject & e)
{
  (&e)->Print(os);
  return os;
}

/**
 * Specific exception types that are subclasses of ExceptionObject follow
 */

/** \class MemoryAllocationError
 * Exception thrown when image memory allocation fails.
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT MemoryAllocationError : public ExceptionObject
{
public:
  // Inherit the constructors from its base class.
  using ExceptionObject::ExceptionObject;

  const char *
  GetNameOfClass() const override
  {
    return "MemoryAllocationError";
  }
};

/** \class RangeError
 * Exception thrown when accessing memory out of range.
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT RangeError : public ExceptionObject
{
public:
  // Inherit the constructors from its base class.
  using ExceptionObject::ExceptionObject;

  const char *
  GetNameOfClass() const override
  {
    return "RangeError";
  }
};

/** \class InvalidArgumentError
 * Exception thrown when invalid argument is given to a method
 * or function.
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT InvalidArgumentError : public ExceptionObject
{
public:
  // Inherit the constructors from its base class.
  using ExceptionObject::ExceptionObject;

  const char *
  GetNameOfClass() const override
  {
    return "InvalidArgumentError";
  }
};

/** \class IncompatibleOperandsError
 * Exception thrown when two operands are incompatible.
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT IncompatibleOperandsError : public ExceptionObject
{
public:
  // Inherit the constructors from its base class.
  using ExceptionObject::ExceptionObject;

  const char *
  GetNameOfClass() const override
  {
    return "IncompatibleOperandsError";
  }
};

/** \class ProcessAborted
 * Exception thrown when a filter (actually a ProcessObject) has been aborted.
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT ProcessAborted : public ExceptionObject
{
public:
  /** Default constructor.  Needed to ensure the exception object can be
   * copied. */
  ProcessAborted()
    : ExceptionObject()
  {
    this->SetDescription("Filter execution was aborted by an external request");
  }

  /** Constructor. Needed to ensure the exception object can be copied. */
  ProcessAborted(const char * file, unsigned int lineNumber)
    : ExceptionObject(file, lineNumber)
  {
    this->SetDescription("Filter execution was aborted by an external request");
  }

  /** Constructor. Needed to ensure the exception object can be copied. */
  ProcessAborted(const std::string & file, unsigned int lineNumber)
    : ExceptionObject(file, lineNumber)
  {
    this->SetDescription("Filter execution was aborted by an external request");
  }

  const char *
  GetNameOfClass() const override
  {
    return "ProcessAborted";
  }
};
} // end namespace itk

#endif // itkExceptionObject_h
