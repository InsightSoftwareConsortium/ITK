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
#ifndef itkMacro_h
#error "Do not include itkExceptionObject.h directly,  include itkMacro.h instead."
#else // itkMacro_h

#ifndef itkExceptionObject_h
#define itkExceptionObject_h

#include <string>
#include <stdexcept>

#include "itkSmartPointer.h"

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
class ITKCommon_EXPORT ExceptionObject:public std::exception
{
public:
  using Superclass = std::exception;
  /** Various types of constructors.  Note that these functions will be
  * called when children are instantiated.  The default constructor and
  * the copy constructor of ExceptionObject never throw an exception. */
  ExceptionObject() noexcept;
  explicit ExceptionObject(const char *file, unsigned int lineNumber = 0,
                           const char *desc = "None", const char *loc = "Unknown");
  explicit ExceptionObject(const std::string & file, unsigned int lineNumber = 0,
                           const std::string & desc = "None",
                           const std::string & loc = "Unknown");
  ExceptionObject(const ExceptionObject & orig) noexcept;

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  ~ExceptionObject() noexcept override;

  /** Assignment operator. */
  ExceptionObject & operator=(const ExceptionObject & orig) noexcept;

  /** Equivalence operator. */
  virtual bool operator==(const ExceptionObject & orig);

  virtual const char * GetNameOfClass() const
  { return "ExceptionObject"; }

  /** Print exception information.  This method can be overridden by
   * specific exception subtypes.  The default is to print out the
   * location where the exception was first thrown and any description
   * provided by the "thrower".   */
  virtual void Print(std::ostream & os) const;

  /** Methods to get and set the Location and Description fields. The Set
   * methods are overloaded to support both std::string and const char
   * array types. Get methods return const char arrays. */
  virtual void SetLocation(const std::string & s);

  virtual void SetDescription(const std::string & s);

  virtual void SetLocation(const char *s);

  virtual void SetDescription(const char *s);

  virtual const char * GetLocation()    const;

  virtual const char * GetDescription() const;

  /** What file did the exception occur in? */
  virtual const char * GetFile()    const;

  /** What line did the exception occur in? */
  virtual unsigned int GetLine() const;

  /** Provide std::exception::what() implementation. */
  const char * what() const noexcept override;

private:
  /** \class ReferenceCounterInterface
   *
   *  Exception data.  Location of the error and description of the error.
   *
   *  Class hierarchy
   *
   *
   *           ReferenceCounterInterface (Register/UnRegister)
   *                     ^
   *                     |
   *               ExceptionData       LightObject (Register/UnRegister)
   *                     ^                  ^
   *                     |                  |
   *                   ReferenceCountedExceptionData (Register/UnRegister)
   *
   *
   *
   *  The ReferenceCounterInterface is an abstract class providing
   *  the API interface expected by the SmartPointer. Its second derived
   *  class, the ReferenceCountedExceptionData, double inherits from LightObject
   *  and ExceptionData, and overloads the Register()/UnRegister() methods to
   *  delegate them to its second parent, the LightObject.
   *
   * \ingroup ITKCommon
   */
  class ReferenceCounterInterface
  {
public:
    virtual void Register() const = 0;

    virtual void UnRegister() const = 0;

    ReferenceCounterInterface();
    virtual ~ReferenceCounterInterface();
  };
  class ExceptionData;
  class ReferenceCountedExceptionData;
  SmartPointer< const ReferenceCounterInterface > m_ExceptionData;
  const ExceptionData * GetExceptionData() const;
};

/** Generic inserter operator for ExceptionObject and its subclasses. */
inline std::ostream & operator<<(std::ostream & os, ExceptionObject & e)
{
  ( &e )->Print(os);
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
class ITKCommon_EXPORT MemoryAllocationError:public ExceptionObject
{
public:
  /** Default constructor.  Needed to ensure the exception object can be
   * copied. */
  MemoryAllocationError() noexcept:ExceptionObject() {}

  /** Constructor. Needed to ensure the exception object can be copied. */
  MemoryAllocationError(const char *file, unsigned int lineNumber):ExceptionObject(file, lineNumber) {}

  /** Constructor. Needed to ensure the exception object can be copied. */
  MemoryAllocationError(const std::string & file, unsigned int lineNumber):ExceptionObject(file, lineNumber) {}

  /** Constructor. Needed to ensure the exception object can be copied. */
  MemoryAllocationError(const std::string & file,
                        unsigned int lineNumber,
                        const std::string & desc,
                        const std::string & loc):ExceptionObject(file, lineNumber, desc, loc) {}

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  ~MemoryAllocationError() noexcept override;

  const char * GetNameOfClass() const override
  { return "MemoryAllocationError"; }
};

/** \class RangeError
 * Exception thrown when accessing memory out of range.
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT RangeError:public ExceptionObject
{
public:
  /** Default constructor.  Needed to ensure the exception object can be
   * copied. */
  RangeError() noexcept:ExceptionObject() {}

  /** Constructor. Needed to ensure the exception object can be copied. */
  RangeError(const char *file, unsigned int lineNumber):ExceptionObject(file, lineNumber) {}

  /** Constructor. Needed to ensure the exception object can be copied. */
  RangeError(const std::string & file, unsigned int lineNumber):ExceptionObject(file, lineNumber) {}

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  ~RangeError() noexcept override;

  const char * GetNameOfClass() const override
  { return "RangeError"; }
};

/** \class InvalidArgumentError
 * Exception thrown when invalid argument is given to a method
 * or function.
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT InvalidArgumentError:public ExceptionObject
{
public:
  /**
   * Default constructor.  Needed to ensure the exception object can be
   * copied.
   */
  InvalidArgumentError() noexcept:ExceptionObject() {}

  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  InvalidArgumentError(const char *file, unsigned int lineNumber):ExceptionObject(file, lineNumber) {}

  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  InvalidArgumentError(const std::string & file, unsigned int lineNumber):ExceptionObject(file, lineNumber) {}

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  ~InvalidArgumentError() noexcept override;

  const char * GetNameOfClass() const override
  { return "InvalidArgumentError"; }
};

/** \class IncompatibleOperandsError
 * Exception thrown when two operands are incompatible.
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT IncompatibleOperandsError:public ExceptionObject
{
public:
  /** Default constructor.  Needed to ensure the exception object can be
   * copied. */
  IncompatibleOperandsError() noexcept:ExceptionObject() {}

  /** Constructor. Needed to ensure the exception object can be copied. */
  IncompatibleOperandsError(const char *file, unsigned int lineNumber):ExceptionObject(file, lineNumber) {}

  /** Constructor. Needed to ensure the exception object can be copied. */
  IncompatibleOperandsError(const std::string & file, unsigned int lineNumber):ExceptionObject(file, lineNumber) {}

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  ~IncompatibleOperandsError() noexcept override;

  const char * GetNameOfClass() const override
  { return "IncompatibleOperandsError"; }
};

/** \class ProcessAborted
 * Exception thrown when a filter (actually a ProcessObject) has been aborted.
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT ProcessAborted:public ExceptionObject
{
public:
  /** Default constructor.  Needed to ensure the exception object can be
   * copied. */
  ProcessAborted():ExceptionObject()
  {
    this->SetDescription("Filter execution was aborted by an external request");
  }

  /** Constructor. Needed to ensure the exception object can be copied. */
  ProcessAborted(const char *file, unsigned int lineNumber):ExceptionObject(file, lineNumber)
  {
    this->SetDescription("Filter execution was aborted by an external request");
  }

  /** Constructor. Needed to ensure the exception object can be copied. */
  ProcessAborted(const std::string & file, unsigned int lineNumber):ExceptionObject(file, lineNumber)
  {
    this->SetDescription("Filter execution was aborted by an external request");
  }

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  ~ProcessAborted()  noexcept override;

  const char * GetNameOfClass() const override
  { return "ProcessAborted"; }
};
} // end namespace itk

#endif //itkExceptionObject_h

#endif //itkMacro_h
