/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExceptionObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkExceptionObject_h
#define __itkExceptionObject_h

#include <string>
#include <stdexcept>

#include "itkWin32Header.h"


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
 */
class ITK_EXPORT ExceptionObject : public std::exception
{
public:
  typedef std::exception Superclass;
  /** Various types of constructors.  Note that these functions will be
   * called when children are instantiated. */
  ExceptionObject(const char *file="Unknown", unsigned int lineNumber=0,
                  const char *desc="None", const char *loc="Unknown")
  {
    m_Location = loc;
    m_Description = desc;
    m_File = file;
    m_Line = lineNumber;
  };
  ExceptionObject(const std::string& file, unsigned int lineNumber,
                  const std::string& desc="None",
                  const std::string& loc="Unknown")
  {
    m_Location = loc;
    m_Description = desc;
    m_File = file;
    m_Line = lineNumber;
  };
  ExceptionObject( const ExceptionObject &orig ): Superclass()
  {
    m_Location    = orig.m_Location;
    m_Description = orig.m_Description;
    m_File = orig.m_File;
    m_Line = orig.m_Line;
  }
  
  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  virtual ~ExceptionObject() throw() {}

  /** Assignment operator. */
  ExceptionObject &operator= ( const ExceptionObject &orig )
  {
    m_Location    = orig.m_Location;
    m_Description = orig.m_Description;
    m_File = orig.m_File;
    m_Line = orig.m_Line;
    return *this;
  }
  
  /** Equivalence operator. */
  virtual bool operator==( const ExceptionObject &orig )
  {
    if ( m_Location    == orig.m_Location &&
         m_Description == orig.m_Description &&
         m_File == orig.m_File &&
         m_Line == orig.m_Line) 
      {
      return true;
      }
    else 
      {
      return false;
      }
  }
          
  virtual const char *GetNameOfClass() const 
    {return "ExceptionObject";}

  /** Print exception information.  This method can be overridden by
   * specific exception subtypes.  The default is to print out the
   * location where the exception was first thrown and any description
   * provided by the ``thrower''.   */
  virtual void Print(std::ostream& os) const;

  /** Methods to get and set the Location and Description fields. The Set
   * methods are overloaded to support both std::string and const char 
   * array types. Get methods return const char arrays. */
  virtual void SetLocation(const std::string& s)    
    { m_Location = s;    }
  virtual void SetDescription(const std::string& s) 
    { m_Description = s; }
  virtual void SetLocation(const char * s)          
    { m_Location = s;    }
  virtual void SetDescription (const char *s)       
    { m_Description = s; }
  virtual const char *GetLocation()    const 
    { return m_Location.c_str();    }
  virtual const char *GetDescription() const 
    { return m_Description.c_str(); }
  
  /** What file did the exception occur in? */
  virtual const char *GetFile()    const 
    { return m_File.c_str(); }

  /** What line did the exception occur in? */
  virtual unsigned int GetLine() const 
    { return m_Line; }
  
  /** Provide std::exception::what() implementation. */
  virtual const char* what() const throw()
    { return m_Description.c_str(); }
  
private:
  /** Exception data.  Location of the error and description of the error. */
  std::string  m_Location;
  std::string  m_Description;
  std::string  m_File;
  unsigned int m_Line;
 
};

/** Generic inserter operator for ExceptionObject and its subclasses. */
inline std::ostream& operator<<(std::ostream& os, ExceptionObject &e)
{
  (&e)->Print(os);
  return os;
}

/**
 * Specific exception types that are subclasses of ExceptionObject follow
 */

/** \class RangeError
 * Exception thrown when accessing memory out of range.
 * \ingroup ITKSystemObjects 
 */
class RangeError : public ExceptionObject
{
public:
  /** Default constructor.  Needed to ensure the exception object can be
   * copied. */
  RangeError() : ExceptionObject() {}
  
  /** Constructor. Needed to ensure the exception object can be copied. */
  RangeError(const char *file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {}

  /** Constructor. Needed to ensure the exception object can be copied. */
  RangeError(const std::string& file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {}  

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  virtual ~RangeError() throw() {}

  virtual const char *GetNameOfClass() const 
    {return "RangeError";}

};

/** \class InvalidArgumentError
 * Exception thrown when invalid argument is given to a method
 * or function.
 * \ingroup ITKSystemObjects 
 */
class InvalidArgumentError : public ExceptionObject
{
public:
  /**
   * Default constructor.  Needed to ensure the exception object can be
   * copied.
   */
  InvalidArgumentError() : ExceptionObject() {}
  
  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  InvalidArgumentError(const char *file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {}

  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  InvalidArgumentError(const std::string& file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {}  

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  virtual ~InvalidArgumentError() throw() {}

  virtual const char *GetNameOfClass() const 
    {return "InvalidArgumentError";}
};

/** \class IncompatibleOperandsError
 * Exception thrown when two operands are incompatible.
 * \ingroup ITKSystemObjects 
 */
class IncompatibleOperandsError : public ExceptionObject
{
public:
  /** Default constructor.  Needed to ensure the exception object can be
   * copied. */
  IncompatibleOperandsError() : ExceptionObject() {}
  
  /** Constructor. Needed to ensure the exception object can be copied. */
  IncompatibleOperandsError(const char *file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {}

  /** Constructor. Needed to ensure the exception object can be copied. */
  IncompatibleOperandsError(const std::string& file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {}  

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  virtual ~IncompatibleOperandsError() throw() {}

  virtual const char *GetNameOfClass() const 
    {return "IncompatibleOperandsError";}
};

/** \class ProcessAborted
 * Exception thrown when a filter (actually a ProcessObject) has been aborted.
 * \ingroup ITKSystemObjects 
 */
class ProcessAborted : public ExceptionObject
{
public:
  /** Default constructor.  Needed to ensure the exception object can be
   * copied. */
  ProcessAborted() : ExceptionObject() {
    this->SetDescription("Filter execution was aborted by an external request"); }
  
  /** Constructor. Needed to ensure the exception object can be copied. */
  ProcessAborted(const char *file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {
    this->SetDescription("Filter execution was aborted by an external request"); }

  /** Constructor. Needed to ensure the exception object can be copied. */
  ProcessAborted(const std::string& file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {  
    this->SetDescription("Filter execution was aborted by an external request"); }

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  virtual ~ProcessAborted() throw() {}

  virtual const char *GetNameOfClass() const 
    {return "ProcessAborted";}

};


} // end namespace itk

#endif

