/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkExceptionObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#ifndef __itkExceptionObject_h
#define __itkExceptionObject_h

#include "itkObject.h"
#include "itkIndent.h"
#include <string>

namespace itk
{

/** \class ExceptionObject
 * \brief Standard exception handling object.
 *
 * ExceptionObject provides standard methods for throwing
 * and managing exceptions in itk. Specific exceptions should be
 * derived from this class.
 *
 * ExceptionObject maintains two types of information: a location
 * and description (both of which are strings). The location is the
 * point in the code where the exception was thrown; the description
 * is an error message that describes the exception.
 *
 * \ingroup ITKSystemObjects 
 */
class ITK_EXPORT ExceptionObject
{
public:
  /**
   * Constructor and copy constructor.  Note that these functions will be
   * called when children are instantiated.
   */
  ExceptionObject(const char *file = "Unknown", unsigned int lineNumber=0)
  {
    m_Location = "";
    m_Description = "";
    m_File = file;
    m_Line = lineNumber;
  };

  ExceptionObject(const std::string& file, unsigned int lineNumber)
  {
    m_Location = "";
    m_Description = "";
    m_File = file;
    m_Line = lineNumber;
  };

  /**
   * Virtual destructor needed 
   */
  virtual ~ExceptionObject() {};

  ExceptionObject( const ExceptionObject &orig )
  {
    m_Location    = orig.m_Location;
    m_Description = orig.m_Description;
    m_File = orig.m_File;
    m_Line = orig.m_Line;
  }
  
  /**
   * Assignment and equivalence operators.
   */
  ExceptionObject &operator= ( const ExceptionObject &orig )
  {
    m_Location    = orig.m_Location;
    m_Description = orig.m_Description;
    m_File = orig.m_File;
    m_Line = orig.m_Line;
    return *this;
  }
  
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
          
  /**
   * Print exception information.  This method can be overridden by
   * specific exception subtypes.  The default is to print out the
   * location where the exception was first thrown and any description
   * provided by the ``thrower''.  
   */
  virtual void Print(std::ostream& os) const;

  /**
   * Methods to get and set the Location and Description fields. The Set
   * methods are overloaded to support both std::string and const char 
   * array types. Get methods return const char arrays.
   */
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

  /**
   * What file did the exception occur in?
   */
  virtual const char *GetFile()    const 
    { return m_File.c_str();    }

  /**
   * What line did the exception occur in?
   */
  virtual const unsigned int GetLine() const 
    { return m_Line; }
  
  /**
   * Return the name of the class.
   */
  itkTypeMacro(ExceptionObject, None);

protected:
  /** 
   * Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes. 
   */
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  virtual void PrintHeader(std::ostream& os, Indent indent) const;
  virtual void PrintTrailer(std::ostream& os, Indent indent) const;
  
private:
  /**
   * Exception data.  Location of the error and description of the error.
   */
  std::string m_Location;
  std::string m_Description;
  std::string m_File;
  unsigned int m_Line;
 
};

/**
 * Generic inserter operator for ExceptionObject and its subclasses.
 *
 */
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
  /**
   * Default constructor.  Needed to ensure the exception object can be
   * copied.
   */
  RangeError() : ExceptionObject() {}
  
  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  RangeError(const char *file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {}

  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  RangeError(const std::string& file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {}  

  itkTypeMacro(RangeError, ExceptionObject);
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

  itkTypeMacro(InvalidArgumentError, ExceptionObject);
};

/** \class IncompatibleOperandsError
 * Exception thrown when two operands are incompatible.
 * \ingroup ITKSystemObjects 
 */
class IncompatibleOperandsError : public ExceptionObject
{
public:
  /**
   * Default constructor.  Needed to ensure the exception object can be
   * copied.
   */
  IncompatibleOperandsError() : ExceptionObject() {}
  
  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  IncompatibleOperandsError(const char *file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {}

  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  IncompatibleOperandsError(const std::string& file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) {}  

  itkTypeMacro(IncompatibleOperandsError, ExceptionObject);
};


/**
 * This class demonstrates subclassing the base class and adding additional
 * data fields.
 */
/*
class SampleError : public ExceptionObject
{
private:
  int m_ExtraInfo;
public:
  // Constructor and copy constructor:  Be sure parent constructors are called.
  SampleError () : ExceptionObject(), m_ExtraInfo(0) {}
  SampleError (const SampleError& orig) : ExceptionObject(orig)
  {
        m_ExtraInfo = orig.m_ExtraInfo;
  }

  // Assignment operator:  Be sure parent assignment operator is called.
  SampleError &operator= ( const SampleError &orig )
  { 
        ExceptionObject::operator= ( orig );
        m_ExtraInfo = orig.m_ExtraInfo;
        return *this;
  }

  // Get and set methods for the new field.
  void SetExtraInfo( const int &i ) { m_ExtraInfo = i; }
  int  GetExtraInfo() { return m_ExtraInfo; }
  
  // Print function is overloaded to display new information.  Parent
  // print function is used to print basic information.
  void Print(std::ostream& os)  const
  {
        ExceptionObject::Print( os );
        os << std::endl << "\tExtra info: " << m_ExtraInfo;
  }

  itkTypeMacro(SampleError, ExceptionObject);

};
*/

} // end namespace itk

#endif

