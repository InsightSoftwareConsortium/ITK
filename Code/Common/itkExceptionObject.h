/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkExceptionObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkExceptionObject_h
#define __itkExceptionObject_h

#include <string>
#include "itkObject.h"

namespace itk
{

class ExceptionObject
{
public:
  /**
   * Constructor and copy constructor.  Note that these functions will be
   * called when children are instantiated..
   */
  ExceptionObject() { };

  /**
   * Virtual destructor needed 
   */
  virtual ~ExceptionObject() {};

  ExceptionObject( const ExceptionObject &orig )
  {
    this->m_Location    = orig.m_Location;
    this->m_Description = orig.m_Description;
  }
  
  /**
   * Assignment and equivalence operators.
   */
  ExceptionObject &operator= ( const ExceptionObject &orig )
  {
    this->m_Location    = orig.m_Location;
    this->m_Description = orig.m_Description;
    return *this;
  }
  
  virtual bool operator==( const ExceptionObject &orig )
  {
    if ( this->m_Location    == orig.m_Location &&
         this->m_Description == orig.m_Description ) 
      {
      return true;
      }
    else 
      {
      return false;
      }
  }
          
  /**
   * Print exception information.  This method can be overridden by specific
   * exception subtypes.  The default is to print out the location where
   * the exception was first thrown and any description provided by the
   * ``thrower''.
   */
  virtual void Print(std::ostream& os) const;

  /**
   * Methods to get and set the Location and Description fields. The Set
   * methods are overloaded to support both std::string and const char 
   * array types. Get methods return const char arrays.
   */
  virtual void SetLocation(const std::string& s)    { m_Location = s;    }
  virtual void SetDescription(const std::string& s) { m_Description = s; }
  virtual void SetLocation(const char * s)          { m_Location = s;    }
  virtual void SetDescription (const char *s)       { m_Description = s; }
  virtual const char *GetLocation()    const { return m_Location.c_str();    }
  virtual const char *GetDescription() const { return m_Description.c_str(); }
  
  /**
   * Return the name of the class.
   */
  static const char *GetClassName() {return "ExceptionObject";}
  
private:
  /**
   * Exception data.  Location of the error and description of the error.
   */
  std::string m_Location;
  std::string m_Description;
 
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

class RangeError : public ExceptionObject
{
public:
  itkTypeMacro(RangeError, ExceptionObject);
};

class InvalidArgumentError : public ExceptionObject
{
public:
  itkTypeMacro(InvalidArgumentError, ExceptionObject);
};

class IncompatibleOperandsError : public ExceptionObject
{
public:
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
        this->m_ExtraInfo = orig.m_ExtraInfo;
  }

  // Assignment operator:  Be sure parent assignment operator is called.
  SampleError &operator= ( const SampleError &orig )
  { 
        ExceptionObject::operator= ( orig );
        this->m_ExtraInfo = orig.m_ExtraInfo;
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

} // namespace itk

#endif

