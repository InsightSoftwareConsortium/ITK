/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmException.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
                                                                                
  Copyright (c) CREATIS (Centre de Recherche et d'Applications en Traitement de
  l'Image). All rights reserved. See Doc/License.txt or
  http://www.creatis.insa-lyon.fr/Public/Gdcm/License.html for details.
                                                                                
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
                                                                                
=========================================================================*/

#ifndef GDCM_EXCEPTION_H
#define GDCM_EXCEPTION_H

#include "gdcmCommon.h"

#include <string>
#include <iostream>
#include <exception>

namespace gdcm 
{

//-----------------------------------------------------------------------------
/**
 * \brief Any exception thrown in the gdcm library
 */
class GDCM_EXPORT Exception : public std::exception
{
public:
   /*
    * \brief Builds an exception with minimal information: name of the thrower
    *        method and error message
    * @param from name of the thrower
    * @param error error description string
    */
   explicit Exception(const std::string &from, const std::string &error = "")
      throw();

   /**
    * \brief virtual destructor makes this class dynamic
    */
   virtual ~Exception() throw() {};

   /// exception caught within exception class: print error message and die
   static void fatal(const char *from) throw();

   /// returns error message
   const std::string &getError() const throw() {
      return Error;
   }

   /// try to discover this (dynamic) class name
   virtual std::string getName() const throw();

   /// returns exception name string (overloads std::exception::what)
   virtual const char *what() const throw() {
      return (const char *) *this;
   }

   /// returns exception name string
   operator const char *() const throw();

   friend std::ostream &operator<<(std::ostream &os, const Exception &e);

protected:
   /// error message part 1
   std::string From;
   /// error message part 2
   std::string Error;
};


//-----------------------------------------------------------------------------
/**
 * \brief File error exception thrown in the gdcm library
 */
class GDCM_EXPORT FileError : public Exception
{
public:
   /**
    * \brief Builds an file-related exception with minimal information: name of
    *               the thrower method and error message
    * @param from name of the thrower
    * @param error error description string
    */
   explicit FileError(const std::string &from,
                          const std::string &error = "File error")
      throw() : Exception(from, error) { }
};


//-----------------------------------------------------------------------------
/**
 * \brief Unexpected file format exception
 */
class GDCM_EXPORT FormatUnexpected : public Exception
{
public:
   /// \brief Builds a file-related exception with minimal information:
   /// name of the thrower method and error message
   /// @param from name of the thrower
   /// @param error error description string
   explicit FormatUnexpected(const std::string &from,
                             const std::string &error = "Unexpected file format")
      throw() : Exception( from, error ) { }
};

//-----------------------------------------------------------------------------
/**
 * \brief Invalid file format exception
 */
class GDCM_EXPORT FormatError : public FormatUnexpected
{
public:
   /// \brief Builds a file-related exception with minimal information:
   /// name of the thrower method and error message
   /// @param from name of the thrower
   /// @param error error description string
   explicit FormatError(const std::string &from,
                        const std::string &error = "Invalid file format")
      throw() : FormatUnexpected( from, error ) { }
};

//-----------------------------------------------------------------------------
/* prints exception stack on output stream
 * @param os output stream
 * @param e exception to print
 * @returns output stream os
 */
std::ostream& operator<<(std::ostream &os, const Exception &e);

} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif // GDCM_EXCEPTION_H
