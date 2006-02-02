/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmException.cxx
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

#include "gdcmException.h"

#include <typeinfo>
#include <stdlib.h> // for exit

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Exception
/**
 * \brief constructor
 * @param f f
 * @param msg msg 
 */
Exception::Exception(const std::string &f, const std::string &msg) throw()
#ifdef __GNUC__
   try
#endif
   : From(f), Error(msg) 
   {
   }
#ifdef __GNUC__
   catch(...) 
   {
      fatal("Exception::Exception(const std::string&, const std::string&, const std::string&)");
   }
#endif

/**
 * \brief fatal
 * @param from from
 */
void Exception::fatal(const char *from) throw() 
{
   try
   {
      std::cerr << "Fatal: exception received in " << from 
                << " while handling exception." << std::endl;
      exit(-1);
   }
   catch(...)
   {
      try
      {
         std::cerr << "Fatal: exception received in Exception::fatal while handling exception."
                   << std::endl;
         exit(-1);
      }
      catch(...)
      {
         exit(-1);
      }
   }
}

/**
 * \brief getName
 * @return string
 */
std::string Exception::getName() const throw()
{
   try
   {
#if defined(__GNUC__) && 0   // GNU C++ compiler class name demangling
      unsigned int nested = 1, i, nb;
      int offset;
      std::string one;

      std::string name;
      std::string iname = typeid(*this).name();
      if ( iname[0] == 'Q' )
      {
         nested = iname[1] - '0';
         iname = std::string(iname, 2, std::string::npos);
      }
      for(i = 0; i < nested; i++)
      {
         ::sscanf(iname.c_str(), "%u%n", &nb, &offset);
         iname = std::string(iname, offset, std::string::npos);
         name += std::string(iname, 0, nb);
         if ( i + 1 < nested) name += "::";
         iname = std::string(iname, nb, std::string::npos );
      }
      return name;
#else           // no class name demangling
      std::string name = typeid(*this).name();
      return name;
#endif
   }
   catch(...) 
   {
      fatal("Exception::getName(std::string &)");
      return "";
   }
}

/**
 * \brief Exception
 */
Exception::operator const char *() const throw() 
{
   return getName().c_str();
}

//-----------------------------------------------------------------------------
/**
 * \brief Exception::operator <<
 * @param os ostream to write to
 * @param e exception to raise
 */
std::ostream& operator<<(std::ostream &os, const Exception &e) 
{
   try 
   {
      os << "Exception " << e.getName() << " thrown: " << e.getError() << std::endl;
   }
   catch(...) 
   {
      Exception::fatal("operator<<(std::ostream &, const Exception&)");
   }
   return os;
}

} // end namespace gdcm
//-----------------------------------------------------------------------------
