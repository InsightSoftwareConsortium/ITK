/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmUtil.cxx
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

#include "gdcmUtil.h"
#include "gdcmDebug.h"

#include <stdarg.h>  //only included in implementation file
#include <stdio.h>   //only included in implementation file
#include <fstream>

namespace gdcm 
{

/**
 * \ingroup Globals
 * \brief Provide a better 'c++' approach for sprintf
 * For example c code is:
 * sprintf(trash, "%04x|%04x", group , element);
 *
 * c++ is 
 * std::ostringstream buf;
 * buf << std::right << std::setw(4) << std::setfill('0') << std::hex
 *     << group << "|" << std::right << std::setw(4) << std::setfill('0') 
 *     << std::hex <<  element;
 * buf.str();
 */

std::string Util::Format(const char* format, ...)
{
   char buffer[2048];
   va_list args;
   va_start(args, format);
   vsprintf(buffer, format, args);  //might be a security flaw
   va_end(args); // Each invocation of va_start should be matched 
                 // by a corresponding invocation of va_end
                 // args is then 'undefined'
   return buffer;
}


/**
 * \ingroup Globals
 * \brief Because not available in C++ (?)
 */
void Util::Tokenize (const std::string& str,
                     std::vector<std::string>& tokens,
                     const std::string& delimiters)
{
   std::string::size_type lastPos = str.find_first_not_of(delimiters,0);
   std::string::size_type pos     = str.find_first_of    (delimiters,lastPos);
   while (std::string::npos != pos || std::string::npos != lastPos)
   {
      tokens.push_back(str.substr(lastPos, pos - lastPos));
      lastPos = str.find_first_not_of(delimiters, pos);
      pos     = str.find_first_of    (delimiters, lastPos);
   }
}

/**
 * \ingroup Globals
 * \brief Because not available in C++ (?)
 *        Counts the number of occurences of a substring within a string
 */
 
int Util::CountSubstring (const std::string& str,
                          const std::string& subStr)
{
   int count = 0;   // counts how many times it appears
   std::string::size_type x = 0;       // The index position in the string

   do
   {
      x = str.find(subStr,x);       // Find the substring
      if (x != std::string::npos)   // If present
      {
         count++;                  // increase the count
         x += subStr.length();     // Skip this word
      }
   }
   while (x != std::string::npos);  // Carry on until not present

   return count;
}

/**
 * \ingroup Globals
 * \brief  Weed out a string from the non-printable characters (in order
 *         to avoid corrupting the terminal of invocation when printing)
 * @param s string to remove non printable characters from
 */
std::string Util::CreateCleanString(std::string const & s)
{
   std::string str = s;

   for(unsigned int i=0;i<str.size();i++)
   {
      if(!isprint(str[i]))
      {
         str[i]='.';
      }
   }

   if(str.size()>0)
   {
      if(!isprint(s[str.size()-1]))
      {
         if(s[str.size()-1]==0)
         {
            str[str.size()-1]=' ';
         }
      }
   }

   return str;
}

/**
 * \ingroup Globals
 * \brief   Add a SEPARATOR to the end of the name is necessary
 * @param name file/directory name to normalize 
 */
std::string Util::NormalizePath(std::string const & pathname)
{
   const char SEPARATOR_X      = '/';
   const char SEPARATOR_WIN    = '\\';
   const std::string SEPARATOR = "/";
   std::string name = pathname;
   int size = name.size();

   if( name[size-1] != SEPARATOR_X && name[size-1] != SEPARATOR_WIN )
   {
      name += SEPARATOR;
   }
   return name;
}

/**
 * \ingroup Globals
 * \brief   Get the (directory) path from a full path file name
 * @param   fullName file/directory name to extract Path from
 */
std::string Util::GetPath(std::string const & fullName)
{
   std::string res = fullName;
   int pos1 = res.rfind("/");
   int pos2 = res.rfind("\\");
   if( pos1 > pos2)
   {
      res.resize(pos1);
   }
   else
   {
      res.resize(pos2);
   }

   return res;
}

/**
 * \ingroup Util
 * \brief   Get the (last) name of a full path file name
 * @param   fullName file/directory name to extract end name from
 */
std::string Util::GetName(std::string const & fullName)
{   
  std::string filename = fullName;

  std::string::size_type slash_pos = filename.rfind("/");
  std::string::size_type backslash_pos = filename.rfind("\\");
  slash_pos = slash_pos > backslash_pos ? slash_pos : backslash_pos;
  if(slash_pos != std::string::npos)
    {
    return filename.substr(slash_pos + 1);
    }
  else
    {
    return filename;
    }
} 

/**
 * \ingroup Util
 * \brief Create a /DICOM/ string:
 * It should a of even lenght (no odd length ever)
 * It can contains as many \0 as you want.
 * This function is similar to DicomString(const char*), 
 * except it doesn't take a lenght. 
 * It only pad with a null character if length is odd
 */
std::string Util::DicomString(const char* s)
{
   size_t l = strlen(s);
   if( l%2 )
   {
      l++;
   }
   std::string r(s, s+l);
   assert( !(r.size() % 2) );
   return r;
}

/**
 * \ingroup Util
 * \brief Create a /DICOM/ string:
 * It should a of even length (no odd length ever)
 * It can contains as many \0 as you want.
 */
std::string Util::DicomString(const char* s, size_t l)
{
   std::string r(s, s+l);
   assert( !(r.size() % 2) );
   return r;
}

template <class T>
std::ostream& binary_write(std::ostream& os, const T& val)
{
    return os.write(reinterpret_cast<const char*>(&val), sizeof val);
}

std::ostream& binary_write(std::ostream& os, const uint16_t& val)
{
#ifdef GDCM_WORDS_BIGENDIAN
    uint16_t swap;
    swap = ((( val << 8 ) & 0x0ff00 ) | (( val >> 8 ) & 0x00ff ) );
    return os.write(reinterpret_cast<const char*>(&swap), 2);
#else
    return os.write(reinterpret_cast<const char*>(&val), 2);
#endif //GDCM_WORDS_BIGENDIAN
}

std::ostream& binary_write(std::ostream& os, const uint32_t& val)
{
#ifdef GDCM_WORDS_BIGENDIAN
    uint32_t swap;
    swap = ( ((val<<24) & 0xff000000) | ((val<<8)  & 0x00ff0000) | 
             ((val>>8)  & 0x0000ff00) | ((val>>24) & 0x000000ff) );
    return os.write(reinterpret_cast<const char*>(&swap), 4);
#else
    return os.write(reinterpret_cast<const char*>(&val), 4);
#endif //GDCM_WORDS_BIGENDIAN
}

std::ostream& binary_write(std::ostream& os, const char* val)
{
    return os.write(val, strlen(val));
}

std::ostream& binary_write(std::ostream& os, std::string const & val)
{
    return os.write(val.c_str(), val.size());
}

} // end namespace gdcm

