/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmapsFileParser.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkSmapsFileParser.h"

namespace itk
{

/* SmapsRecord implementation */

void SmapsRecord::Reset(void)
{
  m_Tokens.clear();
  m_HeaderName.clear();
}

std::istream &operator>>(std::istream &in,SmapsRecord &record)
{
  record.Reset();

  try 
    {
    // Get Header line
    std::string headerline;
    std::getline( in, headerline);

    if (headerline.empty())
      {
      return in;
      }

    // Get name
    std::istringstream stream(headerline);
    std::string address, perms, offset, device;  
    int inode=-1;
    // the header is defined with the following expression: "address permissions offset device inode [name]"
    stream>>address;
    if (!stream.good()) itkGenericExceptionMacro( << "The smaps header is corrupted" );
    stream>>perms;
    if (!stream.good()) itkGenericExceptionMacro( << "The smaps header is corrupted" );
    stream>>offset;
    if (!stream.good()) itkGenericExceptionMacro( << "The smaps header is corrupted" );
    stream>>device;
    if (!stream.good()) itkGenericExceptionMacro( << "The smaps header is corrupted" );
    stream>>inode;
    // name can be empty
    if (!stream.eof())
      {
      std::getline(stream, record.m_HeaderName);
      }
    
    std::string token;
    int lastPos = in.tellg();
    // a token is defined with the following expression: "token: N kB" 
    while ( std::getline(in,token,':').good() )
      {
      //make sure it is a token and not a new record. A token doesn't contains space character
      if ( token.find(' ') != std::string::npos )
        {
        in.seekg (lastPos, std::ios::beg);
        break;
        }
      //fill the token with the memory usage N in kB
      in>>record.m_Tokens[token];
      std::getline(in,token);
      if ( token != " kB" || !in.good()) itkGenericExceptionMacro( << "The smaps header is corrupted" );
      lastPos = in.tellg();
      }
    }
  catch (ExceptionObject excp)
    {
    record.Reset();
    // propagate the exception
    itkGenericExceptionMacro( << "The smaps header is corrupted" );
    }
  return in;
}

} // end namespace itk
