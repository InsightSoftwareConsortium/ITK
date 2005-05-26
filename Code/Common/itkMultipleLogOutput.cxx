/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleLogOutput.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#if defined(_MSC_VER)
   //Warning about: identifier was truncated to '255' characters in the debug information (MVC6.0 Debug)
#pragma warning( disable : 4786 )
#endif

#include "itkMultipleLogOutput.h"

namespace itk
{


MultipleLogOutput::MultipleLogOutput()
{
  this->m_Output.clear();
}



MultipleLogOutput::~MultipleLogOutput()
{
//  this->Flush();
}



/** Adds an output stream to the MultipleLogOutput for writing. */
void 
MultipleLogOutput::AddLogOutput( OutputType * output )
{
  this->m_Output.insert( output ); // insert the address
}



/** The Flush method flushes all the streams. */  
void 
MultipleLogOutput::Flush( void )
{
  ContainerType::iterator itr = m_Output.begin();
  ContainerType::iterator end = m_Output.end();

  while( itr != end )
    {
    (*itr)->Flush(); 
    ++itr;
    }
}



/** Write to multiple outputs */
void MultipleLogOutput::Write( double timestamp )
{
  ContainerType::iterator itr = m_Output.begin();
  ContainerType::iterator end = m_Output.end();

  while( itr != end )
    {
    (*itr)->Write( timestamp );
    ++itr;
    }
}



/** Write to multiple outputs */
void MultipleLogOutput::Write( const std::string & content )
{
  ContainerType::iterator itr = m_Output.begin();
  ContainerType::iterator end = m_Output.end();

  while( itr != end )
    {
    (*itr)->Write( content );
    ++itr;
    }
}



/** Write to a buffer */
void MultipleLogOutput::Write( const std::string & content, double timestamp )
{
  ContainerType::iterator itr = m_Output.begin();
  ContainerType::iterator end = m_Output.end();

  while( itr != end )
    {
    (*itr)->Write( content, timestamp );
    ++itr;
    }
}


}

