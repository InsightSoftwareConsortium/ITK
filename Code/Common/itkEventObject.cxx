/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEventObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkEventObject.h"
#include <typeinfo>

namespace itk
{

void
EventObject
::Print(std::ostream& os) const
{
  Indent indent;

  this->PrintHeader(os,0); 
  this->PrintSelf(os, indent.GetNextIndent());
  this->PrintTrailer(os,0);
}  
  



bool 
EventObject
::operator==( const EventObject &orig ) const
{
  if( typeid( *this ) == typeid( orig ) )
    {
    return true;
    }
  else 
    {
    return false;
    }
}
 


bool 
EventObject
::IsA( const EventObject &orig ) const
{
  if( typeid( *this ) == typeid( orig ) )
    {
    return true;
    }
  else 
    {
    return false;
    }
}
 

const char * 
EventObject
::GetEventName(void) const {
  return "EventObject";
}


/*
 *  This method will have to look in the map
 *  to select the event identified by the 
 *  string given here as parameter.
 */
EventObject * 
EventObject
::CreateEventFromString(const char *)
{
  return new NoEvent;
}





/**
 * Define a default print header for all objects.
 */
void 
EventObject
::PrintHeader(std::ostream& os, Indent indent) const
{
  os << std::endl;
  os << indent << "itk::" << this->GetNameOfClass() << " (" << this << ")\n";
}


/**
 * Define a default print trailer for all objects.
 */
void 
EventObject
::PrintTrailer(std::ostream& os, Indent indent) const
{
  os << indent << std::endl;
}

void
EventObject
::PrintSelf(std::ostream& os, Indent indent) const
{
}

} // end namespace itk
