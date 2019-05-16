/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmEvent.h"

namespace gdcm
{

Event::Event()
= default;

Event::Event(const Event&)
= default;

Event::~Event()
= default;

void Event::Print(std::ostream& os) const
{
  os << GetEventName();
}

} // end namespace gdcm
