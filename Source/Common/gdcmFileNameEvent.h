/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMFILENAMEEVENT_H
#define GDCMFILENAMEEVENT_H

#include "gdcmEvent.h"
#include "gdcmTag.h"

namespace gdcm
{

/**
 * \brief FileNameEvent
 * \details Special type of event triggered during processing of FileSet
 *
 * \see AnyEvent
 */
class FileNameEvent : public AnyEvent
{
public:
  typedef FileNameEvent Self;
  typedef AnyEvent Superclass;
  FileNameEvent(const char *s = ""):m_FileName(s) {}
  ~FileNameEvent() override = default;

  FileNameEvent(const Self&s) : AnyEvent(s){};
  void operator=(const Self&) = delete;
  

  const char * GetEventName() const override { return "FileNameEvent"; }
  bool CheckEvent(const ::gdcm::Event* e) const override
    { return dynamic_cast<const Self*>(e) ? true : false; }
  ::gdcm::Event* MakeObject() const override
    { return new Self; }

  void SetFileName(const char *f) { m_FileName = f; }
  const char *GetFileName() const { return m_FileName.c_str(); }
private:
  std::string m_FileName;
};


} // end namespace gdcm

#endif //GDCMFILENAMEEVENT_H
