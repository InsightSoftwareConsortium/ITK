/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMDATAEVENT_H
#define GDCMDATAEVENT_H

#include "gdcmEvent.h"

namespace gdcm
{

/**
 * \brief DataEvent
 */
class DataEvent : public AnyEvent
{
public:
  typedef DataEvent Self;
  typedef AnyEvent Superclass;
  DataEvent(const char *bytes = nullptr, size_t len = 0):Bytes(bytes),Length(len) {}
  ~DataEvent() override = default;
  DataEvent(const Self&s) : AnyEvent(s){};
  void operator=(const Self&) = delete;

  const char * GetEventName() const override { return "DataEvent"; }
  bool CheckEvent(const ::gdcm::Event* e) const override
  { return (dynamic_cast<const Self*>(e) == nullptr ? false : true) ; }
  ::gdcm::Event* MakeObject() const override
    { return new Self; }

  void SetData(const char *bytes, size_t len) {
    Bytes = bytes;
    Length = len;
  }
  size_t GetDataLength() const { return Length; }
  const char *GetData() const { return Bytes; }

  //std::string GetValueAsString() const { return; }

private:
  const char *Bytes;
  size_t Length;
};


} // end namespace gdcm

#endif //GDCMDATAEVENT_H
