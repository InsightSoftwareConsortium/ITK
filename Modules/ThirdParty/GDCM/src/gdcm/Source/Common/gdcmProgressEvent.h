/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMPROGRESSEVENT_H
#define GDCMPROGRESSEVENT_H

#include "gdcmEvent.h"
#include "gdcmTag.h"

namespace gdcm
{

/**
 * \brief ProgressEvent
 * \details Special type of event triggered during
 *
 * \see AnyEvent
 */
class ProgressEvent : public AnyEvent
{
public:
  typedef ProgressEvent Self;
  typedef AnyEvent Superclass;
  ProgressEvent(double p = 0):m_Progress(p) {}
  ~ProgressEvent() override = default;

  ProgressEvent(const Self&s) : AnyEvent(s){};
  void operator=(const Self&) = delete;

  const char * GetEventName() const override { return "ProgressEvent"; }
  bool CheckEvent(const ::gdcm::Event* e) const override
    { return dynamic_cast<const Self*>(e) ? true : false; }
  ::gdcm::Event* MakeObject() const override
    { return new Self; }

  void SetProgress(double p) { m_Progress = p; }
  double GetProgress() const { return m_Progress; }
private:
  double m_Progress;
};


} // end namespace gdcm

#endif //GDCMPROGRESSEVENT_H
