/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeStamp.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkTimeStamp.h"

//-------------------------------------------------------------------------
itkTimeStamp *itkTimeStamp::New()
{
  return new itkTimeStamp;
}

// Initialize static member
//
void itkTimeStamp::Modified()
{
  static unsigned long itkTimeStampTime = 0; 
  m_ModifiedTime = ++itkTimeStampTime;
}










