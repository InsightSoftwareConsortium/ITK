/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkSmartPointerConstTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>

#include "itkLightObject.h"

int main(void)
{
//  itk::LightObject::ConstPointer lo = itk::LightObject::New();
  itk::LightObject::ConstPointer lo = itk::LightObject::New().GetPointer();
  
  return 0;
}
