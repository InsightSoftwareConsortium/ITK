/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkComposeRGBImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>
#include "itkImage.h"
#include "itkComposeRGBImageFilter.h"



int itkComposeRGBImageFilterTest(int argc, char **argv)
{
  typedef unsigned char PixelType;
  typedef itk::Image< PixelType, 3 > InputImageType;

  typedef itk::ComposeRGBImageFilter< InputImageType >  FilterType;


  typedef InputImageType::RegionType RegionType;
  typedef InputImageType::SizeType    SizeType;

  FilterType::Pointer filter = FilterType::New();


}
