/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelImageToLabelMapFilterTest.cxx
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
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include "itkLabelMap.h"
#include "itkLabelObject.h"
#include "itkLabelImageToLabelMapFilter.h"
#include "itkImage.h"

int itkLabelImageToLabelMapFilterTest(int argc, char * argv[])
{

  if( argc != 1 )
    {
    std::cerr << "usage: " << argv[0] << "" << std::endl;
    return EXIT_FAILURE;
    }

  const int dim = 2;
  
  typedef itk::LabelObject< unsigned long, dim > LabelObjectType;
  typedef LabelObjectType::IndexType             IndexType;
  typedef itk::LabelMap< LabelObjectType >       LabelMapType;
  typedef LabelMapType::RegionType               RegionType;
  typedef LabelMapType::SizeType                 SizeType;
  typedef LabelMapType::LabelObjectVectorType    LabelObjectVectorType;
  typedef LabelMapType::LabelVectorType          LabelVectorType;
  typedef LabelMapType::LabelObjectContainerType LabelObjectContainerType;
  typedef itk::Image< unsigned char, dim >       ImageType;

  typedef itk::LabelImageToLabelMapFilter<ImageType, LabelMapType> LabelImageToLabelMapFilterType;
  
  ImageType::Pointer image = ImageType::New();

  SizeType sizeIn;
  sizeIn[0] = 11;
  sizeIn[1] = 11;
  image->SetRegions( sizeIn );
  image->Allocate();
  image->FillBuffer( 0 );

  IndexType idxHorizontal;
  idxHorizontal[1] = 5;

  IndexType idxVertical;
  idxVertical[0] = 5;
  for (int ctr=0; ctr<11; ctr++)
    {
    idxHorizontal[0] = ctr;
    idxVertical[1] = ctr;
    image->SetPixel( idxHorizontal, 1);
    image->SetPixel( idxVertical, 1 );
    }

  LabelImageToLabelMapFilterType::Pointer conversion = LabelImageToLabelMapFilterType::New();
  conversion->SetInput( image );
  conversion->Update( );

  LabelMapType::Pointer map;
  map = conversion->GetOutput();

  map->Print(std::cout);

  std::cout << "Printing out map." << std::endl;
  for (int ctrI=0; ctrI<11; ctrI++)
    {
    for (int ctrJ=0; ctrJ<11; ctrJ++)
      {
      IndexType index;
      index[0] = ctrI;
      index[1] = ctrJ;
      unsigned long val;
      val = map->GetPixel(index);
      std::cout << "Pixel[" << ctrI << "," << ctrJ << "]: " << val << std::endl;
      if ( (ctrI == 5) || (ctrJ==5) )
        {
        itkAssertOrThrowMacro( (val == 1), "Error in Label Image (foreground).");
        }
      else
        {
        itkAssertOrThrowMacro( (val == 0), "Error in Label Image (background).");
        }
      }
    }
  std::cout << "End - Printing out map." << std::endl << std::endl;

  conversion->Print( std::cout );

  return EXIT_SUCCESS;
}
