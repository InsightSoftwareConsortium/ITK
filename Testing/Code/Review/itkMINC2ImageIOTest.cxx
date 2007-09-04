/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMINC2ImageIOTest.cxx
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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"

#include "itkImageIOFactory.h"
#include "itkMINC2ImageIOFactory.h"
#include "itkMINC2ImageIO.h"
#include <stdio.h>
#include "itkMetaDataObject.h"
#include "itkIOCommon.h"

#include "itkResampleImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkExceptionObject.h"
#include "itkVector.h"
#include "itkAffineTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkIdentityTransform.h"
int itkMINC2ImageIOTest( int ac, char* av[] )
{
  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0] << " Input\n";
    return EXIT_FAILURE;
    }
 
  std::cerr << "This is a test for MINC2!!\n";
  
  typedef unsigned short PixelType;

  typedef itk::Image<PixelType, 3> myImage;
  typedef itk::Image<float, 3>     ImageType;

  myImage::Pointer imagefixed;

  //myImage::Pointer imagemoving;

  itk::ImageFileReader<myImage>::Pointer readerfixed
                                  = itk::ImageFileReader<myImage>::New();
  // itk::ImageFileReader<myImage>::Pointer readermoving
  // = itk::ImageFileReader<myImage>::New();
  
  readerfixed->SetFileName(av[1]);
  //readermoving->SetFileName(av[2]);

  typedef itk::MINC2ImageIO        ImageIOType;
  ImageIOType::Pointer minc2ImageIOfixed = ImageIOType::New();
  //ImageIOType::Pointer minc2ImageIOmoving = ImageIOType::New();
  readerfixed->SetImageIO( minc2ImageIOfixed );
  //readermoving->SetImageIO( minc2ImageIOmoving );

   // Rewrite the image in MINC2.0 format

  typedef itk::ImageFileWriter< myImage >  Writer1Type;

  Writer1Type::Pointer writer1 = Writer1Type::New();

  writer1->SetFileName( "testitk.mnc" );

 
  //writer1->SetInput( readerfixed->GetOutput() );
  writer1->SetImageIO( minc2ImageIOfixed );

  try
    {
    readerfixed->Update();
    // readermoving->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }
  imagefixed = readerfixed->GetOutput();
  //imagemoving = readermoving->GetOutput();

  // do image resampling

  typedef itk::AffineTransform<double,3> AffineTransformType;
 AffineTransformType::Pointer aff3 = AffineTransformType::New();
 itk::Vector<double,3> axis;
    axis[0] = 1;
    axis[1] = 1;
    axis[2] = 1;
    aff3->Rotate3D(axis,0 , 0);
//AffineTransformType::OutputVectorType translation;
//translation[0]= 3;
//translation[1]= -3;
//translation[2]= 3;
//aff3->Translate(translation);
//aff3->Scale(1.2);
  
  typedef itk::LinearInterpolateImageFunction<myImage,double> InterpolatorType;
  //typedef itk::NearestNeighborInterpolateImageFunction<myImage,double> InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  typedef itk::ResampleImageFilter<myImage,myImage> ResamplerType;
  ResamplerType::Pointer resampler = ResamplerType::New();
  interpolator->SetInputImage(readerfixed->GetOutput());
  resampler->SetInput( readerfixed->GetOutput() );
  resampler->SetTransform( aff3);
  resampler->SetInterpolator(interpolator);
  resampler->SetSize( imagefixed->GetLargestPossibleRegion().GetSize() );
  std::cout << resampler->GetSize() << std::endl;
  resampler->SetOutputOrigin(imagefixed->GetOrigin() );
  std::cout << resampler->GetOutputOrigin() << std::endl;
  resampler->SetOutputSpacing(imagefixed->GetSpacing() );
  std::cout << resampler->GetOutputSpacing() << std::endl;
  resampler->SetOutputDirection(imagefixed->GetDirection());
  std::cout << resampler->GetOutputDirection() << std::endl;
  resampler->SetDefaultPixelValue( 100 );
  //std::cout <<resampler->GetOutputStartIndex() << std::endl;
  //resampler->Update();
  std::cout << "done resampling" << std::endl;
  //resampler->GenerateOutputInformation();

 writer1->SetInput( resampler->GetOutput() );

 
   try
    {
    writer1->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
