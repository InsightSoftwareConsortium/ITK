/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkAnalyzeImageIOTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImage.h"

#include "itkImageRegionIterator.h"
#include <iostream>

#include "itkLaplacianImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkImageIOFactory.h"
#include "itkMetaImageIOFactory.h"
#include "itkPNGImageIOFactory.h"
#include "itkMetaImageIO.h"
#include "itkPNGImageIO.h"
#include "itkAnalyzeImageIOFactory.h"
#include "itkAnalyzeImageIO.h"


const unsigned char RPI=16;        /*Bit pattern 0 0 0  10000*/
const unsigned char LEFT=128;      /*Bit pattern 1 0 0  00000*/
const unsigned char ANTERIOR=64;   /*Bit pattern 0 1 0  00000*/
const unsigned char SUPERIOR=32;   /*Bit pattern 0 0 1  00000*/

typedef itk::Image<unsigned char, 3> ImageType ;
typedef itk::ImageFileReader< ImageType > ImageReaderType ;

static int makeImage(const char *filename,  
                     ImageType::Pointer &img)
{

  //Allocate Images
  enum { ImageDimension = ImageType::ImageDimension };

  const ImageType::SizeType size = {{10,10,10}};
  const ImageType::IndexType index = {{0,0,0}};
  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  img = ImageType::New();
  img->SetLargestPossibleRegion( region );
  img->SetBufferedRegion( region );
  img->SetRequestedRegion( region );
  img->Allocate();

  { //Fill in entire image 
      itk::ImageRegionIterator<ImageType> ri(img,region);
      while(!ri.IsAtEnd())
      {
          ri.Set( RPI );
          ++ri;
      }
  }
  { //Fill in left half
      const ImageType::IndexType RPIindex = {{0,0,0}};
      const ImageType::SizeType RPIsize = {{5,10,10}};
      ImageType::RegionType RPIregion;
      RPIregion.SetSize( RPIsize );
      RPIregion.SetIndex( RPIindex );
      itk::ImageRegionIterator<ImageType > RPIiterator(img,RPIregion);
      while(!RPIiterator.IsAtEnd())
      {
          RPIiterator.Set( RPIiterator.Get()|LEFT );
          ++RPIiterator;
      }
  }
  { //Fill in anterior half
      const ImageType::IndexType RPIindex = {{0,5,0}};
      const ImageType::SizeType RPIsize = {{10,5,10}};
      ImageType::RegionType RPIregion;
      RPIregion.SetSize( RPIsize );
      RPIregion.SetIndex( RPIindex );
      itk::ImageRegionIterator<ImageType > RPIiterator(img,RPIregion);
      while(!RPIiterator.IsAtEnd())
      {
          RPIiterator.Set( RPIiterator.Get()|ANTERIOR );
          ++RPIiterator;
      }
  }
  { //Fill in superior half
      const ImageType::IndexType RPIindex = {{0,0,5}};
      const ImageType::SizeType RPIsize = {{10,10,5}};
      ImageType::RegionType RPIregion;
      RPIregion.SetSize( RPIsize );
      RPIregion.SetIndex( RPIindex );
      itk::ImageRegionIterator<ImageType > RPIiterator(img,RPIregion);
      while(!RPIiterator.IsAtEnd())
      {
          RPIiterator.Set( RPIiterator.Get()|SUPERIOR );
          ++RPIiterator;
      }
  }

  //itk::AnalyzeImageIOFactory::RegisterOneFactory();

  typedef itk::ImageFileWriter< ImageType >      ImageWriterType;
  itk::ImageFileWriter< ImageType >::Pointer ImageWriterPointer = 
    ImageWriterType::New();

    //Set the output filename
    ImageWriterPointer->SetFileName(filename);

    //Attach input image to the writer.
    ImageWriterPointer->SetInput( img );
    //Determine file type and instantiate appropriate ImageIO class if not
    //explicitly stated with SetImageIO, then write to disk.
    try {
        ImageWriterPointer->Write();
    }
    catch ( itk::ExceptionObject & ex )
    {
        std::string message;
        message = "Problem found while writing image ";
        message += filename;
        message += "\n";
        message += ex.GetLocation();
        message += "\n";
        message += ex.GetDescription();
        std::cerr << message << std::endl;
        return -1;
    }

  return 0;
}

int itkAnalyzeImageIOTest(int ac, char** av)
{
  const char fname[] = "test.hdr";
  ImageType::Pointer img;
  ImageType::Pointer input;

  if(makeImage(fname,img) != 0)
    return -1;

  //typedef itk::ImageFileReader< ImageType > ImageReaderType ;
  itk::ImageFileReader<ImageType>::Pointer imageReader =
    itk::ImageFileReader<ImageType>::New();
  try
    {
      imageReader->SetFileName(fname) ;
      imageReader->Update() ;
      input = imageReader->GetOutput() ;
    }
  catch (itk::ExceptionObject e)
    {
      e.Print(std::cerr) ;
      return -1;
    }
  
  return 0;
}
