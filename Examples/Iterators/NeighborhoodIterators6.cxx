/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    NeighborhoodIterators6.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkFastMarchingImageFilter.h"
#include "itkNumericTraits.h"
#include "itkRandomImageSource.h"
#include "itkAddImageFilter.h"

// Software Guide : BeginLatex
//
// 
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet

// Software Guide : EndCodeSnippet

int main( int argc, char ** argv )
{
  if ( argc < 4 )
    {
    std::cerr << "Missing parameters. " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
              << " outputImageFile startX startY"
              << std::endl;
    return -1;
    }

  typedef float PixelType;
  typedef itk::Image< PixelType, 2 > ImageType;
  typedef itk::NeighborhoodIterator< ImageType > NeighborhoodIteratorType;

  typedef itk::FastMarchingImageFilter<ImageType, ImageType>  FastMarchingFilterType;

  FastMarchingFilterType::Pointer fastMarching = FastMarchingFilterType::New();

  typedef FastMarchingFilterType::NodeContainer  NodeContainer;
  typedef FastMarchingFilterType::NodeType       NodeType;

  NodeContainer::Pointer seeds = NodeContainer::New();
 
  ImageType::IndexType  seedPosition;
  
  seedPosition[0] = 128;
  seedPosition[1] = 128;
  const double initialDistance = 1.0;

  NodeType node;

  const double seedValue = - initialDistance;

  ImageType::SizeType size = {{256, 256}};
  
  node.SetValue( seedValue );
  node.SetIndex( seedPosition );
  seeds->Initialize();
  seeds->InsertElement( 0, node );

  fastMarching->SetTrialPoints(  seeds  );
  fastMarching->SetSpeedConstant( 1.0 );

  itk::AddImageFilter<ImageType, ImageType, ImageType>::Pointer adder
    = itk::AddImageFilter<ImageType, ImageType, ImageType>::New();
  itk::RandomImageSource<ImageType>::Pointer noise
    = itk::RandomImageSource<ImageType>::New();

  noise->SetSize(size.m_Size);
  noise->SetMin(-.7);
  noise->SetMax(.8);
  adder->SetInput1(noise->GetOutput());
  adder->SetInput2(fastMarching->GetOutput());
  
  try
    {
    fastMarching->SetOutputSize( size );
    fastMarching->Update();

    adder->Update();

    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }

  ImageType::Pointer input = adder->GetOutput();

  ImageType::IndexType index;
  index[0] = ::atoi(argv[2]);
  index[1] = ::atoi(argv[3]);
  
  NeighborhoodIteratorType::RadiusType radius;
  radius.Fill(1);
  NeighborhoodIteratorType it(radius, input, input->GetRequestedRegion());

  it.SetLocation(index);

  bool flag = true;
  while ( flag == true )
    {
    NeighborhoodIteratorType::OffsetType nextMove;
    nextMove.Fill(0);

    flag = false;
    
    PixelType min = it.GetCenterPixel();
    for (unsigned i = 0; i < it.Size(); i++)
      {
      if ( it.GetPixel(i) < min )
        {
        min = it.GetPixel(i);
        nextMove = it.GetOffset(i);
        flag = true;
        }
      }
    it.SetCenterPixel( 255.0 );
    it += nextMove;
    }


  
    
// Software Guide : BeginLatex
//
// The output is rescaled and written as in the previous example.  Filter the
// BLAH BLAH image in the $X$ direction give the same result as in Figure~BLAH BLAH
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet


  typedef unsigned char WritePixelType;
  typedef itk::Image< WritePixelType, 2 > WriteImageType;
  typedef itk::ImageFileWriter< WriteImageType > WriterType;
  
  typedef itk::RescaleIntensityImageFilter< ImageType,
    WriteImageType > RescaleFilterType;
  
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput( input );
  
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[1] );
  writer->SetInput( rescaler->GetOutput() );
  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return -1;
    }
// Software Guide : EndCodeSnippet

  return 0;
}
