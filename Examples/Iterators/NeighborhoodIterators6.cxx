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
// This example takes advantage of the random access capabilities of the
// neighborhood iterators to move through an image in a non-predetermined path.
// Previously, we have been using the neighborhood iterator to visit each pixel
// in a region, in turn, without regard to its location in the image.  For some
// algorithms, however, it may be desirable to move through an image in a path
// that is determined by some metric on the local neighborhood, that is, go to
// the next neighborhood position based on the values at the current position.
// Algorithms that fit this model include connected component analysis and
// flood-fill algorithms.  This example will use a neighborhood iterator to
// find a local minimum.
//
// Given a seed point, we can search the neighborhood of that point and pick
// the smallest value $m$.  While $m$ is not at the center of our
// current neighborhood, we move in the direction of $m$ and repeat the analysis.
// Eventually we discover a local minimum and stop.  This algorithm is made
// trivially simple in ND using an ITK neighborhood iterator.
//
// To illustrate the process, we create an image that descends everywhere to a
// single minimum, a distance transform from a point.  The details of creating
// this distance transform are not relevant to the discussion of neighborhood
// iterators, but can be found in the source code of this example. Some noise
// has been added to the distance transform image for additional interest.
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

// Software Guide : BeginLatex
//
// Once the distance transform has been created, we proceed directly to the
// local minimum algorithm.  The variable \code{input} is now the pointer to
// the distance transform image.
//
// The first step is to define a seed point based on command line arguments.
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  ImageType::IndexType index;
  index[0] = ::atoi(argv[2]);
  index[1] = ::atoi(argv[3]);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// Next we create the neighborhood iterator and position it at the seed point.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  NeighborhoodIteratorType::RadiusType radius;
  radius.Fill(1);
  NeighborhoodIteratorType it(radius, input, input->GetRequestedRegion());

  it.SetLocation(index);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// 
// Searching for the local minimum involves finding the minimum in the current
// neighborhood, then shifting the neighborhood in the direction of that
// minimum.  The \code{for} loop below records the \code{itk::Offset} of the
// minimum neighborhood pixel.  The neighborhood iterator is then moved using
// that offset.  When a local minimum is detected, \code{flag} will remain
// false and the \code{while} loop will exit.  Note that this code is
// valid for an image of any dimensionality.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
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
// Software Guide : EndCodeSnippet

  
    
// Software Guide : BeginLatex
//
// Figure~\ref{fig:NeighborhoodExample6} records the results of the algorithm
// for several seed points.  The effect of the additive noise is visible in
// the perturbations of the descent paths.
//
// \begin{figure} \centering
// \includegraphics[width=0.3\textwidth]{NeighborhoodIterators6a.eps}
// \includegraphics[width=0.3\textwidth]{NeighborhoodIterators6b.eps}
// \includegraphics[width=0.3\textwidth]{NeighborhoodIterators6c.eps}
// \itkcaption[Finding local minima]{Paths traversed by the neighborhood
// iterator to find the local miminim in the image from several different seed
// points.  The minimum is at the center of the image.  The effects of noise in
// the image are seen as small perturbations in the paths.}
// \protect\label{fig:NeighborhoodExamples6}
// \end{figure}
//
// Software Guide : EndLatex

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
  return 0;
}
