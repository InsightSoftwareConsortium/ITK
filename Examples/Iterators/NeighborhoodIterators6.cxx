/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkFastMarchingImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkAddImageFilter.h"

// Software Guide : BeginLatex
//
// Some image processing routines do not need to visit every pixel in an
// image. Flood-fill and connected-component algorithms, for example, only
// visit pixels that are locally connected to one another.  Algorithms
// such as these can be efficiently written using the random access
// capabilities of the neighborhood iterator.
//
// The following example finds local minima.  Given a seed point, we can search
// the neighborhood of that point and pick the smallest value $m$.  While $m$
// is not at the center of our current neighborhood, we move in the direction
// of $m$ and repeat the analysis.  Eventually we discover a local minimum and
// stop.  This algorithm is made trivially simple in ND using an ITK
// neighborhood iterator.
//
// To illustrate the process, we create an image that descends everywhere to a
// single minimum:  a positive distance transform to a point.  The details of
// creating the distance transform are not relevant to the discussion of
// neighborhood iterators, but can be found in the source code of this
// example. Some noise has been added to the distance transform image for
// additional interest.
//
// Software Guide : EndLatex

int main( int argc, char ** argv )
{
  if ( argc < 4 )
    {
    std::cerr << "Missing parameters. " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
              << " outputImageFile startX startY"
              << std::endl;
    return EXIT_FAILURE;
    }

  typedef float                                  PixelType;
  typedef itk::Image< PixelType, 2 >             ImageType;
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
    return EXIT_FAILURE;
    }

  ImageType::Pointer input = adder->GetOutput();

// Software Guide : BeginLatex
//
// The variable \code{input} is the pointer to the distance transform image.
// The local minimum algorithm is initialized with a seed point read from the
// command line.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageType::IndexType index;
  index[0] = ::atoi(argv[2]);
  index[1] = ::atoi(argv[3]);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next we create the neighborhood iterator and position it at the seed point.
//
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
// minimum.  The \code{for} loop below records the \doxygen{Offset} of the
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
// Figure~\ref{fig:NeighborhoodExample6} shows the results of the algorithm
// for several seed points.  The white line is the path of the iterator from
// the seed point to the minimum in the center of the image.  The effect of the
// additive noise is visible as the small perturbations in the paths.
//
// \begin{figure} \centering
// \includegraphics[width=0.3\textwidth]{NeighborhoodIterators6a}
// \includegraphics[width=0.3\textwidth]{NeighborhoodIterators6b}
// \includegraphics[width=0.3\textwidth]{NeighborhoodIterators6c}
// \itkcaption[Finding local minima]{Paths traversed by the neighborhood
// iterator from different seed points to the local minimum.
// The true minimum is at the center
// of the image.  The path of the iterator is shown in white. The effect of
// noise in the image is seen as small perturbations in each path. }
// \protect\label{fig:NeighborhoodExample6} \end{figure}
//
// Software Guide : EndLatex

  typedef unsigned char                          WritePixelType;
  typedef itk::Image< WritePixelType, 2 >        WriteImageType;
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
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
