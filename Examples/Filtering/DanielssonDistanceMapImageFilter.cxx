/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DanielssonDistanceMapImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// This example illustrates the use of the
// \doxygen{DanielssonDistanceMapImageFilter}.  This filter generates a
// distance map form the input image using the algorithm developed by
// Danielsson \cite{Danielsson1980}. As secondary outputs, a Voronoi partition
// if the input elements is produced, as well as a vector image with the
// components of the distance vector to the closest point. The input to the map
// is assumed to be a set of points on the input image. Each point/pixel is
// considered to be a separate entity even if they share the same graylevel
// value.
//
// \index{itk::DanielssonDistanceMapImageFilter!Instantiation|textbf}
// \index{itk::DanielssonDistanceMapImageFilter!Header|textbf}
//
// The first step required to use this filter is to include its header file. 
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkDanielssonDistanceMapImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"


int main( int argc, char * argv[] )
{

  if( argc < 5 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile outputDistanceMapImageFile ";
    std::cerr << " outputVoronoiMapImageFilter ";
    std::cerr << " outputVectorMapImageFilter ";
    std::cerr << std::endl;  
    return 1;
    }
  
  //  Software Guide : BeginLatex
  //
  //  Then we must decide what pixel types to use for the input and output
  //  images. Since the output will contain distances measured in pixels, the
  //  pixel type should be able to represent at least the width of the image,
  //  or said in $N-D$ terms, the maximum extension along all the dimensions.
  //  The input and output image types are now defined using their respective
  //  pixel type and dimension.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  unsigned char   InputPixelType;
  typedef  unsigned short  OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filter type can be instantiated using the input and output image
  //  types defined above. A filter object is created with the \code{New()}
  //  method.
  //
  //  \index{itk::DanielssonDistanceMapImageFilter!instantiation}
  //  \index{itk::DanielssonDistanceMapImageFilter!New()}
  //  \index{itk::DanielssonDistanceMapImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::DanielssonDistanceMapImageFilter<
               InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  typedef itk::RescaleIntensityImageFilter< 
                   OutputImageType, OutputImageType > RescalerType;

  RescalerType::Pointer scaler = RescalerType::New();

  //
  // Reader and Writer types are instantiated.
  //
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  //  Software Guide : BeginLatex
  //
  //  The input to the filter is taken from a reader and its output is passed
  //  to a \doxygen{RescaleIntensityImageFilter} and then to a writer.
  //
  //  \index{itk::DanielssonDistanceMapImageFilter!SetInput()}
  //  \index{itk::DanielssonDistanceMapImageFilter!GetOutput()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  scaler->SetInput( filter->GetOutput() );
  writer->SetInput( scaler->GetOutput() );
  // Software Guide : EndCodeSnippet


  scaler->SetOutputMaximum( 65535L );
  scaler->SetOutputMinimum(     0L );


  //  Software Guide : BeginLatex
  //
  //  The type of input image has to be specified. In this case, a binary image
  //  is selected.
  //
  //  \index{itk::DanielssonDistanceMapImageFilter!InputIsBinaryOn()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->InputIsBinaryOn();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[width=4cm]{FivePoints.eps}
  // \includegraphics[width=4cm]{DanielssonDistanceMapImageFilterOutput1.eps}
  // \includegraphics[width=4cm]{DanielssonDistanceMapImageFilterOutput2.eps}
  // \caption[DanielssonDistanceMapImageFilter
  // output]{DanielssonDistanceMapImageFilter output. Set of pixels, distance
  // map and Voronoi partition}
  // \label{fig:DanielssonDistanceMapImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:DanielssonDistanceMapImageFilterInputOutput} illustrate
  //  the effect of this filter on a binary image with a set of points. The
  //  input image is shown at left, the distance map at the center and the
  //  Voronoi partition at right. This filter computes distance maps in
  //  N-dimensions and is therefore capable of producing $N-D$ Voronoi
  //  partitions.
  //
  //  \index{Voronoi partitions|textbf}
  //  \index{Voronoi partitions!itk::DanielssonDistanceMapImageFilter}
  //
  //  Software Guide : EndLatex 


  writer->Update();

  const char * voronoiMapFileName = argv[3];





  //  Software Guide : BeginLatex
  //
  //  The Voronoi map is obtained with the \code{GetVoronoiMap()} method. In
  //  the lines below we connect this output to the intensity rescaler and save
  //  the result in a file.
  //
  //  \index{itk::DanielssonDistanceMapImageFilter!GetVoronoiMap()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  scaler->SetInput( filter->GetVoronoiMap() );
  writer->SetFileName( voronoiMapFileName );
  writer->Update();
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The distance filter also produces an image of \doxygen{Offset} pixels
  //  representing the vectorial distance to the closest object in the scene.
  //  The type of this output image is defined by the \code{VectorImageType}
  //  trait of the filter type.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef FilterType::VectorImageType   OffsetImageType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  We can use this type for instantiatting an \doxygen{ImageFileWriter} type
  //  and creating an object of this class in the following lines.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< OffsetImageType >  WriterOffsetType;

  WriterOffsetType::Pointer offsetWriter = WriterOffsetType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  The output of the distance filter can be connected as input to the
  //  writer.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  offsetWriter->SetInput(  filter->GetVectorDistanceMap()  );
  // Software Guide : EndCodeSnippet


  offsetWriter->SetFileName( argv[4]  );


  //  Software Guide : BeginLatex
  //  
  //  Execution of the writer is triggered by the invokation of the
  //  \code{Update()} method. Since this method can potentially throw
  //  exceptions it must be placed in a \code{try/catch} block.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  try
    {
    offsetWriter->Update();
    }
  catch( itk::ExceptionObject exp )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr <<     exp    << std::endl;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  Note that only the \doxygen{MetaImageIO} class supports reading and
  //  writing images of pixel type \doxygen{Offset}.
  //
  //  Software Guide : EndLatex 

  return 0;

}

