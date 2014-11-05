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

//  Software Guide : BeginLatex
//
//  RGB images are commonly used for representing data acquired from cryogenic
//  sections, optical microscopy and endoscopy. This example illustrates how to
//  read RGB color images from a set of files containing individual 2D slices
//  in order to compose a 3D color dataset. Then we will save it into a single
//  3D file, and finally save it again as a set of 2D slices with other names.
//
//  This requires the following headers as shown.
//
//  \index{itk::RGBPixel!Image}
//  \index{RGB!writing Image}
//  \index{RGB!reading Image}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkRGBPixel.h"
#include "itkImage.h"
#include "itkImageSeriesReader.h"
#include "itkImageSeriesWriter.h"
#include "itkNumericSeriesFileNames.h"
#include "itkPNGImageIO.h"
// Software Guide : EndCodeSnippet


int main( int argc, char ** argv )
{
  // Verify the number of parameters in the command line
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "first last  outputRGBImageFile " << std::endl;
    return EXIT_FAILURE;
    }


  // Software Guide : BeginLatex
  //
  // The \doxygen{RGBPixel} class is templated over the type used to
  // represent each one of the Red, Green and Blue components. A typical
  // instantiation of the RGB image class might be as follows.
  //
  //  \index{itk::RGBPixel!Instantiation}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RGBPixel< unsigned char >        PixelType;
  const unsigned int Dimension = 3;

  typedef itk::Image< PixelType, Dimension >    ImageType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The image type is used as a template parameter to instantiate
  // the series reader and the volumetric writer.
  //
  // \index{itk::ImageSeriesReader!RGB Image}
  // \index{itk::ImageFileWriter!RGB Image}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageSeriesReader< ImageType >  SeriesReaderType;
  typedef itk::ImageFileWriter<   ImageType >  WriterType;

  SeriesReaderType::Pointer seriesReader = SeriesReaderType::New();
  WriterType::Pointer       writer       = WriterType::New();
  // Software Guide : EndCodeSnippet


  const unsigned int first = atoi( argv[1] );
  const unsigned int last  = atoi( argv[2] );

  const char * outputFilename = argv[3];

  // Software Guide : BeginLatex
  //
  // We use a NumericSeriesFileNames class in order to generate the filenames of
  // the slices to be read. Later on in this example we will reuse this object in
  // order to generate the filenames of the slices to be written.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::NumericSeriesFileNames    NameGeneratorType;

  NameGeneratorType::Pointer nameGenerator = NameGeneratorType::New();

  nameGenerator->SetStartIndex( first );
  nameGenerator->SetEndIndex( last );
  nameGenerator->SetIncrementIndex( 1 );

  nameGenerator->SetSeriesFormat( "vwe%03d.png" );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The ImageIO object that actually performs the read process
  //  is now connected to the ImageSeriesReader.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  seriesReader->SetImageIO( itk::PNGImageIO::New() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The filenames of the input slices are taken from the names generator and
  //  passed to the series reader.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  seriesReader->SetFileNames( nameGenerator->GetFileNames()  );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The name of the volumetric output image is passed to the image writer, and
  // we connect the output of the series reader to the input of the volumetric
  // writer.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  writer->SetFileName( outputFilename );

  writer->SetInput( seriesReader->GetOutput() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Finally, execution of the pipeline can be triggered by invoking the
  //  \code{Update()} method in the volumetric writer. This, of course, is done
  //  from inside a try/catch block.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error reading the series " << std::endl;
    std::cerr << excp << std::endl;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We now proceed to save the same volumetric dataset as a set of slices. This
  // is done only to illustrate the process for saving a volume as a series of 2D
  // individual datasets. The type of the series writer must be instantiated
  // taking into account that the input file is a 3D volume and the output files
  // are 2D images.  Additionally, the output of the series reader is connected
  // as input to the series writer.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< PixelType, 2 >     Image2DType;

  typedef itk::ImageSeriesWriter< ImageType, Image2DType > SeriesWriterType;

  SeriesWriterType::Pointer seriesWriter = SeriesWriterType::New();

  seriesWriter->SetInput( seriesReader->GetOutput() );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We now reuse the filename generator in order to produce the list of
  // filenames for the output series. In this case we just need to modify the
  // format of the filename generator. Then, we pass the list of output
  // filenames to the series writer.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  nameGenerator->SetSeriesFormat( "output%03d.png" );

  seriesWriter->SetFileNames( nameGenerator->GetFileNames() );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Finally we trigger the execution of the series writer from inside a
  // try/catch block.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    seriesWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error reading the series " << std::endl;
    std::cerr << excp << std::endl;
    }
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  You may have noticed that apart from the declaration of the
  //  \code{PixelType} there is nothing in this code that is specific to RGB
  //  images. All the actions required to support color images are implemented
  //  internally in the \doxygen{ImageIO} objects.
  //
  // Software Guide : EndLatex

  return EXIT_SUCCESS;
}
