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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkComposeImageFilter.h"
#include "itkRGBToVectorImageAdaptor.h"


// Pasting enables the writing of a sub-region to a file. This example
// updates a small portion of the 2D coronal slice. The file
// streamed_paste_vm.mha can either not exist or can be copied from
// the output of the previous example.
//

// Below we begin by creating a reader for the file just written that
// is capable of streaming.


int main(int argc, char *argv[])
{

  if ( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile  outputImageFile" << std::endl;
    return EXIT_FAILURE;
    }


  std::string inputImageFile = argv[1];
  std::string outputImageFile = argv[2];

  typedef itk::RGBPixel< unsigned char >  RGBPixelType;
  typedef itk::Image< RGBPixelType, 2 >   RGB2DImageType;

//  we begin by creating a reader for the file just written that is
//  capable of streaming
  typedef itk::ImageFileReader< RGB2DImageType > ImageReaderType;
  ImageReaderType::Pointer reader = ImageReaderType::New();
  reader->SetFileName( inputImageFile );

// The pipeline is continued through a gradient magnitude filter,
// which works on vector images to produce a scalar output. Then a
// color image is recreated by compositing the output as red, green,
// and blue channels.
  typedef itk::VectorGradientMagnitudeImageFilter< RGB2DImageType > GradientMagnitudeImageFilter;
  GradientMagnitudeImageFilter::Pointer grad = GradientMagnitudeImageFilter::New();
  grad->SetInput( reader->GetOutput() );

  grad->SetUseImageSpacingOn();

  typedef  GradientMagnitudeImageFilter::OutputImageType GradientMagnitudeOutputImageType;

  typedef itk::ComposeImageFilter< GradientMagnitudeOutputImageType, RGB2DImageType > ComposeRGBFilterType;
  ComposeRGBFilterType::Pointer composeRGB = ComposeRGBFilterType::New();
  composeRGB->SetInput1( grad->GetOutput() );
  composeRGB->SetInput2( grad->GetOutput() );
  composeRGB->SetInput3( grad->GetOutput() );


// Next we begin to specify the paste region, by creating an
// ImageIORegion that is half the size and centered on the entire
// image. The ImageIORegion class is similar to the ImageRegion class
// except that it is not templated over the image dimension, because
// of the runtime nature of IO.
  composeRGB->UpdateOutputInformation();
  RGB2DImageType::RegionType largest = composeRGB->GetOutput()->GetLargestPossibleRegion();
  itk::ImageIORegion halfIO(2);
  halfIO.SetIndex( 0, largest.GetIndex(0)
                   + (unsigned long) (0.25 * largest.GetSize(0)) );
  halfIO.SetIndex( 1, largest.GetIndex(1)
                   + (unsigned long) (0.25 * largest.GetSize(1)) );
  halfIO.SetSize( 0, (unsigned long) (0.5 * largest.GetSize(0)) );
  halfIO.SetSize( 1, (unsigned long) (0.5 * largest.GetSize(1)) );


// After using an adaptor to convert the color image into a vector
// image, so that the pixel type will match the type in the file, we
// create a writer. Here both streaming and pasting are used.  To
// enable pasting, a call to SetIORegion is made with a valid
// region. Finally, the pipeline is updated, causing the streaming of
// regions
  typedef itk::RGBToVectorImageAdaptor< RGB2DImageType >  ToVectorImageAdaptorType;
  ToVectorImageAdaptorType::Pointer adaptor = ToVectorImageAdaptorType::New();
  adaptor->SetImage( composeRGB->GetOutput() );

  typedef itk::ImageFileWriter< ToVectorImageAdaptorType > ImageWriterType;
  ImageWriterType::Pointer writer = ImageWriterType::New();
  writer->SetFileName( outputImageFile );
  writer->SetNumberOfStreamDivisions( 10 );
  writer->SetIORegion( halfIO );
  writer->SetInput( adaptor );

  itk::SimpleFilterWatcher watcher1(writer, "stream pasting writing");

  itk::SimpleFilterWatcher watcher(grad, "stream gradient magnitude");


  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

// This pasting example only writes the small halfIO region to the
// file, the remainder is not touched. The manner in which the
// pipeline executed is very similar to the previous streaming
// example. The main difference is that the writer only breaks up the
// IORegion for streaming, not the entire image. The other difference
// is that the reader fully supports streaming and only reads the
// required region from the file.

  return EXIT_SUCCESS;
}
