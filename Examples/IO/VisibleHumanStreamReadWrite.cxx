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

#include "itkRawImageIO.h"
#include "itkNumericSeriesFileNames.h"
#include "itkImageSeriesReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkShrinkImageFilter.h"
#include "itkComposeImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkMeanImageFilter.h"

// The Insight Toolkit was originally motivated by a need for software
// tools to segment and register the National Library of Medicine’s
// Visible Human Project data sets. The data is freely available
// through NLM’s website [3]. The original Visible Male cryosectional
// images are non-interlaced 24-bit RGB pixels with a resolution of
// 2048x1216 pixels by 1871 slices with a physical spacing of
// approximately 0.33 mm in slice and 1.0 mm between slices. Theses
// dimensions results in about 13 gigabytes of data, which is an
// appropriate size to demonstrate streaming. The following are two
// examples of streaming which shows all three IO classes capable of
// streaming along with the two types of streaming supported by the
// writer.
//
// A coronal slice is a classic view of the Visible Male. The
// following is an example that reads the entire raw dataset and
// generates that classic image:

int main(int argc, char *argv[])
{
  if ( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " visibleHumanPath  outputImageFile" << std::endl;
    return EXIT_FAILURE;
    }

  std::string visibleHumanPath = argv[1];
  std::string outputImageFile = argv[2];

  typedef itk::RGBPixel<unsigned char> RGBPixelType;
  typedef unsigned char                PixelType;
  typedef itk::Image<PixelType, 3>     ImageType;
  typedef itk::Image<RGBPixelType, 3>  RGB3DImageType;
  typedef itk::Image<RGBPixelType, 2>  RGB2DImageType;

  // genderate the names of the decompressed Visible Male images
  typedef itk::NumericSeriesFileNames    NameGeneratorType;
  NameGeneratorType::Pointer nameGenerator = NameGeneratorType::New();
  nameGenerator->SetSeriesFormat( visibleHumanPath+"a_vm%04d.raw" );
  nameGenerator->SetStartIndex( 1001 );
  nameGenerator->SetEndIndex( 2878 );
  nameGenerator->SetIncrementIndex( 1 );

// create a ImageIO for the red channel
  typedef itk::RawImageIO<PixelType, 2> ImageIOType;
  ImageIOType::Pointer rimageio = ImageIOType::New();
  rimageio->SetDimensions( 0, 2048 );
  rimageio->SetDimensions( 1, 1216 );
  rimageio->SetSpacing( 0, .33 );
  rimageio->SetSpacing( 1, .33 );
  rimageio->SetHeaderSize(rimageio->GetImageSizeInPixels()*0);


// create a ImageIO for the green channel
  ImageIOType::Pointer gimageio = ImageIOType::New();
  gimageio->SetDimensions( 0, 2048 );
  gimageio->SetDimensions( 1, 1216 );
  gimageio->SetSpacing( 0, .33 );
  gimageio->SetSpacing( 1, .33 );
  gimageio->SetHeaderSize(gimageio->GetImageSizeInPixels()*1);


// create a ImageIO for the blue channel
  ImageIOType::Pointer bimageio = ImageIOType::New();
  bimageio->SetDimensions( 0, 2048 );
  bimageio->SetDimensions( 1, 1216 );
  bimageio->SetSpacing( 0, .33 );
  bimageio->SetSpacing( 1, .33 );
  bimageio->SetHeaderSize(bimageio->GetImageSizeInPixels()*2);

  typedef itk::ImageSeriesReader< ImageType > SeriesReaderType;
  SeriesReaderType::Pointer rreader = SeriesReaderType::New();
  rreader->SetFileNames ( nameGenerator->GetFileNames() );
  rreader->SetImageIO( rimageio );
  // the z-spacing will default to be correctly 1mm

  SeriesReaderType::Pointer greader = SeriesReaderType::New();
  greader->SetFileNames ( nameGenerator->GetFileNames() );
  greader->SetImageIO( gimageio );

  SeriesReaderType::Pointer breader = SeriesReaderType::New();
  breader->SetFileNames ( nameGenerator->GetFileNames() );
  breader->SetImageIO( bimageio );

  typedef itk::ComposeImageFilter< ImageType, RGB3DImageType > ComposeRGBFilterType;
  ComposeRGBFilterType::Pointer composeRGB = ComposeRGBFilterType::New();
  composeRGB->SetInput1( rreader->GetOutput() );
  composeRGB->SetInput2( greader->GetOutput() );
  composeRGB->SetInput3( breader->GetOutput() );

// this filter is needed if square pixels are needed
//   const int xyShrinkFactor = 3;
//   typedef itk::ShrinkImageFilter<  RGB3DImageType, RGB3DImageType > ShrinkImageFilterType;
//   ShrinkImageFilterType::Pointer shrinker = ShrinkImageFilterType::New();
//   shrinker->SetInput( composeRGB->GetOutput() );
//   shrinker->SetShrinkFactors(  xyShrinkFactor );
//   shrinker->SetShrinkFactor( 2, 1 );

// update output information to know propagate the size of the largest
// possible region
  composeRGB->UpdateOutputInformation();
  RGB3DImageType::RegionType coronalSlice = composeRGB->GetOutput()->GetLargestPossibleRegion();
  coronalSlice.SetIndex( 1, 448 );
  coronalSlice.SetSize( 1, 0 );

// another interesting view
//   RGB3DImageType::RegionType sagittalSlice = shrinker->GetOutput()->GetLargestPossibleRegion();
//   sagittalSlice.SetIndex( 0, 1024 );
//   sagittalSlice.SetSize( 0, 0 );

// create a 2D coronal slice from the volume
  typedef itk::ExtractImageFilter< RGB3DImageType, RGB2DImageType > ExtractFilterType;
  ExtractFilterType::Pointer extract = ExtractFilterType::New();
  // Note on direction cosines: Because our plane is in the xz-plane,
  // the default submatrix would be invalid, so we must use the identity
  extract->SetDirectionCollapseToIdentity();
  extract->InPlaceOn();
  extract->SetInput( composeRGB->GetOutput() );
  extract->SetExtractionRegion(coronalSlice);


  typedef itk::ImageFileWriter< RGB2DImageType > ImageWriterType;
  ImageWriterType::Pointer writer = ImageWriterType::New();
  writer->SetFileName( outputImageFile );

// this line is a request for the number of regions
// the image will be broken into
  writer->SetNumberOfStreamDivisions( 200 );
  writer->SetInput( extract->GetOutput() );

  itk::SimpleFilterWatcher watcher1(writer, "stream writing");


  try
    {
// update by streaming
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

// This example creates a RawImageIO and ImageSeriesReader for each
// color channel in the data. Notice that there are no special methods
// that are needed to enable streaming; it will just respond correctly
// to requests from the pipeline. In the ComposeImageFilter, the
// channels are composited into a single color image. Then the
// information is updated to initialize the coronal slice region to be
// extracted. The final filter, ImageFileWriter, writes out the file
// as a Meta Image type, which fully supports IO streaming.
//
// The most interesting aspect of this example is not the filters
// used, but how ITK’s pipeline manages its execution. The final
// output image is 2048 by 1878 pixels. The ImageFileWriter breaks
// this 2D image into 200 separate regions, which have the size of
// about 2048 by 10 pixels; each region is streamed and processes
// through the pipeline. The writer makes 200 calls to its ImageIO
// object to write the individual regions. The extractor converts this
// 2D region into a 3D region of 2048 by 1 by 10 pixels, which is
// propagated to the ImageSeriesReader. Then the reader reads the
// entire slice, but only copies the requested sub-region to its
// output. This pipeline is so efficient because very little data is
// actually processed at any one stage of the pipeline due to
// streaming IO.


  return EXIT_SUCCESS;
}
