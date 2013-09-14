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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkStdStreamLogOutput.h"
#include "itkPatchBasedDenoisingImageFilter.h"

template <typename ImageT>
int doDenoising(const std::string & inputFileName, const std::string & outputFileName)
{
  typedef itk::ImageFileReader< ImageT > ReaderType;

  typedef itk::PatchBasedDenoisingImageFilter<ImageT, ImageT> FilterType;

  typedef typename FilterType::OutputImageType OutputImageType;

  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  // read the noisy image to be denoised
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputFileName );
  try
  {
    reader->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << "Problem encountered while reading image file : " << inputFileName << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // create filter and initialize
  // give image to filter and run it
  // get filter output and write to file

  typename FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());

  // use 2 threads for consistency
  filter->SetNumberOfThreads(2);

  // denoise the image
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::ostringstream itkmsg;                                            \
    itkmsg << "Error: In " __FILE__ ", line " << __LINE__ << "\n"
           << "Caught exception <" << excp
           << "> while running patch-based denoising image filter."
           << "\n\n";
    ::itk::OutputWindowDisplayWarningText(itkmsg.str().c_str());
    return EXIT_FAILURE;
  }

  // write the denoised image to file
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outputFileName );
  writer->SetInput( filter->GetOutput() );
  std::cout << "Writing NumberOfComponents: "
            << filter->GetOutput()->GetNumberOfComponentsPerPixel()
            << " to file." << std::endl;
  try
  {
    writer->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int itkPatchBasedDenoisingImageFilterDefaultTest( int argc, char * argv [] )
{

  if( argc < 3 )
  {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  " << argv[0]
              << " inputImageFileName outputImageFileName"
              << " numDimensions"
              << std::endl;
    return EXIT_FAILURE;
  }

  const std::string inFileName(argv[1]);

  const std::string outFileName(argv[2]);

  const unsigned int numDimensions = atoi(argv[3]);

  typedef float PixelComponentType;
  //
  typedef PixelComponentType                           OneComponentType;
  //
  typedef itk::Image< OneComponentType, 2 > OneComponent2DImage;
  typedef itk::Image< OneComponentType, 3 > OneComponent3DImage;

  //
  if (numDimensions == 2)
    {
    return doDenoising<OneComponent2DImage>(inFileName, outFileName);
  }
  //
  else if (numDimensions == 3)
  {
    return doDenoising<OneComponent3DImage>(inFileName, outFileName);
  }
  else
  {
    std::cout << numDimensions << " dimensions "
              << "isn't supported in this test driver."
              << std::endl;
    return EXIT_FAILURE;
  }

  // shouldn't reach this point, return failure here to keep the compiler happy
  return EXIT_FAILURE;
}
