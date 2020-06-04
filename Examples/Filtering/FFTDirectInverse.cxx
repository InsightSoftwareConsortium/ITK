/*=========================================================================
 *
 *  Copyright NumFOCUS
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

//
// This example was originally contributed by Stephan in the users list
//
//     http://public.kitware.com/pipermail/insight-users/2005-June/013482.html
//
//

// Software Guide : BeginLatex
//
// This example illustrates how to compute the direct Fourier transform
// followed by the inverse Fourier transform in order to recover the original
// data.
//
// Software Guide : EndLatex

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkVnlForwardFFTImageFilter.h"
#include "itkVnlInverseFFTImageFilter.h"

int
main(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Usage: " << argv[0] << " input output" << std::endl;
    return EXIT_FAILURE;
  }

  // Software Guide : BeginLatex
  //
  // First we set up the types of the input and output images.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  constexpr unsigned int Dimension = 2;
  using IOPixelType = unsigned short;
  using WorkPixelType = float;

  using IOImageType = itk::Image<IOPixelType, Dimension>;
  using WorkImageType = itk::Image<WorkPixelType, Dimension>;
  // Software Guide : EndCodeSnippet

  // File handling
  using ReaderType = itk::ImageFileReader<IOImageType>;
  using WriterType = itk::ImageFileWriter<IOImageType>;

  ReaderType::Pointer inputreader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  inputreader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Handle padding of the image with resampling
  using ResamplerType = itk::ResampleImageFilter<IOImageType, WorkImageType>;

  ResamplerType::Pointer inputresampler = ResamplerType::New();
  inputresampler->SetDefaultPixelValue(0);

  // Read the image and get its size
  inputreader->Update();

  IOImageType::SizeType inputsize;
  IOImageType::SizeType worksize;

  inputsize = inputreader->GetOutput()->GetLargestPossibleRegion().GetSize();

  // worksize is the nearest multiple of 2 larger than the input
  for (unsigned int i = 0; i < 2; i++)
  {
    unsigned int n = 0;
    worksize[i] = inputsize[i];
    while (worksize[i] >>= 1)
    {
      n++;
    }
    worksize[i] = static_cast<IOImageType::SizeValueType>(1)
                  << static_cast<IOImageType::SizeValueType>(n + 1);

    std::cout << "inputsize[" << i << "]=" << inputsize[i] << std::endl;
    std::cout << "worksize[" << i << "]=" << worksize[i] << std::endl;
  }

  inputresampler->SetSize(worksize);
  inputresampler->SetInput(inputreader->GetOutput());

  // Forward FFT filter
  using FFTFilterType = itk::VnlForwardFFTImageFilter<WorkImageType>;
  FFTFilterType::Pointer fftinput = FFTFilterType::New();
  fftinput->SetInput(inputresampler->GetOutput());

  // This is the output type from the FFT filters
  using ComplexImageType = FFTFilterType::OutputImageType;

  // Do the inverse transform = forward transform / num voxels
  using invFFTFilterType = itk::VnlInverseFFTImageFilter<ComplexImageType>;
  invFFTFilterType::Pointer fftoutput = invFFTFilterType::New();
  fftoutput->SetInput(
    fftinput->GetOutput()); // try to recover the input image

  // undo the padding
  using ResampleOutType =
    itk::ResampleImageFilter<WorkImageType, IOImageType>;
  ResampleOutType::Pointer outputResampler = ResampleOutType::New();
  outputResampler->SetDefaultPixelValue(0);
  outputResampler->SetSize(inputsize);
  outputResampler->SetInput(fftoutput->GetOutput());

  // Write the output
  writer->SetInput(outputResampler->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}
