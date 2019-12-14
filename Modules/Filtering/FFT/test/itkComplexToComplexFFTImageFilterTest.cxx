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

/** Example illustrating use of ComplexToComplexFFTImageFilter
 *
 * \author Simon K. Warfield simon.warfield\@childrens.harvard.edu
 *
 * \note Attribution Notice. This research work was made possible by Grant
 * Number R01 RR021885 (PI Simon K. Warfield, Ph.D.) from
 * the National Center for Research Resources (NCRR), a component of the
 * National Institutes of Health (NIH).  Its contents are solely the
 * responsibility of the authors and do not necessarily represent the
 * official view of NCRR or NIH.
 *
 * Contributed to the Insight Journal paper:
 * http://insight-journal.org/midas/handle.php?handle=1926/326
 *
 */

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkComplexToComplexFFTImageFilter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkTestingMacros.h"

template <typename TPixel, unsigned int VDimension>
int
transformImage(const char * inputImageFileName, const char * outputImageFileName)
{
  using RealPixelType = TPixel;
  using ComplexPixelType = std::complex<RealPixelType>;
  const unsigned int Dimension = VDimension;

  using RealImageType = itk::Image<RealPixelType, Dimension>;
  using ComplexImageType = itk::Image<ComplexPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<RealImageType>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputImageFileName);

  using ForwardFilterType = itk::ForwardFFTImageFilter<RealImageType, ComplexImageType>;
  typename ForwardFilterType::Pointer forwardFilter = ForwardFilterType::New();
  forwardFilter->SetInput(reader->GetOutput());

  using ComplexFilterType = itk::ComplexToComplexFFTImageFilter<ComplexImageType>;
  typename ComplexFilterType::Pointer inverseComplexFilter = ComplexFilterType::New();
  inverseComplexFilter->SetInput(forwardFilter->GetOutput());
  inverseComplexFilter->SetTransformDirection(ComplexFilterType::INVERSE);

  typename ComplexFilterType::Pointer forwardComplexFilter = ComplexFilterType::New();
  forwardComplexFilter->SetInput(inverseComplexFilter->GetOutput());
  forwardComplexFilter->SetTransformDirection(ComplexFilterType::FORWARD);
  // This tests the CanUseDestructiveAlgorithm state with the FFTW version.
  forwardComplexFilter->ReleaseDataFlagOn();

  using InverseFilterType = itk::InverseFFTImageFilter<ComplexImageType, RealImageType>;
  typename InverseFilterType::Pointer inverseFilter = InverseFilterType::New();
  inverseFilter->SetInput(forwardComplexFilter->GetOutput());

  using WriterType = itk::ImageFileWriter<RealImageType>;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(outputImageFileName);
  writer->SetInput(inverseFilter->GetOutput());

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & error)
  {
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkComplexToComplexFFTImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <InputImage> <OutputImage> <float|double>"
              << std::endl;
    return EXIT_FAILURE;
  }
  const char *      inputImageFileName = argv[1];
  const char *      outputImageFileName = argv[2];
  const std::string pixelTypeString(argv[3]);

  itk::ImageIOBase::Pointer imageIO =
    itk::ImageIOFactory::CreateImageIO(inputImageFileName, itk::ImageIOFactory::FileModeEnum::ReadMode);
  imageIO->SetFileName(inputImageFileName);
  imageIO->ReadImageInformation();
  const unsigned int dimension = imageIO->GetNumberOfDimensions();

  if (pixelTypeString.compare("float") == 0)
  {
    switch (dimension)
    {
      case 2:
        return transformImage<float, 2>(inputImageFileName, outputImageFileName);
      case 3:
        return transformImage<float, 3>(inputImageFileName, outputImageFileName);
      default:
        std::cerr << "Unknown image dimension." << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
  }
  else if (pixelTypeString.compare("double") == 0)
  {
    switch (dimension)
    {
      case 2:
        return transformImage<double, 2>(inputImageFileName, outputImageFileName);
      case 3:
        return transformImage<double, 3>(inputImageFileName, outputImageFileName);
      default:
        std::cerr << "Unknown image dimension." << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
  }
  else
  {
    std::cerr << "Unknown pixel type string." << std::endl;
    return EXIT_FAILURE;
  }


  return EXIT_FAILURE;
}
