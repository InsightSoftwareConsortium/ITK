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

#include "itkMaskedFFTNormalizedCorrelationImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkShiftScaleImageFilter.h"

int itkMaskedFFTNormalizedCorrelationImageFilterTest(int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " fixedImageName movingImageName outputImageName [requiredNumberOfOverlappingPixels] [fixedMaskName] [movingMaskName]" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image< uint16_t, 2 >           InputImageType;
  typedef itk::Image< uint8_t, 2 >            MaskImageType;
  typedef itk::Image< uint8_t, 2 >            OutputImageType;
  typedef itk::ImageFileReader< InputImageType >    ReaderType;
  typedef itk::ImageFileReader< MaskImageType >     MaskReaderType;

  // We need the internal type to be either float or double since
  // the correlation image contains values between -1 and 1.
  typedef itk::Image< double, 2 > RealImageType;

  char * fixedImageFileName = argv[1];
  char * movingImageFileName = argv[2];
  const char * outputImageFileName = argv[3];
  itk::SizeValueType requiredNumberOfOverlappingPixels = 0;
  if( argc > 4 )
    {
      requiredNumberOfOverlappingPixels = atoi(argv[4]);
    }

  ReaderType::Pointer fixedImageReader = ReaderType::New();
  fixedImageReader->SetFileName( fixedImageFileName );

  ReaderType::Pointer movingImageReader = ReaderType::New();
  movingImageReader->SetFileName( movingImageFileName );

  typedef itk::MaskedFFTNormalizedCorrelationImageFilter< InputImageType, RealImageType, MaskImageType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetFixedImage( fixedImageReader->GetOutput() );
  filter->SetMovingImage( movingImageReader->GetOutput() );
  // Larger values zero-out pixels on a larger border around the correlation image.
  // Thus, larger values remove less stable computations but also limit the capture range.
  filter->SetRequiredNumberOfOverlappingPixels( requiredNumberOfOverlappingPixels );

  if( argc > 5 )
  {
    char * fixedMaskFileName = argv[5];
    MaskReaderType::Pointer fixedMaskReader = MaskReaderType::New();
    fixedMaskReader->SetFileName(fixedMaskFileName);
    fixedMaskReader->Update();
    filter->SetFixedImageMask(fixedMaskReader->GetOutput());
  }

  if( argc > 6 )
  {
    char * movingMaskFileName = argv[6];
    MaskReaderType::Pointer movingMaskReader = MaskReaderType::New();
    movingMaskReader->SetFileName(movingMaskFileName);
    movingMaskReader->Update();
    filter->SetMovingImageMask(movingMaskReader->GetOutput());
  }

  // Shift the correlation values so they can be written out as a png.
  // The original range is [-1,1], and the new range is [0,255].
  // Shift is computed before scale, so we shift by 1 and then scale by 255/2.
  // The values very close to 0 in the correlation map are not exactly zero
  // because of precision issues.
  // In the shift/scale process, 0 gets mapped to 127.5.  If it were later rounded, it
  // would become 128.
  // But because of precision issues, numbers that are very close to 0 will get
  // mapped to 127 or 128, depending on whether they are slightly negative or positive.
  // Therefore, we truncate instead so that all values near 0 get mapped to 127.
  typedef itk::ShiftScaleImageFilter<RealImageType,OutputImageType> RescaleType;
  RescaleType::Pointer rescaler = RescaleType::New();
  rescaler->SetInput( filter->GetOutput() );
  rescaler->SetShift(1);
  rescaler->SetScale(255.0/2.0);

  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outputImageFileName );
  writer->SetInput( rescaler->GetOutput() );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }

  return EXIT_SUCCESS;
}
