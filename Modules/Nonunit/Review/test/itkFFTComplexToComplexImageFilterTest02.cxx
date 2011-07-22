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

/** Example illustrating use of FFTComplexToComplexImageFilter
 *
 * \author Simon K. Warfield simon.warfield@childrens.harvard.edu
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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFFTComplexToComplexImageFilter.h"

int itkFFTComplexToComplexImageFilterTest02( int argc, char * argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " input output [dir]" << std::endl;
    std::cerr << "       " "dir: -1 for forward" << std::endl;
    std::cerr << "       " "dir: 1 for backward" << std::endl;

    return EXIT_FAILURE;
    }

  const unsigned int                              Dimension = 3;
#if defined(USE_FFTWF) && !defined(USE_FFTWD)
  typedef float                                   PixelComponentType;
#else
  typedef double                                  PixelComponentType;
#endif
  typedef std::complex< PixelComponentType >      PixelType;

  typedef itk::Image< PixelType,  Dimension >     ImageType;
  typedef itk::ImageFileReader< ImageType >       ReaderType;
  typedef itk::ImageFileWriter< ImageType >       WriterType;


  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  //  FFT filter
  typedef itk::FFTComplexToComplexImageFilter < ImageType > FFTFilterType;

  int Direction = 1;

  if( argc == 4 )
    {
    Direction = atoi(argv[3]);
    if( Direction != -1 && Direction != 1 )
      {
      std::cerr << "Usage: " << argv[0] << " input output [dir]" << std::endl;
      std::cerr << "       " "dir: -1 for forward transform" << std::endl;
      std::cerr << "       " "dir: 1 for backward transform" << std::endl;
      return EXIT_FAILURE;
      }

    }


  FFTFilterType::Pointer fftFilter =    FFTFilterType::New();


  if( Direction == 1 )
    {
    fftFilter->SetTransformDirection( FFTFilterType::DIRECT );
    }
  else
    {
    fftFilter->SetTransformDirection( FFTFilterType::INVERSE );
    }


  fftFilter->SetInput( reader->GetOutput() );
  writer->SetInput( fftFilter->GetOutput() );


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
