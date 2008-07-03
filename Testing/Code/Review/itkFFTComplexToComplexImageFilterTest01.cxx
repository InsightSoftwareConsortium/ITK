/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTComplexToComplexImageFilterTest01.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
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
// #include "itkFFTWComplexToComplexImageFilter.h"

#if !defined(USE_FFTWF)
//#error "This example only works when single precision FFTW is used
//Changing WorkPixeltype to double and changing this conditional to USE_FFTWD
//will also work.
#endif

int itkFFTComplexToComplexImageFilterTest( int argc, char * argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " input output [dir]" << std::endl;
    std::cerr << "       " "dir: -1 for forward" << std::endl;
    std::cerr << "       " "dir: 1 for backward" << std::endl;

    return EXIT_FAILURE;
    }

  int Direction = FFT_FORWARD;

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

  const unsigned int                              Dimension = 2;
  typedef float                                   PixelComponentType;
  typedef std::complex< PixelComponentType >      PixelType;

  typedef itk::Image< PixelType,  Dimension >     ImageType;
  typedef itk::ImageFileReader< ImageType >       ReaderType;
  typedef itk::ImageFileWriter< ImageType >       WriterType;


  ReaderType::Pointer inputreader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  inputreader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  // Read the image and get its size
  inputreader->Update();

  // Forward FFT filter
  typedef itk::FFTComplexToComplexImageFilter <
                  PixelType, Dimension, FFT_FORWARD > FFTFilterType;

  // Backward FFT filter
  typedef itk::FFTComplexToComplexImageFilter <
                 PixelType, Dimension, FFT_BACKWARD > invFFTFilterType;

  FFTFilterType::Pointer fftoutput = FFTFilterType::New();
  invFFTFilterType::Pointer invfftoutput = invFFTFilterType::New();
  
  if(Direction == FFT_FORWARD)
    {
    // compute forward FFT
    fftoutput->SetInput( inputreader->GetOutput() );
    fftoutput->Update();
    writer->SetInput(fftoutput->GetOutput());
    }
  else
    {
    // compute inverse FFT
    invfftoutput->SetInput( inputreader->GetOutput() );
    invfftoutput->Update();
    writer->SetInput(invfftoutput->GetOutput());
    }

  writer->Update();

  return EXIT_SUCCESS;

}
