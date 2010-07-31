/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageReadComplexWriteMagnitudeAndPhaseTest.cxx
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

/** Example illustrating use of functions to convert between complex valued
 * voxels, magnitude and phase, and real and imaginary representations.
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
 */

// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkComplexToModulusImageFilter.h"
#include "itkComplexToPhaseImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int itkImageReadComplexWriteMagnitudeAndPhaseTest( int argc, char * argv [] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << argv[0] 
      <<  " inputComplexImage  outputMagnitudePartOfComplexImage "
      <<  "outputPhasePartOfComplexImage" << std::endl;
    }


  const unsigned int Dimension = 2;

  typedef float  InputPixelType;
  typedef float  OutputPixelType;

  typedef itk::Image< std::complex<InputPixelType>, Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >                OutputImageType;
  typedef itk::ImageFileReader< InputImageType >                  ReaderType;
  typedef itk::ComplexToModulusImageFilter< 
                 InputImageType, OutputImageType >                ModulusFilterType;
  typedef itk::ComplexToPhaseImageFilter< 
                 InputImageType, OutputImageType >                PhaseFilterType;
  typedef itk::ImageFileWriter< OutputImageType >                 WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  ModulusFilterType::Pointer modulusFilter = ModulusFilterType::New();
  modulusFilter->SetInput( reader->GetOutput() );

  WriterType::Pointer writerM = WriterType::New();
  writerM->SetInput(modulusFilter->GetOutput());
  writerM->SetFileName( argv[2] );

  try
    {
    writerM->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error writing the magnitude image: " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  PhaseFilterType::Pointer phaseFilter = PhaseFilterType::New();
  phaseFilter->SetInput( reader->GetOutput() );

  WriterType::Pointer writerP = WriterType::New();
  writerP->SetInput(phaseFilter->GetOutput());
  writerP->SetFileName( argv[3] );

  try
    {
    writerP->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error writing the phase image: " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
