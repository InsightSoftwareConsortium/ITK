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
/*******************************************************************************

  Abstract:  -  Multiresolution  symmetric forces demons registration - 4 multiresolution levels
  Created: July 8 2008
  Last Revision 7/9/2008
  by Vidya Rajagopalan  on 7/9/2008

  Copyright (c) 2008, Bioimaging Systems Lab, Virginia Tech
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.
   * Neither the name of Virgina Tech nor the names of its contributors may
     be used to endorse or promote products derived from this software without
     specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
  IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
  OF SUCH DAMAGE.
*******************************************************************************/
// Software Guide : BeginLatex
//
// This example illustrates the use of the
// \doxygen{MultiResolutionPDEDeformableRegistration} class for performing
// deformable registration of two $2D$ images using multiple resolution levels.
//
// The MultiResolution filter drives a SymmetricForcesDemonsRegistrationFilter
// at every level of resolution in the pyramid.
//
// \index{itk::MultiResolutionPDEDeformableRegistration}
// \index{itk::SymmetricForcesDemonsRegistrationFilter}
//
// Software Guide : EndLatex

//
//   CREDITS:
//
//   This example was contributed to ITK by
//
//       Vidya Rajagopalan, Bioimaging Systems Lab, Virginia Tech
//
//   The example was improved during the NAMIC programming week on July 2008
//   http://wiki.na-mic.org/Wiki/index.php/2008_Summer_Project_Week
//
//   National Alliance for Medical Image Computing (NAMIC),
//   funded by the National Institutes of Health
//   through the NIH Roadmap for Medical Research,
//   Grant U54 EB005149.
//
//   Data for these examples have been contributed by
//
//       Paul Laurienti, Wake Forest University School of Medicine,
//       Data acquired under NIH grant number NS042568
//
//

#include <iostream>
#include <cstdlib>

// ITK IO includes
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

// ITK Registration includes
#include "itkMultiResolutionPDEDeformableRegistration.h"
#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkSymmetricForcesDemonsRegistrationFilter.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkWarpImageFilter.h"

unsigned int RmsCounter = 0;
double MaxRmsE[4] = {0.8,  0.75,  0.4, 0.2};

//  The following section of code implements a Command observer
//  that will monitor the evolution of the registration process.
//  This observer has a layer of intelligence, for deciding what
//  MaximumRMS convergence criteria to use at every resolution level.
//

class CommandIterationUpdate : public itk::Command
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
  typedef  itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdate() {};

  // define ITK short-hand types
  typedef short                                  PixelType;
  typedef float                                  InternalPixelType;
  typedef itk::Image< PixelType, 2 >             ImageType;
  typedef itk::Image< InternalPixelType, 2 >     InternalImageType;
    typedef itk::Vector< float, 2 >              VectorPixelType;
  typedef itk::Image< VectorPixelType, 2 >       DisplacementFieldType;
  typedef itk::SymmetricForcesDemonsRegistrationFilter< InternalImageType,
    InternalImageType, DisplacementFieldType>    RegistrationFilterType;

  public:

    void Execute(const itk::Object *, const itk::EventObject & ) ITK_OVERRIDE
      {
      std::cout << "Warning: The const Execute method shouldn't be called" << std::endl;
      }

    void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
      {
      RegistrationFilterType * filter = static_cast<  RegistrationFilterType * >( caller );

      if( !(itk::IterationEvent().CheckEvent( &event )) )
       {
       return;
       }

      if(filter)
        {
        filter->SetMaximumRMSError(MaxRmsE[RmsCounter]);
        std::cout << filter->GetMetric() <<  "  RMS Change: " << filter->GetRMSChange() << std::endl;

        std::cout << "Level Tolerance=  "<<filter->GetMaximumRMSError ()<<std::endl;
        }
  }
};


//
// The following command observer reports the progress of the registration
// inside a given resolution level.
//
class CommandResolutionLevelUpdate : public itk::Command
{
public:
  typedef  CommandResolutionLevelUpdate   Self;
  typedef  itk::Command                   Superclass;
  typedef  itk::SmartPointer<Self>        Pointer;
  itkNewMacro( Self );

protected:
  CommandResolutionLevelUpdate() {};

public:
  void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }
  void Execute(const itk::Object *, const itk::EventObject & ) ITK_OVERRIDE
    {
    std::cout << "----------------------------------" << std::endl;
    RmsCounter = RmsCounter + 1;
    std::cout << "----------------------------------" << std::endl;
    }
};


int main( int argc, char * argv [] )
{

  // Verify the number of parameters in the command line
  if( argc != 5 )
    {
    std::cerr << "usage: " << std::endl;
    std::cerr << argv[0] << " fixedImage movingImage registeredImage deformationField" << std::endl;
    return EXIT_FAILURE;
    }

  // define ITK short-hand types
  const unsigned int Dimension = 2;
  typedef short                                                PixelType;
  typedef float                                                InternalPixelType;
  typedef itk::Image< PixelType, Dimension >                   ImageType;
  typedef itk::Image< InternalPixelType, Dimension >           InternalImageType;
  typedef itk::CastImageFilter< ImageType, InternalImageType > ImageCasterType;

  // setup input file readers
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  ReaderType::Pointer targetReader = ReaderType::New();
  targetReader->SetFileName( argv[1] );
  targetReader->Update();

  ReaderType::Pointer sourceReader = ReaderType::New();
  sourceReader->SetFileName( argv[2] );
  sourceReader->Update();

  // cast target and source to float
  ImageCasterType::Pointer targetImageCaster = ImageCasterType::New();
  ImageCasterType::Pointer sourceImageCaster = ImageCasterType::New();
  targetImageCaster->SetInput( targetReader->GetOutput() );
  sourceImageCaster->SetInput( sourceReader->GetOutput() );

  // match the histograms between source and target
  typedef itk::HistogramMatchingImageFilter<
    InternalImageType, InternalImageType >          MatchingFilterType;

  MatchingFilterType::Pointer matcher = MatchingFilterType::New();

  matcher->SetInput( sourceImageCaster->GetOutput() );
  matcher->SetReferenceImage( targetImageCaster->GetOutput() );
  matcher->SetNumberOfHistogramLevels( 1024 );
  matcher->SetNumberOfMatchPoints( 7 );
  matcher->ThresholdAtMeanIntensityOn();

  // setup the deformation field and filter
  typedef itk::Vector< float, Dimension > VectorPixelType;

  typedef itk::Image< VectorPixelType, Dimension > DisplacementFieldType;

  typedef itk::SymmetricForcesDemonsRegistrationFilter<
    InternalImageType,
    InternalImageType,
    DisplacementFieldType>         RegistrationFilterType;

  RegistrationFilterType::Pointer filter = RegistrationFilterType::New();

  filter->SetStandardDeviations( 1.0 );

  //
  // Create the Command observer and register it with the registration filter.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  filter->AddObserver( itk::IterationEvent(), observer );


  // use multiresolution scheme
  typedef itk::MultiResolutionPDEDeformableRegistration<
    InternalImageType,
    InternalImageType,
    DisplacementFieldType >       MultiResRegistrationFilterType;

  MultiResRegistrationFilterType::Pointer multires =
    MultiResRegistrationFilterType::New();

  multires->SetRegistrationFilter( filter );
  multires->SetNumberOfLevels( 4 );
  multires->SetFixedImage( targetImageCaster->GetOutput() );
  multires->SetMovingImage( matcher->GetOutput() );
  unsigned int nIterations[4] = {40, 40, 32, 32 };
  multires->SetNumberOfIterations( nIterations );

  //
  // Create the Command observer and register it with the registration filter.
  //
  CommandResolutionLevelUpdate::Pointer levelobserver = CommandResolutionLevelUpdate::New();
  multires->AddObserver( itk::IterationEvent(), levelobserver );

  // apply the registration filter
  try
    {
    multires->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  // compute the output (warped) image
  typedef itk::WarpImageFilter< ImageType, ImageType, DisplacementFieldType > WarperType;
  typedef itk::LinearInterpolateImageFunction< ImageType, double > InterpolatorType;

  WarperType::Pointer warper = WarperType::New();

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  ImageType::Pointer targetImage = targetReader->GetOutput();
  warper->SetInput( sourceReader->GetOutput() );
  warper->SetInterpolator( interpolator );
  warper->SetOutputSpacing( targetImage->GetSpacing() );
  warper->SetOutputOrigin( targetImage->GetOrigin() );
  warper->SetOutputDirection( targetImage->GetDirection() );
  warper->SetDisplacementField( multires->GetOutput() );

  typedef itk::ImageFileWriter< ImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[3] );
  writer->SetInput( warper->GetOutput() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  // write the deformation field
  typedef itk::ImageFileWriter< DisplacementFieldType >  DeformationWriterType;
  DeformationWriterType::Pointer defwriter = DeformationWriterType::New();
  defwriter->SetFileName( argv[4] );
  defwriter->SetInput( multires->GetOutput() );

  try
    {
    defwriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
