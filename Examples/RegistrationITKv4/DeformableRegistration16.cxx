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
/*******************************************************************************

  Abstract:  -  Multiresolution   demons registration - 4 multiresolution
levels Created: June 25 2008 Last Revision 7/9/2008 by Vidya Rajagopalan  on
7/9/2008

  Copyright (c) 2008, Bioimaging Systems Lab, Virginia Tech
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
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
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************/
// Software Guide : BeginLatex
//
// This example illustrates the use of the
// \doxygen{MultiResolutionPDEDeformableRegistration} class for performing
// deformable registration of two $2D$ images using multiple resolution
// levels.
//
// The MultiResolution filter drives a DemonsRegistrationFilter
// at every level of resolution in the pyramid.
//
// \index{itk::MultiResolutionPDEDeformableRegistration}
// \index{itk::DemonsRegistrationFilter}
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
#include "itkHistogramMatchingImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkResampleImageFilter.h"
#include "itkDisplacementFieldTransform.h"

unsigned int RmsCounter = 0;
double       MaxRmsE[4] = { 0.8, 0.75, 0.4, 0.2 };

//
//  The following section of code implements a Command observer
//  that will monitor the evolution of the registration process.
//  This observer has a layer of intelligence, for deciding what
//  MaximumRMS convergence criteria to use at every resolution level.
//
class CommandIterationUpdate : public itk::Command
{
public:
  using Self = CommandIterationUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  CommandIterationUpdate() = default;

  // define ITK short-hand types
  using PixelType = short;
  using InternalPixelType = float;
  using ImageType = itk::Image<PixelType, 2>;
  using InternalImageType = itk::Image<InternalPixelType, 2>;
  using VectorPixelType = itk::Vector<float, 2>;
  using DisplacementFieldType = itk::Image<VectorPixelType, 2>;
  using RegistrationFilterType =
    itk::DemonsRegistrationFilter<InternalImageType,
                                  InternalImageType,
                                  DisplacementFieldType>;

public:
  void
  Execute(const itk::Object *, const itk::EventObject &) override
  {
    std::cout << "Warning: The const Execute method shouldn't be called"
              << std::endl;
  }

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    auto * filter = static_cast<RegistrationFilterType *>(caller);

    if (!(itk::IterationEvent().CheckEvent(&event)))
    {
      return;
    }
    if (filter)
    {
      filter->SetMaximumRMSError(MaxRmsE[RmsCounter]);
      std::cout << filter->GetMetric()
                << "  RMS Change: " << filter->GetRMSChange() << std::endl;

      std::cout << "Level Tolerance=  " << filter->GetMaximumRMSError()
                << std::endl;
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
  using Self = CommandResolutionLevelUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  CommandResolutionLevelUpdate() = default;

public:
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }
  void
  Execute(const itk::Object *, const itk::EventObject &) override
  {
    std::cout << "----------------------------------" << std::endl;
    RmsCounter = RmsCounter + 1;
    std::cout << "----------------------------------" << std::endl;
  }
};


int
main(int argc, char * argv[])
{

  // Verify the number of parameters in the command line
  if (argc != 5)
  {
    std::cerr << "usage: " << std::endl;
    std::cerr << argv[0]
              << " fixedImage movingImage registeredImage deformationField"
              << std::endl;
    return EXIT_FAILURE;
  }

  // define ITK short-hand types
  constexpr unsigned int Dimension = 2;
  using PixelType = short;
  using InternalPixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;
  using InternalImageType = itk::Image<InternalPixelType, Dimension>;
  using ImageCasterType = itk::CastImageFilter<ImageType, InternalImageType>;


  // setup input file readers
  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer targetReader = ReaderType::New();
  targetReader->SetFileName(argv[1]);
  targetReader->Update();

  ReaderType::Pointer sourceReader = ReaderType::New();
  sourceReader->SetFileName(argv[2]);
  sourceReader->Update();


  // cast target and source to float
  ImageCasterType::Pointer targetImageCaster = ImageCasterType::New();
  ImageCasterType::Pointer sourceImageCaster = ImageCasterType::New();
  targetImageCaster->SetInput(targetReader->GetOutput());
  sourceImageCaster->SetInput(sourceReader->GetOutput());

  // match the histograms between source and target
  using MatchingFilterType =
    itk::HistogramMatchingImageFilter<InternalImageType, InternalImageType>;

  MatchingFilterType::Pointer matcher = MatchingFilterType::New();

  matcher->SetInput(sourceImageCaster->GetOutput());
  matcher->SetReferenceImage(targetImageCaster->GetOutput());
  matcher->SetNumberOfHistogramLevels(1024);
  matcher->SetNumberOfMatchPoints(7);
  matcher->ThresholdAtMeanIntensityOn();

  // setup the deformation field and filter
  using VectorPixelType = itk::Vector<float, Dimension>;

  using DisplacementFieldType = itk::Image<VectorPixelType, Dimension>;

  using RegistrationFilterType =
    itk::DemonsRegistrationFilter<InternalImageType,
                                  InternalImageType,
                                  DisplacementFieldType>;

  RegistrationFilterType::Pointer filter = RegistrationFilterType::New();

  filter->SetStandardDeviations(1.0);

  //
  // Create the Command observer and register it with the registration filter.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  filter->AddObserver(itk::IterationEvent(), observer);


  // use multiresolution scheme
  using MultiResRegistrationFilterType =
    itk::MultiResolutionPDEDeformableRegistration<InternalImageType,
                                                  InternalImageType,
                                                  DisplacementFieldType>;

  MultiResRegistrationFilterType::Pointer multires =
    MultiResRegistrationFilterType::New();

  multires->SetRegistrationFilter(filter);
  multires->SetNumberOfLevels(4);
  multires->SetFixedImage(targetImageCaster->GetOutput());
  multires->SetMovingImage(matcher->GetOutput());
  unsigned int nIterations[4] = { 40, 40, 32, 32 };
  multires->SetNumberOfIterations(nIterations);

  //
  // Create the Command observer and register it with the registration filter.
  //
  CommandResolutionLevelUpdate::Pointer levelobserver =
    CommandResolutionLevelUpdate::New();
  multires->AddObserver(itk::IterationEvent(), levelobserver);

  // apply the registration filter
  try
  {
    multires->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  using DisplacementFieldTransformType =
    itk::DisplacementFieldTransform<InternalPixelType, Dimension>;
  auto displacementTransform = DisplacementFieldTransformType::New();
  displacementTransform->SetDisplacementField(multires->GetOutput());

  // compute the output (warped) image
  using InterpolatorPrecisionType = double;
  using WarperType = itk::ResampleImageFilter<ImageType,
                                              ImageType,
                                              InterpolatorPrecisionType,
                                              InternalPixelType>;
  using InterpolatorType =
    itk::LinearInterpolateImageFunction<ImageType, InterpolatorPrecisionType>;

  WarperType::Pointer warper = WarperType::New();

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  ImageType::Pointer targetImage = targetReader->GetOutput();
  warper->SetInput(sourceReader->GetOutput());
  warper->SetInterpolator(interpolator);
  warper->SetOutputSpacing(targetImage->GetSpacing());
  warper->SetOutputOrigin(targetImage->GetOrigin());
  warper->SetOutputDirection(targetImage->GetDirection());
  warper->SetTransform(displacementTransform);

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(warper->GetOutput());

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // write the deformation field
  using DeformationWriterType = itk::ImageFileWriter<DisplacementFieldType>;
  DeformationWriterType::Pointer defwriter = DeformationWriterType::New();
  defwriter->SetFileName(argv[4]);
  defwriter->SetInput(multires->GetOutput());

  try
  {
    defwriter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
