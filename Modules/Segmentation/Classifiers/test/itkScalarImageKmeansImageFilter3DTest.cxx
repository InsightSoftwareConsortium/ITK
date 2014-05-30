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
#include <iostream>
#include <fstream>

#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkLabelStatisticsImageFilter.h"
#include "itkThresholdImageFilter.h"
#include "itkMaskImageFilter.h"
#include "itkNotImageFilter.h"
#include "itkScalarImageKmeansImageFilter.h"

int itkScalarImageKmeansImageFilter3DTest (int argc, char *argv[])
{

  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0];
    std::cerr << " inputVolume input3DSkullStripVolume outputLabelMapVolume " << std::endl;
    return EXIT_FAILURE;
    }

  std::string inputVolume(argv[1]);
  std::string input3DSkullStripVolume(argv[2]);
  std::string outputLabelMapVolume(argv[3]);
  float numberOfStdDeviations = 10.0;


  bool debug=true;
  if (debug)
    {
    std::cout << "Input T1 Image: " <<  inputVolume << std::endl;
    std::cout << "Input 3DSkullStrip Mask Image: " <<  input3DSkullStripVolume << std::endl;
    std::cout << "Number of Standard Deviations: " <<  numberOfStdDeviations << std::endl;
    std::cout << "Output K-Means LabelMap Image: " <<  outputLabelMapVolume << std::endl;
    }

  bool violated=false;
  if (inputVolume.size() == 0)
    {
    violated = true; std::cout << "  --inputVolume Required! "  << std::endl;
    }
  if (input3DSkullStripVolume.size() == 0)
    {
    violated = true; std::cout << "  --input3DSkullStripVolume Required! "  << std::endl;
    }
  if (outputLabelMapVolume.size() == 0)
    {
    violated = true; std::cout << "  --outputLabelMapVolume Required! "  << std::endl;
    }
  if (violated)
    {
    exit(1);
    }

  typedef signed short       PixelType;
  const unsigned int          Dimension = 3;

  typedef itk::Image<PixelType, Dimension > ImageType;
  typedef itk::ImageFileReader< ImageType > ReaderType;

  ReaderType::Pointer T1Reader = ReaderType::New();
  T1Reader->SetFileName( inputVolume );

  ReaderType::Pointer maskReader = ReaderType::New();
  maskReader->SetFileName( input3DSkullStripVolume );

  const PixelType imageExclusion = -32000;
  const PixelType maskThresholdBelow = 5;     // someday with more generality?

  /* The Threshold Image Filter is used to produce the brain clipping mask from a 3DSkullStrip result image. */
  typedef itk::ThresholdImageFilter< ImageType > ThresholdFilterType;
  ThresholdFilterType::Pointer brainMaskFilter = ThresholdFilterType::New();
  brainMaskFilter->SetInput( maskReader->GetOutput() );
  brainMaskFilter->ThresholdBelow( maskThresholdBelow );
  brainMaskFilter->Update();

  /* The Not Image Filter is used to produce the other clipping mask. */
  typedef itk::NotImageFilter< ImageType, ImageType > NotFilterType;
  NotFilterType::Pointer nonBrainMaskFilter = NotFilterType::New();
  nonBrainMaskFilter->SetInput( maskReader->GetOutput() );
  nonBrainMaskFilter->Update();

  /* The Statistics Image Filter lets us find the initial cluster means.
     Should this be limited to the excluded region of the clipped T1 image?  */
  typedef itk::LabelStatisticsImageFilter< ImageType, ImageType > LabelStatisticsFilterType;
  typedef LabelStatisticsFilterType::RealType StatisticRealType;
  LabelStatisticsFilterType::Pointer statisticsFilter = LabelStatisticsFilterType::New();
  statisticsFilter->SetInput( T1Reader->GetOutput() );
  statisticsFilter->SetLabelInput( maskReader->GetOutput() );
  statisticsFilter->Update();

  const PixelType imageMin = static_cast<PixelType> ( statisticsFilter->GetMinimum(6) );
  const PixelType imageMax = static_cast<PixelType> ( statisticsFilter->GetMaximum(6) );
  const StatisticRealType imageMean = statisticsFilter->GetMean(6);
  const StatisticRealType imageSigma = statisticsFilter->GetSigma(6);

  std::cout << "Brain Minimum == " << imageMin << std::endl;
  std::cout << "Brain Maximum == " << imageMax << std::endl;
  std::cout << "Brain Mean == " << imageMean << std::endl;
  std::cout << "Brain Sigma == " << imageSigma << std::endl;


  /* The Statistics Image Filter lets us find the initial cluster means.
     Should this be limited to the excluded region of the clipped T1 image?  */
  LabelStatisticsFilterType::Pointer nonBrainStatisticsFilter = LabelStatisticsFilterType::New();
  nonBrainStatisticsFilter->SetInput( T1Reader->GetOutput() );
  nonBrainStatisticsFilter->SetLabelInput( nonBrainMaskFilter->GetOutput() );
  nonBrainStatisticsFilter->Update();

  const PixelType nonBrainImageMin = static_cast<PixelType> ( nonBrainStatisticsFilter->GetMinimum(1) );
  const PixelType nonBrainImageMax = static_cast<PixelType> ( nonBrainStatisticsFilter->GetMaximum(1) );
  const StatisticRealType nonBrainImageMean = nonBrainStatisticsFilter->GetMean(1);
  const StatisticRealType nonBrainImageSigma = nonBrainStatisticsFilter->GetSigma(1);

  //std::cout << "Background Minimum == " << nonBrainImageMin << std::endl;
  std::cout << "Background Maximum == " << nonBrainImageMax << std::endl;
  std::cout << "Background Minimum == " << nonBrainImageMin << std::endl;
  std::cout << "Background Mean == " << nonBrainImageMean << std::endl;
  std::cout << "Background Sigma == " << nonBrainImageSigma << std::endl;


  /* The Mask Image Filter applies the clipping mask by stepping
     on the excluded region with the imageExclusion value. */
  typedef itk::MaskImageFilter< ImageType, ImageType > MaskFilterType;
  MaskFilterType::Pointer clippedBrainT1Filter = MaskFilterType::New();
  clippedBrainT1Filter->SetInput1( T1Reader->GetOutput() );
  clippedBrainT1Filter->SetInput2( brainMaskFilter->GetOutput() );
  clippedBrainT1Filter->SetOutsideValue( imageExclusion );
  std::cout << "clippedBrainT1Filter->Update " << std::endl;
  clippedBrainT1Filter->Update();

  ImageType::Pointer clippedBrainT1Pointer;

  if (numberOfStdDeviations > 0.0 )
    {
    ThresholdFilterType::Pointer clipArterialBloodFilter = ThresholdFilterType::New();
    clipArterialBloodFilter->SetInput( clippedBrainT1Filter->GetOutput() );
    clipArterialBloodFilter->ThresholdAbove( static_cast<PixelType>(imageMean+numberOfStdDeviations*imageSigma ));
    clipArterialBloodFilter->SetOutsideValue( imageExclusion );
    std::cout << "clipArterialBloodFilter->Update " << std::endl;
    clipArterialBloodFilter->Update();
    clippedBrainT1Pointer = clipArterialBloodFilter->GetOutput();
    }
  else
    {
    clippedBrainT1Pointer = clippedBrainT1Filter->GetOutput();
    }

  /* The Mask Image Filter applies the clipping mask by stepping
     on the excluded region with the imageExclusion value. */
  MaskFilterType::Pointer clippedNonBrainT1Filter = MaskFilterType::New();
  clippedNonBrainT1Filter->SetInput1( T1Reader->GetOutput() );
  clippedNonBrainT1Filter->SetInput2( nonBrainMaskFilter->GetOutput() );
  clippedNonBrainT1Filter->SetOutsideValue( imageExclusion );
  std::cout << "clippedNonBrainT1Filter->Update " << std::endl;
  clippedNonBrainT1Filter->Update();

  /* The Scalar Image Kmeans Image Filter will find a code image in 3 classes
     for the interior of the mask, plus a code for the exterior of the mask. */
  typedef itk::ScalarImageKmeansImageFilter< ImageType > KMeansFilterType;
  typedef KMeansFilterType::RealPixelType                RealPixelType;
  KMeansFilterType::Pointer kmeansFilter = KMeansFilterType::New();
  kmeansFilter->SetInput( clippedBrainT1Pointer );

  const unsigned int useNonContiguousLabels = 1;

  RealPixelType backgroundInitialMean = imageExclusion;
  //  RealPixelType bloodInitialMean = imageMax;    // ARTERIAL blood.
  const RealPixelType csfInitialMean = imageMin;
  const RealPixelType whiteInitialMean = imageMean + imageSigma;
  const RealPixelType grayInitialMean = imageMean - imageSigma;

  std::cout << "kmeansFilter InitialMeans  " << backgroundInitialMean << ";  " << csfInitialMean << ";  " << grayInitialMean << ";  " << whiteInitialMean << std::endl;
  kmeansFilter->AddClassWithInitialMean( backgroundInitialMean );
  kmeansFilter->AddClassWithInitialMean( csfInitialMean );
  kmeansFilter->AddClassWithInitialMean( grayInitialMean );
  kmeansFilter->AddClassWithInitialMean( whiteInitialMean );
  //kmeansFilter->AddClassWithInitialMean( bloodInitialMean );

  kmeansFilter->SetUseNonContiguousLabels( useNonContiguousLabels );

  std::cout << "kmeansFilter->Update [[Watch out for infinite loop here!]] " << std::endl;
  try
    {
    kmeansFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encountered while running K-means segmentation ";
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  KMeansFilterType::ParametersType estimatedMeans =
                                            kmeansFilter->GetFinalMeans();

  unsigned int numberOfClasses = estimatedMeans.Size();

  for ( unsigned int i = 0; i < numberOfClasses; ++i )
    {
    std::cout << "Brain cluster[" << i << "] ";
    std::cout << "    estimated mean : " << estimatedMeans[i] << std::endl;
    }


  /* The Scalar Image Kmeans Image Filter will find a code image in 3 classes
     for the interior of the mask, plus a code for the exterior of the mask. */
  KMeansFilterType::Pointer kmeansNonBrainFilter = KMeansFilterType::New();
  kmeansNonBrainFilter->SetInput( clippedNonBrainT1Filter->GetOutput() );

  backgroundInitialMean = imageExclusion;
  const RealPixelType airInitialMean = imageMin;
  const RealPixelType fatInitialMean = imageMax;
  const RealPixelType muscleInitialMean = imageMean;
   // Why are these the brain region versions, and not the background region versions?  Seems to work, though.

  std::cout << "kmeansNonBrainFilter InitialMeans  " << backgroundInitialMean << ";  " << airInitialMean << ";  " << muscleInitialMean << ";  " << fatInitialMean << std::endl;
  kmeansNonBrainFilter->AddClassWithInitialMean( backgroundInitialMean );
  kmeansNonBrainFilter->AddClassWithInitialMean( airInitialMean );
  kmeansNonBrainFilter->AddClassWithInitialMean( muscleInitialMean );
  kmeansNonBrainFilter->AddClassWithInitialMean( fatInitialMean );
  kmeansNonBrainFilter->SetUseNonContiguousLabels( useNonContiguousLabels );

  std::cout << "kmeansNonBrainFilter->Update " << std::endl;
  try
    {
    kmeansNonBrainFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encountered while Background K-Means segmentation ";
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  estimatedMeans = kmeansNonBrainFilter->GetFinalMeans();

  numberOfClasses = estimatedMeans.Size();

  for ( unsigned int i = 0; i < numberOfClasses; ++i )
    {
    std::cout << "Background cluster[" << i << "] ";
    std::cout << "    estimated mean : " << estimatedMeans[i] << std::endl;
    }

  /* Now remap the labels - background first followed by brain */
  typedef KMeansFilterType::OutputImageType LabelImageType;
  LabelImageType::Pointer kmeansLabelImage = LabelImageType::New();
  kmeansLabelImage->SetRegions( maskReader->GetOutput()->GetLargestPossibleRegion() );
  kmeansLabelImage->SetSpacing( maskReader->GetOutput()->GetSpacing() );
  kmeansLabelImage->SetDirection( maskReader->GetOutput()->GetDirection() );
  kmeansLabelImage->SetOrigin( maskReader->GetOutput()->GetOrigin() );
  kmeansLabelImage->Allocate(true);

  typedef itk::LabelStatisticsImageFilter< LabelImageType, LabelImageType > LabelMapStatisticsFilterType;
  LabelMapStatisticsFilterType::Pointer statisticsNonBrainFilter = LabelMapStatisticsFilterType::New();
  statisticsNonBrainFilter->SetInput( kmeansNonBrainFilter->GetOutput() );
  statisticsNonBrainFilter->SetLabelInput( kmeansNonBrainFilter->GetOutput() );
  std::cout << "statisticsNonBrainFilter->Update " << std::endl;
  statisticsNonBrainFilter->Update();

  /* Background Tissues are Lower Label values */
  unsigned char currentLabel = 0;
  for (unsigned int i=1; i<256; i++)
    {
    if ( statisticsNonBrainFilter->HasLabel( static_cast<unsigned char> ( i ) ) )
      {
      currentLabel++;
      LabelImageType::RegionType labelRegion = statisticsNonBrainFilter->GetRegion( static_cast<unsigned char> ( i ) );
      itk::ImageRegionIterator<LabelImageType> it( kmeansNonBrainFilter->GetOutput(), labelRegion );

      it.GoToBegin();
      while( !it.IsAtEnd() )
        {
        if ( it.Get() == static_cast<unsigned char> ( i ) )
          {
          // Set Output Image
          kmeansLabelImage->SetPixel(it.GetIndex(), currentLabel);
          }
        ++it;
        }
      }
    }

  /* Brain Tissues are Higher Label values */
  LabelMapStatisticsFilterType::Pointer statisticsBrainFilter = LabelMapStatisticsFilterType::New();
  statisticsBrainFilter->SetInput( kmeansFilter->GetOutput() );
  statisticsBrainFilter->SetLabelInput( kmeansFilter->GetOutput() );
  std::cout << "statisticsBrainFilter->Update " << std::endl;
  statisticsBrainFilter->Update();

  for (unsigned int i=1; i<256; i++)
    {
    if ( statisticsBrainFilter->HasLabel( static_cast<unsigned char> ( i ) ) )
      {
      currentLabel++;
      LabelImageType::RegionType labelRegion = statisticsBrainFilter->GetRegion( static_cast<unsigned char> ( i ) );
      itk::ImageRegionIterator<LabelImageType> it( kmeansFilter->GetOutput(), labelRegion );

      it.GoToBegin();
      while( !it.IsAtEnd() )
        {
        if ( it.Get() == static_cast<unsigned char> ( i ) )
          {
          // Set Output Image
          kmeansLabelImage->SetPixel(it.GetIndex(), currentLabel);
          }
        ++it;
        }
      }
    }

  /* Write out the resulting Label Image */
  typedef itk::ImageFileWriter<LabelImageType> WriterType;
  WriterType::Pointer labelWriter = WriterType::New();
  labelWriter->SetInput( kmeansLabelImage );
  labelWriter->SetFileName( outputLabelMapVolume );
  std::cout << "labelWriter->Update " << std::endl;
  labelWriter->Update( );


  return EXIT_SUCCESS;


}
