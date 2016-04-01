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
/***************************************************************
  This test is illustrates the desired behavior of shrinking of
  images.  It is desired that if an image is shrunk by any
  factor, that the resulting image should have the same center
  of mass as the original image.

  This had not been the case due to bugs in the
  itkShrinkImageFilter. When shrinking an image, it is required
  that the origin location be changed in order to preserve the
  physical space of the objects in the image.  The
  itkShrinkImageFilter now has changes that allow the proper
  behavior to occur.

 ***************************************************************/


#include "itkImageMomentsCalculator.h"
#include "itkMultiResolutionPyramidImageFilter.h"

//typedef itk::Image<signed short, 2> TImageType;
typedef itk::Image<signed short, 2> WImageType;
//typedef itk::Image<signed short, 2> TImageType;
typedef itk::Image<float, 2> TImageType;
//typedef itk::Image<double, 2> TImageType;

//Need to use a Pyramid filter here instead of just downsampling to a 32x32 image
typedef itk::MultiResolutionPyramidImageFilter<TImageType,TImageType> PyramidFilterType;

PyramidFilterType::Pointer MakeTwoLevelPyramid(TImageType::Pointer refImage)
{
  PyramidFilterType::ScheduleType pyramidSchedule;

  PyramidFilterType::Pointer MyPyramid=PyramidFilterType::New();
  MyPyramid->SetInput(refImage);
  MyPyramid->SetNumberOfLevels(2);
  MyPyramid->SetMaximumError(1.e-5);
  pyramidSchedule.SetSize(2,2);
  for( unsigned int c=0;c<pyramidSchedule.cols();c++ )
    {
    pyramidSchedule[0][c]=8;
    pyramidSchedule[1][c]=4;
    }
  MyPyramid->SetSchedule(pyramidSchedule);
  MyPyramid->Update();
  return MyPyramid;
}


TImageType::PointType GetImageCenterPhysicalPoint(TImageType::Pointer & image)
{
  const TImageType::SizeType imageOverallSize=image->GetLargestPossibleRegion().GetSize();
  itk::ContinuousIndex<itk::SpacePrecisionType, TImageType::ImageDimension> centerIndex;
  itk::ContinuousIndex<itk::SpacePrecisionType, TImageType::ImageDimension> firstIndex;
  itk::ContinuousIndex<itk::SpacePrecisionType, TImageType::ImageDimension> lastIndex;
  for( unsigned int q=0;q<TImageType::ImageDimension;q++ )
    {
    lastIndex[q]=(imageOverallSize[q]-1);
    firstIndex[q]=0;
    centerIndex[q]=0.5*(imageOverallSize[q]-1);
    }
  TImageType::PointType centerLocation;
  image->TransformContinuousIndexToPhysicalPoint( centerIndex,  centerLocation );
  TImageType::PointType firstLocation;
  image->TransformContinuousIndexToPhysicalPoint( firstIndex,  firstLocation );
  TImageType::PointType lastLocation;
  image->TransformContinuousIndexToPhysicalPoint( lastIndex,  lastLocation );
  std::cout << "FirstLocation=" << firstLocation << " LastLocation=" << lastLocation << " CenterLocation=" << centerLocation << std::endl;
  return centerLocation;
}

TImageType::PointType GetCenterOfMass(TImageType::Pointer volume)
{
  TImageType::PointType CenterOfMass;
    {
    typedef itk::ImageMomentsCalculator<TImageType> momentsCalculatorType;
    momentsCalculatorType::Pointer moments=momentsCalculatorType::New();
    moments->SetImage(volume);
    moments->Compute();
    TImageType::PointType::VectorType tempCenterOfMass=moments->GetCenterOfGravity();
    for( unsigned int q=0;q<TImageType::ImageDimension;q++ )
      {
      CenterOfMass[q]=tempCenterOfMass[q];
      }
    }
  return  CenterOfMass;
}

TImageType::PointType ComputeCG(TImageType::Pointer img)
{
  itk::ImageRegionConstIteratorWithIndex< TImageType > it( img,
    img->GetLargestPossibleRegion() );
  TImageType::PointType Cg;
  Cg.Fill( 0.0 );
  double sumMass=0.0;
  while( !it.IsAtEnd() )
    {
    const double value = it.Value();
    sumMass += value;
    TImageType::IndexType indexPosition = it.GetIndex();
    TImageType::PointType physicalPosition;
    img->TransformIndexToPhysicalPoint(indexPosition, physicalPosition);

    for(unsigned int i=0; i<TImageType::ImageDimension; i++)
      {
      Cg[i] += physicalPosition[i] * value;
      }
    ++it;
    }
  for(unsigned int i=0; i<TImageType::ImageDimension; i++)
    {
    Cg[i]=Cg[i]/sumMass;
    }
  return Cg;
}

int itkShrinkImagePreserveObjectPhysicalLocations(int, char* [] )
{

  /*  Make an image that is 32x32 */
  TImageType::SizeType newSize;
  TImageType::SpacingType newSpacing;
  TImageType::PointType newOrigin;

  for( unsigned int i =0; i < TImageType::ImageDimension; i++ )
    {
    newSize[i]=32;
    newSpacing[i]=(1.0+3.0*i);
    newOrigin[i]=-1.0*(newSize[i]-1)*(newSpacing[i])*0.5;
    }
  TImageType::DirectionType newDirection;
  newDirection.SetIdentity();
  newDirection[0][0]=0;
  newDirection[0][1]=-1;
  newDirection[1][0]=1;
  newDirection[1][1]=0;
  newOrigin=newDirection*newOrigin;

  TImageType::Pointer image=TImageType::New();
  image->SetOrigin( newOrigin );
  image->SetSpacing( newSpacing );
  image->SetRegions( newSize );
  image->SetDirection( newDirection );
  image->Allocate();
  image->FillBuffer(0.0);
  image->Print(std::cout);

  TImageType::IndexType Index;
  for( int u=12;u<20;u++ )
    {
    Index[0]=u;
    for( int v=12;v<20;v++ )
      {
      Index[1]=v;
      image->SetPixel(Index,255.0);
      }
    }

  PyramidFilterType::Pointer MyPyramid=MakeTwoLevelPyramid(image);
  TImageType::Pointer ReallySmallImage=MyPyramid->GetOutput(0);
  TImageType::Pointer SmallImage=MyPyramid->GetOutput(1);

  itk::ShrinkImageFilter< TImageType, TImageType >::Pointer Shrinkfilter= itk::ShrinkImageFilter< TImageType, TImageType >::New();
  Shrinkfilter->SetInput( image );
  Shrinkfilter->SetShrinkFactors( 4 );
  Shrinkfilter->Update();
  TImageType::Pointer ShrinkSmallImage=Shrinkfilter->GetOutput();

  itk::DiscreteGaussianImageFilter< TImageType,  TImageType>::Pointer smoother= itk::DiscreteGaussianImageFilter< TImageType,  TImageType>::New();
  smoother->SetInput( image );
  smoother->SetUseImageSpacing( true );
  smoother->SetMaximumError( MyPyramid->GetMaximumError() );
  // compute shrink factors and variances
  double variance[2];
  for( unsigned int idim = 0; idim < TImageType::ImageDimension; idim++ )
    {
    variance[idim] = itk::Math::sqr( 0.5 * static_cast<float>( 4 ) );
    }
  smoother->SetVariance( variance );
  smoother->Update();

  TImageType::Pointer GaussianImage=smoother->GetOutput();

  itk::ShrinkImageFilter< TImageType, TImageType >::Pointer smootherShrinkfilter= itk::ShrinkImageFilter< TImageType, TImageType >::New();
  smootherShrinkfilter->SetInput( GaussianImage );
  smootherShrinkfilter->SetShrinkFactors( 4 );
  smootherShrinkfilter->Update();
  TImageType::Pointer GaussianShrinkSmallImage=smootherShrinkfilter->GetOutput();

//#define __WRITE_DEBUG_IMAGING__
#ifdef __WRITE_DEBUG_IMAGING__
  typedef itk::ImageFileWriter< WImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  itk::CastImageFilter<TImageType, WImageType>::Pointer castFilter=itk::CastImageFilter<TImageType, WImageType>::New();

  try
    {
    writer->SetFileName("PyramidTestFullSize.tif");
    castFilter->SetInput(image);
    writer->SetInput(castFilter->GetOutput());
    writer->Update();
    writer->SetFileName("PyramidTestQuarterSize.tif");
    castFilter->SetInput(SmallImage);
    writer->SetInput(castFilter->GetOutput());
    writer->Update();
    writer->SetFileName("PyramidTestEightSize.tif");
    castFilter->SetInput(ReallySmallImage);
    writer->SetInput(castFilter->GetOutput());
    writer->Update();

    writer->SetFileName("ShrinkTestQuarterSize.tif");
    castFilter->SetInput(ShrinkSmallImage);
    writer->SetInput(castFilter->GetOutput());
    writer->Update();

    writer->SetFileName("GaussianTestFullSize.tif");
    castFilter->SetInput(GaussianImage);
    writer->SetInput(castFilter->GetOutput());
    writer->Update();

    writer->SetFileName("GaussianTestQuarterSize.tif");
    castFilter->SetInput(GaussianShrinkSmallImage);
    writer->SetInput(castFilter->GetOutput());
    writer->Update();
    }
  catch (itk::ExceptionObject &err)
    {
    std::cout << "Exception Object caught: " << std::endl;
    std::cout << err << std::endl;
    throw;
    }
#endif

  //Known CG=[0,0]
  TImageType::PointType testCG;
  int errorCount=0;
    {
    TImageType::Pointer test=image;
    std::cout << "\nFullSizeImage...";
    testCG=ComputeCG( test );
    if (testCG.GetVectorFromOrigin().GetNorm() > 1e-4)
      {
      errorCount++;
      std::cout << "FAILED" << std::endl;
      }
    else
      {
      std::cout << "PASSED" << std::endl;
      }
    std::cout << "Origin="<< test->GetOrigin() << " CenterOfSpace=" << GetImageCenterPhysicalPoint(test) << " CenterOfMass=" << GetCenterOfMass(test) << "CG=" <<testCG << std::endl;
    std::cout << "\nQuarterSizeImage...";
    test=SmallImage;
    testCG=ComputeCG( test );
    if (testCG.GetVectorFromOrigin().GetNorm() > (newSpacing[1] * 4.0 / 2.0))
      {
      errorCount++;
      std::cout << "FAILED" << std::endl;
      }
    else
      {
      std::cout << "PASSED" << std::endl;
      }
    std::cout << "Origin="<< test->GetOrigin() << " CenterOfSpace=" << GetImageCenterPhysicalPoint(test) << " CenterOfMass=" << GetCenterOfMass(test) << "CG=" <<testCG << std::endl;
    std::cout << "\nEighthSizeImage...";
    test=ReallySmallImage;
    testCG=ComputeCG( test );
    if (testCG.GetVectorFromOrigin().GetNorm() > (newSpacing[1] * 8.0 / 2.0))
      {
      errorCount++;
      std::cout << "FAILED" << std::endl;
      }
    else
      {
      std::cout << "PASSED" << std::endl;
      }
    std::cout << "Origin="<< test->GetOrigin() << " CenterOfSpace=" << GetImageCenterPhysicalPoint(test) << " CenterOfMass=" << GetCenterOfMass(test) << "CG=" <<testCG << std::endl;
    std::cout << "\nShrinkSmallSizeImage...";
    test=ShrinkSmallImage;
    testCG=ComputeCG( test );
    if (testCG.GetVectorFromOrigin().GetNorm() > 1e-4)
      {
      errorCount++;
      std::cout << "FAILED" << std::endl;
      }
    else
      {
      std::cout << "PASSED" << std::endl;
      }
    std::cout << "Origin="<< test->GetOrigin() << " CenterOfSpace=" << GetImageCenterPhysicalPoint(test) << " CenterOfMass=" << GetCenterOfMass(test) << "CG=" <<testCG << std::endl;

    std::cout << "\nGaussianFullSizeImage...";
    test=GaussianImage;
    testCG=ComputeCG( test );
    if (testCG.GetVectorFromOrigin().GetNorm() > 1e-4)
      {
      errorCount++;
      std::cout << "FAILED" << std::endl;
      }
    else
      {
      std::cout << "PASSED" << std::endl;
      }
    std::cout << "Origin="<< test->GetOrigin() << " CenterOfSpace=" << GetImageCenterPhysicalPoint(test) << " CenterOfMass=" << GetCenterOfMass(test) << "CG=" <<testCG << std::endl;

    std::cout << "\nGaussianQuarterSizeImage...";
    test=GaussianShrinkSmallImage;
    testCG=ComputeCG( test );
    if (testCG.GetVectorFromOrigin().GetNorm() > (newSpacing[1] * 4.0 / 2.0))
      {
      errorCount++;
      std::cout << "FAILED" << std::endl;
      }
    else
      {
      std::cout << "PASSED" << std::endl;
      }
    std::cout << "Origin="<< test->GetOrigin() << " CenterOfSpace=" << GetImageCenterPhysicalPoint(test) << " CenterOfMass=" << GetCenterOfMass(test) << "CG=" <<testCG << std::endl;
    }
  std::cout << "Found " << errorCount << " errors." << std::endl;

  return errorCount;
}
