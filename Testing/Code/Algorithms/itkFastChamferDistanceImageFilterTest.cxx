/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastChamferDistanceImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkFastChamferDistanceImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
//#include "itkImageFileReader.h"
//#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
// simple signed distance function

namespace {
template <typename TPoint>
double
SimpleSignedDistance( const TPoint & p )
{
  TPoint center;
  center.Fill( 32 );
  double radius = 10;

  double accum = 0.0;
  for( unsigned int j = 0; j < TPoint::PointDimension; j++ )
    {
    accum += vnl_math_sqr( p[j] - center[j] );
    }
  accum = vcl_sqrt( accum );
  if (fabs(accum - radius) > 1)
    {
    if((accum - radius) > 0)
      return radius;
    else
      return -radius;
    }
  else
    {
    return ( accum - radius );
    }
}

}





int itkFastChamferDistanceImageFilterTest(int, char* [] )
{

  std::cout<< "Test ITK Chamfer Distance Image Filter" << std::endl;
   
  std::cout << "Compute the distance map of a 64x64 image" << std::endl;

  const unsigned int ImageDimension = 2;
  typedef float PixelType;
  
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Point<double,ImageDimension> PointType;
  
  ImageType::SizeType size = {{64,64}};
  ImageType::IndexType index = {{0,0}};
  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  
  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();
  

  typedef  itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType it(inputImage,region);

  // Set the image to 0
  while( !it.IsAtEnd() ) 
  { 
    PointType point;
    inputImage->TransformIndexToPhysicalPoint( it.GetIndex(), point );
    it.Set( SimpleSignedDistance( point ) );
    ++it;
  }
  
  /* Create Fast Chamfer Distance filter */
  typedef itk::FastChamferDistanceImageFilter<
                                ImageType,ImageType> ChamferFilterType;
  ChamferFilterType::Pointer filter = ChamferFilterType::New();
  
  filter->SetInput(inputImage);
  
  ImageType::Pointer outputImage = filter->GetOutput();
  
  try
    {
      filter->Update();
    }
  catch( itk::ExceptionObject & err )
    { 
      std::cout << "ExceptionObject caught !" << std::endl; 
      std::cout << err << std::endl; 
      return -1;
    } 
  
  //Create NarrowBand
  typedef ChamferFilterType::BandNodeType BandNodeType;
  typedef ChamferFilterType::NarrowBandType NarrowBandType;
  
  NarrowBandType::Pointer band = NarrowBandType::New();
  band->SetTotalRadius(4);
  band->SetInnerRadius(2);
  filter->SetMaximumDistance(5);
  std::cout<<"Band initial size: "<<band->Size()<<std::endl;  
  filter->SetNarrowBand(band.GetPointer());
  filter->Update();
  
  std::cout<<"Band size: "<<band->Size()<<std::endl;

  //Loop through the band
  typedef NarrowBandType::ConstIterator itNBType;
  itNBType itNB = band->Begin();
  itNBType itNBend = band->End();
 
//  BandNodeType *tmp;
  unsigned int innerpositive=0;
  unsigned int innernegative=0;
  unsigned int otherpoints=0;
  for( ; itNB != itNBend ; itNB++)
  {  
    if(itNB->m_NodeState == 3)
      {
      innerpositive++;
      }
    else if(itNB->m_NodeState == 2)
      {
      innernegative++;
      }
    else
      {
      otherpoints++;
      }
  }     
    
  std::cout<<"Inner positive points: "<<innerpositive
      <<" Inner negative points: "<<innernegative
      <<" Rest of points: "<<otherpoints<<std::endl;  
  
  //Exercising filter methods
  float inweights[2];
  inweights[0]=0.926;
  inweights[1]=1.34;
  const float *outweights;
  filter->SetWeights(inweights);
  outweights =  filter->GetWeights();

  std::cout << "outweights = " << outweights << std::endl;
  
  /* For debugging write the result
  typedef itk::ImageFileWriter< ImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  
  writer->SetFileName("chamferoutput.mhd");
  writer->SetInput(filter->GetOutput());
  writer->Update();
  */
  
  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;  

}
