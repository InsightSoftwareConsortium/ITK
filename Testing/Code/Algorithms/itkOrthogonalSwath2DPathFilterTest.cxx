/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrthogonalSwath2DPathFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkOrthogonallyCorrected2DParametricPath.h"
#include "itkOrthogonalSwath2DPathFilter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkPolyLineParametricPath.h"
#include "itkChainCodePath.h"
#include "itkFourierSeriesPath.h"
#include "itkPathToChainCodePathFilter.h"
#include "itkChainCodeToFourierSeriesPathFilter.h"

int itkOrthogonalSwath2DPathFilterTest(int, char*[])
{
  typedef itk::Image<unsigned char, 2>                ImageType;         
  typedef itk::PolyLineParametricPath<2>              InPathType;        
  typedef itk::ChainCodePath<2>                       ChainPathType;     
  typedef itk::FourierSeriesPath<2>                   FSPathType;        

  typedef InPathType::VertexType                      VertexType;        
  typedef InPathType::OffsetType                      OffsetType;        
  typedef InPathType::InputType                       InPathInputType;   

  typedef ImageType::IndexType                        IndexType;         
  typedef ImageType::SizeType                         SizeType;          

  typedef itk::OrthogonallyCorrected2DParametricPath  OutputPathType;    

  typedef itk::PathToChainCodePathFilter<InPathType,ChainPathType>  Filter1Type;
  typedef itk::ChainCodeToFourierSeriesPathFilter<ChainPathType,FSPathType>
                                                                    Filter2Type;
  typedef itk::OrthogonalSwath2DPathFilter<FSPathType,ImageType>
                                                                    Filter3Type;
  
  // Setup the image
  std::cout << "Making a 64x64 white square centered in a 128x128 black image"<<std::endl;
  ImageType::Pointer  inImage = ImageType::New();
  IndexType start;
  start[0]=0;
  start[1]=0;
  ImageType::SizeType size;
  size[0]=128;
  size[1]=128;
  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(start);
  inImage->SetRegions(region);
  double spacing[ ImageType::ImageDimension ];
  spacing[0]=1.0;
  spacing[1]=1.0;
  inImage->SetSpacing(spacing);
  inImage->Allocate();
  typedef itk::ImageRegionIterator<ImageType> ImageItType;
  ImageItType it( inImage, inImage->GetRequestedRegion() );
  it.GoToBegin();
  IndexType pixelIndex;
  while( !it.IsAtEnd() )
    {
    pixelIndex = it.GetIndex();
    if( pixelIndex[0] >= int(size[0]/4) && pixelIndex[0] < int(size[0]*3/4) &&
        pixelIndex[1] >= int(size[1]/4) && pixelIndex[1] < int(size[1]*3/4) )
      it.Set(255);
    else
      it.Set(0);
    ++it;
    }
  
  // Setup the path
  std::cout << "Making a square Path with v0 at (24,24) -> (24,104) -> (104,104) -> (104,24)" << std::endl;
  InPathType::Pointer inPath;
  VertexType        v;
  inPath = InPathType::New();
  v.Fill(24);
  inPath->AddVertex(v);
  v[0]=24;
  v[1]=104;
  inPath->AddVertex(v);
  v.Fill(104);
  inPath->AddVertex(v);
  v[0]=104;
  v[1]=24;
  inPath->AddVertex(v);
  v.Fill(24);
  inPath->AddVertex(v);
  
  // Setup the first path filter
  Filter1Type::Pointer filter1 = Filter1Type::New();
  filter1->SetInput(inPath);
  
  // Setup the second path filter
  Filter2Type::Pointer filter2 = Filter2Type::New();
  filter2->SetInput(filter1->GetOutput());
  filter2->SetNumberOfHarmonics(7); // make a nice, round, path for the swath
  
  // Setup the third filter; THIS IS THE MAIN FILTER TO BE TESTED
  std::cerr << "Creating the test filter" << std::endl;
  Filter3Type::Pointer filter3 = Filter3Type::New();
  std::cerr << "Setting up the test filter" << std::endl;
  filter3->SetPathInput(filter2->GetOutput());
  filter3->SetImageInput(inImage);
  
  // Setup the output
  std::cerr << "Setting up the test filter's output" << std::endl;
  OutputPathType::Pointer            outPath;
  outPath=filter3->GetOutput();
  
  // Testing PrintSelf
  std::cerr << filter3 << std::endl;
  
  // Update the pipeline
  std::cerr << "Running the Pipeline: ";
  outPath->Update();
  std::cerr << "[DONE]" << std::endl;
  
  return EXIT_SUCCESS;
}
