/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImplicitManifoldNormalVectorFilterTest.cxx
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

#include <iostream>
#include "itkImplicitManifoldNormalVectorFilter.h"
#include "itkNormalVectorDiffusionFunction.h"
#include "itkSparseImage.h"
#include "itkImage.h"

namespace itk
{

template <class TImageType>
class NormalBandNode
{
public:
  typedef TImageType LevelSetImageType;
  typedef typename LevelSetImageType::PixelType   NodeValueType; 
  typedef typename LevelSetImageType::IndexType   IndexType;
  typedef Vector <NodeValueType,
                  ::itk::GetImageDimension<TImageType>::ImageDimension>
  NodeDataType;
  
  NodeDataType m_Data, m_InputData, m_Update;
  NodeDataType
  m_ManifoldNormal [::itk::GetImageDimension<TImageType>::ImageDimension];
  NodeDataType m_Flux [::itk::GetImageDimension<TImageType>::ImageDimension];

  IndexType m_Index;  
  NormalBandNode *Next;  
  NormalBandNode *Previous;
};

}

int itkImplicitManifoldNormalVectorFilterTest(int, char* [] )
{
  typedef itk::Image  <float, 2> InputImageType;
  typedef itk::NormalBandNode <InputImageType> NodeType;
  typedef itk::SparseImage <NodeType, 2> OutputImageType;
  typedef itk::ImplicitManifoldNormalVectorFilter<InputImageType,
    OutputImageType> FilterType;
  typedef itk::NormalVectorDiffusionFunction<OutputImageType> FunctionType;
  
  InputImageType::Pointer im_init = InputImageType::New();
  InputImageType::RegionType r;
  InputImageType::SizeType   sz = {{50, 50}};
  InputImageType::IndexType  idx = {{0,0}};
  r.SetSize(sz);
  r.SetIndex(idx);
  im_init->SetLargestPossibleRegion(r);
  im_init->SetBufferedRegion(r);
  im_init->SetRequestedRegion(r);
  im_init->Allocate();

  InputImageType::IndexType index;
  for ( index[0]=0; index[0] < 50; index[0]++ )
    for ( index[1]=0; index[1] < 50; index[1]++ )
      {
      im_init->SetPixel (index, static_cast<float>(index[0]));
      }
  
  FilterType::Pointer filter = FilterType::New();
  FunctionType::Pointer function = FunctionType::New();
  filter->SetInput(im_init);
  filter->SetNormalFunction(function);
  filter->SetIsoLevelLow (15.0);
  filter->SetIsoLevelHigh (35.0);
  filter->SetMaxIteration (100);
  filter->SetMinVectorNorm (0.001);
     
  std::cout<<"Max iteration = "<<(filter->GetMaxIteration())<<"\n";
  std::cout<<"IsoLevelLow = "<<(filter->GetIsoLevelLow())<<"\n";
  std::cout<<"IsoLevelHigh = "<<(filter->GetIsoLevelHigh())<<"\n";
  std::cout<<"MinVectorNorm = "<<(filter->GetMinVectorNorm())<<"\n";
  std::cout<<"UnsharpMaskingFlag = "<<(filter->GetUnsharpMaskingFlag())<<"\n";
  std::cout<<"UnsharpMaskingWeight = "
           <<(filter->GetUnsharpMaskingWeight())<<"\n";
  std::cout<<"Precomputeflag = "<<(filter->GetPrecomputeFlag())<<"\n";
  
  filter->Print(std::cout);
  function->Print(std::cout);
  try {
  filter->Update();
  }
  catch (itk::ExceptionObject &e)
    {
      std::cerr << e << std::endl;
    }
  
  return EXIT_SUCCESS;
}
