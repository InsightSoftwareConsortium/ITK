/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdLabelerImageFilterTest.cxx
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

//  

#include "itkImage.h"
#include "itkThresholdLabelerImageFilter.h"

int itkThresholdLabelerImageFilterTest( int, char *[] )
{
  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  const unsigned int Dimension = 2;
  
  typedef float InputPixelType;
  typedef unsigned long LabeledPixelType;

  typedef itk::Image<InputPixelType,Dimension> InputImageType;
  typedef itk::Image<LabeledPixelType,Dimension> LabeledImageType;

  // create an image with stripes to label
  InputImageType::IndexType index = {{0, 0}};
  InputImageType::SizeType size = {{32, 32}};
  InputImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);

  InputImageType::Pointer inputImage = InputImageType::New();
  inputImage->SetLargestPossibleRegion(region);
  inputImage->SetBufferedRegion(region);
  inputImage->Allocate();

  size[0] = 32;
  size[1] = 8;
  region.SetSize(size);

  // stripe y indexes
  typedef std::vector<InputImageType::IndexType::IndexValueType> IndexValueVectorType;
  IndexValueVectorType yindexes;
  yindexes.push_back(0);
  yindexes.push_back(8);
  yindexes.push_back(16);
  yindexes.push_back(24);

  // values for each stripe
  std::vector<InputPixelType> values;
  values.push_back(0.5);
  values.push_back(1.5);
  values.push_back(2.5);
  values.push_back(3.5);

  // thresholds between values
  std::vector<InputPixelType> thresholds;
  thresholds.push_back(1.0);
  thresholds.push_back(2.0);
  thresholds.push_back(3.0);

  // offset
  unsigned long offset = 4;

  // labels
  std::vector<LabeledPixelType> labels;
  labels.push_back(0 + offset);
  labels.push_back(1 + offset);
  labels.push_back(2 + offset);
  labels.push_back(3 + offset);

  // fill in the image
  unsigned int i;
  IndexValueVectorType::const_iterator indexIter;
  for (indexIter = yindexes.begin(), i=0; indexIter != yindexes.end(); ++indexIter, ++i) 
    {
    index[0] = 0;
    index[1] = *indexIter;
    region.SetIndex(index);
    itk::ImageRegionIterator<InputImageType> iter(inputImage,region);
    for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
      {
      iter.Set(values[i]);
      }
    }

  // apply labeler filter
  typedef itk::ThresholdLabelerImageFilter<InputImageType,LabeledImageType> LabelerFilterType;
  LabelerFilterType::Pointer labelerFilter = LabelerFilterType::New();
  labelerFilter->SetInput(inputImage);
  labelerFilter->SetThresholds(thresholds);
  labelerFilter->SetLabelOffset(offset);
  labelerFilter->Print(std::cout);
  try
    {
      labelerFilter->Update();
      labelerFilter->SetFunctor(labelerFilter->GetFunctor());
    }

  catch(itk::ExceptionObject & exp)
    {
      std::cerr << exp << std::endl;
    }

  // check if labels coincide with expected labels
  bool passed = true;

  for (indexIter = yindexes.begin(), i=0; indexIter != yindexes.end(); ++indexIter, ++i) 
    {
    index[0] = 0;
    index[1] = *indexIter;
    region.SetIndex(index);
    itk::ImageRegionConstIterator<LabeledImageType> iter(labelerFilter->GetOutput(),region);
    for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
      {
      if (iter.Get() != labels[i])
        {
        passed = false;
        break;
        }
      }
    if (!passed)
      {
      break;
      }
    }
  
  if (!passed)
    {  
    std::cout << "Test failed." << std::endl;
    std::cout << labelerFilter << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
