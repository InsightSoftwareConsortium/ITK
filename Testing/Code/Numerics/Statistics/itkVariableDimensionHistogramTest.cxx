/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVariableDimensionHistogramTest.cxx
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
#include "itkVariableDimensionHistogram.h"


int itkVariableDimensionHistogramTest(int, char* [] ) 
{
  std::cout << "VariableDimensionHistogram Test" << std::endl;
  bool pass = true;
  std::string whereFail = "" ;
  
  unsigned int HistogramDimension = 3;
  typedef float MeasurementType ;

  // creates a histogram with 3 components measurement vectors
  typedef itk::Statistics::VariableDimensionHistogram< MeasurementType > HistogramType ;
  HistogramType::Pointer histogram = HistogramType::New() ;

  // initializes a 64 x 64 x 64 histogram with equal size interval
  HistogramType::SizeType size(HistogramDimension) ;
  size.Fill(64) ;
  unsigned long totalSize = size[0] * size[1] * size[2] ;

  HistogramType::MeasurementVectorType lowerBound( HistogramDimension ) ;
  HistogramType::MeasurementVectorType upperBound( HistogramDimension ) ;
  lowerBound.Fill(0.0) ;
  upperBound.Fill(1024.0) ;
  
  histogram->Initialize(size, lowerBound, upperBound ) ;
  
  histogram->SetToZero();
  
  HistogramType::MeasurementType interval = 
    (upperBound[0] - lowerBound[0]) / 
    static_cast< HistogramType::MeasurementType >(size[0]) ;

  // tests begin
  HistogramType::MeasurementVectorType measurements( HistogramDimension ) ;
  measurements.Fill(512.0) ;
  HistogramType::IndexType index( HistogramDimension ) ;
  HistogramType::IndexType ind(   HistogramDimension );
  index.Fill(32) ;
  
  if(histogram->GetIndex(measurements,ind))
    {
    if(index != ind)
      {
      pass = false ;
      whereFail = "GetIndex(MeasurementVectorType&)";
      }
    }
  else
    {
    pass = false ;
    whereFail = "GetIndex(MeasurementVectorType&)";
    }
  
  HistogramType::InstanceIdentifier id = 
    histogram->GetInstanceIdentifier(index);
  if (index != histogram->GetIndex(id))
    {
    pass = false ;
    whereFail = "GetIndex(InstanceIdentifier&)" ;
    }

  index.Fill(100) ;
  
  if (!histogram->IsIndexOutOfBounds(index))
    {
    pass = false ;
    whereFail = "IsIndexOutOfBound(IndexType)" ;
    }

  if (totalSize != histogram->Size())
    {
    pass = false ;
    whereFail = "Size()" ;
    }

  if (size != histogram->GetSize())
    {
    pass = false ;
    whereFail = "GetSize()" ;
    }

  if ((lowerBound[0] + interval * 31) != histogram->GetBinMin(0,31))
    {
    pass = false ;
    whereFail = "GetBinMin(Dimension, nthBin)" ;
    }

  if ((lowerBound[0] + interval * 32) != histogram->GetBinMax(0,31))
    {
    pass = false ;
    whereFail = "GetBinMax(Dimension, nthBin)" ;
    }

  for (id = 0 ; 
       id < static_cast< HistogramType::InstanceIdentifier >(totalSize) ;
       id++)
    {
    histogram->SetFrequency(id, 1) ;
    histogram->IncreaseFrequency(id, 1) ;
    if (histogram->GetFrequency(id) != 2)
      {
      pass = false ;
      whereFail = 
        "SetFrequency(InstanceIdentifier, 1) + IncreaseFrequency(InstanceIdentifier, 1) + GetFrequency(InstanceIdentifier)" ;
      }
    }

  if (histogram->Quantile(0, 0.5) != 512.0)
    {
    pass = false ;
    whereFail = "Quantile(Dimension, percent)" ;
    }

  if( !pass )
    {
    std::cout << "Test failed in " << whereFail << "." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}



