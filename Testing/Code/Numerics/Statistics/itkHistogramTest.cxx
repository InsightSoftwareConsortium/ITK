/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHistogramTest.cxx
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
#include "itkHistogram.h"

int itkHistogramTest(int argc, char* argv [] ) 
{

  {
  // Test out histogram.. first with FixedArrays.....
  std::cout << "Histogram Test With Compile-time fixed length MeasurementVector" << std::endl; 
  bool pass = true;
  std::string whereFail = "" ;
  
  typedef float MeasurementType ;

  // creats a histogram with 3 components measurement vectors
  typedef itk::Statistics::Histogram< MeasurementType, 3 > HistogramType ;
  HistogramType::Pointer histogram = HistogramType::New() ;

  // initializes a 64 x 64 x 64 histogram with equal size interval
  HistogramType::SizeType size ;
  size.Fill(64) ;
  unsigned long totalSize = size[0] * size[1] * size[2] ;
  HistogramType::MeasurementVectorType lowerBound ;
  HistogramType::MeasurementVectorType upperBound ;
  lowerBound.Fill(0.0) ;
  upperBound.Fill(1024.0) ;
  histogram->Initialize(size, lowerBound, upperBound ) ;
  histogram->SetToZero();
  HistogramType::MeasurementType interval = 
    (upperBound[0] - lowerBound[0]) / 
    static_cast< HistogramType::MeasurementType >(size[0]) ;

  // tests begin
  HistogramType::MeasurementVectorType measurements ;
  measurements.Fill(512.0) ;
  HistogramType::IndexType index ;
  HistogramType::IndexType ind;
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


  // Histogram with SparseFrequencyContainer
  typedef itk::Statistics::Histogram< MeasurementType, 3, 
    itk::Statistics::SparseFrequencyContainer< float > > SparseHistogramType ;
  SparseHistogramType::Pointer sparseHistogram = SparseHistogramType::New() ;

  // initializes a 64 x 64 x 64 histogram with equal size interval
  sparseHistogram->Initialize(size, lowerBound, upperBound ) ;
  sparseHistogram->SetToZero();
  interval = (upperBound[0] - lowerBound[0]) / 
    static_cast< SparseHistogramType::MeasurementType >(size[0]) ;

  measurements.Fill(512.0) ;
  index.Fill(32);
  sparseHistogram->GetIndex(measurements,ind);

  if (index != ind)
    {
    pass = false ;
    whereFail = "Sparse Histogram: GetIndex(MeasurementVectorType&)" ;
    }
  
  id = sparseHistogram->GetInstanceIdentifier(index);
  if (index != sparseHistogram->GetIndex(id))
    {
    pass = false ;
    whereFail = "Sparse Histogram: GetIndex(InstanceIdentifier&)" ;
    }

  index.Fill(100) ;
  
  if (!sparseHistogram->IsIndexOutOfBounds(index))
    {
    pass = false ;
    whereFail = "Sparse Histogram: IsIndexOutOfBound(IndexType)" ;
    }

  if (totalSize != sparseHistogram->Size())
    {
    pass = false ;
    whereFail = "Sparse Histogram: Size()" ;
    }

  if (size != sparseHistogram->GetSize())
    {
    pass = false ;
    whereFail = "Sparse Histogram: GetSize()" ;
    }

  if ((lowerBound[0] + interval * 31) != sparseHistogram->GetBinMin(0,31))
    {
    pass = false ;
    whereFail = "Sparse Histogram: GetBinMin(Dimension, nthBin)" ;
    }

  if ((lowerBound[0] + interval * 32) != sparseHistogram->GetBinMax(0,31))
    {
    pass = false ;
    whereFail = "Sparse Histogram: GetBinMax(Dimension, nthBin)" ;
    }


  for (id = 0 ; 
       id < static_cast< SparseHistogramType::InstanceIdentifier >(totalSize) ;
       id++)
    {
    bool result = sparseHistogram->SetFrequency(id, 1) ;
    if( !result )
      {
      pass = false ;
      whereFail = 
        "SetFrequency(InstanceIdentifier, 1) " ;
      break;

      }

    result = sparseHistogram->IncreaseFrequency(id, 1) ;
    if( !result )
      {
      pass = false ;
      whereFail = 
        "IncreaseFrequency(InstanceIdentifier, 1) " ;
      break;
      }

    if (sparseHistogram->GetFrequency(id) != 2)
      {
      pass = false ;
      whereFail = 
        "SetFrequency(InstanceIdentifier, 1) + IncreaseFrequency(InstanceIdentifier, 1) + GetFrequency(InstanceIdentifier)" ;
      break;
      }
    }

  if (pass && (sparseHistogram->Quantile(0, 0.5) != 512.0))
    {
    pass = false ;
    whereFail = "Sparse Histogram: Quantile(Dimension, percent)" ;
    }

  if( !pass )
    {
    std::cout << "Test failed in " << whereFail << "." << std::endl;
    return EXIT_FAILURE;
    }
  
    std::cout << "[PASSED]" << std::endl;
  }


  {
  // Histogram test with variable length container.  
  std::cout << "Histogram Test With itk::Array as MeasurementVector" << std::endl; 
  bool pass = true;
  std::string whereFail = "" ;
  
  typedef float MeasurementType ;
  typedef itk::Array< MeasurementType > MeasurementVectorType;

  // creats a histogram with with itk::Array as its measurement vectors..
  typedef itk::Statistics::Histogram< MeasurementType, 
    itk::MeasurementVectorTraits< MeasurementVectorType >::MeasurementVectorLength
                                                              > HistogramType;
  HistogramType::Pointer histogram = HistogramType::New();
  HistogramType::MeasurementVectorSizeType measurementVectorLength = 3;
  histogram->SetMeasurementVectorSize( measurementVectorLength );
  std::cout << "Histogram MeasurementVectorLength set to: " << 
                histogram->GetMeasurementVectorSize() << std::endl;

  // initializes a 64 x 64 x 64 histogram with equal size interval
  HistogramType::SizeType size( measurementVectorLength );
  size.Fill(64) ;

  unsigned long totalSize=1;
  for( unsigned int i = 0; i< measurementVectorLength; i++ )
    {
    totalSize *= size[i];
    }
  
  // Two alternate ways of setting creating a MeasurementVectorType of the right
  // length using traits.. The first will work for itk::Arrays or any of the 
  // containers in MeasurementVectorTraits. The second is easier to write and 
  // is specific to itk::Array.
  HistogramType::MeasurementVectorType lowerBound
     = itk::MeasurementVectorTraits< MeasurementVectorType >::SetSize( 
                                             measurementVectorLength );
  HistogramType::MeasurementVectorType upperBound( measurementVectorLength );

  lowerBound.Fill(0.0) ;
  upperBound.Fill(1024.0) ;

  histogram->Initialize(size, lowerBound, upperBound ) ;
  histogram->SetToZero();
  HistogramType::MeasurementType interval = 
    (upperBound[0] - lowerBound[0]) / 
    static_cast< HistogramType::MeasurementType >(size[0]) ;

  // tests begin
  HistogramType::MeasurementVectorType measurements( measurementVectorLength );
  measurements.Fill(512.0) ;
  HistogramType::IndexType index( measurementVectorLength );
  HistogramType::IndexType ind( measurementVectorLength );
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
  std::cout << "[PASSED]" << std::endl;

  }


  {
  // Test quantile methods etc with the variable length container..     
  // mostly copied from Examples/Statistics/Histogram.cxx
  std::cout << "Histogram for arrays.. subtest" << std::endl;
  typedef float MeasurementType ;
  unsigned int measurementVectorLength = 2;
  typedef itk::Array< MeasurementType > MeasurementVectorType;
  
  typedef itk::Statistics::Histogram< MeasurementType, 
          itk::MeasurementVectorTraits< MeasurementVectorType >::MeasurementVectorLength 
                                                                   > HistogramType;
  HistogramType::Pointer histogram = HistogramType::New() ;
  HistogramType::SizeType size( measurementVectorLength ) ;
  size.Fill(3) ;
  HistogramType::MeasurementVectorType lowerBound( measurementVectorLength ) ;
  HistogramType::MeasurementVectorType upperBound( measurementVectorLength ) ;
  lowerBound[0] = 1.1 ;
  lowerBound[1] = 2.6 ;
  upperBound[0] = 7.1 ;
  upperBound[1] = 8.6 ;

  histogram->Initialize(size, lowerBound, upperBound ) ;
  
  histogram->SetFrequency(0UL, 0.0) ; 
  histogram->SetFrequency(1UL, 2.0) ; 
  histogram->SetFrequency(2UL, 3.0) ; 
  histogram->SetFrequency(3UL, 2.0) ; 
  histogram->SetFrequency(4UL, 0.5) ; 
  histogram->SetFrequency(5UL, 1.0) ; 
  histogram->SetFrequency(6UL, 5.0) ; 
  histogram->SetFrequency(7UL, 2.5) ; 
  histogram->SetFrequency(8UL, 0.0) ; 
  
  HistogramType::IndexType index( measurementVectorLength ) ;
  index[0] = 0 ;
  index[1] = 2 ;
  std::cout << "Frequency of the bin at index  " << index
            << " is " << histogram->GetFrequency(index) 
            << ", and the bin's instance identifier is " 
            << histogram->GetInstanceIdentifier(index) << std::endl ;
 
  if( histogram->GetFrequency( index ) != 5)
    { 
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  
  HistogramType::MeasurementVectorType mv( measurementVectorLength ) ;
  mv[0] = 4.1 ;
  mv[1] = 5.6 ;
  index.Fill(1) ;
  
  std::cout << "Measurement vector at the center bin is " 
            << histogram->GetMeasurementVector(index) << std::endl ;
  if( ((histogram->GetMeasurementVector(index)[0] - 4.1) > 0.001)
      || ((histogram->GetMeasurementVector(index)[1] -5.6) > 0.001))
    { 
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
 
  HistogramType::IndexType resultingIndex( measurementVectorLength );
  histogram->GetIndex(mv,resultingIndex);
  std::cout << "Index of the measurement vector " << mv 
            << " is " << resultingIndex << std::endl ;
  if( (resultingIndex[0] != 1) || (resultingIndex[1] != 1) )
    { 
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
   
  std::cout << "Instance identifier of index " << index
            << " is " << histogram->GetInstanceIdentifier(index) 
            << std::endl;

  if( histogram->GetInstanceIdentifier(index) !=4 )
    { 
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
    
  
  index.Fill(100) ;
  if ( histogram->IsIndexOutOfBounds(index) )
    {
    std::cout << "Index " << index << "is out of bounds." << std::endl ;
    }
  else
    { 
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  
  std::cout << "Number of bins = " << histogram->Size()
            << " Total frequency = " << histogram->GetTotalFrequency()
            << " Dimension sizes = " << histogram->GetSize() << std::endl ;
  std::cout << "50th percentile along the first dimension = " 
            << histogram->Quantile(0, 0.5) << std::endl ;
  
  if( (histogram->Size() != 9) || (histogram->GetTotalFrequency() != 16 ) 
      || (histogram->GetSize()[1] != 3) || ((histogram->Quantile(0,0.5)-3.5)>0.001))
    { 
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
      
  std::cout << "[PASSED]" << std::endl;
  
  }
 
  return EXIT_SUCCESS;
}



