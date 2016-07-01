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

#include "itkMath.h"
#include "itkHistogram.h"
#include "itkSample.h"
#include "itkTestingMacros.h"

int itkHistogramTest( int, char* [] )
{
  std::cout << "Histogram Test \n \n";
  bool pass = true;
  std::string whereFail = "";

  typedef float MeasurementType;
  const unsigned int numberOfComponents = 3;

  // create a histogram with 3 components measurement vectors
  typedef itk::Statistics::Histogram< MeasurementType,
          itk::Statistics::DenseFrequencyContainer2 > HistogramType;
  HistogramType::Pointer histogram = HistogramType::New();

  EXERCISE_BASIC_OBJECT_METHODS( histogram, Histogram, Sample );

  typedef HistogramType::MeasurementVectorType MeasurementVectorType;
  typedef HistogramType::InstanceIdentifier    InstanceIdentifier;
  typedef HistogramType::IndexType             IndexType;

  // initializes a 64 x 64 x 64 histogram with equal size interval
  HistogramType::SizeType size( numberOfComponents );
  size.Fill(64);
  unsigned long totalSize = size[0] * size[1] * size[2];

  MeasurementVectorType lowerBound( numberOfComponents );
  MeasurementVectorType upperBound( numberOfComponents );

  lowerBound.Fill(0);
  upperBound.Fill(1024);

  //
  // Exercise exception case
  //
  try
    {
    // purposely calling Initialize() before calling SetMeasurementVectorSize()
    // in order to trigger an expected exception.
    histogram->Initialize(size);
    pass = false;
    whereFail = "Initialize(size) before SetMeasurementVectorSize() didn't throw expected exception";
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << "Expected exception ";
    std::cout << excp << std::endl;
    }


  //
  // Now call SetMeasurementVectorSize() correctly
  //
  histogram->SetMeasurementVectorSize( numberOfComponents );

  if( histogram->GetMeasurementVectorSize() != numberOfComponents )
    {
    std::cerr << "Error in Get/SetMeasurementVectorSize() " << std::endl;
    return EXIT_FAILURE;
    }

  //
  //  Exercise Initialize with size and bounds
  //
  histogram->Initialize(size, lowerBound, upperBound );

  histogram->SetToZero();

  double interval =
    (upperBound[0] - lowerBound[0]) /
    static_cast< HistogramType::MeasurementType >(size[0]);

  // tests begin
  MeasurementVectorType measurements( numberOfComponents );
  measurements.Fill(512);

  IndexType index( numberOfComponents );
  IndexType ind( numberOfComponents );
  index.Fill(32);
  if(histogram->GetIndex(measurements,ind))
    {
    if(index != ind)
      {
      pass = false;
      whereFail = "GetIndex(MeasurementVectorType&)";
      }
    }
  else
    {
    pass = false;
    whereFail = "GetIndex(MeasurementVectorType&)";
    }

  InstanceIdentifier id = histogram->GetInstanceIdentifier(index);
  if (index != histogram->GetIndex(id))
    {
    pass = false;
    whereFail = "GetIndex(InstanceIdentifier&)";
    }

  index.Fill( -5 ); // test for outside below

  if( !histogram->IsIndexOutOfBounds(index) )
    {
    std::cerr << "IsIndexOutOfBounds() for " << index << std::endl;
    pass = false;
    whereFail = "IsIndexOutOfBounds(IndexType)";
    }


  index.Fill(32); // test for inside

  if( histogram->IsIndexOutOfBounds(index) )
    {
    std::cerr << "IsIndexOutOfBounds() for " << index << std::endl;
    pass = false;
    whereFail = "IsIndexOutOfBounds(IndexType)";
    }

  index.Fill(100); // test for outside

  if( !histogram->IsIndexOutOfBounds(index) )
    {
    std::cerr << "IsIndexOutOfBounds() for " << index << std::endl;
    pass = false;
    whereFail = "IsIndexOutOfBounds(IndexType)";
    }

  if (totalSize != histogram->Size())
    {
    pass = false;
    whereFail = "Size()";
    }

  if (size != histogram->GetSize())
    {
    pass = false;
    whereFail = "GetSize()";
    }

  // Query the bounds of the bin using the index of the bin.

  if (itk::Math::NotAlmostEquals( (lowerBound[0] + interval * 31), histogram->GetBinMin(0,31) ))
    {
    pass = false;
    whereFail = "GetBinMin(Dimension, nthBin)";
    }

  if (itk::Math::NotAlmostEquals( (lowerBound[0] + interval * 32), histogram->GetBinMax(0,31) ))
    {
    pass = false;
    whereFail = "GetBinMax(Dimension, nthBin)";
    }

  // Query the histogram bin extremes using a value within the bin

  if (itk::Math::NotAlmostEquals( (lowerBound[0] + interval * 31         ), histogram->GetBinMinFromValue(0, lowerBound[0] + interval * 31.5 ) )
   || itk::Math::NotAlmostEquals( (lowerBound[0]                         ), histogram->GetBinMinFromValue(0, itk::NumericTraits< float >::min()   ) )
   || itk::Math::NotAlmostEquals( (lowerBound[0] + interval * (size[0]-1)), histogram->GetBinMinFromValue(0, itk::NumericTraits< float >::max() ) ) )
    {
    pass = false;
    whereFail = "GetBinMinFromValue(Dimension, A Value Within The Nth Bin)";
    }

  if (itk::Math::NotAlmostEquals( (lowerBound[0] + interval * 32         ), histogram->GetBinMaxFromValue(0, lowerBound[0] + interval * 31.5 ) )
   || itk::Math::NotAlmostEquals( (lowerBound[0] + interval              ), histogram->GetBinMaxFromValue(0, itk::NumericTraits< float >::min()   ) )
   || itk::Math::NotAlmostEquals( (upperBound[0]                         ), histogram->GetBinMaxFromValue(0, itk::NumericTraits< float >::max() ) ) )
    {
    pass = false;
    whereFail = "GetBinMaxFromValue(Dimension, A Value Within The Nth Bin)";
    }

  index.Fill(31);
  if (itk::Math::NotAlmostEquals( (lowerBound[0] + interval * 31), histogram->GetHistogramMinFromIndex(index)[0]) )
    {
    pass = false;
    whereFail = "GetHistogramMinFromIndex(Dimension, nthBin)";
    }

  if (itk::Math::NotAlmostEquals( (lowerBound[0] + interval * 32), histogram->GetHistogramMaxFromIndex(index)[0]) )
    {
    pass = false;
    whereFail = "GetHistogramMaxFromIndex(Dimension, nthBin)";
    }

  for (id = 0;
       id < static_cast< InstanceIdentifier >(totalSize);
       id++)
    {
    histogram->SetFrequency(id, 1);
    histogram->IncreaseFrequency(id, 1);
    if (histogram->GetFrequency(id) != 2)
      {
      pass = false;
      whereFail =
        "SetFrequency(InstanceIdentifier, 1) + IncreaseFrequency(InstanceIdentifier, 1) + GetFrequency(InstanceIdentifier)";
      }
    }

  double quantile1 = histogram->Quantile(0, 0.3);
  if( itk::Math::NotAlmostEquals( quantile1, 307.2) )
    {
    std::cerr << "quantile1 = " << quantile1 << std::endl;
    pass = false;
    whereFail = "Quantile(Dimension, percent)";
    }

  double quantile2 = histogram->Quantile(0, 0.5);
  if( quantile2 != 512.0)
    {
    std::cerr << "quantile2 = " << quantile2 << std::endl;
    pass = false;
    whereFail = "Quantile(Dimension, percent)";
    }

  double quantile3 = histogram->Quantile(0, 0.7);
  if( itk::Math::NotAlmostEquals( quantile3, 716.8) )
    {
    std::cerr << "quantile3 = " << quantile3 << std::endl;
    pass = false;
    whereFail = "Quantile(Dimension, percent)";
    }


  if( !pass )
    {
    std::cerr << "Test failed in " << whereFail << "." << std::endl;
    return EXIT_FAILURE;
    }


  // Histogram with SparseFrequencyContainer2
  typedef itk::Statistics::Histogram< MeasurementType,
    itk::Statistics::SparseFrequencyContainer2 > SparseHistogramType;
  SparseHistogramType::Pointer sparseHistogram = SparseHistogramType::New();

  sparseHistogram->SetMeasurementVectorSize( numberOfComponents );

  // initializes a 64 x 64 x 64 histogram with equal size interval
  sparseHistogram->Initialize(size, lowerBound, upperBound );
  sparseHistogram->SetToZero();
  interval = (upperBound[0] - lowerBound[0]) /
    static_cast< SparseHistogramType::MeasurementType >(size[0]);

  measurements.Fill(512);
  index.Fill(32);
  sparseHistogram->GetIndex(measurements,ind);

  if (index != ind)
    {
    pass = false;
    whereFail = "Sparse Histogram: GetIndex(MeasurementVectorType&)";
    }

  id = sparseHistogram->GetInstanceIdentifier(index);
  if (index != sparseHistogram->GetIndex(id))
    {
    pass = false;
    whereFail = "Sparse Histogram: GetIndex(InstanceIdentifier&)";
    }

  index.Fill(100);

  if (!sparseHistogram->IsIndexOutOfBounds(index))
    {
    pass = false;
    whereFail = "Sparse Histogram: IsIndexOutOfBounds(IndexType)";
    }

  if (totalSize != sparseHistogram->Size())
    {
    pass = false;
    whereFail = "Sparse Histogram: Size()";
    }

  if (size != sparseHistogram->GetSize())
    {
    pass = false;
    whereFail = "Sparse Histogram: GetSize()";
    }

  if (itk::Math::NotAlmostEquals( (lowerBound[0] + interval * 31), sparseHistogram->GetBinMin(0,31) ))
    {
    pass = false;
    whereFail = "Sparse Histogram: GetBinMin(Dimension, nthBin)";
    }

  if (itk::Math::NotAlmostEquals( (lowerBound[0] + interval * 32), sparseHistogram->GetBinMax(0,31) ))
    {
    pass = false;
    whereFail = "Sparse Histogram: GetBinMax(Dimension, nthBin)";
    }


  for (id = 0;
       id < static_cast< SparseHistogramType::InstanceIdentifier >(totalSize);
       id++)
    {
    bool result = sparseHistogram->SetFrequency(id, 1);
    if( !result )
      {
      pass = false;
      whereFail =
        "SetFrequency(InstanceIdentifier, 1) ";
      break;

      }

    result = sparseHistogram->IncreaseFrequency(id, 1);
    if( !result )
      {
      pass = false;
      whereFail =
        "IncreaseFrequency(InstanceIdentifier, 1) ";
      break;
      }

    if (sparseHistogram->GetFrequency(id) != 2)
      {
      pass = false;
      whereFail =
        "SetFrequency(InstanceIdentifier, 1) + IncreaseFrequency(InstanceIdentifier, 1) + GetFrequency(InstanceIdentifier)";
      break;
      }
    }

  if (pass && (sparseHistogram->Quantile(0, 0.5) != 512.0))
    {
    pass = false;
    whereFail = "Sparse Histogram: Quantile(Dimension, percent)";
    }


  histogram->SetClipBinsAtEnds( true );
  if( !histogram->GetClipBinsAtEnds() )
    {
    pass = false;
    whereFail = "Set/GetClipBinsAtEnds()";
    }

  histogram->SetClipBinsAtEnds( false );
  if( histogram->GetClipBinsAtEnds() )
    {
    pass = false;
    whereFail = "Set/GetClipBinsAtEnds()";
    }

  if( histogram->GetMeasurementVectorSize() != numberOfComponents )
    {
    pass = false;
    whereFail = "Set/GetMeasurementVectorSize()";
    }

  const unsigned int measurementVectorSize = 17;

  try
    {
    histogram->SetMeasurementVectorSize( measurementVectorSize );
    pass = false;
    whereFail = "SetMeasurementVectorSize() didn't throw expected exception";
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << "Expected exception ";
    std::cout << excp << std::endl;
    }

  index.Fill(0);
  MeasurementVectorType measurement = histogram->GetMeasurementVector( index );
  for( unsigned kid0 = 0; kid0 < numberOfComponents; kid0++ )
    {
    if( itk::Math::NotAlmostEquals( measurement[kid0], 8 ))
      {
      std::cerr << "GetMeasurementVector() for index = ";
      std::cerr << index << std::endl;
      pass = false;
      whereFail = "GetMeasurementVector() failed for index";
      break;
      }
    }

  histogram->SetClipBinsAtEnds( true );

  measurement = histogram->GetMeasurementVector( index );
  for( unsigned kid1 = 0; kid1 < numberOfComponents; kid1++ )
    {
    if( itk::Math::NotAlmostEquals( measurement[kid1], 8 ) )
      {
      std::cerr << "GetMeasurementVector() for index = ";
      std::cerr << index << std::endl;
      pass = false;
      whereFail = "GetMeasurementVector() failed for index";
      break;
      }
    }

  const InstanceIdentifier instanceId = 0;
  measurement = histogram->GetMeasurementVector( instanceId );
  for( unsigned kid2 = 0; kid2 < numberOfComponents; kid2++ )
    {
    if( itk::Math::NotAlmostEquals( measurement[kid2], 8 ) )
      {
      std::cerr << "GetMeasurementVector() for instanceId = ";
      std::cerr << instanceId << std::endl;
      pass = false;
      whereFail = "GetMeasurementVector() failed for instanceId";
      break;
      }
    }

  // Test GetIndex with different settings of SetClipBinsAtEnds
  MeasurementVectorType outOfLowerRange( numberOfComponents );
  MeasurementVectorType outOfUpperRange( numberOfComponents );

  for(unsigned int k = 0; k < numberOfComponents; k++)
    {
    outOfLowerRange[k] = lowerBound[k] - 13;
    outOfUpperRange[k] = upperBound[k] + 23;
    }

  histogram->SetClipBinsAtEnds( false );

  IndexType index1( numberOfComponents );
  bool getindex1 = histogram->GetIndex( outOfLowerRange, index1 );

  std::cout << "GetIndex() with SetClipBinsAtEnds() = false " << std::endl;
  std::cout << "Boolean " << getindex1 << " Index " << index1 << std::endl;

  if( !getindex1 )
    {
    pass = false;
    whereFail = "GetIndex() returned boolean failed for outOfLowerRange";
    }

  for(unsigned k1=0; k1 < numberOfComponents; k1++ )
    {
    if( index1[ k1 ] != 0 )
      {
      pass = false;
      whereFail = "GetIndex() index value failed for outOfLowerRange";
      }
    }


  histogram->SetClipBinsAtEnds( true );

  getindex1 = histogram->GetIndex( outOfLowerRange, index1 );

  std::cout << "GetIndex() with SetClipBinsAtEnds() = true " << std::endl;
  std::cout << "Boolean " << getindex1 << " Index " << index1 << std::endl;

  if( getindex1 )
    {
    pass = false;
    whereFail = "GetIndex() failed for outOfLowerRange";
    }

  histogram->SetClipBinsAtEnds( false );

  IndexType index2( numberOfComponents );
  bool getindex2 = histogram->GetIndex( outOfUpperRange, index2 );

  std::cout << "GetIndex() with SetClipBinsAtEnds() = false " << std::endl;
  std::cout << "Boolean " << getindex2 << " Index " << index2 << std::endl;

  if( !getindex2 )
    {
    pass = false;
    whereFail = "GetIndex() returned boolean failed for outOfUpperRange";
    }

  for(unsigned k2=0; k2 < numberOfComponents; k2++ )
    {
    if( index2[ k2 ] != (long)size[k2] - 1 )
      {
      pass = false;
      whereFail = "GetIndex() index value failed for outOfUpperRange";
      }
    }


  histogram->SetClipBinsAtEnds( true );

  getindex2 = histogram->GetIndex( outOfUpperRange, index2 );

  std::cout << "GetIndex() with SetClipBinsAtEnds() = true " << std::endl;
  std::cout << "Boolean " << getindex2 << " Index " << index2 << std::endl;

  if( getindex2 )
    {
    pass = false;
    whereFail = "GetIndex() failed for outOfUpperRange";
    }


  // Testing GetIndex() for values that are above the median value of the Bin.
  IndexType pindex( numberOfComponents );
  pindex.Fill( 32 );
  MeasurementVectorType measurementVector =
    histogram->GetMeasurementVector( pindex );

  for( unsigned int gik1=0; gik1<numberOfComponents; gik1++)
    {
    measurementVector[gik1] += 0.3;
    }

  IndexType gindex;
  histogram->GetIndex( measurementVector, gindex );

  for( unsigned int gik2=0; gik2<numberOfComponents; gik2++)
    {
    if( gindex[gik2] != 32 )
      {
      std::cerr << "GetIndex() / GetMeasurementVector() failed " << std::endl;
      std::cerr << "MeasurementVector = " << measurementVector << std::endl;
      std::cerr << "Index returned = " << gindex << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Testing GetIndex() for values that are below the median value of the Bin.
  for( unsigned int gik3=0; gik3<numberOfComponents; gik3++)
    {
    measurementVector[gik3] -= 0.6;
    }

  histogram->GetIndex( measurementVector ,gindex);

  for( unsigned int gik4=0; gik4<numberOfComponents; gik4++)
    {
    if( gindex[gik4] != 32 )
      {
      std::cerr << "GetIndex() / GetMeasurementVector() failed " << std::endl;
      std::cerr << "MeasurementVector = " << measurementVector << std::endl;
      std::cerr << "Index returned = " << gindex << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Testing GetIndex on the upper and lower bounds
  IndexType upperIndex( numberOfComponents );
  bool upperIndexBool = histogram->GetIndex( upperBound, upperIndex );
  if( !upperIndexBool )
    {
    pass = false;
    whereFail = "GetIndex() returned boolean failed for upper bound";
    }

  for(unsigned k1=0; k1 < numberOfComponents; k1++ )
    {
    if( upperIndex[ k1 ] != 63 )
      {
      pass = false;
      whereFail = "GetIndex() index value failed for upperBound, as upper bound should map to the last bin.";
      }
    }

  IndexType lowerIndex( numberOfComponents );
  bool lowerIndexBool = histogram->GetIndex( lowerBound, lowerIndex );
  if( !lowerIndexBool )
    {
    pass = false;
    whereFail = "GetIndex() returned boolean failed for lower bound";
    }

  for(unsigned k1=0; k1 < numberOfComponents; k1++ )
    {
    if( lowerIndex[ k1 ] != 0 )
      {
      pass = false;
      whereFail = "GetIndex() index value failed for lowerIndex, as lower bound should map to the first bin.";
      }
    }

  // Testing GetIndex above the upper bound of a bin
  histogram->SetClipBinsAtEnds( false );
  MeasurementVectorType measurementVectorAbove( numberOfComponents );
  for( unsigned int gupk1 = 0; gupk1<numberOfComponents; gupk1++)
    {
    measurementVectorAbove[gupk1] = 129.9;
    }

  IndexType aboveUpperIndex( numberOfComponents );
  bool aboveUpperIndexBool =
    histogram->GetIndex( measurementVectorAbove, aboveUpperIndex );
  if( !aboveUpperIndexBool )
    {
    std::cerr << "Upper bound index = " << aboveUpperIndex << std::endl;
    }

  // Get the mean value for a dimension
  unsigned int dimension = 0;
  double mean = histogram->Mean( dimension );
  std::cout << "Mean value along dimension " << dimension << " : " << mean << std::endl;

  HistogramType::Iterator itr = histogram->Begin();
  HistogramType::Iterator end = histogram->End();

  HistogramType::TotalAbsoluteFrequencyType totalFrequency =
    histogram->GetTotalFrequency();

  InstanceIdentifier histogramSize = histogram->Size();

  while( itr != end )
    {
    itr.SetFrequency( itr.GetFrequency() + 1 );
    ++itr;
    }

  HistogramType::TotalAbsoluteFrequencyType newTotalFrequency =
    histogram->GetTotalFrequency();

  if( newTotalFrequency != histogramSize + totalFrequency )
    {
    std::cerr << "Get/SetFrequency error in the Iterator" << std::endl;
    return EXIT_FAILURE;
    }


  //
  // Exercise GetIndex() method in the iterator.
  //
  std::cout << "TEST GetIndex() and GetFrequency() in the iterator" << std::endl;
  itr = histogram->Begin();
  end = histogram->End();

  // Print out only some of them, the first 10...
  for( unsigned int kk = 0; kk < 10 && itr != end; ++itr, ++kk )
    {
    std::cout << itr.GetIndex() <<  " : " << itr.GetFrequency() << std::endl;
    }


  //
  // Exercise GetMin / GetMax methods
  //
    {
    const double epsilon = 1e-6;
    HistogramType::SizeType size2 = histogram->GetSize();

    HistogramType::BinMinContainerType binMinimums = histogram->GetMins();

    for( unsigned int dim = 0; dim < numberOfComponents; dim++ )
      {
      HistogramType::BinMinVectorType binDimensionMinimums = histogram->GetDimensionMins( dim );
      for( unsigned int k = 0; k < size2[dim]; k++ )
        {
        HistogramType::MeasurementType minA = binMinimums[dim][k];
        HistogramType::MeasurementType minB = binDimensionMinimums[k];
        HistogramType::MeasurementType minC = histogram->GetBinMin( dim, k );
        if( ( itk::Math::abs( minA - minB ) > epsilon ) ||
            ( itk::Math::abs( minA - minC ) > epsilon )    )
          {
          std::cerr << "Error in Get Bin Mins methods" << std::endl;
          std::cerr << "dim = " << dim << " k = " << k << std::endl;
          std::cerr << "GetMins()          = " << minA << std::endl;
          std::cerr << "GetDimensionMins() = " << minB << std::endl;
          std::cerr << "GetMin()           = " << minC << std::endl;
          return EXIT_FAILURE;
          }
        }
      }

    HistogramType::BinMaxContainerType binMaximums = histogram->GetMaxs();

    for( unsigned int dim = 0; dim < numberOfComponents; dim++ )
      {
      HistogramType::BinMaxVectorType binDimensionMaximums = histogram->GetDimensionMaxs( dim );
      for( unsigned int k = 0; k < size2[dim]; k++ )
        {
        HistogramType::MeasurementType maxA = binMaximums[dim][k];
        HistogramType::MeasurementType maxB = binDimensionMaximums[k];
        HistogramType::MeasurementType maxC = histogram->GetBinMax( dim, k );
        if( ( itk::Math::abs( maxA - maxB ) > epsilon ) ||
            ( itk::Math::abs( maxA - maxC ) > epsilon )    )
          {
          std::cerr << "Error in Get Bin Maxs methods" << std::endl;
          std::cerr << "dim = " << dim << " k = " << k << std::endl;
          std::cerr << "GetMaxs()          = " << maxA << std::endl;
          std::cerr << "GetDimensionMaxs() = " << maxB << std::endl;
          std::cerr << "GetMax()           = " << maxC << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    }


  // Testing methods specific to Iterators
    {
    typedef HistogramType::Iterator IteratorType;
    IteratorType iter = histogram->Begin();
    IteratorType iter2 = histogram->End();

    iter2 = iter;
    if( iter2 != iter )
      {
      std::cerr << "Iterator operator=() failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter3( histogram );
    if( iter3 != histogram->Begin() )
      {
      std::cerr << "Iterator constructor from histogram failed" << std::endl;
      return EXIT_FAILURE;
      }

    unsigned int counter = 0;
    while( iter3 != histogram->End() )
      {
      ++iter3;
      counter++;
      }

    if( counter != histogram->Size() )
      {
      std::cerr << "Iterator walk failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter4( iter2 );
    if( iter4 != iter2 )
      {
      std::cerr << "Iterator copy constructor failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter5 = iter2;
    if( iter5 != iter2 )
      {
      std::cerr << "Iterator operator= failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter6( 7, histogram );
    if( iter6.GetInstanceIdentifier() != 7 )
      {
      std::cerr << "Iterator Constructor with instance identifier 7 failed" << std::endl;
      return EXIT_FAILURE;
      }

    }

  // Testing methods specific to ConstIterators
    {
    typedef HistogramType::ConstIterator ConstIteratorType;
    ConstIteratorType iter = histogram->Begin();
    ConstIteratorType iter2 = histogram->End();

    iter2 = iter;

    if( iter2 != iter )
      {
      std::cerr << "ConstIterator operator!=() or operator=() failed" << std::endl;
      return EXIT_FAILURE;
      }

    if( !( iter2 == iter ) )
      {
      std::cerr << "ConstIterator operator==() failed" << std::endl;
      return EXIT_FAILURE;
      }

    ConstIteratorType iter3( iter2 );
    if( iter3 != iter2 )
      {
      std::cerr << "ConstIterator copy constructor failed" << std::endl;
      return EXIT_FAILURE;
      }

    const HistogramType * constHistogram = histogram.GetPointer();

    ConstIteratorType iter4( constHistogram->Begin() );
    ConstIteratorType iter5( histogram->Begin() );
    if( iter4 != iter5 )
      {
      std::cerr << "Constructor from const container Begin() differs from non-const Begin() " << std::endl;
      return EXIT_FAILURE;
      }

    ConstIteratorType iter6( constHistogram );
    ConstIteratorType iter7( histogram );
    if( iter6 != iter7 )
      {
      std::cerr << "ConstIterator Constructor from const container differs from non-const container" << std::endl;
      return EXIT_FAILURE;
      }

    ConstIteratorType iter8( histogram );
    if( iter8.GetInstanceIdentifier() != 0 )
      {
      std::cerr << "Constructor with instance identifier 0 failed" << std::endl;
      return EXIT_FAILURE;
      }

    unsigned int counter = 0;
    ConstIteratorType iter10( constHistogram );
    if( iter10 != constHistogram->Begin() )
      {
      std::cerr << "ConstIterator constructor from histogram failed" << std::endl;
      return EXIT_FAILURE;
      }


    while( iter10 != constHistogram->End() )
      {
      ++iter10;
      counter++;
      }

    if( counter != constHistogram->Size() )
      {
      std::cerr << "Iterator walk failed" << std::endl;
      return EXIT_FAILURE;
      }

    }

  if( !pass )
    {
    std::cout << "Test failed in " << whereFail << "." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
