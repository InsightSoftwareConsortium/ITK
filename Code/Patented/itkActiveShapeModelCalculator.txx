/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkActiveShapeModelCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkActiveShapeModelCalculator_txx
#define _itkActiveShapeModelCalculator_txx
#include "itkActiveShapeModelCalculator.h"

namespace itk
{ 

class ITK_EXPORT InvalidActiveShapeModeError : public ExceptionObject
{
public:
  /*
   * Constructor. Needed to ensure the exception object can be copied.
   */
  InvalidActiveShapeModeError(const char *file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) { this->SetDescription("No valid training image are availble.");}

  /*
   * Constructor. Needed to ensure the exception object can be copied.
   */
  InvalidActiveShapeModeError(const std::string& file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) { this->SetDescription("No valid training image are availble.");}  
  
  itkTypeMacro(InvalidActiveShapeModeError, ExceptionObject);
};

/**
 * Generate data (start the model building process)
 */
template<class TImage>
void 
ActiveShapeModelCalculator<TImage>
::GenerateData( )
{
  if( !m_Image ) 
    {
    return;
    }

  typename GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
  typename BinaryFilterType::Pointer binaryFilter = BinaryFilterType::New();
  typename DistanceMapFilterType::Pointer distanceFilter = DistanceMapFilterType::New();

  binaryFilter->SetOutsideValue( 0.0 );
  binaryFilter->SetInsideValue( 1.0 );
  binaryFilter->SetLowerThreshold( m_LowerThreshold );
  binaryFilter->SetUpperThreshold( 255.0 );

  gradientFilter->SetInput( m_Image );
  binaryFilter->SetInput( gradientFilter->GetOutput() );
  distanceFilter->InputIsBinaryOn();
  distanceFilter->SetInput( binaryFilter->GetOutput() );
  distanceFilter->Update();

  typedef typename Image2DType::RegionType Region2DType;
  typedef typename Region2DType::SizeType  Size2DType;
  typedef typename Region2DType::IndexType Index2DType;

  Region2DType  region;
  Size2DType    size;
  Index2DType   index;

  typename Image3DType::ConstPointer inputImage;    
  typename Image2DType::Pointer outputImage = Image2DType::New();

  inputImage = distanceFilter->GetOutput();
  typename Image3DType::RegionType requestedRegion = inputImage->GetRequestedRegion();

  unsigned int projectionDirection = 2;
  unsigned int i, j;
  unsigned int direction[2];

  for (i = 0, j = 0; i < 3; ++i )
    {
       if (i != projectionDirection)
        {
            direction[j] = i;
            j++;
        }
    }

  index[ direction[0] ]    = requestedRegion.GetIndex()[ direction[0] ];
  index[ 1- direction[0] ] = requestedRegion.GetIndex()[ direction[1] ];
  size[ direction[0] ]     = requestedRegion.GetSize()[  direction[0] ];
  size[ 1- direction[0] ]  = requestedRegion.GetSize()[  direction[1] ];
  unsigned int slices = requestedRegion.GetSize()[ 2 ];
  
  /**
   * Set the number of training images
   */
  m_NumberOfTrainingImages = slices;

  region.SetSize( size );
  region.SetIndex( index );
  outputImage->SetRegions( region );
  outputImage->Allocate();
  SliceIteratorType  inputIt(  inputImage,  inputImage->GetRequestedRegion() );
  LinearIteratorType outputIt( outputImage, outputImage->GetRequestedRegion() );
  inputIt.SetFirstDirection(  direction[1] );
  inputIt.SetSecondDirection( direction[0] );
  outputIt.SetDirection( 1 - direction[0] );
  outputIt.GoToBegin();

  while ( ! outputIt.IsAtEnd() )
    {
        while ( ! outputIt.IsAtEndOfLine() )
        {
            outputIt.Set( NumericTraits<unsigned char>::NonpositiveMin() );
            ++outputIt;
        }
        outputIt.NextLine();
    }

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while( !inputIt.IsAtEnd() )
    {
        while ( !inputIt.IsAtEndOfSlice() )
         {
            while ( !inputIt.IsAtEndOfLine() )
             {
                float valueOutput = outputIt.Get();
                float valueInput = inputIt.Get();
                float sum = valueOutput + valueInput;
                outputIt.Set( static_cast<PixelType> (sum) );
                ++inputIt;
                ++outputIt;
             }
            outputIt.NextLine();
            inputIt.NextLine();
         }
         outputIt.GoToBegin();
         inputIt.NextSlice();
    }
  outputIt.GoToBegin();

  while ( ! outputIt.IsAtEnd() )
    {
      while ( ! outputIt.IsAtEndOfLine() )
        {
            float valueOutput = outputIt.Get();
            float mean = valueOutput / static_cast<float> (slices);
            outputIt.Set( static_cast<PixelType>(mean) );
            ++outputIt;
        }
        outputIt.NextLine();
    }
  
  typename BinaryFilterType1::Pointer binaryFilter1 = BinaryFilterType1::New();
  binaryFilter1->SetOutsideValue( 0 );
  binaryFilter1->SetInsideValue( 1 );
  binaryFilter1->SetLowerThreshold( 0.0 );
  binaryFilter1->SetUpperThreshold( m_UpperThreshold1 );
  typename ThinFilterType::Pointer thinFilter = ThinFilterType::New();
  typename PruneFilterType::Pointer pruneFilter = PruneFilterType::New();
  typename PointSetType::Pointer pointSet = PointSetType::New();
  typename PointsContainer::Pointer points = PointsContainer::New();
  typename PointDataContainer::Pointer pointData = PointDataContainer::New();

  binaryFilter1->SetInput( outputImage );
  thinFilter->SetInput( binaryFilter1->GetOutput() );
  pruneFilter->SetIteration( m_PruneIteration );

  pruneFilter->SetInput( thinFilter->GetOutput() );
  pruneFilter->Update();

  typename Image2D8bitsType::Pointer pruneImage;
  pruneImage = pruneFilter->GetOutput();
  IteratorType ot( pruneImage, pruneImage->GetRequestedRegion() );

  PointType p;
  IndexType position;
  Pixel8bitsType value0 = 0;
  Pixel8bitsType value1 = 1;
  ot.GoToBegin();

  while( !ot.IsAtEnd() )
    {
       if ( ot.Get() )
        {
            position = ot.GetIndex();
            ot.Set ( value0 );
            break;
        }
        ++ot;
    }

   for (unsigned int id = 0; id<2; id++)
    {
      p[id] = position[id];
    }

  int pointId = 0;
  points->InsertElement( pointId, p );
  pointData->InsertElement( pointId, value1 );
  pointId++;
  typename NeighborIteratorType::RadiusType radius;
  radius.Fill(1);
  NeighborIteratorType otNeighbor( radius, pruneImage, pruneImage->GetRequestedRegion() );
  typename NeighborIteratorType::OffsetType offset1 = {{1,0}};
  typename NeighborIteratorType::OffsetType offset2 = {{0,-1}};
  typename NeighborIteratorType::OffsetType offset3 = {{-1,0 }};
  typename NeighborIteratorType::OffsetType offset4 = {{0,1}};
  typename NeighborIteratorType::OffsetType offset5 = {{1,1}};
  typename NeighborIteratorType::OffsetType offset6 = {{1,-1}};
  typename NeighborIteratorType::OffsetType offset7 = {{-1,-1}};
  typename NeighborIteratorType::OffsetType offset8 = {{-1,1}};
  otNeighbor.SetLocation( position );
  unsigned int count = 1;

  while(count)
  { 
    count = 0;
    if ( otNeighbor.GetPixel(offset1) )
    {
      position = otNeighbor.GetIndex(offset1);
      otNeighbor.SetPixel ( offset1, value0 );
      for (unsigned int id = 0; id<2; id++)
        p[id] = position[id];
    points->InsertElement( pointId, p );
    pointData->InsertElement( pointId, value1 );
    pointId++;
    count = 1;
    otNeighbor += offset1;
    }
    else
    {
      if ( otNeighbor.GetPixel(offset2) )
      {
        position = otNeighbor.GetIndex(offset2);
        otNeighbor.SetPixel ( offset2, value0 );
        for (unsigned int id = 0; id<2; id++)
          p[id] = position[id];
        points->InsertElement( pointId, p );
        pointData->InsertElement( pointId, value1 );
        pointId++;
        count = 1;
        otNeighbor += offset2;
      }
      else
      {
        if ( otNeighbor.GetPixel(offset3) )
      { 
        position = otNeighbor.GetIndex(offset3);
        otNeighbor.SetPixel ( offset3, value0 );
        for (unsigned int id = 0; id<2; id++)
           p[id] = position[id];
        points->InsertElement( pointId, p );
        pointData->InsertElement( pointId, value1 );
        pointId++;
        count = 1;
        otNeighbor += offset3;
      }
      else
      {
       if ( otNeighbor.GetPixel(offset4) )
        {
        position = otNeighbor.GetIndex(offset4);
        otNeighbor.SetPixel ( offset4, value0 );
        for (unsigned int id = 0; id<2; id++)
        p[id] = position[id];
        points->InsertElement( pointId, p );
        pointData->InsertElement( pointId, value1 );
        pointId++;
        count = 1;
        otNeighbor += offset4;
        }
      else
      {
       if ( otNeighbor.GetPixel(offset5) )
        {
         position = otNeighbor.GetIndex(offset5);
         otNeighbor.SetPixel ( offset5, value0 );
         for (unsigned int id = 0; id<2; id++)
           p[id] = position[id];
         points->InsertElement( pointId, p );
         pointData->InsertElement( pointId,value1 );
         pointId++;
         count = 1;
         otNeighbor += offset5;
        }
        else
        {
         if ( otNeighbor.GetPixel(offset6) )
         {
          position = otNeighbor.GetIndex(offset6);
          otNeighbor.SetPixel ( offset6,value0 );
          for (unsigned int id = 0; id<2; id++)
            p[id] = position[id];
          points->InsertElement( pointId, p );
          pointData->InsertElement( pointId, value1 );
          pointId++;
          count = 1;
          otNeighbor += offset6;
         }
         else
         {
          if ( otNeighbor.GetPixel(offset7) )
           {
            position = otNeighbor.GetIndex(offset7);
            otNeighbor.SetPixel ( offset7, value0 );
            for (unsigned int id = 0; id<2; id++)
              p[id] = position[id];
            points->InsertElement( pointId, p );
            pointData->InsertElement( pointId, value1 );
            pointId++;
            count = 1;
            otNeighbor += offset7;
           }
          else
           {
            if ( otNeighbor.GetPixel(offset8) )
             { 
              position = otNeighbor.GetIndex(offset8);
              otNeighbor.SetPixel ( offset8, value0 );
              for (unsigned int id = 0; id<2; id++)
              p[id] = position[id];
              points->InsertElement( pointId, p );
              pointData->InsertElement( pointId, value1 );
              pointId++;
              count = 1;
              otNeighbor += offset8;
             } 
           } 
          }  
        }   
      }
     }
   }
  }
 }

  pointSet->SetPoints( points );
  pointSet->SetPointData( pointData );
  List2DType m_Queue;
  List2DType m_Queue2;
  IndexType  current;
  m_Queue.clear();
  m_Queue2.clear();
  position[ 0 ] = 0;
  position[ 1 ] = (pointSet->GetNumberOfPoints()) - 1;
  PointType p1, p2;
  Vector3DType v1, v2, v3;
  double squareNorm1, squareNorm2;
  unsigned int pointId2 = 0;

  m_Queue.push_front(position);
  while (! m_Queue.empty())
  {
     current = m_Queue.front();
     m_Queue.pop_front();
     double m_distance = 0;
     pointSet->GetPoint( current[ 0 ], & p1 );
     pointSet->GetPoint( current[ 1 ], & p2 );
     for (unsigned int id = 0; id<2; id++)
       v1[ id ]= p2[ id ] - p1[ id ];
     v1[ 2 ] = 0;
     squareNorm1 = v1.GetSquaredNorm();
     for( pointId = (current[ 0 ] + 1); pointId < (current[ 1 ] - 1); pointId++)
      {
       pointSet->GetPoint( pointId, & p2 );
       for (unsigned int id = 0; id < 2; id++)
          v2[ id ]= p2[ id ] - p1[ id ];
       v2[ 2 ] = 0;
       for (unsigned int id = 0; id < 2; id++)
          v3 [id] = 0;
       v3[ 2 ] = (v1[0] * v2[ 1 ]) - (v1[ 1 ] * v2[ 0 ]);
       squareNorm2 = v3.GetSquaredNorm();
       double m_temp = squareNorm2 / squareNorm1;
       if ( m_temp > m_distance)
        {
         m_distance = m_temp;
         pointId2 = pointId;
        }
      }
     if (m_distance > m_Tolerance)
      {
        position[ 0 ] = current[ 0 ];
        position[ 1 ] = pointId2;
        m_Queue.push_front( position );
        position[ 0 ] = pointId2;
        position[ 1 ] = current[ 1 ];
        m_Queue.push_front( position );
      }
      else
      {
        position[ 0 ] = static_cast<unsigned long int> (p1[ 0 ]);
        position[ 1 ] = static_cast<unsigned long int> (p1[ 1 ]);
        m_Queue2.push_back( position );
      } 
  }

  unsigned int numberOfLandmarks = static_cast<unsigned int> (m_Queue2.size());
  typename SampleType::Pointer sampleLandmarks = SampleType::New();
  MeasurementVectorType mv;

  
  while (! m_Queue2.empty())
  {
      current = m_Queue2.front();
      m_Queue2.pop_front();
      mv [ 0 ] = current [ 0 ];
      mv [ 1 ] = current [ 1 ];
      sampleLandmarks->PushBack( mv );
  }

  
  VectorType v;
  typename VectorType::iterator p6;
  MatrixOfIntegerType coordLandmarks( 2*numberOfLandmarks, slices );
  coordLandmarks.fill(0);
  m_Means.set_size( 2*numberOfLandmarks );
  m_Means.fill(0);
  MatrixOfDoubleType covarianceMatrix( 2*numberOfLandmarks, 2*numberOfLandmarks );
  covarianceMatrix.fill(0);
  MatrixOfDoubleType identityMatrix( 2*numberOfLandmarks, 2*numberOfLandmarks );
  identityMatrix.set_identity();

  typename BinaryFilterType2::Pointer binaryFilter2 = BinaryFilterType2::New();
  binaryFilter2->SetOutsideValue( 0 );
  binaryFilter2->SetInsideValue( 255 );
  binaryFilter2->SetLowerThreshold( 0.0 );
  binaryFilter2->SetUpperThreshold( m_UpperThreshold2 );
  binaryFilter2->SetInput( distanceFilter->GetOutput() );
  typename Image3D8bitsType::ConstPointer inputImage2;
  inputImage2 = binaryFilter2->GetOutput();
  binaryFilter2->Update();
  ConstIteratorType constIterator( inputImage2, inputImage2->GetLargestPossibleRegion());

  Index3DType position3D;
  MeasurementVectorType posRight, posLeft, dxyRef1, dxyRef2;  
  int d, dx, dy, sx, sy;
  unsigned int ax, ay;

  for (unsigned int i = 0; i < slices; i++)
  {
      for ( unsigned long j = 0 ; j < numberOfLandmarks ; j++ )
      {
          mv = sampleLandmarks->GetMeasurementVector(j);
          position3D [ 0 ] = mv [ 0 ];
          position3D [ 1 ] = mv [ 1 ];
          position3D [ 2 ] = i;
          constIterator.SetIndex(position3D);
          if (constIterator.Get())
           {
              v.push_back (mv [ 0 ]);
              v.push_back (mv [ 1 ]);
           }
          else
           {
            for (unsigned int id = 0; id<2; id++)
              {
                  posRight[ id ] = mv [ id ];
                  posLeft[ id ] = mv [ id ];
               }
             if (j == 0)
              {
                dxyRef1 = sampleLandmarks->GetMeasurementVector(1) -
                sampleLandmarks->GetMeasurementVector(0);
                dxyRef2 = sampleLandmarks->GetMeasurementVector(numberOfLandmarks - 1) -
                sampleLandmarks->GetMeasurementVector(0);
              }
             if (j == numberOfLandmarks - 1)
             {
                dxyRef1 = sampleLandmarks->GetMeasurementVector(1) -
                sampleLandmarks->GetMeasurementVector(j);
                dxyRef2 = sampleLandmarks->GetMeasurementVector(j-1) -
                sampleLandmarks->GetMeasurementVector(j);
             }
            if ((j < numberOfLandmarks - 1) && (j > 1))
             {
               dxyRef1 = sampleLandmarks->GetMeasurementVector(j + 1) -
               sampleLandmarks->GetMeasurementVector(j);
               dxyRef2 = sampleLandmarks->GetMeasurementVector(j-1) -
               sampleLandmarks->GetMeasurementVector(j);
             }

             /**
              * Normal slope ****************
              */
              dx = dxyRef2[ 1 ] - dxyRef1[ 1 ];
              dy = dxyRef1[ 0 ] - dxyRef2[ 0 ];

              ax = vcl_abs(dx) * 2;
              ay = vcl_abs(dy) * 2;
              if (dx < 0) sx = -1; else sx = 1;
              if (dy < 0) sy = -1; else sy = 1;
              count = 1;
              if (ax > ay)
               {
                d = ay - ax/2;
                while (count)
                  {
                  if (d >= 0)
                   {
                     posRight[ 1 ] = posRight[ 1 ] + sy;
                     posLeft[ 1 ]  = posLeft[ 1 ] - sy;
                     d = d - ax;
                    }
                   posRight[ 0 ] = posRight[ 0 ] + sx;
                   posLeft[ 0 ]  = posLeft[ 0 ] - sx;
                   d = d + ay;
                   position3D [ 0 ] = posRight [ 0 ];
                   position3D [ 1 ] = posRight[ 1 ];
                   position3D [ 2 ] = i;
                   constIterator.SetIndex(position3D);
                   if (constIterator.Get())
                     {
                          count = 0;
                          v.push_back (posRight[ 0 ]);
                          v.push_back (posRight[ 1 ]);
                      }
                   else
                     {
                  position3D [ 0 ] = posLeft [ 0 ];
                  position3D [ 1 ] = posLeft[ 1 ];
                  position3D [ 2 ] = i;
                  constIterator.SetIndex(position3D);
                  if (constIterator.Get())
                     {
                         count = 0;
                         v.push_back (posLeft[ 0 ]);
                         v.push_back (posLeft[ 1 ]);
                     }
                  }
                }
              }
              else
             {
              d = ax - ay/2;
              while (count)
                {
                  if (d >= 0)
                    {
                          posRight[ 0 ] = posRight[ 0 ] + sx;
                          posLeft[ 0 ]  = posLeft[ 0 ] - sx;
                          d = d - ay;
                     }
                     posRight[ 1 ] = posRight[ 1 ] + sy;
                     posLeft[ 1 ]  = posLeft[ 1 ] - sy;
                     d = d + ax;
                     position3D [ 0 ] = posRight [ 0 ];
                     position3D [ 1 ] = posRight[ 1 ];
                     position3D [ 2 ] = i;
                     constIterator.SetIndex(position3D);
                     if (constIterator.Get())
                       {
                         count = 0;
                         v.push_back (posRight[ 0 ]);
                         v.push_back (posRight[ 1 ]);
                        }
                     else
                        {
                          position3D [ 0 ] = posLeft [ 0 ];
                          position3D [ 1 ] = posLeft[ 1 ];
                          position3D [ 2 ] = i;
                          constIterator.SetIndex(position3D);
                          if (constIterator.Get())
                            {
                               count = 0;
                               v.push_back (posLeft[ 0 ]);
                               v.push_back (posLeft[ 1 ]);
                             }
                     }
                 }
             }
          }
     }
     unsigned int row = 0;
     for (p6 = v.begin(); p6 != v.end(); p6++)
      {
        coordLandmarks[row][i] = (*p6);
        row++;
       }
      v.erase(v.begin(), v.end());
  }

  for(unsigned int i = 0; i < slices; i++)
    {
    for(unsigned int j = 0; j < 2*numberOfLandmarks; j++)
      {
      m_Means[j] += coordLandmarks[j][i];
      }
    } 
  m_Means /= slices;

   
  for(unsigned int i = 0; i < 2*numberOfLandmarks; i++)
  {
    for(unsigned int j = 0; j < 2*numberOfLandmarks; j++)
     {
       for(unsigned int k = 0; k < slices; k++) 
         covarianceMatrix[i][j] += (coordLandmarks[i][k] - m_Means[i]) * (coordLandmarks[j][k] - m_Means[j]);
     }
  }
  if( ( slices - 1 ) != 0 )
    {
      covarianceMatrix /= ( slices - 1 );
    }
  else
    {
      covarianceMatrix.fill(0);
    }  

  vnl_generalized_eigensystem eigenVectors_eigenValues(covarianceMatrix, identityMatrix);

  MatrixOfDoubleType eigenVectorsFull = eigenVectors_eigenValues.V;
  VectorOfDoubleType  eigenValuesFull = (eigenVectors_eigenValues.D).diagonal();
  eigenValuesFull.flip();
  double maxVariance = 0.98 * eigenValuesFull.sum();
  count = 0;
  double temp = 0;
  while (temp < maxVariance)
  {
    temp += eigenValuesFull[count];
    count++;
  }
  m_EigenVectors.set_size(eigenVectorsFull.rows(),count);
  m_EigenVectors = eigenVectorsFull.extract (eigenVectorsFull.rows(),count, 0, 0);
  m_EigenValues.set_size(count);
  m_EigenValues = eigenValuesFull.extract(count,0);

  /* *
   * Remember that the moments are valid
   */
  m_Valid = 1;

}

/* *
   * Get the mean shape
   */
template<class TImage>
typename ActiveShapeModelCalculator<TImage>::VectorOfDoubleType
ActiveShapeModelCalculator<TImage>::
GetMeanShape()
{
  if (!m_Valid)        throw InvalidActiveShapeModeError(__FILE__, __LINE__);
  return m_Means;
}

/* *
   * Get the largest t eigenvalues
   */
template<class TImage>
typename ActiveShapeModelCalculator<TImage>::VectorOfDoubleType
ActiveShapeModelCalculator<TImage>::
GetEigenvalues()
{
  if (!m_Valid)        throw InvalidActiveShapeModeError(__FILE__, __LINE__);
  return m_EigenValues;
}

/* *
   * Get the eigen vectors
   */
template<class TImage>
typename ActiveShapeModelCalculator<TImage>::MatrixOfDoubleType
ActiveShapeModelCalculator<TImage>::
GetEigenvector()
{
  if (!m_Valid)        throw InvalidActiveShapeModeError(__FILE__, __LINE__);
  return m_EigenVectors;
}

} // end namespace itk



#endif
