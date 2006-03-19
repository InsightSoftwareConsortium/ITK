/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkActiveShapeModelGradientSearchMethod.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkActiveShapeModelGradientSearchMethod_txx
#define _itkActiveShapeModelGradientSearchMethod_txx

#include "itkActiveShapeModelGradientSearchMethod.h"

namespace itk
{ 


/**
 * Constructor
 */
template<class TImage>
ActiveShapeModelGradientSearchMethod<TImage>
::ActiveShapeModelGradientSearchMethod( ):m_LenghtOfProfile(3)
  {
    m_NumberOfIteration = 1;     // default value
    m_EigenVectors.set_size(0,0);
    m_EigenValues.set_size(0);
    m_MeanShape.set_size(0);
    m_NewShape.set_size( 0);
    m_DiffVector.set_size( 0 );
    m_Db.set_size( 0 );
    m_Blimit.set_size( 0 );
    m_Valid = false;
    m_Image = NULL;
  }


/**
 * Generate data (start Searching process)
 */
template<class TImage>
void 
ActiveShapeModelGradientSearchMethod<TImage>
::GenerateData( )
{
  if( !m_Image ) 
    {
    return;
    }

  typename GradientFilterType::Pointer gradientFilter = GradientFilterType::New();

  typename Image2DType::RegionType region;
  typename Image2DType::ConstPointer gradImage;

   
  gradientFilter->SetInput( m_Image );
  gradientFilter->Update();
  gradImage = gradientFilter->GetOutput();

  
  ConstIteratorType constIterator( gradImage, gradImage->GetLargestPossibleRegion());

  typename SampleType::Pointer sampleLandmarks = SampleType::New();
  MeasurementVectorUIntType mv;
  MeasurementVectorDoubleType centerShape;
  
  unsigned int numberOfLandmarks = m_MeanShape.size()/2;
  unsigned int numberOfEigenValues = m_EigenValues.size();

  m_NewShape = m_MeanShape;
  centerShape [ 0 ] = 0.0;
  centerShape [ 1 ] = 0.0;
  
  
  for ( unsigned int j = 0 ; j < numberOfLandmarks ; j++ )
    {
      mv [ 0 ] = (unsigned int) (m_NewShape [ 2*j ] + 0.5);
      mv [ 1 ] = (unsigned int) (m_NewShape [ 2*j+1 ] + 0.5);
      sampleLandmarks->PushBack( mv );
      centerShape [ 0 ] += mv [ 0 ];
      centerShape [ 1 ] += mv [ 1 ];
    }
  centerShape [ 0 ] = centerShape [ 0 ] / numberOfLandmarks;
  centerShape [ 1 ] = centerShape [ 1 ] / numberOfLandmarks;
  
  IndexType position2D, newPosition2D;
  MeasurementVectorUIntType posRight, posLeft, dxyRef1, dxyRef2;
  double maximumGrad, deltaX, deltaY;
  int d, dx, dy, sx, sy;
  unsigned int  ax, ay;
  VectorType v;
  typename VectorType::iterator p6;

  m_NewShape.set_size( 2*numberOfLandmarks );
  m_NewShape.fill(0);

  m_DiffVector.set_size( 2*numberOfLandmarks );
  m_DiffVector.fill(0);

  m_Db.set_size( numberOfEigenValues );
  m_Db.fill(0);

  m_Blimit.set_size( numberOfEigenValues );
  m_Blimit.fill(0);


  for ( unsigned int i = 0 ; i < m_NumberOfIteration ; i++ )
  {
    for ( unsigned int j = 0 ; j < numberOfLandmarks ; j++ )
     {
          mv = sampleLandmarks->GetMeasurementVector(j);
          position2D [ 0 ] = mv [ 0 ];
          position2D [ 1 ] = mv [ 1 ];
          constIterator.SetIndex(position2D);
          maximumGrad = constIterator.Get();
          newPosition2D = position2D;

          for (unsigned int id = 0; id<2; id++)
           {
               posRight[ id ] = position2D [ id ];
               posLeft[ id ] = position2D [ id ];
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

         dx = dxyRef2[ 1 ] - dxyRef1[ 1 ];
         dy = dxyRef1[ 0 ] - dxyRef2[ 0 ];

         ax = vcl_abs(dx) * 2;
         ay = vcl_abs(dy) * 2;
         if (dx < 0) sx = -1; else sx = 1;
         if (dy < 0) sy = -1; else sy = 1;
           unsigned int count = 0;
         if (ax > ay)
           {
             d = ay - ax/2;
             while (count < m_LenghtOfProfile)
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
                    position2D[ 0 ] = posRight[ 0 ];
                    position2D[ 1 ] = posRight[ 1 ];
                    constIterator.SetIndex(position2D);
                    if (maximumGrad < constIterator.Get())
                      {
                        maximumGrad = constIterator.Get();
                        newPosition2D = position2D;
                      }
                    position2D[ 0 ]  = posLeft[ 0 ];
                    position2D[ 1 ]  = posLeft[ 1 ];
                    constIterator.SetIndex(position2D);
                    if (maximumGrad < constIterator.Get())
                      {
                        maximumGrad = constIterator.Get();
                        newPosition2D = position2D;
                      }
                    count++;
               }
             v.push_back (newPosition2D[ 0 ]);
             v.push_back (newPosition2D[ 1 ]);
             }
             else
             {
               d = ax - ay/2;
               while (count < m_LenghtOfProfile)
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
                  position2D[ 0 ] = posRight[ 0 ];
                  position2D[ 1 ] = posRight[ 1 ];
                  constIterator.SetIndex(position2D);
                  if (maximumGrad < constIterator.Get())
                    {
                      maximumGrad = constIterator.Get();
                      newPosition2D = position2D;
                    }
                 position2D[ 0 ]  = posLeft[ 0 ];
                 position2D[ 1 ]  = posLeft[ 1 ];
                 constIterator.SetIndex(position2D);
                 if (maximumGrad < constIterator.Get())
                   {
                      maximumGrad = constIterator.Get();
                      newPosition2D = position2D;
                   }
                 count++;
                }
                v.push_back (newPosition2D[ 0 ]);
                v.push_back (newPosition2D[ 1 ]);
             }
           }
     
         unsigned int row = 0;
         for (p6 = v.begin(); p6 != v.end(); p6++)
          {
            m_NewShape[row] = (*p6);
            row++;
          }
         v.erase(v.begin(), v.end());
         sampleLandmarks->Clear();

         m_DiffVector = m_NewShape - m_MeanShape;


         m_Db = m_EigenVectors.transpose() * m_DiffVector;

         for(unsigned int j = 0; j < numberOfEigenValues; j++)
           {
           m_Blimit[j] = 2 * vcl_sqrt(m_EigenValues[j]);
           }

         for(unsigned int j = 0; j < numberOfEigenValues; j++)
           {
           if(fabs(m_Db[j]) >  m_Blimit[j])
             {
             m_Db[j] = ( m_Db[j] / vcl_fabs(m_Db[j]) ) * m_Blimit[j];
             }
           }

    m_NewShape = m_MeanShape + m_EigenVectors * m_Db;

    MeasurementVectorDoubleType newCenterShape;
    newCenterShape [ 0 ] = 0.0;
    newCenterShape [ 1 ] = 0.0;
  
    for ( unsigned int j = 0 ; j < numberOfLandmarks ; j++ )
     {
        mv [ 0 ] = (unsigned int) (m_NewShape [ 2*j ] + 0.5);
        mv [ 1 ] = (unsigned int) (m_NewShape [ 2*j+1 ] + 0.5);
        sampleLandmarks->PushBack( mv );
        newCenterShape [ 0 ] += mv [ 0 ];
        newCenterShape [ 1 ] += mv [ 1 ];
     }
     newCenterShape [ 0 ] =  newCenterShape [ 0 ] / numberOfLandmarks;
     newCenterShape [ 1 ] =  newCenterShape [ 1 ] / numberOfLandmarks;

     deltaX = newCenterShape[ 0 ] - centerShape[ 0 ];
     deltaY = newCenterShape[ 1 ] - centerShape[ 1 ];

     for ( unsigned int j = 0 ; j < numberOfLandmarks ; j++ )
       {
         m_MeanShape [ 2*j ]   = m_MeanShape [ 2*j ] + deltaX;
         m_MeanShape [ 2*j+1 ] = m_MeanShape [ 2*j+1 ] + deltaY;
         centerShape [ 0 ] += m_MeanShape [ 2*j ];
         centerShape [ 1 ] += m_MeanShape [ 2*j+1 ];
       }
     centerShape [ 0 ] = centerShape [ 0 ] / numberOfLandmarks;
     centerShape [ 1 ] = centerShape [ 1 ] / numberOfLandmarks;

  }

  /* *
     * Remember that the moments are valid
     */
  m_Valid = 1;
  
}

/* *
   * Get the new shape
   */
template<class TImage>
typename ActiveShapeModelGradientSearchMethod<TImage>::VectorOfDoubleType
ActiveShapeModelGradientSearchMethod<TImage>::
GetNewShape()
{
  if (!m_Valid)        throw InvalidActiveShapeModeError(__FILE__, __LINE__);
  return m_NewShape;
}


/**
  * Print the current status
  */
template<class TImage>
void
ActiveShapeModelGradientSearchMethod<TImage>::
PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "Valid : " << m_Valid << std::endl;
  os << indent << "Mean Shape : " << m_MeanShape << std::endl;
  os << indent << "Eigen Vectors: " << m_EigenVectors << std::endl;
  os << indent << "Eigen Values: " << m_EigenValues << std::endl;
  os << indent << "Lenght of Profile: " << m_LenghtOfProfile << std::endl;
  os << indent << "Number of Iteration: " << m_NumberOfIteration << std::endl;
  os << indent << "Diff Vector: " << m_DiffVector << std::endl;
  os << indent << "Db Vector: " << m_Db << std::endl;
  os << indent << "Blimit Vector: " << m_Blimit << std::endl;
  os << indent << "New Shape: " << m_NewShape << std::endl;
  os << indent << "Image: " << m_Image.GetPointer() << std::endl;

  itkDebugMacro(<<"                                    ");
  itkDebugMacro(<<"Results of the shape model");
  itkDebugMacro(<<"====================================");

  itkDebugMacro(<< " ");
  itkDebugMacro(<< "==================   ");

  itkDebugMacro(<< "The new shape: ");
  
  itkDebugMacro(<< m_NewShape);

  itkDebugMacro(<< " ");
  itkDebugMacro(<< "+++++++++++++++++++++++++");
}// end PrintSelf



} // end namespace itk



#endif

