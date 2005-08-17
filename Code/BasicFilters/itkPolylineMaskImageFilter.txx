/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolylineMaskImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPolylineMaskImageFilter_txx
#define _itkPolylineMaskImageFilter_txx

#include "itkPolylineMaskImageFilter.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkProgressReporter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkResampleImageFilter.h"
#include "itkLineIterator.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkPathIterator.h"
#include "itkVector.h"

namespace itk
{
/**
 * Constructor
 */
template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::PolylineMaskImageFilter()
{
  this->SetNumberOfRequiredInputs( 2 );
  m_ViewVector.Fill(1);
  m_UpVector.Fill(1);
  m_CameraCenterPoint.Fill(0);
}

/**
 *
 */
  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  void PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::SetInput1(const InputImageType *input)
  {
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0,
                                   const_cast< InputImageType * >( input ) );
  }

/**
 *
 */
  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  void PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::SetInput2(const PolylineType *input)
  {
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(1,
                                   const_cast< PolylineType * >( input ) );
  }

/**
 Generate 3D roatation matrix using the viewing and up vector
*/
  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  void PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::GenerateRotationMatrix()
  {
    /* Normalize the view and up vector */
    TVector nUpVector; /* normalized Up vector */
    TVector nViewVector; /* normalized View vector */

    nUpVector = m_UpVector;
    nUpVector.Normalize();
    
    nViewVector = m_ViewVector;
    nViewVector.Normalize();

    itkDebugMacro(<<"Normalized Up vector" <<nUpVector);
    itkDebugMacro(<<"Normalized View vector"<<nViewVector);

    /* orthogonalize nUpVector and nViewVector */
    TVector nOrthogonalVector;

    nOrthogonalVector = nUpVector - (nViewVector*(nUpVector*nViewVector));

    itkDebugMacro(<<"Up vector component orthogonal to View vector "<<nOrthogonalVector);
    
    /* Perform cross product and determine a third coordinate axis
    orthogonal to both nOrthogonalVector and nViewVector */

    TVector nThirdAxis;
    nThirdAxis = CrossProduct(nOrthogonalVector,nViewVector);

    itkDebugMacro(<<"Thrid basis vector"<<nThirdAxis);

    /* populate the rotation matrix using the unit vectors of the
       camera reference coordinate system */

    m_RotationMatrix[0][0] = nThirdAxis[0];
    m_RotationMatrix[0][1] = nThirdAxis[1];
    m_RotationMatrix[0][2] = nThirdAxis[2]; 

    m_RotationMatrix[1][0] = nOrthogonalVector[0];
    m_RotationMatrix[1][1] = nOrthogonalVector[1];
    m_RotationMatrix[1][2] = nOrthogonalVector[2];

    m_RotationMatrix[2][0] = nViewVector[0];
    m_RotationMatrix[2][1] = nViewVector[1];
    m_RotationMatrix[2][2] = nViewVector[2];

  }

/**
 3D rotation and perspective projection transform
 */
  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  typename PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>::ProjPlanePointType PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::TransformProjectPoint(PointType inputPoint)
  {
  unsigned int i;
  PointType centered;

  // itkDebugMacro(<<"Point transforming"<<inputPoint);

  for(i=0;i<3;i++)
    { 
    centered[i] = inputPoint[i] - m_CameraCenterPoint[i];
    }

  // itkDebugMacro(<<"Point centered"<<centered);

  PointType rotated =  m_RotationMatrix * centered;

  // itkDebugMacro(<<"Point rotated"<<rotated);

  ProjPlanePointType result;

  double factor = m_FocalDistance / ( rotated[2] );
  
  result[0] = m_FocalPoint[0] + (rotated[0] * factor);
  result[1] = m_FocalPoint[1] + (rotated[1] * factor);

  // itkDebugMacro(<<"Point projected"<<result);

  return result;
  }

/**
 *
 */
  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  void PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::GenerateData(void)
  {


  typedef typename TInputImage::Pointer                               InputImagePointer;
  typedef ImageRegionConstIterator<TInputImage>         InputImageConstIteratorType;

  typedef typename TOutputImage::IndexType                            ImageIndexType;
  typedef typename TOutputImage::PixelType                            PixelType;
  typedef ImageRegionIterator<TOutputImage>             OutputImageIteratorType;


  typedef typename TPolyline::Pointer                                 PolylinePointer;
  typedef typename TPolyline::VertexType                              VertexType;
  typedef typename TPolyline::VertexListType                          VertexListType;
  typedef typename TPolyline::IndexType                               PolylineIndexType;

  typename TInputImage::ConstPointer inputImagePtr(
    dynamic_cast<const TInputImage  * >(
      this->ProcessObject::GetInput(0)));
  typename TPolyline::ConstPointer polylinePtr(
    dynamic_cast<const TPolyline    * >(
      this->ProcessObject::GetInput(1)));
  typename TOutputImage::Pointer outputImagePtr(
    dynamic_cast<TOutputImage * >(
      this->ProcessObject::GetOutput(0)));


  outputImagePtr->SetRequestedRegion( inputImagePtr->GetRequestedRegion() );
  outputImagePtr->SetBufferedRegion(  inputImagePtr->GetBufferedRegion() );
  outputImagePtr->SetLargestPossibleRegion( inputImagePtr->GetLargestPossibleRegion() );
  outputImagePtr->Allocate();   


  InputImageConstIteratorType  inputIt(inputImagePtr,inputImagePtr->GetLargestPossibleRegion());
  OutputImageIteratorType outputIt(outputImagePtr,outputImagePtr->GetLargestPossibleRegion());

  typedef NearestNeighborInterpolateImageFunction< TInputImage, double> InterpolatorType;
  typedef typename InterpolatorType::OutputType OutputType;
  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
  typedef typename InterpolatorType::PointType    PointType;
 
  // to an output pixel
  PointType inputPoint;
  PointType tmpPoint;
  ProjPlanePointType outputPoint;
   
  // Walk the output region

  // Generate a 2D image with the viewing polygon as a mask 
  typedef Image<unsigned char,2> TmpImageType;
  typedef typename TmpImageType::IndexType          TmpImageIndexType;
  typedef typename TmpImageType::PixelType          TmpPixelType;
  typedef TmpImageType::RegionType                  TmpRegionType;
  typedef TmpImageType::SizeType                    TmpSizeType;
  typedef TmpImageType::IndexType                    TmpIndexType;
  typedef LineIterator<TmpImageType>                    LineIteratorType;
  typedef ImageLinearIteratorWithIndex< TmpImageType >  ImageLineIteratorType;
  
  TmpIndexType  tmpStart;
  TmpSizeType   tmpSize;
  TmpRegionType tmpRegion;

  tmpStart[0] = 0;
  tmpStart[1] = 0;

  tmpSize[0] = inputImagePtr->GetLargestPossibleRegion().GetSize()[0];
  tmpSize[1] = inputImagePtr->GetLargestPossibleRegion().GetSize()[1];

  itkDebugMacro(<<"Projection image Tmp size:"<<tmpSize);

  tmpRegion.SetIndex(tmpStart);
  tmpRegion.SetSize(tmpSize);

  typename TmpImageType::Pointer tmpImagePtr = TmpImageType::New();

  tmpImagePtr->SetRequestedRegion( tmpRegion );
  tmpImagePtr->SetBufferedRegion(  tmpRegion );
  tmpImagePtr->SetLargestPossibleRegion( tmpRegion );
  tmpImagePtr->Allocate();   
  tmpImagePtr->FillBuffer(0);

  typedef ImageRegionIterator<TmpImageType>  TmpImageIteratorType;
  TmpImageIteratorType tmpIt(tmpImagePtr,tmpImagePtr->GetLargestPossibleRegion());

  /* Generate the transformation matrix */
  this->GenerateRotationMatrix();

  itkDebugMacro(<<"Rotation matrix"<<std::cout<<m_RotationMatrix);

  typedef typename VertexListType::Pointer      VertexListPointer;
   
  const VertexListType * container      = polylinePtr->GetVertexList();

  typename VertexListType::ConstIterator piter = container->Begin();
    
  /* Rasterize each polyline segment using breshnan line iterator  */
    
  VertexType startIndex;
  VertexType endIndex;
  VertexType tmpIndex;
  VertexType pstartIndex;

  /* define flag to indicate the line segment slope */
  bool pflag;

  /* define background, foreground pixel values and unlabed pixel value */
  PixelType u_val = static_cast<TmpPixelType> (0);
  PixelType b_val = static_cast<TmpPixelType> (2);
  PixelType f_val = static_cast<TmpPixelType> (255);

  tmpImagePtr->FillBuffer(u_val);

  /* polyon start index */
  pstartIndex = piter.Value();
  tmpIndex = pstartIndex;
  ++piter;
  TmpImageIndexType startImageIndex;
  TmpImageIndexType endImageIndex;
  TmpImageIndexType tmpImageIndex;
    
  typedef typename TmpImageIndexType::IndexValueType IndexValueType;

  ImageLineIteratorType imit(tmpImagePtr, tmpImagePtr->GetLargestPossibleRegion());
  imit.SetDirection( 0 );

  while ( piter != container->End() )
    {
    pflag         = false;
    startIndex    = tmpIndex;
    endIndex      = piter.Value();
    
    for(unsigned int i=0; i < TmpImageType::ImageDimension; i++ )
      {
      startImageIndex[i] = static_cast<IndexValueType> (startIndex[i]);
      endImageIndex[i]   = static_cast<IndexValueType> (endIndex[i]);
      }


    if (endImageIndex[1] > startImageIndex[1]) 
      {
      pflag = true;
      }

    // itkDebugMacro(<<"Polyline:"<<startImageIndex<<","<<endImageIndex);
    LineIteratorType      it(tmpImagePtr, startImageIndex, endImageIndex);
    it.GoToBegin();

    while (!it.IsAtEnd())
      {
      tmpImageIndex[0] = it.GetIndex()[0];
      tmpImageIndex[1] = it.GetIndex()[1];

      //initialize imit using it

      imit.SetIndex(tmpImageIndex);
      while ( ! imit.IsAtEndOfLine() )
        {
        if ( pflag ) 
          {
          if ( imit.Get() == u_val)
            {
            imit.Set(f_val);
            }
          }
        else 
          imit.Set(b_val);

        ++imit;
        }
      ++it;
      }
    tmpIndex = endIndex;
    ++piter;
    }

  /* Close the polygon */
  pflag         = false;
  startIndex    = tmpIndex;
  endIndex      = pstartIndex;
    
  for(unsigned int i=0; i < TmpImageType::ImageDimension; i++ )
    {
    startImageIndex[i] = static_cast<IndexValueType> (startIndex[i]);
    endImageIndex[i]   = static_cast<IndexValueType> (endIndex[i]);
    }

  if (endImageIndex[1] > startImageIndex[1]) 
    {
    pflag = true;
    }

  LineIteratorType      it(tmpImagePtr, startImageIndex, endImageIndex);
  it.GoToBegin();

  while (!it.IsAtEnd())
    {
    tmpImageIndex[0] = it.GetIndex()[0];
    tmpImageIndex[1] = it.GetIndex()[1];
    //initialize imit using it

    imit.SetIndex(tmpImageIndex);
    while ( ! imit.IsAtEndOfLine() )
      {
      if ( pflag ) 
        {
        if ( imit.Get() == u_val)
          {
          imit.Set(f_val);
          }
        }
      else 
        imit.Set(b_val);
      ++imit;
      }
    ++it;
    }

  outputIt.GoToBegin();
  inputIt.GoToBegin();

  while ( !inputIt.IsAtEnd() )
    {
    outputImagePtr->TransformIndexToPhysicalPoint( inputIt.GetIndex(), inputPoint );
    outputPoint = this->TransformProjectPoint(inputPoint);
    tmpImagePtr->TransformPhysicalPointToIndex(outputPoint,tmpImageIndex);
    tmpIt.SetIndex(tmpImageIndex);

    if(tmpIt.Get() == f_val)
      {
      outputIt.Set(inputIt.Get());
      }
    else
      {
      outputIt.Set(u_val);
      }

    ++inputIt;
    ++outputIt;
    }
  }

  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
  void PolylineMaskImageFilter<TInputImage,TPolyline,TVector,TOutputImage>
  ::PrintSelf(std::ostream& os, Indent indent) const
  {
  Superclass::PrintSelf(os,indent);
  os << indent << "Viewing vector: "
     << static_cast<typename NumericTraits<VectorType>::PrintType>(m_ViewVector)
     << std::endl;
  os << indent << "Up Vector: " 
     << static_cast<typename NumericTraits<VectorType>::PrintType>(m_UpVector)
     << std::endl;
  os << indent << "Camera Center Point: " << m_CameraCenterPoint << std::endl;
  os << indent << "Focal  Point       : " << m_FocalPoint        << std::endl;
  os << indent << "Focal Distance     : " << m_FocalDistance     << std::endl;
  os << indent << "Rotation matrix    : " << m_RotationMatrix   << std::endl;

  }
} // end namespace itk
#endif
