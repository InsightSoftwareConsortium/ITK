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
#ifndef itkPolylineMaskImageFilter_hxx
#define itkPolylineMaskImageFilter_hxx

#include "itkPolylineMaskImageFilter.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkImageRegionConstIterator.h"
#include "itkProgressReporter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkResampleImageFilter.h"
#include "itkLineIterator.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkPathIterator.h"
#include "itkVector.h"
#include "itkBoundingBox.h"

namespace itk
{

template< typename TInputImage, typename TPolyline, typename TVector,
          typename TOutputImage >
PolylineMaskImageFilter< TInputImage, TPolyline, TVector, TOutputImage >
::PolylineMaskImageFilter() :
  m_ViewVector( 1 ),
  m_UpVector( 1 ),
  m_CameraCenterPoint( 0 ),
  m_FocalPoint( 0.0 ),
  m_FocalDistance( 0.0 )
{
  this->SetNumberOfRequiredInputs( 2 );

  // This filter is meant only for 3D input and output images. We must
  // throw an exception otherwise.
  if ( ( TInputImage::ImageDimension != 3 )
       || ( TOutputImage::ImageDimension != 3 ) )
    {
    itkExceptionMacro(<< "PolylineMaskImageFilter must be templated over "
                      << "input and output images of dimension 3");
    }

  // View vectors must be of dimension 3
  if ( TVector::Length != 3 )
    {
    itkExceptionMacro(<< "PolylineMaskImageFilter must be templated over "
                      << "a view vector of length 3");
    }
}

template< typename TInputImage, typename TPolyline, typename TVector,
          typename TOutputImage >
void PolylineMaskImageFilter< TInputImage, TPolyline, TVector, TOutputImage >
::SetInput1(const InputImageType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< InputImageType * >( input ) );
}

template< typename TInputImage, typename TPolyline, typename TVector,
          typename TOutputImage >
void PolylineMaskImageFilter< TInputImage, TPolyline, TVector, TOutputImage >
::SetInput2(const PolylineType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 1,
                                    const_cast< PolylineType * >( input ) );
}

template< typename TInputImage, typename TPolyline, typename TVector,
          typename TOutputImage >
void PolylineMaskImageFilter< TInputImage, TPolyline, TVector, TOutputImage >
::GenerateRotationMatrix()
{
  // Normalize view and up vectors
  TVector nUpVector;
  TVector nViewVector;

  nUpVector = m_UpVector;
  nUpVector.Normalize();

  nViewVector = m_ViewVector;
  nViewVector.Normalize();

  itkDebugMacro(<< "Normalized Up vector" << nUpVector);
  itkDebugMacro(<< "Normalized View vector" << nViewVector);

  // Orthogonalize nUpVector and nViewVector
  TVector nOrthogonalVector;

  nOrthogonalVector = nUpVector - ( nViewVector * ( nUpVector * nViewVector ) );

  itkDebugMacro(<< "Up vector component orthogonal to View vector " << nOrthogonalVector);

  // Perform the cross product and determine a third coordinate axis
  // orthogonal to both nOrthogonalVector and nViewVector.

  TVector nThirdAxis;
  nThirdAxis = itk::CrossProduct(nOrthogonalVector, nViewVector);

  itkDebugMacro(<< "Third basis vector" << nThirdAxis);

  // Populate the rotation matrix using the unit vectors of the
  // camera reference coordinate system.

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

template< typename TInputImage, typename TPolyline, typename TVector, typename TOutputImage >
typename PolylineMaskImageFilter< TInputImage, TPolyline, TVector,
                                  TOutputImage >::ProjPlanePointType
PolylineMaskImageFilter< TInputImage, TPolyline, TVector, TOutputImage >
::TransformProjectPoint(PointType inputPoint)
{
  PointType centered;

  for ( unsigned int i = 0; i < 3; ++i )
    {
    centered[i] = inputPoint[i] - m_CameraCenterPoint[i];
    }

  PointType rotated = m_RotationMatrix * centered;

  ProjPlanePointType result;

  double factor = m_FocalDistance / ( rotated[2] );

  result[0] = m_FocalPoint[0] + ( rotated[0] * factor );
  result[1] = m_FocalPoint[1] + ( rotated[1] * factor );

  return result;
}

template< typename TInputImage, typename TPolyline, typename TVector,
          typename TOutputImage >
void PolylineMaskImageFilter< TInputImage, TPolyline, TVector, TOutputImage >
::GenerateData()
{
  typedef typename TInputImage::SizeType          InputImageSizeType;
  typedef typename TInputImage::PointType         InputImagePointType;
  typedef typename TInputImage::SpacingType       InputImageSpacingType;
  typedef ImageRegionConstIterator< TInputImage > InputImageConstIteratorType;

  typedef typename TOutputImage::PixelType    PixelType;
  typedef ImageRegionIterator< TOutputImage > OutputImageIteratorType;

  typedef typename TPolyline::VertexType     VertexType;
  typedef typename TPolyline::VertexListType VertexListType;

  typedef Point< double, 3 > OriginType;

  typename TInputImage::ConstPointer inputImagePtr(
    dynamic_cast< const TInputImage  * >(
      this->ProcessObject::GetInput(0) ) );
  typename TPolyline::ConstPointer polylinePtr(
    dynamic_cast< const TPolyline    * >(
      this->ProcessObject::GetInput(1) ) );
  typename TOutputImage::Pointer outputImagePtr(
    dynamic_cast< TOutputImage * >(
      this->ProcessObject::GetOutput(0) ) );

  OriginType originInput;
  originInput.Fill(0.0);
  //outputImagePtr->SetOrigin(inputImagePtr->GetOrigin());
  outputImagePtr->SetOrigin(originInput);
  outputImagePtr->SetSpacing( inputImagePtr->GetSpacing() );
  outputImagePtr->SetDirection( inputImagePtr->GetDirection() );

  outputImagePtr->SetRequestedRegion( inputImagePtr->GetRequestedRegion() );
  outputImagePtr->SetBufferedRegion( inputImagePtr->GetBufferedRegion() );
  outputImagePtr->SetLargestPossibleRegion( inputImagePtr->GetLargestPossibleRegion() );
  outputImagePtr->Allocate(true); // initialize buffer to zero

  InputImageConstIteratorType inputIt( inputImagePtr, inputImagePtr->GetLargestPossibleRegion() );
  OutputImageIteratorType     outputIt( outputImagePtr, outputImagePtr->GetLargestPossibleRegion() );

  typedef NearestNeighborInterpolateImageFunction< TInputImage, double > InterpolatorType;
  typedef typename InterpolatorType::PointType InterpolatorPointType;

  // Generate the transformation matrix
  this->GenerateRotationMatrix();

  // Generate input and output points
  InterpolatorPointType inputPoint;
  ProjPlanePointType    outputPoint;

  // Generate a 2D image with the viewing polygon as a mask
  typedef Image< PixelType, 2 >                        ProjectionImageType;
  typedef typename ProjectionImageType::IndexType      ProjectionImageIndexType;
  typedef typename ProjectionImageType::PointType      ProjectionImagePointType;
  typedef typename ProjectionImageType::SpacingType    ProjectionImageSpacingType;
  typedef typename ProjectionImageType::PixelType      ProjectionImagePixelType;
  typedef typename ProjectionImageType::RegionType     ProjectionImageRegionType;
  typedef typename ProjectionImageType::SizeType       ProjectionImageSizeType;

  ProjectionImageRegionType projectionRegion;

  // Determine the projection image size by transforming the eight corners
  // of the 3D input image

  InputImageSizeType inputImageSize;
  typedef Point< double, 3 > CornerPointType;
  typedef Point< double, 2 > CornerPointProjectionType;

  typedef BoundingBox< unsigned long int, 2, double > BoundingBoxType;
  typedef BoundingBoxType::PointsContainer            CornerPointProjectionContainer;

  CornerPointProjectionContainer::Pointer cornerPointProjectionlist = CornerPointProjectionContainer::New();
  CornerPointType                         cornerPoint;
  CornerPointType                         originPoint;
  CornerPointProjectionType               cornerProjectionPoint;

  originPoint[0] = 0.0;
  originPoint[1] = 0.0;
  originPoint[2] = 0.0;

  originPoint    = inputImagePtr->GetOrigin();
  inputImageSize = inputImagePtr->GetLargestPossibleRegion().GetSize();

  // 1st corner (xmin,ymin,zmin)
  cornerPoint = originPoint;
  cornerProjectionPoint = this->TransformProjectPoint(cornerPoint);
  cornerPointProjectionlist->push_back(cornerProjectionPoint);

  // 2nd corner (xmin,ymin,zmax)
  cornerPoint[0] = originPoint[0];
  cornerPoint[1] = originPoint[1];
  cornerPoint[2] = originPoint[2] + inputImageSize[2];
  cornerProjectionPoint = this->TransformProjectPoint(cornerPoint);
  cornerPointProjectionlist->push_back(cornerProjectionPoint);

  // 3rd corner (xmin,ymax,zmin)
  cornerPoint[0] = originPoint[0];
  cornerPoint[1] = originPoint[1] + inputImageSize[1];
  cornerPoint[2] = originPoint[2];
  cornerProjectionPoint = this->TransformProjectPoint(cornerPoint);
  cornerPointProjectionlist->push_back(cornerProjectionPoint);

  // 4th corner (xmin,ymax,zmax)
  cornerPoint[0] = originPoint[0];
  cornerPoint[1] = originPoint[1] + inputImageSize[1];
  cornerPoint[2] = originPoint[2] + inputImageSize[2];
  cornerProjectionPoint = this->TransformProjectPoint(cornerPoint);
  cornerPointProjectionlist->push_back(cornerProjectionPoint);

  // 5th corner (xmax,ymin,zmin)
  cornerPoint[0] = originPoint[0] + inputImageSize[0];
  cornerPoint[1] = originPoint[1];
  cornerPoint[2] = originPoint[2];

  cornerProjectionPoint = this->TransformProjectPoint(cornerPoint);
  cornerPointProjectionlist->push_back(cornerProjectionPoint);

  // 6th corner (xmax,ymin,zmax)
  cornerPoint[0] = originPoint[0] + inputImageSize[0];
  cornerPoint[1] = originPoint[1];
  cornerPoint[2] = originPoint[2] + inputImageSize[2];

  cornerProjectionPoint = this->TransformProjectPoint(cornerPoint);
  cornerPointProjectionlist->push_back(cornerProjectionPoint);

  // 7th corner (xmax,ymax,zmin)
  cornerPoint[0] = originPoint[0] + inputImageSize[0];
  cornerPoint[1] = originPoint[1] + inputImageSize[1];
  cornerPoint[2] = originPoint[2];

  cornerProjectionPoint = this->TransformProjectPoint(cornerPoint);
  cornerPointProjectionlist->push_back(cornerProjectionPoint);

  // 8th corner (xmax,ymax,zmax)
  cornerPoint[0] = originPoint[0] + inputImageSize[0];
  cornerPoint[1] = originPoint[1] + inputImageSize[1];
  cornerPoint[2] = originPoint[2] + inputImageSize[2];

  cornerProjectionPoint = this->TransformProjectPoint(cornerPoint);
  cornerPointProjectionlist->push_back(cornerProjectionPoint);

  // Compute the bounding box of the projected points
  BoundingBoxType::Pointer boundingBox = BoundingBoxType::New();

  boundingBox->SetPoints (cornerPointProjectionlist);

  if ( !boundingBox->ComputeBoundingBox() )
    {
    itkExceptionMacro(<< "Bounding box computation error");
    }

  const BoundingBoxType::BoundsArrayType & bounds = boundingBox->GetBounds();
  itkDebugMacro(<< "Projection image bounding box=" << bounds);

  ProjectionImageIndexType projectionStart;
  projectionStart[0] = 0;
  projectionStart[1] = 0;

  ProjectionImageSizeType projectionSize;
  IndexValueType pad;

  pad = 5;

  projectionSize[0] = (IndexValueType)( bounds[1] - bounds[0] ) + pad;
  projectionSize[1] = (IndexValueType)( bounds[3] - bounds[2] ) + pad;

  projectionRegion.SetIndex(projectionStart);
  projectionRegion.SetSize(projectionSize);

  typename ProjectionImageType::Pointer projectionImagePtr = ProjectionImageType::New();

  ProjectionImagePointType origin;
  origin[0] = bounds[0];
  origin[1] = bounds[2];

  projectionImagePtr->SetOrigin(origin);

  ProjectionImageSpacingType spacing;

  spacing[0] = 1.0;
  spacing[1] = 1.0;

  projectionImagePtr->SetSpacing(spacing);

  itkDebugMacro(<< "Projection image size:" << projectionSize);
  itkDebugMacro(<< "Projection image start index:" << projectionStart);
  itkDebugMacro(<< "Projection image origin:" << origin);

  projectionImagePtr->SetRequestedRegion(projectionRegion);
  projectionImagePtr->SetBufferedRegion(projectionRegion);
  projectionImagePtr->SetLargestPossibleRegion(projectionRegion);
  projectionImagePtr->Allocate(true); // initialize buffer to zero

  typedef ImageRegionIterator< ProjectionImageType > ProjectionImageIteratorType;
  ProjectionImageIteratorType projectionIt( projectionImagePtr, projectionImagePtr->GetLargestPossibleRegion() );

  itkDebugMacro(<< "Rotation matrix" << m_RotationMatrix);

  const VertexListType *container = polylinePtr->GetVertexList();

  typename VertexListType::ConstIterator piter = container->Begin();

  // Rasterize each polyline segment using Bresenham line iterator
  VertexType startIndex;
  VertexType endIndex;
  VertexType projectionIndex;
  VertexType pstartIndex;

  // Define a flag to indicate the line segment slope
  bool pflag;

  // Define background, foreground, and unlabeled pixel values
  PixelType u_val = static_cast< ProjectionImagePixelType >( 0 );
  PixelType b_val = static_cast< ProjectionImagePixelType >( 2 );
  PixelType f_val = static_cast< ProjectionImagePixelType >( 255 );

  projectionImagePtr->FillBuffer(u_val);

  // Polygon start index
  pstartIndex = piter.Value();
  projectionIndex = pstartIndex;
  ++piter;
  ProjectionImageIndexType startImageIndex;
  ProjectionImageIndexType endImageIndex;
  ProjectionImageIndexType projectionImageIndex;

  typedef LineIterator< ProjectionImageType >                 LineIteratorType;
  typedef ImageLinearIteratorWithIndex< ProjectionImageType > ImageLineIteratorType;

  ImageLineIteratorType imit( projectionImagePtr, projectionImagePtr->GetLargestPossibleRegion() );
  imit.SetDirection(0);

  while ( piter != container->End() )
    {
    pflag         = false;
    startIndex    = projectionIndex;
    endIndex      = piter.Value();

    for ( unsigned int i = 0; i < ProjectionImageType::ImageDimension; ++i )
      {
      startImageIndex[i] = static_cast< IndexValueType >( startIndex[i] );
      endImageIndex[i]   = static_cast< IndexValueType >( endIndex[i] );
      }

    if ( endImageIndex[1] > startImageIndex[1] )
      {
      pflag = true;
      }

    itkDebugMacro(<< "Polyline:" << startImageIndex << "," << endImageIndex);
    LineIteratorType it(projectionImagePtr, startImageIndex, endImageIndex);
    it.GoToBegin();

    while ( !it.IsAtEnd() )
      {
      projectionImageIndex[0] = it.GetIndex()[0];
      projectionImageIndex[1] = it.GetIndex()[1];

      // Initialize the image line iterator
      imit.SetIndex(projectionImageIndex);
      while ( !imit.IsAtEndOfLine() )
        {
        if ( pflag )
          {
          if ( imit.Get() == u_val )
            {
            imit.Set(f_val);
            }
          }
        else
          {
          imit.Set(b_val);
          }

        ++imit;
        }
      ++it;
      }
    projectionIndex = endIndex;
    ++piter;
    }

  // Close the polygon
  pflag         = false;
  startIndex    = projectionIndex;
  endIndex      = pstartIndex;

  for ( unsigned int i = 0; i < ProjectionImageType::ImageDimension; ++i )
    {
    startImageIndex[i] = static_cast< IndexValueType >( startIndex[i] );
    endImageIndex[i]   = static_cast< IndexValueType >( endIndex[i] );
    }

  if ( endImageIndex[1] > startImageIndex[1] )
    {
    pflag = true;
    }

  LineIteratorType it(projectionImagePtr, startImageIndex, endImageIndex);
  it.GoToBegin();

  while ( !it.IsAtEnd() )
    {
    projectionImageIndex[0] = it.GetIndex()[0];
    projectionImageIndex[1] = it.GetIndex()[1];

    // Initialize the image line iterator
    imit.SetIndex(projectionImageIndex);
    while ( !imit.IsAtEndOfLine() )
      {
      if ( pflag )
        {
        if ( imit.Get() == u_val )
          {
          imit.Set(f_val);
          }
        }
      else
        {
        imit.Set(b_val);
        }
      ++imit;
      }
    ++it;
    }

  // Mask the input image using the binary image defined by the region
  // demarcated by the polyline contour
  outputIt.GoToBegin();
  inputIt.GoToBegin();

  InputImageSpacingType inputImageSpacing;
  InputImagePointType   inputImageOrigin;

  inputImageSpacing = inputImagePtr->GetSpacing();
  inputImageOrigin = inputImagePtr->GetOrigin();
  inputImageSize = inputImagePtr->GetLargestPossibleRegion().GetSize();

  while ( !inputIt.IsAtEnd() )
    {
    outputImagePtr->TransformIndexToPhysicalPoint(outputIt.GetIndex(), inputPoint);
    outputPoint = this->TransformProjectPoint(inputPoint);
    projectionImagePtr->TransformPhysicalPointToIndex(outputPoint, projectionImageIndex);

    if ( !projectionImagePtr->GetBufferedRegion().IsInside(projectionImageIndex) )
      {
      itkExceptionMacro(<< "Projection Image index out of bound:" << projectionImageIndex);
      }

    if ( projectionImagePtr->GetPixel(projectionImageIndex) == f_val )
      {
      outputIt.Set( inputIt.Get() );
      }
    else
      {
      outputIt.Set(u_val);
      }

    ++inputIt;
    ++outputIt;
    }
}

template< typename TInputImage, typename TPolyline, typename TVector,
          typename TOutputImage >
void PolylineMaskImageFilter< TInputImage, TPolyline, TVector, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Viewing vector: "
     << static_cast< typename NumericTraits< VectorType >::PrintType >( m_ViewVector )
     << std::endl;
  os << indent << "Up Vector: "
     << static_cast< typename NumericTraits< VectorType >::PrintType >( m_UpVector )
     << std::endl;
  os << indent << "Camera Center Point: " << m_CameraCenterPoint << std::endl;
  os << indent << "Focal  Point       : " << m_FocalPoint        << std::endl;
  os << indent << "Focal Distance     : " << m_FocalDistance     << std::endl;
  os << indent << "Rotation matrix    : " << m_RotationMatrix   << std::endl;
}
} // end namespace itk
#endif
