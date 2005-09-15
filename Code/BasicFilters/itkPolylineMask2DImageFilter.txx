/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolylineMask2DImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPolylineMask2DImageFilter_txx
#define _itkPolylineMask2DImageFilter_txx

#include "itkPolylineMask2DImageFilter.h"
#include "itkLineIterator.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkPathIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkProgressReporter.h"
#include "itkVectorContainer.h"

namespace itk
{
/**
 * Constructor
 */
template <class TInputImage, class TPolyline, 
          class TOutputImage>
PolylineMask2DImageFilter<TInputImage,TPolyline,TOutputImage>
::PolylineMask2DImageFilter()
{
  this->SetNumberOfRequiredInputs( 2 );
}

/**
 *
 */
template <class TInputImage, class TPolyline,
          class TOutputImage>
void PolylineMask2DImageFilter<TInputImage,TPolyline,TOutputImage>
::SetInput1(const TInputImage *input)

{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0,
                                   const_cast< TInputImage * >( input ) );
}

/**
 *
 */
template <class TInputImage, class TPolyline,
          class TOutputImage>
void PolylineMask2DImageFilter<TInputImage,TPolyline,TOutputImage>
::SetInput2(const TPolyline *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(1,
                                   const_cast< TPolyline * >( input ) );
}

template <class TInputImage, class TPolyline,
          class TOutputImage>
void PolylineMask2DImageFilter<TInputImage,TPolyline,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

/**
 *
 */
template <class TInputImage, class TPolyline,
          class TOutputImage>
void PolylineMask2DImageFilter<TInputImage,TPolyline,TOutputImage>
::GenerateData(void)
{
  typedef  LineIterator<TOutputImage>                    LineIteratorType;
  typedef  ImageLinearIteratorWithIndex< TOutputImage >  ImageLineIteratorType;

  typedef typename TInputImage::Pointer                               InputImagePointer;
  typedef ImageRegionConstIterator<TInputImage>         InputImageConstIteratorType;

  typedef typename TOutputImage::IndexType                            ImageIndexType;
  typedef typename TOutputImage::PixelType                            PixelType;
  typedef ImageRegionIterator<TOutputImage>             OutputImageIteratorType;


  typedef typename TPolyline::Pointer                                 PolylinePointer;
  typedef typename TPolyline::VertexType                              VertexType;
  typedef typename TPolyline::VertexListType                          VertexListType;
    

  typename TInputImage::ConstPointer inputImagePtr(
    dynamic_cast<const TInputImage  * >(
      this->ProcessObject::GetInput(0)));
  typename TPolyline::ConstPointer polylinePtr(
    dynamic_cast<const TPolyline    * >(
      this->ProcessObject::GetInput(1)));
  typename TOutputImage::Pointer outputImagePtr(
    dynamic_cast<TOutputImage * >(
      this->ProcessObject::GetOutput(0)));  


  outputImagePtr->SetSpacing(inputImagePtr->GetSpacing());
  outputImagePtr->SetOrigin(inputImagePtr->GetOrigin());
  outputImagePtr->SetRequestedRegion( inputImagePtr->GetRequestedRegion() );
  outputImagePtr->SetBufferedRegion(  inputImagePtr->GetBufferedRegion() );
  outputImagePtr->SetLargestPossibleRegion( inputImagePtr->GetLargestPossibleRegion() );
  outputImagePtr->Allocate();   


  typedef typename VertexListType::Pointer      VertexListPointer;
   
  const VertexListType * container      = polylinePtr->GetVertexList();

  typename VertexListType::ConstIterator piter = container->Begin();
    
  /* Rasterize each polyline segment using breshnan line iterator  */
    
  VertexType startVertex;
  VertexType endVertex;
  VertexType tmpVertex;
  VertexType pstartVertex;

  ImageIndexType tmpIndex;

/* Check if the polyline coordinates are within the input image */
  while ( piter != container->End() )
    {
    tmpVertex      = piter.Value();
    outputImagePtr->TransformPhysicalPointToIndex(tmpVertex,tmpIndex);    
    if ( !outputImagePtr->GetBufferedRegion().IsInside(tmpIndex))
      {
      itkExceptionMacro(<<"Polyline vertex is out of bound(Vertex,Index)"<<tmpVertex<<","<<tmpIndex);
      }
    ++piter;
    }

  // reset piter  
  piter = container->Begin();

  /* define flag to indicate the line segment slope */
  bool pflag;

  /* define background, foreground pixel values and unlabed pixel value */
  PixelType zero_val = NumericTraits< PixelType >::ZeroValue();
  PixelType u_val = static_cast<PixelType> (0);
  PixelType b_val = static_cast<PixelType> (2);
  PixelType f_val = static_cast<PixelType> (255);
  outputImagePtr->FillBuffer(u_val);

  pstartVertex = piter.Value();

  tmpVertex = pstartVertex;
  ++piter;

  ImageIndexType startImageIndex;
  ImageIndexType endImageIndex;
  ImageIndexType tmpImageIndex;
    
  typedef typename ImageIndexType::IndexValueType IndexValueType;

  ImageLineIteratorType imit(outputImagePtr, outputImagePtr->GetLargestPossibleRegion());
  imit.SetDirection( 0 );

  itkDebugMacro(<<"Generating the mask defined by the polyline.....");

  while ( piter != container->End() )
    {
    pflag         = false;
    startVertex    = tmpVertex;
    endVertex      = piter.Value();
    
    outputImagePtr->TransformPhysicalPointToIndex(startVertex,startImageIndex);
    outputImagePtr->TransformPhysicalPointToIndex(endVertex,endImageIndex);

    //itkDebugMacro(<<"Projection image (index,physical coordinate):"<<startImageIndex<<","<<startVertex<<std::endl);

    if (endImageIndex[1] > startImageIndex[1]) 
      {
      pflag = true;
      }

    LineIteratorType      it(outputImagePtr, startImageIndex, endImageIndex);
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
    tmpVertex = endVertex;
    ++piter;
    }
  
  /* Close the polygon */
  pflag         = false;
  startVertex    = tmpVertex;
  endVertex      = pstartVertex;

  outputImagePtr->TransformPhysicalPointToIndex(startVertex,startImageIndex);
  outputImagePtr->TransformPhysicalPointToIndex(endVertex,endImageIndex);

  if (endImageIndex[1] > startImageIndex[1]) 
    {
    pflag = true;
    }

  LineIteratorType      it(outputImagePtr, startImageIndex, endImageIndex);
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

  /* Mask the input image with the mask generated */
  InputImageConstIteratorType inputI(inputImagePtr,inputImagePtr->GetLargestPossibleRegion());
  OutputImageIteratorType     outputI(outputImagePtr,outputImagePtr->GetLargestPossibleRegion());
  inputI.GoToBegin();
  outputI.GoToBegin();
  while (! outputI.IsAtEnd())
    {
    if(outputI.Get() == f_val)
      {
      outputI.Set(inputI.Get());
      }
    else
      {
      outputI.Set(zero_val);
      }
    ++inputI;
    ++outputI;
    }
}

} // end namespace itk
#endif
