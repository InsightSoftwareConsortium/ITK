/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIsoContourDistanceImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/    
#ifndef _itkIsoContourDistanceImageFilter_txx
#define _itkIsoContourDistanceImageFilter_txx

#include "itkIsoContourDistanceImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkExceptionObject.h"
#include "itkNumericTraits.h"
#include "itkIndex.h"
#include "itkExceptionObject.h"

namespace itk
{

/*
 * Default constructor.
 */
template <class TInputImage, class TOutputImage>
IsoContourDistanceImageFilter<TInputImage,TOutputImage>
::IsoContourDistanceImageFilter()
{

  m_LevelSetValue = NumericTraits<InputPixelType>::Zero;
  
  m_FarValue = 10*NumericTraits<PixelType>::One;
  
  m_NarrowBanding = false;
  m_NarrowBand = NULL;
  
  m_Barrier = Barrier::New();

}


/*
 * Set the input narrowband container.
 */
template <class TInputImage, class TOutputImage>
void
IsoContourDistanceImageFilter<TInputImage,TOutputImage>
::SetNarrowBand(
  NarrowBandType *  ptr )
{
  if( m_NarrowBand != ptr )
    {
    m_NarrowBand = ptr;
    this->Modified();
    }
}


/*
 * PrintSelf method.
 */
template <class TInputImage, class TOutputImage>
void
IsoContourDistanceImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Narrowbanding: " << m_NarrowBanding << std::endl;
  os << indent << "LevelSetValue: " << m_LevelSetValue << std::endl;
  os << indent << "FarValue: " << m_FarValue << std::endl;
  os << std::endl;
}


/*
 * GenerateInputRequestedRegion method.
 */
template <class TInputImage, class TOutputImage>
void
IsoContourDistanceImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // use the default implementation.
  this->Superclass::GenerateInputRequestedRegion();
}


/*
 * EnlargeOutputRequestedRegion method.
 */
template <class TInputImage, class TOutputImage>
void
IsoContourDistanceImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(
  DataObject *output )
{

  // this filter requires the all of the output image to be in
  // the buffer
  TOutputImage *imgData;
  imgData = dynamic_cast<TOutputImage*>( output );
  if ( imgData )
    {
    imgData->SetRequestedRegionToLargestPossibleRegion();
    }
  else
    {
    // pointer could not be cast to TLevelSet *
    itkWarningMacro(<< "itk::IsoContourDistanceImageFilter" <<
                    "::EnlargeOutputRequestedRegion cannot cast "
                    << typeid(output).name() << " to "
                    << typeid(TOutputImage*).name() );

    }

}


/*
 * Before ThreadedGenerateData:
 *  Split the band if we use narrowband mode
 */
template <class TInputImage, class TOutputImage>
void
IsoContourDistanceImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{

  // Instead of using GetNumberOfThreads, we need to split the image into the
  // number of regions that will actually be returned by
  // itkImageSource::SplitRequestedRegion.  Sometimes this number is less than
  // the number of threads requested.
  typename TOutputImage::RegionType dummy;
  unsigned int actualThreads = this->SplitRequestedRegion(0, this->GetNumberOfThreads(),
                                            dummy);
  
  if( m_NarrowBanding )
     {
     // Split the narrow band into sections, one section for each thread
       this->m_NarrowBandRegion
       = this->m_NarrowBand->SplitBand(actualThreads);
       
     // Initialize the barrier for the thread syncronization in
     // the narrowband case.
     this->m_Barrier->Initialize(actualThreads);

     }
}

//----------------------------------------------------------------------------
// The execute method created by the subclass.
template <class TInputImage, class TOutputImage>
void 
IsoContourDistanceImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{

  //Iterate over split region or split band as convinient.
  if( m_NarrowBanding == false )
     {  
     this->ThreadedGenerateDataFull(outputRegionForThread,threadId);
     }
  else 
     {
     this->ThreadedGenerateDataBand(outputRegionForThread,threadId);
     }
     
}

// The execute method created by the subclass.
template <class TInputImage, class TOutputImage>
void 
IsoContourDistanceImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateDataFull(const OutputImageRegionType& outputRegionForThread,
                       int itkNotUsed(threadId))
{
  typedef typename InputImageType::ConstPointer ImageConstPointer;
  typedef typename OutputImageType::Pointer OutputPointer;
  ImageConstPointer inputPtr = this->GetInput();
  OutputPointer outputPtr = this->GetOutput();
  
  typedef ImageRegionConstIterator<InputImageType> ConstIteratorType;
  typedef ImageRegionIterator<OutputImageType> IteratorType;
  ConstIteratorType inIt (inputPtr,
                          outputRegionForThread);
  IteratorType outIt (outputPtr,
                      outputRegionForThread); 
  
  unsigned int n,ng;
 
  //Initialize output image 
  while(!inIt.IsAtEnd())
   {
     if(inIt.Get() > m_LevelSetValue)
       {
       outIt.Set(+m_FarValue);
       }
     else if (inIt.Get() < m_LevelSetValue)
       {
       outIt.Set(-(double)m_FarValue);
       }
     else
       {
       outIt.Set(NumericTraits<PixelType>::Zero);    
       }
     ++inIt;
     ++outIt;
    } 
  
  inIt.GoToBegin();
  outIt.GoToBegin();
  
  InputSizeType radius_in;
  SizeType radius_out;
  for (n=0 ; n<ImageDimension ; n++)
    {
    radius_in[n]= 2;
    radius_out[n]= 1;
    }

  //Define Neighborhood iterator       
  ConstNeighborhoodIterator<InputImageType> inNeigIt(radius_in, inputPtr, 
                                                        inputPtr->GetRequestedRegion());
  NeighborhoodIterator<OutputImageType> outNeigIt(radius_out, outputPtr, 
                                                        outputPtr->GetRequestedRegion());
  PixelType val,val0,val1,val0_new,val1_new,diff;
  PixelType norm;
  bool sign,neigh_sign;
 
  PixelType grad0[ImageDimension];
  PixelType grad1[ImageDimension];
  PixelType grad[ImageDimension];
  
  PixelType alpha0 = 0.5;  //Interpolation factor
  PixelType alpha1 = 0.5;  //Interpolation factor
  const typename InputImageType::SpacingType& vs = inputPtr->GetSpacing();
  double vs_2[ImageDimension];
  
  for(n = 0; n<ImageDimension ; n++)
     vs_2[n]=2*vs[n];
  
  //Get Stride information to move across dimension
  ::size_t stride[ImageDimension];
  unsigned int center;
  
  for (n=0 ; n<ImageDimension ; n++) 
   {
    stride[n]=inNeigIt.GetStride(n);
   }
  center = inNeigIt.Size() / 2;
   
  for (inNeigIt.GoToBegin(); !inNeigIt.IsAtEnd() ; ++inNeigIt, ++outNeigIt) 
    {
     val0 = inNeigIt.GetPixel(center) - static_cast< PixelType >( m_LevelSetValue );
     sign = (val0>0);
     
     //Compute gradient at val0
     for (ng=0;ng<ImageDimension;ng++) 
       {
       grad0[ng] = static_cast< PixelType >( inNeigIt.GetNext(ng,1) ) -
                   static_cast< PixelType >( inNeigIt.GetPrevious(ng,1) );
       } 
     
     for (n=0;n<ImageDimension;n++)
       {
       
       val1 =  static_cast< PixelType >( inNeigIt.GetPixel(center+stride[n]) )
              -static_cast< PixelType >( m_LevelSetValue );
       
       neigh_sign = (val1>0);
       
       if(sign != neigh_sign) {
         for (ng=0;ng<ImageDimension;ng++)
           {
           grad1[ng]= static_cast< PixelType >( inNeigIt.GetPixel(center+stride[n]+stride[ng]) ) 
                     -static_cast< PixelType >( inNeigIt.GetPixel(center+stride[n]-stride[ng]) );
           }
         if(sign)
           {
           diff = val0-val1;
           }
         else
           {
           diff = val1-val0;
           }
         if(diff < NumericTraits<PixelType>::min()) 
           {
          //do something: printf, or thorw exception. ??
           continue;
           }
          //Interpolate values
          norm = NumericTraits<PixelType>::Zero;
          for (ng=0;ng<ImageDimension;ng++)
            {
            grad[ng] = (grad0[ng]*alpha0 + grad1[ng]*alpha1)/vs_2[ng];
            norm += grad[ng]*grad[ng];
            }
          norm = vcl_sqrt((float)norm);

          if (norm > NumericTraits<PixelType>::min())
            {
            val = vcl_fabs((float)grad[n])*vs[n]/norm/diff;
            
            val0_new = val0*val;
            val1_new = val1*val;    
            
            if(fabs((float)val0_new)<fabs((float)outNeigIt.GetNext(n,0)))
              {
              outNeigIt.SetNext(n,0,static_cast<PixelType>(val0_new) );
              }
            if(fabs((float)val1_new)<fabs((float)outNeigIt.GetNext(n,1)))
              {
              outNeigIt.SetNext(n,1,static_cast<PixelType>(val1_new) );
              }
             }
           else
             {
             itkExceptionMacro(<<"Gradient norm is lower than pixel precision");
            }
         } // end if (sign != sign_neigh)
       } //end for n               
       
     }
}

// The execute method created by the subclass.
template <class TInputImage, class TOutputImage>
void 
IsoContourDistanceImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateDataBand(const OutputImageRegionType& outputRegionForThread,
                           int threadId)
{
  typename InputImageType::ConstPointer inputPtr = this->GetInput();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  
  //Tasks:
  //1. Initialize whole output image
  //2. Wait for threads
  //3. Compute over the narrowband
  
  //1. Initialization of the output image
  //Each thread initializes the region given by outputRegionForThread.
  typedef ImageRegionConstIterator<InputImageType> ConstIteratorType;
  typedef ImageRegionIterator<OutputImageType> IteratorType;
  ConstIteratorType inInitIt (inputPtr,
                          outputRegionForThread);
  IteratorType outInitIt (outputPtr,
                      outputRegionForThread); 
  

  //Initialize output image 
  while(!inInitIt.IsAtEnd())
   {
     if(inInitIt.Get() > m_LevelSetValue)
       {
       outInitIt.Set(+m_FarValue);
       }
     else if (inInitIt.Get() < m_LevelSetValue)
       {
       outInitIt.Set(-(double)m_FarValue);
       }
     else
       {
       outInitIt.Set(NumericTraits<PixelType>::Zero);    
       }
     ++inInitIt;
     ++outInitIt;
    } 

  //2. Threads must wait till all are done
  this->m_Barrier->Wait();
  //3. Computation over the narrowband
  ConstBandIterator bandIt  = m_NarrowBandRegion[threadId].Begin;
  ConstBandIterator bandEnd = m_NarrowBandRegion[threadId].End;
  typedef ImageRegionConstIterator<InputImageType> ConstIteratorType;
  typedef ImageRegionIterator<OutputImageType> IteratorType;
  
  ConstIteratorType inIt (inputPtr,
                          inputPtr->GetRequestedRegion());
  IteratorType outIt (outputPtr,
                      outputPtr->GetRequestedRegion());
  unsigned int n,ng;
                       
  //We don't have to initialize the band again.
  //Following commented lines will be deprecetated.
  /**
  for( ; bandIt != bandEnd; ++bandIt )
    {
    inIt.SetIndex(bandIt->m_Index);
    outIt.SetIndex(bandIt->m_Index);
    if (inputPtr->GetPixel(bandIt->m_Index) > m_LevelSetValue)
      { 
      outputPtr->SetPixel(bandIt->m_Index,+m_FarValue);
      }
    else if (inputPtr->GetPixel(bandIt->m_Index) < m_LevelSetValue)
       {
       outputPtr->SetPixel(bandIt->m_Index,-m_FarValue);
       }
    else
       {
       outputPtr->SetPixel(bandIt->m_Index,NumericTraits<PixelType>::Zero);     
       }
    }
   bandIt = m_NarrowBandRegion[threadId].Begin;
   bandEnd = m_NarrowBandRegion[threadId].End;
  **/

  InputSizeType radius_in;
  SizeType radius_out;
  for (n=0 ; n<ImageDimension ; n++)
    {
    //radius_in[n]= 2*NumericTraits<InputSizeType>::One();
    radius_in[n] = 2;
    radius_out[n] = 1;
    //radius_out[n]= NumericTraits<SizeType>::One();
    }

  //Create neigh iterator       
  ConstNeighborhoodIterator<InputImageType> inNeigIt(radius_in, inputPtr, 
                                                        inputPtr->GetRequestedRegion());
  NeighborhoodIterator<OutputImageType> outNeigIt(radius_out, outputPtr, 
                                                        outputPtr->GetRequestedRegion());
  PixelType val,val0,val1,val0_new,val1_new,diff;
  PixelType norm;
  bool sign,neigh_sign;
 
  PixelType grad0[ImageDimension];
  PixelType grad1[ImageDimension];
  PixelType grad[ImageDimension];
  
  PixelType alpha0 = 0.5;  //Interpolation factor
  PixelType alpha1 = 0.5;  //Interpolation factor
  const typename InputImageType::SpacingType& vs = inputPtr->GetSpacing();
  double vs_2[ImageDimension];
  
  for(n = 0; n<ImageDimension ; n++)
     {
     vs_2[n]=2*vs[n];
     }
  //Get Stride information to move across dimension
  ::size_t stride[ImageDimension];
  unsigned int center;
  

  for (n=0 ; n<ImageDimension ; n++) 
     {
     stride[n]=inNeigIt.GetStride(n);
     }
  center = inNeigIt.Size() / 2;  

  for ( ; bandIt != bandEnd ; bandIt++)
    {
     inNeigIt.SetLocation(bandIt->m_Index);
     outNeigIt.SetLocation(bandIt->m_Index);
     
     val0 = inNeigIt.GetPixel(center)-m_LevelSetValue;
     sign = (val0>0);
     
     //Compute gradient at val0
     for (ng=0;ng<ImageDimension;ng++) 
        {
        grad0[ng]=inNeigIt.GetNext(ng,1)-inNeigIt.GetPrevious(ng,1);
        } 
     //Compute gradient at val0
     for (ng=0;ng<ImageDimension;ng++) 
        {
        grad0[ng]=inNeigIt.GetNext(ng,1)-inNeigIt.GetPrevious(ng,1);
        } 
     
     for (n=0;n<ImageDimension;n++)
        {
       
        val1 = inNeigIt.GetPixel(center+stride[n])-m_LevelSetValue;
       
        neigh_sign = (val1>0);
       
        if(sign != neigh_sign)
           {
           for (ng=0;ng<ImageDimension;ng++)
              {
              grad1[ng]=inNeigIt.GetPixel(center+stride[n]+stride[ng]) -
                       inNeigIt.GetPixel(center+stride[n]-stride[ng]);  
              }
           if(sign)
             diff = val0-val1;
           else
             diff = val1-val0;  
          
           if (diff < NumericTraits<PixelType>::min())
             {
             //do something: printf, or thorw exception.??
             continue;
             }
          //Interpolate values
          norm = NumericTraits<PixelType>::Zero;
          for (ng=0;ng<ImageDimension;ng++)
             {
             grad[ng] = (grad0[ng]*alpha0 + grad1[ng]*alpha1)/vs_2[ng];
             norm += grad[ng]*grad[ng];
             }
          norm = vcl_sqrt((float)norm);
          
          if (norm > NumericTraits<PixelType>::min())
            {
            val = vcl_fabs((float)grad[n])*vs[n]/norm/diff;
            
            val0_new = val0*val;
            val1_new = val1*val;    
           
            if(fabs((float)val0_new) < vcl_fabs((float)outNeigIt.GetNext(n,0)))
              outNeigIt.SetNext(n,0,static_cast<PixelType>(val0_new) );
            if(fabs((float)val1_new) < vcl_fabs((float)outNeigIt.GetNext(n,1)))
              outNeigIt.SetNext(n,1,static_cast<PixelType>(val1_new) );
            }
          else
            {
             itkExceptionMacro(<<"Gradient norm is lower than pixel precision");
            }
         } // end if (sign != sign_neigh)
       } //end for n       
   } //Band iteratior
}

 
} // namespace itk

#endif
