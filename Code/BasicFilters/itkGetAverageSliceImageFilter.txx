/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGetAverageSliceImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGetAverageSliceImageFilter_txx
#define _itkGetAverageSliceImageFilter_txx




namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage >
GetAverageSliceImageFilter<TInputImage,TOutputImage >
::GetAverageSliceImageFilter()
{
  this->SetNumberOfRequiredInputs( 1 );
  m_Direction = 0;
  m_AveragedOutDimension=-1;
}


/**
 * GenerateData Performs the reflection
 */
template <class TInputImage, class TOutputImage >
void
GetAverageSliceImageFilter<TInputImage,TOutputImage>
::GenerateData( void )
{


  typename Superclass::InputImageConstPointer  inputPtr = this->GetInput();
  typedef itk::ImageRegionIterator<TOutputImage> outputIterType;
  typedef itk::ImageRegionConstIterator<TInputImage> inputIterType;
  typedef itk::Image<double, 2> double2DimageType;
  typedef itk::Image<unsigned char, 2> char2DimageType;
  typedef itk::RescaleIntensityImageFilter<double2DimageType, char2DimageType> doubleToCharRescalerType;

  TOutputImage::Pointer outputImage=TOutputImage::New();
  TOutputImage::RegionType::SizeType outputSize;
  TOutputImage::RegionType::IndexType outputIndex;
  TOutputImage::RegionType outputRegion;

  double outputOrigin[TOutputImage::ImageDimension], outputSpacing[TOutputImage::ImageDimension];
//  const double *outputOrigin, *outputSpacing;
  TInputImage::RegionType::SizeType inputSize;
  TInputImage::RegionType::SizeType requestedSize;
  TInputImage::RegionType::IndexType requestedIndex;
  TInputImage::RegionType requestedRegion;
  itk::PNGImageIO::Pointer PNGio;
    PNGio= itk::PNGImageIO::New();
  doubleToCharRescalerType::Pointer doubleToCharRescaler=doubleToCharRescalerType::New();
  itk::ImageFileWriter<TOutputImage>::Pointer writer=itk::ImageFileWriter<TOutputImage>::New();
//  double inputSpacing[TInputImage::ImageDimension], inputOrigin[TInputImage::ImageDimension];
  const double *inputSpacing, *inputOrigin;
  int j=0;

  if(m_AveragedOutDimension<0 || m_AveragedOutDimension>=TInputImage::ImageDimension)
  {
    fprintf(stderr, "\ngetAveragedSliceImageFilter: wrong Dimension to Average Out. Quitting!\n");
    exit(2);
  }
  
  doubleToCharRescaler->SetOutputMaximum(255);
  doubleToCharRescaler->SetOutputMinimum(0);
  
  writer->SetImageIO(PNGio);

  inputSize=inputPtr->GetLargestPossibleRegion().GetSize();
  inputOrigin=inputPtr->GetOrigin();
  inputSpacing=inputPtr->GetSpacing();
  for(int i=0; i<TInputImage::ImageDimension; i++)
  {  
    if(i!=m_AveragedOutDimension)
    {
      requestedSize[i]=inputSize[i];
      requestedIndex[i]=0;
      outputSize[i]=inputSize[i];
      outputOrigin[i]=inputOrigin[i];
    }
    else 
    {
      requestedSize[i]=1;
      requestedIndex[i]=1;
      outputSize[i]=1;
//      outputIndex[i]=1;
      outputOrigin[i]=0;
    }
    outputIndex[i]=0;
    outputSpacing[i]=inputSpacing[i];
  }
  
  
  requestedRegion.SetSize(requestedSize);
//  requestedRegion->SetIndex(requestedIndex);
  outputRegion.SetSize(outputSize);
  outputRegion.SetIndex(outputIndex);
  outputImage->SetRegions(outputRegion);
  outputImage->SetOrigin(outputOrigin);
  outputImage->SetSpacing(outputSpacing);
  outputImage->Allocate();
  outputIterType outputIter(outputImage, outputImage->GetLargestPossibleRegion());
  outputIter.GoToBegin();
  while(!outputIter.IsAtEnd())
  {
    outputIter.Set(0);
    ++outputIter;
  }

  outputIter.GoToBegin();
  for(int z=0; z<inputSize[m_AveragedOutDimension]; z++)
  {
    requestedIndex[m_AveragedOutDimension]=z;
    requestedRegion.SetIndex(requestedIndex);
    inputIterType inputIter(inputPtr, requestedRegion);
    inputIter.GoToBegin();
    while(!inputIter.IsAtEnd())
    {
      outputIter.GoToBegin();
      while(!outputIter.IsAtEnd())
      {
          outputIter.Set(inputIter.Get()+outputIter.Get());
          ++inputIter;
          ++outputIter;
      }
    }
  }

  outputIter.GoToBegin();
  while(!outputIter.IsAtEnd())
  {
    outputIter.Set(outputIter.Get()/inputSize[m_AveragedOutDimension]);
    ++outputIter;
  }

/*  writer->SetFileName("c:\tmp\average.png");
  writer->SetInput(outputImage);
  writer->Write(); */
//  printVolumeTo(outputImage, 2-m_AveragedOutDimension, "c:\\tmp", "average0");
  this->GraftOutput(outputImage);
/*  size=inputPtr->GetLargestPossibleRegion().GetSize();
  outputImage->SetRegions(inputPtr->GetLargestPossibleRegion());
  outputImage->SetOrigin(inputPtr->GetOrigin());
  outputImage->SetSpacing(inputPtr->GetSpacing());
  outputImage->Allocate();

  outputIterType outputIter(outputImage, outputImage->GetLargestPossibleRegion());
  outputIter.GoToBegin();

  sliceSize[0]=size[0];
  sliceSize[1]=size[1];
  sliceRegion.SetSize(sliceSize);

  extractionRegionSize[0]=size[0];
  extractionRegionSize[1]=size[1];
  extractionRegionSize[2]=0;

  extractionRegion.SetSize(extractionRegionSize);
  extractionRegionIndex[0]=extractionRegionIndex[1]=0;

  edgeDetector->SetThreshold(edgeDetectorThreshold);
  edgeDetector->SetOutsideValue(edgeDetectorOutsideValue);
  edgeDetector->SetVariance(edgeDetectorVariance);
  edgeDetector->SetMaximumError(edgeDetectorMaximumError);

  extractor->SetInput(inputPtr);
  edgeDetector->SetInput(extractor->GetOutput());


  for(int z=0; z<size[2]; z++)
  {
    extractionRegionIndex[2]=z;
    extractionRegion.SetIndex(extractionRegionIndex);
    extractor->SetExtractionRegion(extractionRegion);
    edgeDetector->Update();
    float2Dimage=edgeDetector->GetOutput();
    float2Dimage->SetRegions(sliceRegion);
    sliceIterType sliceIter(float2Dimage, float2Dimage->GetLargestPossibleRegion());
    sliceIter.GoToBegin();
    while(!sliceIter.IsAtEnd())
    {
      outputIter.Set(sliceIter.Value());
      ++outputIter;
      ++sliceIter;
    }
  }
this->GraftOutput(outputImage);
*/

}

template <class TInputImage, class TOutputImage >
void
GetAverageSliceImageFilter<TInputImage,TOutputImage>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Direction: " << m_Direction << std::endl;
}



//} // end namespace itk


} // end namespace itk


#endif
