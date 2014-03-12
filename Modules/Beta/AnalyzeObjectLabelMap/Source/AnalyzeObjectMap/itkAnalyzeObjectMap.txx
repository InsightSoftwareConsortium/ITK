/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    $RCSfile: itkNiftiImageIO.cxx,v $
Language:  C++
Date:      $Date: 2007/07/27 18:00:56 $
Version:   $Revision: 1.37 $

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//Acknowdlegment: Biomedical Imaging Resource
//         Mayo Foundation
//For lending us documention about the layout of an object map and the entries that correspond with the object map
#ifndef __itkAnalyzeObjectMap_txx
#define __itkAnalyzeObjectMap_txx


#include "itkAnalyzeObjectMap.h"
#include "itkImageRegionIterator.h"
namespace itk
{
template<class TImage, class TRGBImage>
AnalyzeObjectMap<TImage, TRGBImage>
::AnalyzeObjectMap(): m_NumberOfObjects(1)
{
    //Create an object map of size 1,1,1 and have the pixles be 0.  Also, create one
    //object entry just like Analyze does with the name "Original", this entry
    //is usually the background.
  this->m_AnaylzeObjectEntryArray.resize(1);
  this->m_AnaylzeObjectEntryArray[0] = itk::AnalyzeObjectEntry::New();
    this->m_AnaylzeObjectEntryArray[0]->SetName("Original");
    typename ImageType::SizeType size;
    typename ImageType::IndexType orgin;
    for(unsigned int i = 0; i < TImage::GetImageDimension(); i++)
    {
      size[i] = 1;
      orgin[i] = 0;
    }
    typename ImageType::RegionType region;
    region.SetSize(size);
    region.SetIndex(orgin);
    this->SetRegions(region);
    this->Allocate();
    this->FillBuffer(0);
}

  template<class TImage, class TRGBImage>
AnalyzeObjectMap<TImage, TRGBImage>
::~AnalyzeObjectMap( void )
{
}
template<class TImage, class TRGBImage>
  AnalyzeObjectEntryArrayType *AnalyzeObjectMap<TImage, TRGBImage>::GetAnalyzeObjectEntryArrayPointer()
  {
  return &(this->m_AnaylzeObjectEntryArray);
}

  //This function will have the user pick which entry they want to be placed into
  //a new object map that will be returned.  The function will also go through the image
  //and change the values so that there is either 0 or 1.  1 corresponds to the entry
  //the user specified and 0 is the background.
template<class TImage, class TRGBImage>
  typename itk::AnalyzeObjectMap<TImage>::Pointer AnalyzeObjectMap<TImage, TRGBImage>::PickOneEntry(const int numberOfEntry)
  {
    typename itk::AnalyzeObjectMap<TImage>::Pointer ObjectMapNew = itk::AnalyzeObjectMap<TImage>::New();
    ObjectMapNew->SetRegions(this->GetLargestPossibleRegion());
    ObjectMapNew->Allocate();
    ObjectMapNew->AddAnalyzeObjectEntry(this->GetObjectEntry(numberOfEntry)->GetName());
    ObjectMapNew->GetObjectEntry(1)->Copy(this->m_AnaylzeObjectEntryArray[numberOfEntry]);
    typename itk::ThresholdImageFilter<ImageType>::Pointer changeOldObjectMap = itk::ThresholdImageFilter<ImageType>::New();
    
    changeOldObjectMap->SetInput(this);
    changeOldObjectMap->SetOutsideValue(0);
    changeOldObjectMap->ThresholdOutside(numberOfEntry,numberOfEntry);
    changeOldObjectMap->Update();
    changeOldObjectMap->SetInput(changeOldObjectMap->GetOutput());
    changeOldObjectMap->SetOutsideValue(1);
    changeOldObjectMap->ThresholdAbove(numberOfEntry-1);
    typename ImageType::Pointer newestObjectMapImage = changeOldObjectMap->GetOutput();
    changeOldObjectMap->Update();
    ObjectMapNew->SetPixelContainer(newestObjectMapImage->GetPixelContainer());
    ObjectMapNew->PlaceObjectMapEntriesIntoMetaData();

   return ObjectMapNew;
}

  //This function will convert an object map into an unsigned char RGB image.
template<class TImage, class TRGBImage>
  typename TRGBImage::Pointer AnalyzeObjectMap<TImage, TRGBImage>::ObjectMapToRGBImage()
  {
  typename TRGBImage::Pointer RGBImage = TRGBImage::New();
    RGBImage->SetRegions(this->GetLargestPossibleRegion());
    RGBImage->Allocate();
    itk::ImageRegionIterator<TRGBImage> RGBIterator(RGBImage, this->GetLargestPossibleRegion());
    itk::ImageRegionIterator<ImageType> ObjectIterator(this, this->GetLargestPossibleRegion());

    /*std::ofstream myfile;
    myfile.open("RGBImageVoxels2.txt");*/
    for(ObjectIterator.Begin(), RGBIterator.Begin(); !ObjectIterator.IsAtEnd(); ++ObjectIterator, ++RGBIterator)
    {
    typename itk::ImageRegionIterator<TRGBImage>::PixelType setColors;
//      typename RGBImage->ImageType setColors;
      setColors.SetBlue(this->m_AnaylzeObjectEntryArray[ObjectIterator.Get()]->GetEndBlue());
      setColors.SetGreen(this->m_AnaylzeObjectEntryArray[ObjectIterator.Get()]->GetEndGreen());
      setColors.SetRed(this->m_AnaylzeObjectEntryArray[ObjectIterator.Get()]->GetEndRed());

      RGBIterator.Set(setColors);
      //myfile<<RGBIterator.Get()<<std::endl;
    }
    //myfile.close();
    return RGBImage;
}

  //This function will take in an unsigned char of dimension size 3 and go through it and figure out the value the user wants picked out.  The user will also have to
  //specify what they want the new entry's name to be.  The user can also specify what RGB values they want but if they are not speficied the default values
  //are 0.
template<class TImage, class TRGBImage>
  void AnalyzeObjectMap<TImage, TRGBImage>::AddObjectEntryBasedOnImagePixel(ImageType *Image, const int value, const std::string ObjectName, const int Red,const int Green,const int Blue)
  {
    itk::ImageRegion<TImage::ImageDimension> ObjectMapRegion = this->GetLargestPossibleRegion();
    itk::ImageRegion<TImage::ImageDimension> ImageRegion = Image->GetLargestPossibleRegion();
    if(  ImageRegion != ObjectMapRegion)
    {
      this->SetRegions(Image->GetLargestPossibleRegion());
    this->Allocate();
    this->FillBuffer(0);
    }
    itk::ImageRegionIterator<ImageType > indexImage(Image, Image->GetLargestPossibleRegion());

    itk::ImageRegionIterator<ImageType > indexObjectMap(this,Image->GetLargestPossibleRegion());
    
    this->AddAnalyzeObjectEntry(ObjectName);
    unsigned int i = this->GetNumberOfObjects()-1;
    this->m_AnaylzeObjectEntryArray[i]->SetEndRed(Red);
    this->m_AnaylzeObjectEntryArray[i]->SetEndGreen(Green);
    this->m_AnaylzeObjectEntryArray[i]->SetEndBlue(Blue);
    for(indexImage.Begin(), indexObjectMap.Begin();!indexImage.IsAtEnd() && !indexObjectMap.IsAtEnd(); ++indexImage, ++indexObjectMap)
    {
    if(indexImage.Get() == value)
      {
        indexObjectMap.Set(i);
      }
    }
}
  

  /*NOTE: This function will add an object entry to the end of the vector.  However, you will still have to fill in the values that you would like stored.
  TODO: Rastor through the image to place the value at the specifed locations.*/
template<class TImage, class TRGBImage>
  void AnalyzeObjectMap<TImage, TRGBImage>::AddAnalyzeObjectEntry(const std::string ObjectName)
  {
    this->m_AnaylzeObjectEntryArray.insert(this->m_AnaylzeObjectEntryArray.end(), itk::AnalyzeObjectEntry::New());
    this->SetNumberOfObjects(this->GetNumberOfObjects()+1);
    this->m_AnaylzeObjectEntryArray[this->GetNumberOfObjects()-1]->SetName(ObjectName);
    this->PlaceObjectMapEntriesIntoMetaData();
}

  /*NOTE: This function will move all object entry's so that the vector stays in the smallest order starting from 0.*/
template<class TImage, class TRGBImage>
  void AnalyzeObjectMap<TImage, TRGBImage>::DeleteAnalyzeObjectEntry(const std::string ObjectName)
  {
  int i = this->FindObjectEntry(ObjectName);
  if(i == -1)
    {
      return;
    }
  for(int j = i; j < this->GetNumberOfObjects()-1; j++)
    {
    this->m_AnaylzeObjectEntryArray[j] = this->m_AnaylzeObjectEntryArray[j+1];
    }
    this->m_AnaylzeObjectEntryArray.erase(this->m_AnaylzeObjectEntryArray.end()-1);
  this->SetNumberOfObjects(this->GetNumberOfObjects()-1);
  //this->m_AnaylzeObjectEntryArray.resize(this->GetNumberOfObjects());
  itk::ImageRegionIterator<ImageType > indexIt(this,this->GetLargestPossibleRegion());
    for(indexIt.Begin();!indexIt.IsAtEnd(); ++indexIt)
    {
    if(indexIt.Get() == i)
      {
        indexIt.Set(0);
      }
    else if(indexIt.Get()>i)
      {
      indexIt.Set(indexIt.Get()-1);
      }
    }
  this->PlaceObjectMapEntriesIntoMetaData();
}

  //This function will go through the entries looking for the specfic name.  If no name was found then the function
  //will return -1.  So, if you use this, then make sure you check to see if -1 was returned.
template<class TImage, class TRGBImage>
  int AnalyzeObjectMap<TImage, TRGBImage>::FindObjectEntry(const std::string ObjectName)
  {
  for(int i=0; i < this->GetNumberOfObjects(); i++)
    {
    if(!ObjectName.compare(this->m_AnaylzeObjectEntryArray.at(i)->GetName()))
      {
      return i;
      }
    }
    //If not found return -1
  return -1;
}

template<class TImage, class TRGBImage>
  void AnalyzeObjectMap<TImage, TRGBImage>::PlaceObjectMapEntriesIntoMetaData()
  {
    itk::AnalyzeObjectEntryArrayType *my_reference=this->GetAnalyzeObjectEntryArrayPointer();

    MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
    itk::EncapsulateMetaData<itk::AnalyzeObjectEntryArrayType>(thisDic,ANALYZE_OBJECT_LABEL_MAP_ENTRY_ARRAY,*my_reference);
}

template<class TImage, class TRGBImage>
  AnalyzeObjectEntry::Pointer AnalyzeObjectMap<TImage, TRGBImage>::GetObjectEntry( const int index )
  {
  return this->m_AnaylzeObjectEntryArray.at(index);
}

template<class TImage, class TRGBImage>
void
AnalyzeObjectMap<TImage, TRGBImage>
::ImageToObjectMap(TImage *image)
{
  this->SetRegions(image->GetLargestPossibleRegion());
  this->Allocate();
  this->SetPixelContainer(image->GetPixelContainer());
  itk::AnalyzeObjectEntryArrayType *my_reference = this->GetAnalyzeObjectEntryArrayPointer();
  if(itk::ExposeMetaData<itk::AnalyzeObjectEntryArrayType>(image->GetMetaDataDictionary(),ANALYZE_OBJECT_LABEL_MAP_ENTRY_ARRAY, *my_reference))
  {
    this->SetNumberOfObjects(this->GetAnalyzeObjectEntryArrayPointer()->size());
    }
  this->PlaceObjectMapEntriesIntoMetaData();
}

template<class TImage, class TRGBImage>
  void AnalyzeObjectMap<TImage, TRGBImage>::PrintSelf(std::ostream& os, Indent indent) const
  {
  Superclass::PrintSelf(os, indent);
}

}
#endif
