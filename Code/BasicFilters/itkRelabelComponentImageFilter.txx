/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRelabelComponentImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRelabelComponentImageFilter_txx
#define _itkRelabelComponentImageFilter_txx

#include "itkRelabelComponentImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itk_hash_map.h"
#include <map>

// anonymous namespace to hold a helper object
namespace
{
struct ObjectType
{
  unsigned long m_ObjectNumber;
  unsigned long m_SizeInPixels;
  float m_SizeInPhysicalUnits;
};


// put the function objects here for sorting in descending order
class SizeInPixelsComparator
{
public:
  bool operator()(const ObjectType &a, const ObjectType&b)
    {
      if (a.m_SizeInPixels > b.m_SizeInPixels)
        {
        return true;
        }
      else if (a.m_SizeInPixels < b.m_SizeInPixels)
        {
        return false;
        }
      // size in pixels are the same, sort base on size in physical units
      else if (a.m_SizeInPhysicalUnits > b.m_SizeInPhysicalUnits)
        {
        return true;
        }
      else if (a.m_SizeInPhysicalUnits < b.m_SizeInPhysicalUnits)
        {
        return false;
        }
      // size in pixels and physical units are the same, sort based on
      // original object number
      else if (a.m_ObjectNumber < b.m_ObjectNumber)
        {
        return true;
        }
      else
        {
        return false;
        }
    }
};

class SizeInPhysicalUnitsComparator
{
public:
  bool operator()(const ObjectType &a, const ObjectType&b)
    {
      if (a.m_SizeInPhysicalUnits > b.m_SizeInPhysicalUnits)
        {
        return true;
        }
      else if (a.m_SizeInPhysicalUnits < b.m_SizeInPhysicalUnits)
        {
        return false;
        }
      // size in pixels are the same, sort base on size in physical units
      else if (a.m_SizeInPixels > b.m_SizeInPixels)
        {
        return true;
        }
      else if (a.m_SizeInPixels < b.m_SizeInPixels)
        {
        return false;
        }
      // size in pixels and physical units are the same, sort based on
      // original object number
      else if (a.m_ObjectNumber < b.m_ObjectNumber)
        {
        return true;
        }
      else
        {
        return false;
        }
    }
};

}


namespace itk
{
template< class TInputImage, class TOutputImage >
void
RelabelComponentImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template< class TInputImage, class TOutputImage >
void
RelabelComponentImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  unsigned long i;
  
  // Use a map to keep track of the size of each object.  Object
  // number -> ObjectType (which has Object number and the two sizes)
  typedef itk::hash_map<unsigned long, ObjectType> MapType;
  MapType sizeMap;
  MapType::iterator mapIt;

  // Get the input and the output
  typename TInputImage::ConstPointer input = this->GetInput();
  typename TOutputImage::Pointer output = this->GetOutput();

  // Setup a progress reporter.  We have 2 stages to the algorithm so
  // use the total number of pixels accessed. We walk the entire input
  // in the first pass, then walk just the output requested region in
  // the second pass.
  ProgressReporter progress(this, 0,
                            input->GetRequestedRegion().GetNumberOfPixels() +
                            output->GetRequestedRegion().GetNumberOfPixels());


  // Calculate the size of pixel
  float physicalPixelSize = 1.0;
  for (i=0; i < TInputImage::ImageDimension; ++i)
    {
    physicalPixelSize *= input->GetSpacing()[i];
    }

  ObjectType initialSize;
  initialSize.m_SizeInPixels = 1;
  initialSize.m_SizeInPhysicalUnits = physicalPixelSize;

  // First pass: walk the entire input image and determine what
  // labels are used and the number of pixels used in each label.
  //

  // walk the input
  InputImagePixelType inputValue;
  ImageRegionConstIterator<InputImageType> it;
  it = ImageRegionConstIterator<InputImageType>(input,
                                                input->GetRequestedRegion());
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    // Get the input pixel value
    inputValue = it.Get();

    // if the input pixel is not the background
    if (inputValue != NumericTraits<InputPixelType>::Zero)
      {
      // Does this label already exist
      mapIt = sizeMap.find( inputValue );
      if ( mapIt == sizeMap.end() )
        {
        // label is not currently in the map
        initialSize.m_ObjectNumber = inputValue;
        sizeMap.insert( MapType::value_type( inputValue, initialSize ) );
        }
      else
        {
        // label is already in the map, update the values
        (*mapIt).second.m_SizeInPixels++;
        (*mapIt).second.m_SizeInPhysicalUnits += physicalPixelSize;
        }
      }

    // increment the iterators
    ++it;
    progress.CompletedPixel();
    }

  // Now we need to reorder the labels. Use the m_ObjectSortingOrder
  // to determine how to sort the objects. Define a map for converting
  // input labels to output labels.
  //
  typedef std::vector<ObjectType> VectorType;
  VectorType sizeVector;
  VectorType::iterator vit;

  typedef std::map<unsigned long, unsigned long> RelabelMapType;
  RelabelMapType relabelMap;

  // copy the original object map to a vector so we can sort it
  for (mapIt = sizeMap.begin(); mapIt != sizeMap.end(); ++mapIt)
    {
    sizeVector.push_back( (*mapIt).second );
    }

  // sort the objects and define the map to use to relabel the image
  if (m_ObjectSortingOrder == SortBySizeInPixels)
    {
    // sort objects by number of pixels
    std::sort(sizeVector.begin(), sizeVector.end(), SizeInPixelsComparator() );
    }
  else
    {
    // sort object by their size in physical units
    std::sort(sizeVector.begin(), sizeVector.end(),
              SizeInPhysicalUnitsComparator() );
    }

  // create a lookup table to map the input label to the output label.
  // cache the object sizes for later access by the user
  m_NumberOfObjects = sizeVector.size();
  m_SizeOfObjectsInPixels.clear();
  m_SizeOfObjectsInPixels.resize(m_NumberOfObjects);
  m_SizeOfObjectsInPhysicalUnits.clear();
  m_SizeOfObjectsInPhysicalUnits.resize(m_NumberOfObjects);
  for (i=0, vit = sizeVector.begin(); vit != sizeVector.end(); ++vit, ++i)
    {
    // map for input labels to output labels (Note we use i+1 in the
    // map since index 0 is the background)
    relabelMap.insert(RelabelMapType::value_type( (*vit).m_ObjectNumber, i+1));

    // cache object sizes for later access by the user
    m_SizeOfObjectsInPixels[i] = (*vit).m_SizeInPixels;
    m_SizeOfObjectsInPhysicalUnits[i] = (*vit).m_SizeInPhysicalUnits;
    }

  // Second pass: walk just the output requested region and relabel
  // the necessary pixels.
  //

  // Allocate the output and initialize to zeros
  this->AllocateOutputs();
  output->FillBuffer( NumericTraits<OutputPixelType>::Zero );

  // Remap the labels.  Note we only walk the region of the output
  // that was requested.  This may be a subset of the input image.
  OutputImagePixelType outputValue;
  ImageRegionIterator<OutputImageType> oit;
  oit = ImageRegionIterator<OutputImageType>(output,
                                             output->GetRequestedRegion());
  it = ImageRegionConstIterator<InputImageType>(input,
                                                output->GetRequestedRegion());

  it.GoToBegin();
  oit.GoToBegin();
  while ( !oit.IsAtEnd() )
    {
    inputValue = it.Get();
    if (inputValue != NumericTraits<InputPixelType>::Zero)
      {
      // lookup the mapped label
      outputValue = static_cast<OutputPixelType>(relabelMap[inputValue]); 
      oit.Set( outputValue );
      }

    // increment the iterators
    ++it;
    ++oit;
    progress.CompletedPixel();
    }
}


template< class TInputImage, class TOutputImage >
void
RelabelComponentImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  
  os << indent << "NumberOfObjects: " << m_NumberOfObjects << std::endl;

  std::vector<unsigned long>::const_iterator it;
  std::vector<float>::const_iterator fit;
  unsigned long i;
  
  for (i=0, it = m_SizeOfObjectsInPixels.begin(),
         fit = m_SizeOfObjectsInPhysicalUnits.begin();
       it != m_SizeOfObjectsInPixels.end(); ++it, ++fit, ++i)
    {
    std::cout << indent << "Object #" << i+1 << ": " << *it << " pixels, "
              << *fit << " physical units" << std::endl;
    }
}

} // end namespace itk

#endif
