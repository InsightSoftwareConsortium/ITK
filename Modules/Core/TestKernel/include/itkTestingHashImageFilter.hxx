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
#ifndef itkTestingHashImageFilter_hxx
#define itkTestingHashImageFilter_hxx

#include "itkTestingHashImageFilter.h"
#include "itkByteSwapper.h"

#include "itksys/MD5.h"

namespace itk
{
namespace Testing
{

//
// Constructor
//
template<typename TImageType>
HashImageFilter<TImageType>::HashImageFilter() :
  m_HashFunction(MD5)
{
  // create data object
  this->ProcessObject::SetNthOutput( 1, this->MakeOutput(1).GetPointer() );

  this->InPlaceOn();
}

//
// MakeOutput
//
template<typename TImageType>
typename HashImageFilter<TImageType>::DataObjectPointer
HashImageFilter<TImageType>::MakeOutput(DataObjectPointerArraySizeType idx)
{
  if ( idx == 1 )
    {
    return HashObjectType::New().GetPointer();
    }
  return Superclass::MakeOutput(idx);
}

//
// AfterThreadedGenerateData
//
template<typename TImageType>
void
HashImageFilter<TImageType>::AfterThreadedGenerateData()
{
  // NOTE: We have choose our super class to be the
  // CastImageFilter. The filter is derived from the
  // InPlaceFilter. The CastImageFilter will copy its input, in the
  // best fashion based on the InPlace setting. We want the
  // behavior. This methods is called after the caster has done it's
  // work.

  Superclass::AfterThreadedGenerateData();

  typedef TImageType                                   ImageType;
  typedef typename ImageType::PixelType                PixelType;
  typedef typename NumericTraits<PixelType>::ValueType ValueType;
  typedef itk::ByteSwapper<ValueType>                  Swapper;

  itksysMD5 *md5 = itksysMD5_New();
  itksysMD5_Initialize( md5 );

  try
    {
    typename ImageType::ConstPointer input = this->GetInput();


    // make a good guess about the number of components in each pixel
    size_t numberOfComponent =   sizeof(PixelType) / sizeof(ValueType );

    if ( strcmp(input->GetNameOfClass(), "VectorImage") == 0 )
      {
      // spacial case for VectorImages
      numberOfComponent = ImageType::AccessorFunctorType::GetVectorLength(input);
      }
    else if ( sizeof(PixelType) % sizeof(ValueType) != 0 )
      {
      itkExceptionMacro("Unsupported data type for hashing!");
      }

    // we feel bad about accessing the data this way
    ValueType *buffer = static_cast<ValueType*>( const_cast<void*>((const void *)input->GetBufferPointer()) );

    typename ImageType::RegionType largestRegion = input->GetBufferedRegion();
    const size_t numberOfValues = largestRegion.GetNumberOfPixels()*numberOfComponent;


    // Possible byte swap so we always calculate on little endian data
    if ( Swapper::SystemIsBigEndian() )
      {
      Swapper::SwapRangeFromSystemToLittleEndian ( buffer, static_cast<typename Swapper::BufferSizeType> ( numberOfValues ) );
      }

    itksysMD5_Append( md5, (unsigned char*)buffer, static_cast<int>( numberOfValues*sizeof(ValueType) ) );

    if ( Swapper::SystemIsBigEndian() )
      {
      Swapper::SwapRangeFromSystemToLittleEndian ( buffer, static_cast<typename Swapper::BufferSizeType> ( numberOfValues ) );
      }

    ////////
    // NOTE: THIS IS NOT A ITK_NULLPTR TERMINATED STRING!!!
    ////////
    const size_t DigestSize = 32u;
    char Digest[DigestSize];

    itksysMD5_FinalizeHex( md5, Digest );

    this->GetHashOutput()->Set( std::string(Digest, DigestSize) );
    }
  catch (... )
    {
    // free all resources when an exception occours
    itksysMD5_Delete( md5 );
    throw;
    }

  // free resources
  itksysMD5_Delete( md5 );
}


//
// EnlargeOutputRequestedRegion
//
template<typename TImageType>
void
HashImageFilter<TImageType>::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);

  // set the output region to the largest and let the pipeline
  // propagate the requested region to the input
  data->SetRequestedRegionToLargestPossibleRegion();
}

//
// PrintSelf
//
template<typename TImageType>
void
HashImageFilter<TImageType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "HashFunction: " << m_HashFunction << std::endl;
}

} // end namespace Testing
} // end namespace itk

#endif // itkTestingHashImageFilter_hxx
