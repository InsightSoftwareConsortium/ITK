/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkTestingHashImageFilter_h
#define itkTestingHashImageFilter_h


#include "itkSimpleDataObjectDecorator.h"
#include "itkInPlaceImageFilter.h"

namespace itk
{
namespace Testing
{
/**\class HashImageFilterEnums
 * \brief Enum classes for HashImageFilter
 * \ingroup ITKTestKernel
 * */
class HashImageFilterEnums
{
public:
  /**\class HashFunction
   * \ingroup ITKTestKernel
   * Describes the hash function
   */
  enum class HashFunction : uint8_t
  {
    MD5
  };
};
// Define how to print enumeration
extern std::ostream &
operator<<(std::ostream & out, HashImageFilterEnums::HashFunction value);

/**
 *\class HashImageFilter
 * \brief Generates a md5 hash string from an image.
 *
 * \note This class utlizes low level buffer pointer access, to work
 * with itk::Image and itk::VectorImage. It is modelled after the access
 * an ImageFileWriter provides to an ImageIO.
 *
 * \warning The output is not very useful. This filter is derived from
 * InPlace to prevent a needless copy. If inplace is turned off then
 * the output image bulk data will be allocated, and not set to
 * anything meaningful.
 *
 * \ingroup ITKTestKernel
 */
template <typename TImageType>
class ITK_TEMPLATE_EXPORT HashImageFilter : public InPlaceImageFilter<TImageType, TImageType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HashImageFilter);

  /** Standard Self type alias */
  using Self = HashImageFilter;
  using Superclass = InPlaceImageFilter<TImageType, TImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using RegionType = typename TImageType::RegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(HashImageFilter, CastImageFilter);

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Type of DataObjects used for scalar outputs */
  using HashObjectType = SimpleDataObjectDecorator<std::string>;

  /** Get the computed Hash values */
  std::string
  GetHash() const
  {
    return this->GetHashOutput()->Get();
  }
  HashObjectType *
  GetHashOutput()
  {
    return static_cast<HashObjectType *>(this->ProcessObject::GetOutput(1));
  }
  const HashObjectType *
  GetHashOutput() const
  {
    return static_cast<const HashObjectType *>(this->ProcessObject::GetOutput(1));
  }

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

  using HashFunctionEnum = HashImageFilterEnums::HashFunction;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr HashFunctionEnum MD5 = HashFunctionEnum::MD5;
#endif

  /** Set/Get The hash function type. Currently only MD5 is supported
   * and this value is ignores.
   */
  itkSetEnumMacro(HashFunction, HashFunctionEnum);
  itkGetMacro(HashFunction, HashFunctionEnum);

protected:
  HashImageFilter();

  // virtual ~HashImageFilter(); // implementation not needed

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** All work is done by AfterThreadedGenerateData(). */
  void
  ThreadedGenerateData(const typename Superclass::OutputImageRegionType &, ThreadIdType) override
  {}
  void
  DynamicThreadedGenerateData(const typename Superclass::OutputImageRegionType &) override
  {}

  // See superclass for doxygen documentation
  //
  // This method is to do work after the superclass potential threaded
  // copy.
  void
  AfterThreadedGenerateData() override;

  // See superclass for doxygen documentation
  //
  // Override since the filter produces all of its output
  void
  EnlargeOutputRequestedRegion(DataObject * data) override;

private:
  HashFunctionEnum m_HashFunction{ HashFunctionEnum::MD5 };
};

} // end namespace Testing
} // end namespace itk


#include "itkTestingHashImageFilter.hxx"

#endif // itkTestingHashImageFilter_h
