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
#ifndef itkPathAndImageToPathFilter_h
#define itkPathAndImageToPathFilter_h

#include "itkPathToPathFilter.h"

namespace itk
{
/** \class PathAndImageToPathFilter
 * \brief Base class for filters that take both a path and an image as input and produce a path as output.
 *
 * This class is the base class for filters that take both a path and an image
 * as input and produce a path as output.  Specifically, this class defines the
 * methods SetPathInput() and SetImageInput().  (It also establishes the
 * precedent of having path inputs precede image inputs for functions producing
 * paths as outputs, according to the underlying DataObject implementation.)
 *
 * \ingroup PathFilters
 * \ingroup ITKPath
 */
template< typename TInputPath, typename TInputImage, typename TOutputPath >
class ITK_TEMPLATE_EXPORT PathAndImageToPathFilter:
  public PathToPathFilter< TInputPath, TOutputPath >
{
public:
  /** Standard class typedefs. */
  typedef PathAndImageToPathFilter                    Self;
  typedef PathToPathFilter< TInputPath, TOutputPath > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PathAndImageToPathFilter, PathToPathFilter);

  /** Some convenient typedefs. */
  typedef          TInputPath                   InputPathType;
  typedef typename InputPathType::Pointer       InputPathPointer;
  typedef typename InputPathType::ConstPointer  InputPathConstPointer;
  typedef typename InputPathType::InputType     InputPathInputType;
  typedef typename InputPathType::OutputType    InputPathOutputType;
  typedef typename InputPathType::IndexType     InputPathIndexType;
  typedef typename InputPathType::OffsetType    InputPathOffsetType;
  typedef          TInputImage                  InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef          TOutputPath                  OutputPathType;
  typedef typename OutputPathType::Pointer      OutputPathPointer;
  typedef typename OutputPathType::InputType    OutputPathInputType;
  typedef typename OutputPathType::OutputType   OutputPathOutputType;
  typedef typename OutputPathType::IndexType    OutputPathIndexType;
  typedef typename OutputPathType::OffsetType   OutputPathOffsetType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Set/Get the path input of this process object. */
  virtual void SetPathInput(const TInputPath *path);

  const InputPathType * GetPathInput();

  /** Set/Get the image input of this process object. */
  virtual void SetImageInput(const TInputImage *image);

  const InputImageType * GetImageInput();

protected:
  PathAndImageToPathFilter();
  virtual ~PathAndImageToPathFilter() ITK_OVERRIDE {}

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** What is the input requested region that is required to produce the output
   * requested region?  Up till and including now, the base assumption is that
   * the largest possible region will be requested of the input.  If this method
   * is overridden, the new method should call its superclass' implementation as
   * its first step.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PathAndImageToPathFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPathAndImageToPathFilter.hxx"
#endif

#endif
