/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPathAndImageToPathFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPathAndImageToPathFilter_h
#define __itkPathAndImageToPathFilter_h

#include "itkPathToPathFilter.h"

namespace itk
{
  
/** \class PathAndImageToPathFilter
 * \brief Base class for filters that take both a path and an image as input and produce a path as output.
 *
 * This class is the base class for filters that take both a path and an image
 * as input and produce a path as output.  Specifically, this class defines the
 * methods SetPathInput() and SetImageInput().  (It also establishes the
 * precedent of having path inputs preceed image inputs for functions producing
 * paths as outputs, according to the underlying DataObject implementation.)
 * 
 * \ingroup PathFilters
 */
template <class TInputPath, class TInputImage, class TOutputPath>
class ITK_EXPORT PathAndImageToPathFilter :
    public PathToPathFilter<TInputPath,TOutputPath> 
{
public:
  /** Standard class typedefs. */
  typedef PathAndImageToPathFilter  Self;
  typedef PathToPathFilter<TInputPath,TOutputPath>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(PathAndImageToPathFilter, PathToPathFilter);

  /** Some convenient typedefs. */
  typedef          TInputPath                     InputPathType;
  typedef typename InputPathType::Pointer         InputPathPointer;
  typedef typename InputPathType::ConstPointer    InputPathConstPointer;
  typedef typename InputPathType::InputType       InputPathInputType;
  typedef typename InputPathType::OutputType      InputPathOutputType;
  typedef typename InputPathType::IndexType       InputPathIndexType;
  typedef typename InputPathType::OffsetType      InputPathOffsetType;
  typedef          TInputImage                    InputImageType;
  typedef typename InputImageType::ConstPointer   InputImagePointer;
  typedef typename InputImageType::RegionType     InputImageRegionType; 
  typedef typename InputImageType::PixelType      InputImagePixelType; 
  typedef          TOutputPath                    OutputPathType;
  typedef typename OutputPathType::Pointer        OutputPathPointer;
  typedef typename OutputPathType::InputType      OutputPathInputType;
  typedef typename OutputPathType::OutputType     OutputPathOutputType;
  typedef typename OutputPathType::IndexType      OutputPathIndexType;
  typedef typename OutputPathType::OffsetType     OutputPathOffsetType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  
  /** Set/Get the path input of this process object. */
  virtual void SetPathInput( const TInputPath * path);
  const InputPathType * GetPathInput(void);

  /** Set/Get the image input of this process object. */
  virtual void SetImageInput( const TInputImage * image);
  const InputImageType * GetImageInput(void);

protected:
  PathAndImageToPathFilter();
  virtual ~PathAndImageToPathFilter() {};

  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** What is the input requested region that is required to produce the output
   * requested region?  Up till and including now, the base assumption is that
   * the largest possible region will be requested of the input.  If this method
   * is overridden, the new method should call its superclass' implementation as
   * its first step.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

private:
  PathAndImageToPathFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPathAndImageToPathFilter.txx"
#endif

#endif
