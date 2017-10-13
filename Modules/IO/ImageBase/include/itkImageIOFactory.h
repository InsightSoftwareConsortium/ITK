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
#ifndef itkImageIOFactory_h
#define itkImageIOFactory_h
#include "ITKIOImageBaseExport.h"

#include "itkObject.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class ImageIOFactory
 * \brief Create instances of ImageIO objects using an object factory.
 * \ingroup ITKIOImageBase
 */
class ITKIOImageBase_EXPORT ImageIOFactory:public Object
{
public:
  /** Standard class typedefs. */
  typedef ImageIOFactory             Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class Methods used to interface with the registered factories */

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageIOFactory, Object);

  /** Convenient typedefs. */
  typedef::itk::ImageIOBase::Pointer ImageIOBasePointer;

  /** Mode in which the files is intended to be used */
  typedef enum { ReadMode, WriteMode } FileModeType;

  /** Create the appropriate ImageIO depending on the particulars of the file.
    */
  static ImageIOBasePointer CreateImageIO(const char *path, FileModeType mode);

protected:
  ImageIOFactory();
  ~ImageIOFactory() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageIOFactory);
};
} // end namespace itk

#endif
