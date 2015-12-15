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
#ifndef itkDCMTKTransformIO_h
#define itkDCMTKTransformIO_h

#include "IOTransformDCMTKExport.h"

#include "itkTransformIOBase.h"

namespace itk
{

/** \class DCMTKTransformIO
 *
 *  \brief Read transforms in DICOM format.
 *
 *  \todo Detailed description.
 *
 * \ingroup IOTransformDCMTK
 */
template <typename TInternalComputationValueType>
class IOTransformDCMTK_EXPORT DCMTKTransformIO : public TransformIOBaseTemplate<TInternalComputationValueType>
{
public:
  typedef DCMTKTransformIO                                       Self;
  typedef TransformIOBaseTemplate<TInternalComputationValueType> Superclass;
  typedef SmartPointer<Self>                                     Pointer;

  typedef typename Superclass::TransformType          TransformType;
  typedef typename Superclass::TransformListType      TransformListType;
  typedef typename Superclass::ConstTransformListType ConstTransformListType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(DCMTKTransformIO, TransformIOBaseTemplate);

  itkNewMacro(Self);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool
  CanReadFile(const char *);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool
  CanWriteFile(const char *);

  /** Reads the data from disk into the memory buffer provided. */
  virtual void
  Read();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. The buffer is cast to a
   * pointer to the beginning of the image data. */
  virtual void
  Write();

  /** Set/Get the desired Frame of Reference UID, tag 0020|0052, for the
   * transform to be extracted. */
  itkSetStringMacro(FrameOfReferenceUID);
  itkGetStringMacro(FrameOfReferenceUID);

protected:
  DCMTKTransformIO();
  virtual ~DCMTKTransformIO();

private:
  DCMTKTransformIO(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  std::string m_FrameOfReferenceUID;
};

} // end namespace itk

// Note: Explicit instantiation is done in itkDCMTKTransformIOInstantiation.cxx

#endif // itkDCMTKTransformIO_h
