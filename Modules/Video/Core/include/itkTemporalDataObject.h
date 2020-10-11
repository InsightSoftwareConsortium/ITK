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
#ifndef itkTemporalDataObject_h
#define itkTemporalDataObject_h

#include "itkDataObject.h"
#include "itkRingBuffer.h"
#include "itkTemporalRegion.h"
#include "ITKVideoCoreExport.h"

namespace itk
{
/**\class TemporalDataObjectEnums
 * \brief Contains all enum classes used by TemporalUnit class.
 * \ingroup ITKVideoCore
 */
class TemporalDataObjectEnums
{
public:
  /**
   * \class TemporalUnit
   * \ingroup ITKVideoCore
   * \brief For defining the way in which to compare temporal regions.
   */
  enum class TemporalUnit : uint8_t
  {
    Frame,
    RealTime,
    FrameAndRealTime
  };
};
// Define how to print enumeration
extern ITKVideoCore_EXPORT std::ostream &
                           operator<<(std::ostream & out, TemporalDataObjectEnums::TemporalUnit value);

/**
 *\class TemporalDataObject
 * \brief DataObject subclass with knowledge of temporal region
 *
 * This class represents a data object that relies on temporal regions. It uses
 * an itk::RingBuffer to store DataObject pointers in sequential order. The
 * pointers in the ring buffer should correspond to the BufferedTemporalRegion.
 * The LargestPossibleTemporalRegion should indicate the maximum extent that
 * data object is logically capable of holding, and the RequestedTemporalRegion
 * is used in the pipeline to request that a certain temporal region be
 * buffered
 *
 * \ingroup ITKVideoCore
 */
class ITK_FORCE_EXPORT_MACRO(ITKVideoCore) TemporalDataObject : public DataObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TemporalDataObject);

  /** Standard class type aliases */
  using Self = TemporalDataObject;
  using Superclass = DataObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  using BufferType = RingBuffer<DataObject>;
  using TemporalRegionType = TemporalRegion;

  using TemporalUnitEnum = TemporalDataObjectEnums::TemporalUnit;
  using TemporalUnitType = TemporalUnitEnum;

#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr TemporalUnitEnum Frame = TemporalUnitEnum::Frame;
  static constexpr TemporalUnitEnum RealTime = TemporalUnitEnum::RealTime;
  static constexpr TemporalUnitEnum FrameAndRealTime = TemporalUnitEnum::FrameAndRealTime;
#endif

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TemporalDataObject, DataObject);

  /** Get the type of temporal units we care about (Defaults to Frame)*/
  virtual TemporalUnitType
  GetTemporalUnit() const;

  /** Explicity set temporal units (Defaults to Frame)*/
  virtual void
  SetTemporalUnitToFrame();
  virtual void
  SetTemporalUnitToRealTime();
  virtual void
  SetTemporalUnitToFrameAndRealTime();

  /** Get/Set the number of frames that the internal buffer can hold */
  SizeValueType
  GetNumberOfBuffers();
  void
  SetNumberOfBuffers(SizeValueType num);

  virtual void
  SetLargestPossibleTemporalRegion(const TemporalRegionType & region);
  virtual const TemporalRegionType &
  GetLargestPossibleTemporalRegion() const;

  virtual void
  SetBufferedTemporalRegion(const TemporalRegionType & region);
  virtual const TemporalRegionType &
  GetBufferedTemporalRegion() const;
  virtual void
  SetRequestedTemporalRegion(const TemporalRegionType & region);
  virtual const TemporalRegionType &
  GetRequestedTemporalRegion() const;

  /** Get the portion of the requested region that is not covered by the
   * buffered region */
  virtual const TemporalRegionType
  GetUnbufferedRequestedTemporalRegion();

  void
  SetRequestedRegionToLargestPossibleRegion() override;

  bool
  RequestedRegionIsOutsideOfTheBufferedRegion() override;

  bool
  VerifyRequestedRegion() override;

  void
  CopyInformation(const DataObject *) override;

  void
  SetRequestedRegion(const DataObject *) override;

  void
  Graft(const DataObject *) override;

protected:
  TemporalDataObject();
  ~TemporalDataObject() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Buffer for holding component data objects */
  BufferType::Pointer m_DataObjectBuffer;

  /** We want to keep track of our regions in time. **/
  TemporalRegionType m_LargestPossibleTemporalRegion;
  TemporalRegionType m_RequestedTemporalRegion;
  TemporalRegionType m_BufferedTemporalRegion;

  TemporalUnitEnum m_TemporalUnit{ TemporalUnitEnum::Frame };
}; // end class TemporalDataObject
} // end namespace itk

#endif
