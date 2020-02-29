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
#ifndef itkAnalyzeObjectEntry_h
#define itkAnalyzeObjectEntry_h

#include <string>
#include <cstdio>
#include <iostream>
#include <fstream>

#include "itkObjectFactory.h"
#include "itkImage.h"
#include "itkByteSwapper.h"
#include "itksys/SystemTools.hxx"
#include "itkImageIOBase.h"

#include "AnalyzeObjectLabelMapExport.h"

namespace itk
{
constexpr const char * const ANALYZE_OBJECT_LABEL_MAP_ENTRY_ARRAY{"ANALYZE_OBJECT_LABEL_MAP_ENTRY_ARRAY"};
/**
 * Constants representing the current version number of the object map file for Analyze
 */
constexpr int VERSION1{880102};
constexpr int VERSION2{880801};
constexpr int VERSION3{890102};
constexpr int VERSION4{900302};
constexpr int VERSION5{910402};
constexpr int VERSION6{910926};
constexpr int VERSION7{20050829};


/**
 * \class AnalyzeObjectEntry
 * \ingroup AnalyzeObjectMapIO
 * \brief This class encapsulates a single object in an Analyze object file
 */
class AnalyzeObjectLabelMap_EXPORT AnalyzeObjectEntry : public Object
{
public:
  /** Standard type alias. */
  using Self = AnalyzeObjectEntry;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using intRGBPixel = itk::RGBPixel<int>;
  using Index = itk::Index<3>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AnalyzeObjectEntry, Object);

  /**
   * \brief Copy
   *
   *This function will copy all of the ivars except the name of the entry.
   *The reason why the function does not copy the name, is that each object entry should have a unique name.
   */
  void
  Copy(AnalyzeObjectEntry::Pointer rhs);

  /**
   * \brief getName/setName
   *
   * The Name member of the AVW_Object structure contains a user
   * defined name for this object. Any zero-terminated string can be used,
   * including stringswith embedded spaces.
   */
  virtual std::string
  GetName() const
  {
    itkDebugMacro("returning "
                  << "Name of " << this->m_Name);
    return std::string(this->m_Name);
  }

  /** Set built-in type.  Creates member Set"name"() (e.g., SetVisibility()); */
  virtual void
  SetName(const std::string _arg)
  {
    char temp[32];

    strncpy(temp, _arg.c_str(), 31);
    temp[31] = '\0';
    itkDebugMacro("setting "
                  << "Name"
                  << " to " << temp);
    if (strcmp(this->m_Name, temp) != 0)
    {
      strncpy(this->m_Name, temp, 32);
      this->Modified();
    }
  }

  /**
   * \brief getDisplayFlag/setDisplayFlag
   *
   * DisplayFlag is used to enable or disable the display of this
   * particular object. A value of zero value indicates
   * that voxels of this object type are ignored or assumed to be outside
   * the threshold range during the raycasting process.
   */
  itkGetConstMacro(DisplayFlag, int);
  itkSetMacro(DisplayFlag, int);

  /**
   * \brief getCopyFlag gets the Copy Flag
   *
   * CopyFlag indicates object translation, rotation, and mirroring are
   * applied to a copy of the actual object, rather
   * than the object itself. [ANALYZE only]
   */
  itkGetConstMacro(CopyFlag, unsigned char);
  itkSetMacro(CopyFlag, unsigned char);

  /**
   * \brief getMirrorFlag/setMirrorFlag
   *
   * MirrorFlag indicates the axis this object is mirrored around.
   * [ANALYZE only]
   */
  itkGetConstMacro(MirrorFlag, unsigned char);
  itkSetMacro(MirrorFlag, unsigned char);

  /**
   * \brief getStatusFlag/setStatusFlag
   *
   * StatusFlag is used to indicate when an object has changed and may
   * need it's minimum bounding box recomputed. [ANALYZE only]
   */
  itkGetConstMacro(StatusFlag, unsigned char);
  itkSetMacro(StatusFlag, unsigned char);

  /**
   * \brief getNeighborsUsedFlag/setNeighborsUsedFlag
   *
   * NeighborsUsedFlag indicates which neighboring voxels are used in
   * calculating the objects shading. [ANALYZE only]
   */
  itkGetConstMacro(NeighborsUsedFlag, unsigned char);
  itkSetMacro(NeighborsUsedFlag, unsigned char);

  /**
   * \brief getShades/setShades
   *
   * Shades indicates the number of shades available for this object.
   * Only 256 (250 in ANALYZE) total shades are available.
   */
  itkGetConstMacro(Shades, int);
  itkSetMacro(Shades, int);

  /**
   * \brief getStartRed/setStartRed
   *
   * StartRed specify the starting color for this object. This is usually a
   * darker shade of the ending color. ANALYZE defaults these values to 10%
   * of the ending color.
   */
  itkGetConstMacro(StartRed, int);
  itkSetMacro(StartRed, int);

  /**
   * \brief getStartGreen/setStartGreen
   *
   * StartGreen specify the starting color for this object. This is usually
   * a darker shade of the ending color.  ANALYZE defaults these values to
   * 10% of the ending color.
   */
  itkGetConstMacro(StartGreen, int);
  itkSetMacro(StartGreen, int);

  /**
   * \brief getStartBlue/setStartBlue
   *
   * StartBlue specify the starting color for this object. This is usually a
   * darker shade of the ending color. ANALYZE defaults these values to 10%
   * of the ending color.
   */
  itkGetConstMacro(StartBlue, int);
  itkSetMacro(StartBlue, int);

  /**
   * \brief getEndRed/setEndRed
   *
   * EndRed specify the ending color for this object.
   */
  itkGetConstMacro(EndRed, int);
  itkSetMacro(EndRed, int);

  /**
   * \brief getEndGreen/setEndGreen
   *
   * EndGreen specify the ending color for this object.
   */
  itkGetConstMacro(EndGreen, int);
  itkSetMacro(EndGreen, int);

  /**
   * \brief getEndBlue/setEndBlue
   *
   * EndBlue specify the ending color for this object.
   */
  itkGetConstMacro(EndBlue, int);
  itkSetMacro(EndBlue, int);

  /**
   * \brief getXRotation
   *
   * XRotation specify a rotation which is applied to this object only.
   * [ANALYZE only]
   */
  itkGetConstMacro(XRotation, int);
  itkSetMacro(XRotation, int);

  /**
   *\brief getXRotationIncrement
   *
   * XRotationIncrement specify increments that are applies to XRotation,
   * YRotation, and ZRotation when making a sequence. [ANALYZE only]
   */
  itkGetConstMacro(XRotationIncrement, int);
  itkSetMacro(XRotationIncrement, int);

  /**
   * \brief getYRotation
   *
   * YRotation specify a rotation which is applied to this object only.
   * [ANALYZE only]
   */
  itkGetConstMacro(YRotation, int);
  itkSetMacro(YRotation, int);

  /**
   *\brief getYRotationIncrement
   *
   * YRotationIncrement specify increments that are applies to XRotation,
   * YRotation, and ZRotation when making a sequence. [ANALYZE only]
   */
  itkGetConstMacro(YRotationIncrement, int);
  itkSetMacro(YRotationIncrement, int);

  /**
   * \brief getZRotation
   *
   * ZRotation specify a rotation which is applied to this object only.
   * [ANALYZE only]
   */
  itkGetConstMacro(ZRotation, int);
  itkSetMacro(ZRotation, int);

  /**
   *\brief getZRotationIncrement
   *
   * ZRotationIncrement specify increments that are applies to XRotation,
   * YRotation, and ZRotation when making a sequence. [ANALYZE only]
   */
  itkGetConstMacro(ZRotationIncrement, int);
  itkSetMacro(ZRotationIncrement, int);

  /**
   * \brief getXTranslation
   *
   * XTranslation specify a translation which is applied to this object only.
   * [ANALYZE only]
   */
  itkGetConstMacro(XTranslation, int);
  itkSetMacro(XTranslation, int);

  /**
   *\brief getXTranslation
   *
   * XTranslationIncrement specify increments that are applies to
   * XTranslation, YTranslation, and ZTranslation when making a sequence.
   * [ANALYZE only]
   */
  itkGetConstMacro(XTranslationIncrement, int);
  itkSetMacro(XTranslationIncrement, int);

  /**
   * \brief getYTranslation
   *
   * YTranslation specify a translation which is applied to this object only.
   * [ANALYZE only]
   */
  itkGetConstMacro(YTranslation, int);
  itkSetMacro(YTranslation, int);

  /**
   *\brief getYTranslationIncrement
   *
   * YTranslationIncrement specify increments that are applies to
   * XTranslation, YTranslation, and ZTranslation when making a sequence.
   * [ANALYZE only]
   */
  itkGetConstMacro(YTranslationIncrement, int);
  itkSetMacro(YTranslationIncrement, int);

  /**
   * \brief getZTranslation
   *
   * ZTranslation specify a translation which is applied to this object only.
   * [ANALYZE only]
   */
  itkGetConstMacro(ZTranslation, int);
  itkSetMacro(ZTranslation, int);

  /**
   *\brief getZTranslation
   *
   * ZTranslationIncrement specify increments that are applies to
   * XTranslation, YTranslation, and ZTranslation when making a sequence.
   * [ANALYZE only]
   */
  itkGetConstMacro(ZTranslationIncrement, int);
  itkSetMacro(ZTranslationIncrement, int);

  /**
   * \brief getXCenter/setXCenter
   *
   * XCenter specify the rotation center, relative to the volumes center,
   * which the XRotation, YRotation, and ZRotation are rotated around.
   * [ANALYZE only]
   */
  itkGetConstMacro(XCenter, int);
  itkSetMacro(XCenter, int);

  /**
   * \brief getYCenter/setYCenter
   *
   * YCenter specify the rotation center, relative to the volumes center,
   * which the XRotation, YRotation, and ZRotation are rotated around.
   * [ANALYZE only]
   */
  itkGetConstMacro(YCenter, int);
  itkSetMacro(YCenter, int);

  /**
   * \brief getZCenter/setZCenter
   *
   * ZCenter specify the rotation center, relative to the volumes center,
   * which the XRotation, YRotation, and ZRotation are rotated around.
   * [ANALYZE only]
   */
  itkGetConstMacro(ZCenter, int);
  itkSetMacro(ZCenter, int);

  /**
   * \brief getMinimumXValue/setMinimumXValue
   *
   * MinimumXValue specify the minimum enclosing brick used by ANALYZE to
   * increase the rendering speed of individual objects during object
   * translations and rotations. [ANALYZE only]
   */
  itkGetConstMacro(MinimumXValue, short int);
  itkSetMacro(MinimumXValue, int);

  /**
   * \brief getMinimumYValue/setMinimumYValue
   *
   * MinimumYValue specify the minimum enclosing brick used by ANALYZE to
   * increase the rendering speed of individual objects during object
   * translations and rotations. [ANALYZE only]
   */
  itkGetConstMacro(MinimumYValue, short int);
  itkSetMacro(MinimumYValue, int);

  /**
   * \brief getMinimumZValue/setMinimumZValue
   *
   * MinimumZValue specify the minimum enclosing brick used by ANALYZE to
   * increase the rendering speed of individual objects during object
   * translations and rotations. [ANALYZE only]
   */
  itkGetConstMacro(MinimumZValue, short int);
  itkSetMacro(MinimumZValue, int);

  /**
   * \brief getMaximumXValue/setMaximumXValue
   *
   * MaximumXValue specify the maximum enclosing brick used by ANALYZE to
   * increase the rendering speed of individual objects during object
   * translations and rotations. [ANALYZE only]
   */
  itkGetConstMacro(MaximumXValue, short int);
  itkSetMacro(MaximumXValue, int);

  /**
   * \brief getMaximumYValue/setMaximumYValue
   *
   * MaximumYValue specify the maximum enclosing brick used by ANALYZE to
   * increase the rendering speed of individual objects during object
   * translations and rotations. [ANALYZE only]
   */
  itkGetConstMacro(MaximumYValue, short int);
  itkSetMacro(MaximumYValue, int);

  /**
   * \brief getMaximumZValue/setMaximumZValue
   *
   * MaximumZValue specify the maximum enclosing brick used by ANALYZE to
   * increase the rendering speed of individual objects during object
   * translations and rotations. [ANALYZE only]
   */
  itkGetConstMacro(MaximumZValue, short int);
  itkSetMacro(MaximumZValue, int);

  /**
   * \brief getOpacity/setOpacity
   *
   * Opacity and OpacityThickness are used only when rendering 24-bit
   * transparency images. Opacity is a floating point value between .0001
   * (very transparent) and 1.0 (opaque). The Opacity value is multiplied
   * times the color of each surface voxel intersected, it is also
   * subtracted from 1.0 to determine the amount of shading which can be
   * applied by objects intersected after this object. Ray-casting continues
   * as long as it is possible for remaining objects along the ray path to
   * make contributions. The OpacityThickness parameter allows the
   * surface determined color, to be added in multiple times for thicker
   * objects. This allows the thickness of each object to make a
   * contribution into what can be seen behind it. A value of 1 for
   * OpacityThickness results in only the surface of each object having a
   * contribution.
   */
  itkGetConstMacro(Opacity, float);
  itkSetMacro(Opacity, float);

  /**
   * \brief getOpacityThickness/setOpacityThickness
   *
   * The thickness of the object
   */
  itkGetConstMacro(OpacityThickness, int);
  itkSetMacro(OpacityThickness, int);

  /**
   * \brief getBlendFactor/setBlendFactor
   *
   * Determines the amount of object color verses
   * composite color. A value of 1.0, causes all the
   * color to come from the object. A value of 0.0
   * causes all the color to come from the alpha map.
   * A value of .5 will cause half to come from each.
   */
  itkGetConstMacro(BlendFactor, float);
  itkSetMacro(BlendFactor, float);

  // TODO: Need to use these at some point or maybe just delete them
#if 0
  /**
   * \brief setStartColor
   *
   * Set the starting colors (red, green, blue) for creating the colormap.
   */
  itkSetMacro( StartColor, intRGBPixel);
  itkGetConstMacro(StartColor, intRGBPixel);

  /**
   * \brief setEndColor
   *
   * Set the ending colors (red, green, blue) for creating the colormap.
   */
  itkSetMacro(EndColor, intRGBPixel);
  itkGetConstMacro(EndColor, intRGBPixel);

  /**
   * \brief setRotation
   *
   * Set the rotation of the object (xRotation, yRotation, zRotation).
   */
  itkSetMacro(Rotation, Index);
  itkGetConstMacro(Rotation, Index);

  /**
   * \brief setTranslation
   *
   * Set the translation of the object (xTranslation, yTranslation, zTranslation).
   */
  itkSetMacro(Translation, Index);
  itkGetConstMacro(Translation, Index);

  /**
   * \brief setCenter
   *
   * Set the center of the object (xCenter, yCenter, zCenter).
   */
  itkSetMacro(Center, Index);
  itkGetConstMacro(Center, Index);

  /**
   * \brief setRotationIncrement
   *
   * Set the rotation increment (xRotationIncrement, yRotationIncrement, zRotationIncrement).
   */
  itkSetMacro(RotationIncrement, Index);
  itkGetConstMacro(RotationIncrement, Index);

  /**
   * \brief setTranslationIncrement
   *
   * Set the traslation increment of the object (xTranslationIncrement, yTranslationIncrement, zTranslatoinIncrement).
   */
  itkSetMacro(TranslationIncrement, Index);
  itkGetConstMacro(TranslationIncrement, Index);

  /**
   * \brief setMinimumCoordinate
   *
   * Set the minimum coordinate of the bounding brick of the object used for rendering by Analyze
   */
  itkSetMacro(MinimumCoordinateValue, Index);
  itkGetConstMacro(MinimumCoordinateValue, Index);

  /**
   * \brief setMaximumCoordinate
   *
   * Set the maximum coordinate of the bounding brick of the object used for rendering by Analyze
   */
  itkSetMacro(MaximumCoordinateValue, Index);
  itkGetConstMacro(MaximumCoordinateValue, Index);
#endif

  /**
   *\brief Print
   *
   *This function will print out all of the ivars out to any file that the user wants.
   *This is mostly used for debugging purposes.
   */
  void
  Print(std::ostream & myfile);

  /**
   *\brief ReadFromFilePointer
   *
   *This function will read in all of the ivars from a file location that is passed into it.
   */
  void
  ReadFromFilePointer(std::ifstream & inputFileStream, const bool NeedByteSwap, const bool /* NeedBlendFactor */);

  /**
   *\brief SwapObjectEndeness
   *
   *This function will change the object endedness if the computer is a little endian machine,
   *since the object maps are written in big endian.
   */
  void
  SwapObjectEndedness();
  /**
   *\brief Write
   *
   *This function will write out all of the ivars to a file location that is passed into it.
   */
  void
  Write(std::ofstream & outputFileStream);

protected:
  /**
   * \brief AnalyzeObjectEntry( ) is the default constructor, initializes to 0 or NULL
   * Possible Causes of Failure:
   * - unknown
   */
  AnalyzeObjectEntry();

  /**
   * \brief ~AnalyzeObjectEntry( void ) is the destructor, which does nothing explicitly due to
   * no use of dynamic allocation
   * Possible Causes of Failure:
   * - unknown
   * \sa AnalyzeObjectEntry
   */
  ~AnalyzeObjectEntry() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  template <typename TValue>
  void
  ReadBytes(std::ifstream & inputFileStream, TValue * dest, const int Replications, const bool NeedByteSwap);

  char          m_Name[33];                   /*bytes   0-31*/
  int           m_DisplayFlag{ 1 };           /*bytes  32-35*/
  unsigned char m_CopyFlag{ 0 };              /*bytes  36-36*/
  unsigned char m_MirrorFlag{ 0 };            /*bytes  37-37*/
  unsigned char m_StatusFlag{ 0 };            /*bytes  38-38*/
  unsigned char m_NeighborsUsedFlag{ 0 };     /*bytes  39-39*/
  int           m_Shades{ 1 };                /*bytes  40-43*/
  int           m_StartRed{ 0 };              /*bytes  44-47*/
  int           m_StartGreen{ 0 };            /*bytes  48-51*/
  int           m_StartBlue{ 0 };             /*bytes  52-55*/
  int           m_EndRed{ 0 };                /*bytes  53-58*/
  int           m_EndGreen{ 0 };              /*bytes  59-62*/
  int           m_EndBlue{ 0 };               /*bytes  63-66*/
  int           m_XRotation{ 0 };             /*bytes  67-70*/
  int           m_YRotation{ 0 };             /*bytes  71-74*/
  int           m_ZRotation{ 0 };             /*bytes  75-78*/
  int           m_XTranslation{ 0 };          /*bytes  79-82*/
  int           m_YTranslation{ 0 };          /*bytes  83-86*/
  int           m_ZTranslation{ 0 };          /*bytes  87-90*/
  int           m_XCenter{ 0 };               /*bytes  91-94*/
  int           m_YCenter{ 0 };               /*bytes  95-98*/
  int           m_ZCenter{ 0 };               /*bytes  99-102*/
  int           m_XRotationIncrement{ 0 };    /*bytes  103-106*/
  int           m_YRotationIncrement{ 0 };    /*bytes  107-110*/
  int           m_ZRotationIncrement{ 0 };    /*bytes  111-114*/
  int           m_XTranslationIncrement{ 0 }; /*bytes  115-118*/
  int           m_YTranslationIncrement{ 0 }; /*bytes  119-121*/
  int           m_ZTranslationIncrement{ 0 }; /*bytes  122-125*/
  short int     m_MinimumXValue{ 0 };         /*bytes  126-127*/
  short int     m_MinimumYValue{ 0 };         /*bytes  128-129*/
  short int     m_MinimumZValue{ 0 };         /*bytes  130-131*/
  short int     m_MaximumXValue{ 0 };         /*bytes  132-133*/
  short int     m_MaximumYValue{ 0 };         /*bytes  134-135*/
  short int     m_MaximumZValue{ 0 };         /*bytes  136-137*/
  float         m_Opacity{ 0.5 };             /*bytes  138-141*/
  int           m_OpacityThickness{ 1 };      /*bytes  142-145*/
  float         m_BlendFactor{ 0 };           /*bytes  146-149*/

  // TODO: Need to use these at some point or maybe just delete them
#if 0
  // Three seperate Start Colors (Red, Green, Blue) have been put together to use the set macro.
  intRGBPixel m_StartColor;
  // Three seperate End Colors (Red, Green, Blue) have been put together to use the set macro.
  intRGBPixel m_EndColor;
  // Three seperate Rotations (x, y, z) have been put together to use the set macro.
  Index m_Rotation;
  // Three seperate Translations (x, y, z) have been put together to use the set macro.
  Index m_Translation;
  // Three seperate Centers (x, y, z) have been put together to use the set macro.
  Index m_Center;
  // Three seperate Rotation Increments (x, y, z) have been put together to use the set macro.
  Index m_RotationIncrement;
  // Three seperate Translation Increments (x, y, z) have been put together to use the set macro.
  Index m_TranslationIncrement;
  // Three seperate Minimum Coordinate Values (x, y, z) have been put together to use the set macro.
  Index m_MinimumCoordinateValue;
  // Three seperate Maximum Coordiante Values (x, y, z) have been put together to use the set macro.
  Index m_MaximumCoordinateValue;
#endif
};
} // namespace itk
#endif // __OBJECTENTR_H__
