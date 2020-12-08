/*============================================================================
  MetaIO
  Copyright 2000-2010 Insight Software Consortium

  Distributed under the OSI-approved BSD License (the "License");
  see accompanying file Copyright.txt for details.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the License for more information.
============================================================================*/
#include "metaTypes.h"

#ifndef ITKMetaIO_METATUBE_H
#  define ITKMetaIO_METATUBE_H

#  include "metaUtils.h"
#  include "metaObject.h"

#  ifdef _MSC_VER
#    pragma warning(disable : 4251)
#  endif

#  include <list>


/*!    MetaTube (.h and .cpp)
 *
 * Description:
 *    Reads and Writes MetaTubeFiles.
 *
 * \author Julien Jomier
 *
 * \date May 22, 2002
 */

#  if (METAIO_USE_NAMESPACE)
namespace METAIO_NAMESPACE
{
#  endif

class METAIO_EXPORT TubePnt
{
public:
  typedef std::pair<std::string, float> FieldType;
  typedef std::vector<FieldType>        FieldListType;

  TubePnt(int dim);
  TubePnt(const TubePnt * _tubePnt);
  ~TubePnt();

  unsigned int m_NDims;
  int          m_ID;
  float *      m_X;
  float        m_Color[4];
  bool         m_Mark;
  float        m_R;
  float        m_Ridgeness;
  float        m_Medialness;
  float        m_Branchness;
  float        m_Curvature;
  float        m_Levelness;
  float        m_Roundness;
  float        m_Intensity;
  float *      m_T;
  float *      m_V1;
  float *      m_V2;
  float        m_Alpha1;
  float        m_Alpha2;
  float        m_Alpha3;

  FieldListType m_ExtraFields;

  virtual void
  CopyInfo(const TubePnt * _tubePnt);

  const FieldListType &
  GetExtraFields() const;

  size_t
  GetNumberOfExtraFields(void) const;
  void
  SetNumberOfExtraFields(int size);

  void
  SetField(int indx, const char * name, float value);
  void
  SetField(const char * name, float value);
  void
  AddField(const char * name, float value);
  int
  GetFieldIndex(const char * name) const;
  float
  GetField(int indx) const;
  float
  GetField(const char * name) const;
};


class METAIO_EXPORT MetaTube : public MetaObject
{

  // PUBLIC
public:
  typedef TubePnt                PointType;
  typedef std::list<PointType *> PointListType;

  // Constructors & Destructor
  MetaTube(void);

  MetaTube(const char * _headerName);

  MetaTube(const MetaTube * _Tube);

  MetaTube(unsigned int dim);

  ~MetaTube(void) override;

  void
  PrintInfo(void) const override;

  using MetaObject::CopyInfo;

  void
  CopyInfo(const MetaTube * _object);

  void
  Clear(void) override;

  MET_ValueEnumType
  ElementType(void) const;
  void
  ElementType(MET_ValueEnumType _elementType);

  //    PointDim(...)
  //       Required Field
  //       Definition of points
  const char *
  PointDim(void) const;
  void
  PointDim(const char * pntDim);

  //    NPoints(...)
  //       Required Field
  //       Number of points which compose the MetaPointObject
  void
  NPoints(int npnt);
  int
  NPoints(void) const;

  PointListType &
  GetPoints(void)
  {
    return m_PointList;
  }
  const PointListType &
  GetPoints(void) const
  {
    return m_PointList;
  }

  //    Root(...)
  //       Optional Field
  //       Set if this Tube is a root
  void
  Root(bool root)
  {
    m_Root = root;
  };
  bool
  Root(void) const
  {
    return m_Root;
  };

  //    Artery(...)
  //       Optional Field
  //       Set if this Tube is a root
  void
  Artery(bool artery)
  {
    m_Artery = artery;
  };
  bool
  Artery(void) const
  {
    return m_Artery;
  };

  //    ParentPoint(...)
  //       Optional Field
  //       Set the point number of the parent Tube where the branch occurs
  void
  ParentPoint(int parentPoint)
  {
    m_ParentPoint = parentPoint;
  };
  int
  ParentPoint(void) const
  {
    return m_ParentPoint;
  };

  // PROTECTED
protected:
  typedef std::pair<std::string, unsigned int> PositionType;

  int
  M_GetPosition(const char *, std::vector<bool> & used) const;

  float
  M_GetFloatFromBinaryData(size_t pos, const char * _data, size_t readSize) const;

  void
  M_SetFloatIntoBinaryData(float val, char * _data, int i) const;

  void
  M_Destroy(void) override;

  void
  M_SetupReadFields(void) override;

  void
  M_SetupWriteFields(void) override;

  bool
  M_Read(void) override;

  bool
  M_Write(void) override;

  int m_NPoints;

  std::string m_PointDim;

  PointListType m_PointList;

  MET_ValueEnumType m_ElementType;

  int m_ParentPoint;

  bool m_Root;

  bool m_Artery;

  std::vector<PositionType> m_Positions;
};

#  if (METAIO_USE_NAMESPACE)
};
#  endif

#endif
