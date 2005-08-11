#ifndef METACOEF_H
#define METACOEF_H

#include <metaTypes.h>
#include <metaUtils.h>
#include <metaObject.h>

#include <list>


/** Define a mesh point */
class CoefPoint
{
public:

  CoefPoint(int dim)
  { 
    m_Dim = dim;
    m_X = new double[m_Dim];
    for(unsigned int i=0;i<m_Dim;i++)
    {
      m_X[i] = 0;
    }
  }
  ~CoefPoint()
  { 
    delete []m_X;
  };
  
  unsigned int m_Dim;
  double* m_X;

};


class MetaCoef : public MetaObject
{
public:
    typedef std::list<CoefPoint*> PointListType;
    ////
    //
    // Constructors & Destructor
    //
    ////
    MetaCoef(void);

    MetaCoef(const char *_headerName);   

    MetaCoef(const MetaCoef *_coef); 
    
    MetaCoef(unsigned int dim);

    ~MetaCoef(void);
    
    void PrintInfo(void) const;

    void CopyInfo(const MetaCoef * _coef);
    
    //    NPoints(...)
    //       Required Field
    //       Number of points wich compose the tube
    void  NPoints(int npnt);
    int   NPoints(void) const;

    //    PointDim(...)
    //       Required Field
    //       Definition of points
    void        PointDim(const char* pointDim);
    const char* PointDim(void) const;

    void  Clear(void);
    
    PointListType & GetPoints(void) {return m_PointList;}
    const PointListType & GetPoints(void) const {return m_PointList;}
    
    MET_ValueEnumType ElementType(void) const;
    void  ElementType(MET_ValueEnumType _elementType);

protected:

    void  M_Destroy(void);

    void  M_SetupReadFields(void);

    void  M_SetupWriteFields(void);

    bool  M_Read(void);

    bool  M_Write(void);

    int m_NPoints;      // "NPoints = "         0

    char m_PointDim[255]; // "PointDim = "       "x y z r"

    PointListType m_PointList;
    
    MET_ValueEnumType m_ElementType;   
};

#endif

