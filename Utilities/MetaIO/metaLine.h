#ifndef METALINE_H
#define METALINE_H

#include <metaTypes.h>
#include <metaUtils.h>
#include <metaObject.h>

#include <list>


/*!    MetaLine (.h and .cxx)
 *
 * Description:
 *    Reads and Writes MetaLineFiles.
 *
 * \author Julien Jomier
 * 
 * \date July 02, 2002
 *
 */

class LinePnt
{
public:

  LinePnt(int dim)
  { 
    m_Dim = dim;

    m_X = new float[m_Dim];
    m_V = new float*[m_Dim-1];

    for(unsigned int i=0;i<m_Dim-1;i++)
    {
      m_V[i] = new float[m_Dim];
      for(unsigned int j=0;j<m_Dim;j++)
      {
        m_V[i][j] = 0;
        m_X[j] = 0;
      } 
    }
    
    //Color is red by default
    m_Color[0]=1.0;
    m_Color[1]=0.0;
    m_Color[2]=0.0;
    m_Color[3]=1.0;  
  }

  ~LinePnt()
  {
    delete m_X;
    for(unsigned int i=0;i<m_Dim-1;i++)
      {
        delete [] m_V[i];
      }
    delete [] m_V;
  };
  
  unsigned int m_Dim;
  float*   m_X;
  float**  m_V;
  float    m_Color[4];
};




class MetaLine : public MetaObject
  {

  /////
  //
  // PUBLIC
  //
  ////
  public:

   typedef std::list<LinePnt*> PointListType;
    ////
    //
    // Constructors & Destructor
    //
    ////
    MetaLine(void);

    MetaLine(const char *_headerName);   

    MetaLine(const MetaLine *_tube); 
    
    MetaLine(unsigned int dim);

    ~MetaLine(void);

    void PrintInfo(void) const;

    void CopyInfo(const MetaLine * _tube);


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

  ////
  //
  // PROTECTED
  //
  ////
  protected:

    bool  m_ElementByteOrderMSB;

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
