#ifndef METAEllipse_H
#define METAEllipse_H

#include <metaTypes.h>
#include <metaUtils.h>
#include <metaObject.h>

#include <list>


/*!    MetaEllipse (.h and .cpp)
 *
 * Description:
 *    Reads and Writes MetaEllipseFiles.
 *
 * \author Julien Jomier
 * 
 * \date May 22, 2002
 * 
 * Depends on:
 *    MetaUtils.h
 *    MetaFileLib.h
 */


class MetaEllipse : public MetaObject
  {

  /////
  //
  // PUBLIC
  //
  ////
  public:

    ////
    //
    // Constructors & Destructor
    //
    ////
    MetaEllipse(void);

    MetaEllipse(const char *_headerName);   

    MetaEllipse(const MetaEllipse *_ellipse);    

    ~MetaEllipse(void);

    void PrintInfo(void) const;

    void CopyInfo(const MetaEllipse * _ellipse);

    //
    //
    //
    bool Read(const char *_headerName=NULL);

    bool Write(const char *_headName=NULL);


    void  Clear(void);

    bool ReadStream(int ndims, std::ifstream * stream);

    bool Append(const char *_headName=NULL);
 
    void  Radius(const float* radius);
    void  Radius(float radius);
    void  Radius(float r1,float r2);
    void  Radius(float r1,float r2, float r3);
    const float* Radius(void);


  ////
  //
  // PROTECTED
  //
  ////
  protected:

    void  M_Destroy(void);

    void  M_SetupReadFields(void);

    void  M_SetupWriteFields(void);

    bool  M_Read(void);

    float m_Radius[100];  // "Radius = "     0

  };


#endif
