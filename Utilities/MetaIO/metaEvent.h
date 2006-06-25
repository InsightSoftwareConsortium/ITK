#include "metaTypes.h"

#define NAMESPACE_METAEVENT_H META_MERGE_TOKENS($METAIO_NAMESPACE, \
                                                 METAEVENT_H)
#ifndef $NAMESPACE_METAEVENT_H
#define $NAMESPACE_METAEVENT_H


/*!    MetaEvent (.h)
 *
 * Description:
 *    Event abstract class
 *
 * \author Julien Jomier
 * February 20, 2003
 *
 */

#if (METAIO_USE_NAMESPACE)
namespace METAIO_NAMESPACE {
#endif


class METAIO_EXPORT metaEvent
{
 
public:

  metaEvent(){m_Level = -1;};
  virtual ~metaEvent(){};

  virtual void SetCurrentIteration(unsigned int n) {m_CurrentIteration = n;}
  virtual void StartReading(unsigned int n) 
    {
    m_NumberOfIterations = n;
    m_Level++;
    };
  virtual void StopReading() 
    {
    m_Level--;
    };

protected:

  unsigned int m_CurrentIteration;
  unsigned int m_NumberOfIterations;
  int m_Level;

};

#if (METAIO_USE_NAMESPACE)
};
#endif


#endif
