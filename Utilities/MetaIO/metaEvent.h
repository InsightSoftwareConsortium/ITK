#ifndef METAEvent_H
#define METAEvent_H

/*!    MetaEvent (.h)
 *
 * Description:
 *    Event abstract class
 *
 * \author Julien Jomier
 * February 20, 2003
 *
 */


class metaEvent
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


#endif
