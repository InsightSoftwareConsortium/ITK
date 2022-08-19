#ifndef itkRANSAC_txx
#define itkRANSAC_txx

#include "itkRANSAC.h"

namespace itk
{


template <class T, class S>
RANSAC<T, S>::RANSAC()
{
  this->numberOfThreads = 1;
}


template <class T, class S>
RANSAC<T, S>::~RANSAC()
{}


template <class T, class S>
void
RANSAC<T, S>::SetNumberOfThreads(unsigned int numberOfThreads)
{
  if (numberOfThreads == 0 || numberOfThreads > itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads())
    throw ExceptionObject(__FILE__, __LINE__, "Invalid setting for number of threads.");

  this->numberOfThreads = 1; // numberOfThreads;
}


template <class T, class S>
unsigned int
RANSAC<T, S>::GetNumberOfThreads()
{
  return this->numberOfThreads;
}


template <class T, class S>
void
RANSAC<T, S>::SetParametersEstimator(ParametersEstimatorType * paramEstimator)
{
  // check if the given parameter estimator can be used in combination
  // with the data, if there aren't enough data elements then throw an
  // exception. If there is no data then any parameter estimator works
  if (!this->data.empty())
    if (data.size() < paramEstimator->GetMinimalForEstimate())
      throw ExceptionObject(__FILE__, __LINE__, "Not enough data elements for use with this parameter estimator.");
  this->paramEstimator = paramEstimator;
}


template <class T, class S>
void
RANSAC<T, S>::SetData(std::vector<T> & data)
{
  // check if the given data vector has enough elements for use with
  // the parameter estimator. If the parameter estimator hasn't been
  // set yet then any vector is good.
  if (this->paramEstimator.IsNotNull())
    if (data.size() < this->paramEstimator->GetMinimalForEstimate())
      throw ExceptionObject(__FILE__, __LINE__, "Not enough data elements for use with the parameter estimator.");
  this->data = data;
}


template <class T, class S>
double
RANSAC<T, S>::Compute(std::vector<S> & parameters, double desiredProbabilityForNoOutliers)
{
  // STEP1: setup
  parameters.clear();
  // the data or the parameter estimator were not set
  // or desiredProbabilityForNoOutliers is not in (0.0,1.0)
  if (this->paramEstimator.IsNull() || this->data.empty() || desiredProbabilityForNoOutliers >= 1.0 ||
      desiredProbabilityForNoOutliers <= 0.0)
    return 0;

  unsigned int numForEstimate = this->paramEstimator->GetMinimalForEstimate();
  size_t       numDataObjects = this->data.size();

  this->bestVotes = new bool[numDataObjects];
  // initalize with 0 so that the first computation which gives
  // any type of fit will be set to best
  this->numVotesForBest = 0;

  SubSetIndexComparator subSetIndexComparator(numForEstimate);
  this->chosenSubSets = new std::set<int *, SubSetIndexComparator>(subSetIndexComparator);
  // initialize with the number of all possible subsets
  this->allTries = Choose(numDataObjects, numForEstimate);
  this->numTries = this->allTries;
  this->numerator = log(1.0 - desiredProbabilityForNoOutliers);

  std::cout << "Number of allTries " << this->allTries << std::endl;

  srand((unsigned)time(NULL)); // seed random number generator

  // STEP2: create the threads that generate hypotheses and test

  itk::MultiThreaderBase::Pointer threader = itk::MultiThreaderBase::New();
  threader->SetSingleMethod(RANSAC<T, S>::RANSACThreadCallback, this);
  // runs all threads and blocks till they finish
  threader->SingleMethodExecute();

  // STEP3: least squares estimate using largest consensus set and cleanup

  std::vector<T *> leastSquaresEstimateData;
  if (this->numVotesForBest > 0)
  {
    for (unsigned int j = 0; j < numDataObjects; j++)
    {
      if (this->bestVotes[j])
        leastSquaresEstimateData.push_back(&(data[j]));
    }
    paramEstimator->LeastSquaresEstimate(leastSquaresEstimateData, parameters);
  }


  // cleanup
  typename std::set<int *, SubSetIndexComparator>::iterator it = this->chosenSubSets->begin();
  typename std::set<int *, SubSetIndexComparator>::iterator chosenSubSetsEnd = this->chosenSubSets->end();
  while (it != chosenSubSetsEnd)
  {
    delete[] (*it);
    it++;
  }
  this->chosenSubSets->clear();
  delete this->chosenSubSets;
  delete[] this->bestVotes;

  return (double)this->numVotesForBest / (double)numDataObjects;
}


template <class T, class S>
ITK_THREAD_RETURN_TYPE
RANSAC<T, S>::RANSACThreadCallback(void * arg)
{
  typedef itk::MultiThreaderBase::WorkUnitInfo ThreadInfoType;
  ThreadInfoType *                             infoStruct = static_cast<ThreadInfoType *>(arg);
  // dynamic_cast doesn't work with void *
  RANSAC<T, S> * caller = reinterpret_cast<RANSAC<T, S> *>(infoStruct->UserData);

  if (caller != NULL)
  {
    unsigned int i, k, l, m, maxIndex, numVotesForCur;
    int          j;
    int *        curSubSetIndexes;

    unsigned int     numDataObjects = caller->data.size();
    unsigned int     numForEstimate = caller->paramEstimator->GetMinimalForEstimate();
    std::vector<T *> exactEstimateData;
    std::vector<S>   exactEstimateParameters;
    double           denominator;

    // true if data[i] agrees with the current model, otherwise false
    bool * curVotes = new bool[numDataObjects];
    // true if data[i] is NOT chosen for computing the exact fit, otherwise false
    bool * notChosen = new bool[numDataObjects];

    // for (i = 0; i < caller->numTries; i++)
    for (i = 0; i < 1000; i++)
    {
      // randomly select data for exact model fit ('numForEstimate' objects).
      std::fill(notChosen, notChosen + numDataObjects, true);
      curSubSetIndexes = new int[numForEstimate];
      exactEstimateData.clear();
      maxIndex = numDataObjects - 1;
      for (l = 0; l < numForEstimate; l++)
      {
        // selectedIndex is in [0,maxIndex]
        int selectedIndex = (int)(((float)rand() / (float)RAND_MAX) * maxIndex + 0.5);
        for (j = -1, k = 0; k < numDataObjects && j < selectedIndex; k++)
        {
          if (notChosen[k])
            j++;
        }
        k--;
        exactEstimateData.push_back(&(caller->data[k]));
        notChosen[k] = false;
        maxIndex--;
      }
      // get the indexes of the chosen objects so we can check that
      // this sub-set hasn't been chosen already
      for (l = 0, m = 0; m < numDataObjects; m++)
      {
        if (!notChosen[m])
        {
          curSubSetIndexes[l] = m + 1;
          l++;
        }
      }

      caller->hypothesisMutex.lock();

      // check that the sub-set just chosen is unique
      std::pair<typename std::set<int *, SubSetIndexComparator>::iterator, bool> res =
        caller->chosenSubSets->insert(curSubSetIndexes);

      caller->hypothesisMutex.unlock();

      if (res.second == true)
      { // first time we chose this sub set
        // use the selected data for an exact model parameter fit
        caller->paramEstimator->Estimate(exactEstimateData, exactEstimateParameters);
        // selected data is a singular configuration (e.g. three
        // colinear points for a circle fit)
        if (exactEstimateParameters.size() == 0)
          continue;
        // see how many agree on this estimate
        numVotesForCur = 0;
        std::fill(curVotes, curVotes + numDataObjects, false);
        // continue checking data until there is no chance of getting a larger consensus set
        // or all the data has been checked
        for (m = 0; m < numDataObjects && caller->numVotesForBest - numVotesForCur < numDataObjects - m + 1; m++)
        {
          if (caller->paramEstimator->Agree(exactEstimateParameters, caller->data[m]))
          {
            curVotes[m] = true;
            numVotesForCur++;
          }
        } // found a larger consensus set?
        caller->resultsMutex.lock();
        if (numVotesForCur > caller->numVotesForBest)
        {
          caller->numVotesForBest = numVotesForCur;
          std::copy(curVotes, curVotes + numDataObjects, caller->bestVotes);
          // all data objects are inliers, terminate the search
          if (caller->numVotesForBest == numDataObjects)
            i = caller->numTries;
          else
          { // update the estimate of outliers and the number of iterations we need
            denominator = log(1.0 - pow((double)numVotesForCur / (double)numDataObjects, (double)(numForEstimate)));
            caller->numTries = (int)(caller->numerator / denominator + 0.5);
            // there are cases when the probablistic number of tries is greater than all possible sub-sets
            caller->numTries = caller->numTries < caller->allTries ? caller->numTries : caller->allTries;
          }
        }
        caller->resultsMutex.unlock();
      }
      else
      { // this sub set already appeared, release memory
        delete[] curSubSetIndexes;
      }
    }
    delete[] curVotes;
    delete[] notChosen;
  }
  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

/*****************************************************************************/

template <class T, class S>
unsigned int
RANSAC<T, S>::Choose(unsigned int n, unsigned int m)
{
  double denominatorEnd, numeratorStart, numerator, denominator, i, result;
  // perform smallest number of multiplications
  if ((n - m) > m)
  {
    numeratorStart = n - m + 1;
    denominatorEnd = m;
  }
  else
  {
    numeratorStart = m + 1;
    denominatorEnd = n - m;
  }

  for (i = numeratorStart, numerator = 1; i <= n; i++)
    numerator *= i;
  for (i = 1, denominator = 1; i <= denominatorEnd; i++)
    denominator *= i;
  result = numerator / denominator;

  // check for overflow both in computation and in result
  if (denominator > std::numeric_limits<double>::max() || numerator > std::numeric_limits<double>::max() ||
      static_cast<double>(std::numeric_limits<unsigned int>::max()) < result)
    return std::numeric_limits<unsigned int>::max();
  else
    return static_cast<unsigned int>(result);
}

} // end namespace itk

#endif
