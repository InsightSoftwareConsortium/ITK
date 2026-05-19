/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkRANSAC_hxx
#define itkRANSAC_hxx

#include "itkRANSAC.h"
#include "itkPointsLocator.h"
#include "itkSimilarity3DTransform.h"

namespace itk
{


template <typename T, typename SType, typename TTransform>
RANSAC<T, SType, TTransform>::RANSAC()
{
  this->numberOfThreads = 1;
}


template <typename T, typename SType, typename TTransform>
RANSAC<T, SType, TTransform>::~RANSAC()
{}


template <typename T, typename SType, typename TTransform>
void
RANSAC<T, SType, TTransform>::SetNumberOfThreads(unsigned int inputNumberOfThreads)
{
  if (inputNumberOfThreads == 0 || inputNumberOfThreads > itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads())
    throw ExceptionObject(__FILE__, __LINE__, "Invalid setting for number of threads.");

  this->numberOfThreads = inputNumberOfThreads; // numberOfThreads;
}

template <typename T, typename SType, typename TTransform>
void
RANSAC<T, SType, TTransform>::SetMaxIteration(unsigned int inputMaxIteration)
{
  this->maxIteration = inputMaxIteration;
}

template <typename T, typename SType, typename TTransform>
void
RANSAC<T, SType, TTransform>::SetCheckCorresspondenceDistance(bool inputFlag)
{
  this->checkCorresspondenceDistanceFlag = inputFlag;
}

template <typename T, typename SType, typename TTransform>
bool
RANSAC<T, SType, TTransform>::GetCheckCorresspondenceDistance()
{
  return this->checkCorresspondenceDistanceFlag;
}

template <typename T, typename SType, typename TTransform>
void
RANSAC<T, SType, TTransform>::SetCheckCorrespondenceEdgeLength(double inputLength)
{
  this->checkCorrespondenceEdgeLengthTest = inputLength;
}

template <typename T, typename SType, typename TTransform>
double
RANSAC<T, SType, TTransform>::GetCheckCorrespondenceEdgeLength()
{
  return this->checkCorrespondenceEdgeLengthTest;
}


template <typename T, typename SType, typename TTransform>
unsigned int
RANSAC<T, SType, TTransform>::GetNumberOfThreads()
{
  return this->numberOfThreads;
}

template <typename T, typename SType, typename TTransform>
void
RANSAC<T, SType, TTransform>::SetParametersEstimator(ParametersEstimatorType * inputParamEstimator)
{
  // check if the given parameter estimator can be used in combination
  // with the data, if there aren't enough data elements then throw an
  // exception. If there is no data then any parameter estimator works
  if (!this->data.empty())
    if (data.size() < inputParamEstimator->GetMinimalForEstimate())
      throw ExceptionObject(__FILE__, __LINE__, "Not enough data elements for use with this parameter estimator.");
  this->paramEstimator = inputParamEstimator;
}


template <typename T, typename SType, typename TTransform>
void
RANSAC<T, SType, TTransform>::SetData(std::vector<T> & inputData)
{
  // check if the given data vector has enough elements for use with
  // the parameter estimator. If the parameter estimator hasn't been
  // set yet then any vector is good.
  if (this->paramEstimator.IsNotNull())
    if (inputData.size() < this->paramEstimator->GetMinimalForEstimate())
      throw ExceptionObject(__FILE__, __LINE__, "Not enough data elements for use with the parameter estimator.");
  this->data = inputData;
}

template <typename T, typename SType, typename TTransform>
void
RANSAC<T, SType, TTransform>::SetAgreeData(std::vector<T> & inputData)
{
  // check if the given data vector has enough elements for use with
  // the parameter estimator. If the parameter estimator hasn't been
  // set yet then any vector is good.
  if (this->paramEstimator.IsNotNull())
    if (inputData.size() < this->paramEstimator->GetMinimalForEstimate())
      throw ExceptionObject(__FILE__, __LINE__, "Not enough data elements for use with the parameter estimator.");
  this->agreeData = inputData;
}

template <typename T, typename SType, typename TTransform>
std::vector<double>
RANSAC<T, SType, TTransform>::Compute(std::vector<SType> & parameters, double desiredProbabilityForNoOutliers)
{
  std::vector<double> outputPair;

  // STEP1: setup
  parameters.clear();
  // the data or the parameter estimator were not set
  // or desiredProbabilityForNoOutliers is not in (0.0,1.0)
  if (this->paramEstimator.IsNull() || this->data.empty() || desiredProbabilityForNoOutliers >= 1.0 ||
      desiredProbabilityForNoOutliers <= 0.0)
  {
    outputPair.push_back(0);
    outputPair.push_back(0);
    return outputPair;
  }


  unsigned int numForEstimate = this->paramEstimator->GetMinimalForEstimate();
  size_t       numAgreeObjects = this->agreeData.size();
  size_t       numDataObjects = this->data.size();

  this->bestVotes = new bool[numAgreeObjects];
  // initalize with 0 so that the first computation which gives
  // any type of fit will be set to best
  this->numVotesForBest = 0;

  SubSetIndexComparator subSetIndexComparator(numForEstimate);
  this->chosenSubSets = new std::set<int *, SubSetIndexComparator>(subSetIndexComparator);
  // initialize with the number of all possible subsets
  this->allTries = Choose(numDataObjects, numForEstimate);
  this->numTries = this->allTries;
  // this->
  this->numerator = log(1.0 - desiredProbabilityForNoOutliers);

  srand((unsigned)time(NULL)); // seed random number generator

  // STEP2: create the threads that generate hypotheses and test
  itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads(this->numberOfThreads);
  itk::MultiThreaderBase::Pointer threader = itk::MultiThreaderBase::New();
  // runs all threads and blocks till they finish
  threader->SetSingleMethodAndExecute(RANSAC<T, SType, TTransform>::RANSACThreadCallback, this);

  auto transform = TTransform::New();

  auto         optParameters = transform->GetParameters();
  auto         fixedParameters = transform->GetFixedParameters();
  unsigned int totalParameters = optParameters.GetSize() + fixedParameters.GetSize();

  int counter = 0;
  for (unsigned int i = optParameters.GetSize(); i < totalParameters; ++i)
  {
    fixedParameters.SetElement(counter, this->parametersRansac[i]);
    counter = counter + 1;
  }
  transform->SetFixedParameters(fixedParameters);

  counter = 0;
  for (unsigned int i = 0; i < optParameters.GetSize(); ++i)
  {
    optParameters.SetElement(counter, this->parametersRansac[i]);
    counter = counter + 1;
  }
  transform->SetParameters(optParameters);


  // STEP3: least squares estimate using largest consensus set and cleanup
  using PointsLocatorType = itk::PointsLocator<itk::VectorContainer<IdentifierType, itk::Point<double, 3>>>;
  auto pointsLocator = PointsLocatorType::New();

  using PointsContainer = itk::VectorContainer<IdentifierType, itk::Point<double, 3>>;
  auto                  points = PointsContainer::New();
  itk::Point<double, 3> testPoint;
  itk::Point<double, 6> inlierPoint;

  points->Reserve(this->agreeData.size());
  for (unsigned int i = 0; i < this->agreeData.size(); ++i)
  {
    auto point = this->agreeData[i];
    testPoint[0] = point[3];
    testPoint[1] = point[4];
    testPoint[2] = point[5];
    points->InsertElement(i, testPoint);
  }

  pointsLocator->SetPoints(points);
  pointsLocator->Initialize();

  std::vector<T> leastSquaresEstimateData;
  leastSquaresEstimateData.reserve(this->numVotesForBest);

  if (this->numVotesForBest > 0)
  {
    for (unsigned int j = 0; j < numAgreeObjects; j++)
    {
      if (this->bestVotes[j])
      {
        // Find the corresponding point by performing query using KDTree
        auto tempPoint = this->agreeData[j];
        testPoint[0] = tempPoint[0];
        testPoint[1] = tempPoint[1];
        testPoint[2] = tempPoint[2];

        const size_t                    num_results = 1;
        std::vector<size_t>             ret_indexes(num_results);
        std::vector<double>             out_dists_sqr(num_results);
        nanoflann::KNNResultSet<double> resultSet(num_results);

        auto transformedPoint = transform->TransformPoint(testPoint);
        auto pointId = pointsLocator->FindClosestPoint(transformedPoint);
        auto corresPoint = points->GetElement(pointId);

        // Insert the corresponding point for leastSquaresEstimate
        inlierPoint[0] = tempPoint[0];
        inlierPoint[1] = tempPoint[1];
        inlierPoint[2] = tempPoint[2];
        inlierPoint[3] = corresPoint[0];
        inlierPoint[4] = corresPoint[1];
        inlierPoint[5] = corresPoint[2];

        leastSquaresEstimateData.push_back(inlierPoint);
      }
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

  outputPair.push_back((double)this->numVotesForBest / (double)numAgreeObjects);
  outputPair.push_back(this->bestRMSE);
  return outputPair;
}


template <typename T, typename SType, typename TTransform>
ITK_THREAD_RETURN_TYPE
RANSAC<T, SType, TTransform>::RANSACThreadCallback(void * arg)
{
  typedef itk::MultiThreaderBase::WorkUnitInfo ThreadInfoType;
  ThreadInfoType *                             infoStruct = static_cast<ThreadInfoType *>(arg);
  // dynamic_cast doesn't work with void *
  RANSAC<T, SType, TTransform> * caller = reinterpret_cast<RANSAC<T, SType, TTransform> *>(infoStruct->UserData);

  if (caller != NULL)
  {
    unsigned int i, k, l, m, maxIndex, numVotesForCur;
    int          j;
    int *        curSubSetIndexes;

    unsigned int numDataObjects = caller->data.size();
    unsigned int numAgreeObjects = caller->agreeData.size();

    unsigned int       numForEstimate = caller->paramEstimator->GetMinimalForEstimate();
    std::vector<T *>   exactEstimateData;
    std::vector<SType> exactEstimateParameters;

    // true if agreeData[i] agrees with the current model, otherwise false
    bool * curVotes = new bool[numAgreeObjects];
    // true if data[i] is NOT chosen for computing the exact fit, otherwise false
    bool * notChosen = new bool[numDataObjects];

    unsigned int counter = 0;
    for (i = 0; i < caller->numTries; i++)
    {
      counter = counter + 1;
      if (counter > caller->maxIteration)
      {
        break;
      }
      // randomly select data for exact model fit ('numForEstimate' objects).
      std::fill(notChosen, notChosen + numDataObjects, true);
      curSubSetIndexes = new int[numForEstimate];
      exactEstimateData.clear();
      exactEstimateData.reserve(numForEstimate);
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

        // Inexpensive Test
        if (caller->checkCorresspondenceDistanceFlag == true)
        {
          auto distanceFlag =
            caller->paramEstimator->CheckCorresspondenceDistance(exactEstimateParameters, exactEstimateData);
          if (distanceFlag == false)
          {
            continue;
          }
        }

        // Inexpensive Test
        if (caller->checkCorrespondenceEdgeLengthTest > 0)
        {
          auto edgeFlag = caller->paramEstimator->CheckCorresspondenceEdgeLength(
            exactEstimateParameters, exactEstimateData, caller->checkCorrespondenceEdgeLengthTest);
          if (edgeFlag == false)
          {
            continue;
          }
        }

        // see how many agree on this estimate
        numVotesForCur = 0;
        std::fill(curVotes, curVotes + numAgreeObjects, false);

        // Expensive Inlier Test
        auto result =
          caller->paramEstimator->AgreeMultiple(exactEstimateParameters, caller->agreeData, caller->numVotesForBest);
        double rmse_value = 0.0;

        for (m = 0; m < numAgreeObjects; m++)
        {
          if (result[m] > 0)
          {
            curVotes[m] = true;
            numVotesForCur++;
            rmse_value = rmse_value + result[m];
          }
        } // found a larger consensus set?

        caller->resultsMutex.lock();
        if (numVotesForCur > caller->numVotesForBest ||
            (numVotesForCur == caller->numVotesForBest && rmse_value < caller->bestRMSE))
        {
          caller->numVotesForBest = numVotesForCur;
          caller->bestRMSE = rmse_value;

          std::copy(curVotes, curVotes + numAgreeObjects, caller->bestVotes);

          caller->parametersRansac.clear();
          for (unsigned int kp = 0; kp < exactEstimateParameters.size(); ++kp)
          {
            caller->parametersRansac.push_back(exactEstimateParameters[kp]);
          }
        }
        caller->resultsMutex.unlock();
      }
      else
      {
        // this sub set already appeared, release memory
        delete[] curSubSetIndexes;
      }
    }
    delete[] curVotes;
    delete[] notChosen;
  }
  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

/*****************************************************************************/

template <typename T, typename SType, typename TTransform>
unsigned int
RANSAC<T, SType, TTransform>::Choose(unsigned int n, unsigned int m)
{
  double denominatorEnd, numeratorStart, numeratorLocal, denominatorLocal, i, resultLocal;
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

  for (i = numeratorStart, numeratorLocal = 1; i <= n; i++)
  {
    numeratorLocal *= i;
  }
  for (i = 1, denominatorLocal = 1; i <= denominatorEnd; i++)
  {
    denominatorLocal *= i;
  }
  resultLocal = numeratorLocal / denominatorLocal;

  // check for overflow both in computation and in result
  if (denominatorLocal > std::numeric_limits<double>::max() || numeratorLocal > std::numeric_limits<double>::max() ||
      static_cast<double>(std::numeric_limits<unsigned int>::max()) < resultLocal)
  {
    return std::numeric_limits<unsigned int>::max();
  }
  else
    return static_cast<unsigned int>(resultLocal);
}

} // end namespace itk

#endif
