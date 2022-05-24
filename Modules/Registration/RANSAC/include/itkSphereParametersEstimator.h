#ifndef itkSphereParametersEstimator_h
#define itkSphereParametersEstimator_h

#include "itkParametersEstimator.h"
#include "itkPoint.h"
#include "itkObjectFactory.h"
#include <vnl/vnl_least_squares_function.h>

namespace itk
{

/**
 * This class estimates the parameters of a (hyper)sphere.
 * A sphere is represented as: (1) (p-c)^T(p-c) = sum(p_i-c_i)^2 = r^2
 * where p in R^n is a point on the (hyper)sphere, c in R^n is the sphere's
 * center, and r its radius.
 *
 * @author: Ziv Yaniv (zivy@isis.georgetown.edu)
 *
 */
template <unsigned int dimension>
class ITK_TEMPLATE_EXPORT SphereParametersEstimator : public ParametersEstimator<Point<double, dimension>, double>
{
public:
  typedef SphereParametersEstimator                             Self;
  typedef ParametersEstimator<Point<double, dimension>, double> Superclass;
  typedef SmartPointer<Self>                                    Pointer;
  typedef SmartPointer<const Self>                              ConstPointer;

  itkTypeMacro(SphereParametersEstimator, ParametersEstimator);
  /** New method for creating an object using a factory. */
  itkNewMacro(Self)

    enum LeastSquaresType {
      ALGEBRAIC = 0,
      GEOMETRIC
    };

  /**
   * Compute the hyper(sphere) defined by the given data points.
   * @param data A vector containing k+1 kD points.
   * @param parameters This vector is cleared and then filled with the computed
   *                   parameter values. The parameter values of the
   *                   hyper(sphere) passing through these points [c_0,...,c_k,r].
   *                   If the vector contains less than k+1 points or the points
   *                   are linearly dependent (e.g. four coplanar points in the
   *                   3D case) then the resulting parameters vector is empty
   *                   (size = 0).
   */
  virtual void
  Estimate(std::vector<Point<double, dimension> *> & data, std::vector<double> & parameters);
  virtual void
  Estimate(std::vector<Point<double, dimension>> & data, std::vector<double> & parameters);

  /**
   * Compute a least squares estimate of the (hyper) sphere defined by the given
   * points. This may be either an algebraic or geomtric least squares
   * estimate depending on the LeastSquaresType settings.
   * @param data The sphere should minimize the least squares error to these
   *             points.
   * @param parameters This vector is cleared and then filled with the estimated
   *                   parameter values. The parameters of the hyper(sphere)
   *                   passing through these points [c_0,...,c_k,r]. If the
   *                   vector contains less than k+1 points or the points are
   *                   linearly dependent (e.g. all points are coplanar in the
   *                   3D case) then the resulting parameters vector is empty
   *                   (size = 0).
   */
  virtual void
  LeastSquaresEstimate(std::vector<Point<double, dimension> *> & data, std::vector<double> & parameters);
  virtual void
  LeastSquaresEstimate(std::vector<Point<double, dimension>> & data, std::vector<double> & parameters);

  /**
   * Return true if the distance between the (hyper)sphere and the given point
   * is smaller than 'delta' (see SetDelta()).
   *
   * @param parameters The sphere parameters [c_0,...,c_k,r].
   * @param data Check that the Euclidean distance between this point and the
   *             sphere is smaller than 'delta'.
   */
  virtual bool
  Agree(std::vector<double> & parameters, Point<double, dimension> & data);

  /**
   * Change the type of least squares solution.
   * @param lsType When the leastSquaresEstimate() method is called it computes
   *               an algebraic or geometric fit. This flag tells it which one.
   */
  void
  SetLeastSquaresType(LeastSquaresType lsType);

  /**
   * Compute a least squares estimate of the (hyper)sphere defined by the given
   * points.
   * This implementation is of an algebraic least squares error:
   * min||Ax-b||, where A = [-2p_0,-2p_1,...,-2p_{k-1},1], x = [c_0,c_1,...,c_{k-1},d], b = [-p_0^2 - p_1^2 - ...
   * -p_{k-1}^2]
   *
   * @param data The (hyper)sphere should minimize the algebraic least squares
   *             error to these points.
   * @param parameters This vector is cleared and then filled with the estimated
   *                   parameter values. The parameters of the hyper(sphere)
   *                   passing through these points [c_0,...,c_k,r].
   *                   If the vector contains less than k+1 points or the points
   *                   are linearly dependent (e.g. all points are coplanar in
   *                   the 3D case) then the resulting parameters vector is
   *                   empty (size = 0).
   */
  void
  AlgebraicLeastSquaresEstimate(std::vector<Point<double, dimension> *> & data, std::vector<double> & parameters);

  /**
   * Compute a least squares estimate of the circle defined by the given points.
   * This implementation is of a geometric least squares error:
   *                     min (sum (r - sqrt((p_0-c_0)^2 + (p_1-c_1)^2 +...+ (p_{k-1}-c_{k-1})^2))^2
   *
   * @param data The (hyper)sphere should minimize the geometric least squares error to these points.
   * @param initialParameters This vector contains the initial parameter values for nonlinear estimation.
   * @param finalParameters This vector is cleared and then filled with the computed sphere parameters
   * [centerX,centerY,centerZ,r].
   */
  void
  GeometricLeastSquaresEstimate(std::vector<Point<double, dimension> *> & data,
                                std::vector<double> &                     initialParameters,
                                std::vector<double> &                     finalParameters);

  /**
   * Set parameter which defines a threshold for a point to be considered on the
   * sphere.
   * @param delta A point is on the (hyper)sphere if its distance from the
   *              (hyper)sphere is less than 'delta'.
   */
  void
  SetDelta(double delta);
  double
  GetDelta();

  /**
   * Get the distances between the given (hyper)sphere and the given points.
   * These are pushed at the end of the 'distances' vector.
   *
   * @param parameters The (hyper)sphere parameters [c,r].
   * @param data The points whose distance from the (hyper)sphere we want.
   * @param distances The computed distances. If the vector is empty then the
   *                  point and distance indexes match,
   *                  otherwise there is an offset due to the original number
   *                  of entries previously found in the vector.
   * @param min Minimal distance.
   * @param max Maximal distance.
   * @param mean Mean distance.
   */
  static void
  GetDistanceStatistics(std::vector<double> &                   parameters,
                        std::vector<Point<double, dimension>> & data,
                        std::vector<double> &                   distances,
                        double &                                min,
                        double &                                max,
                        double &                                mean);

protected:
  SphereParametersEstimator();
  ~SphereParametersEstimator();

private:
  SphereParametersEstimator(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  enum
  {
    CIRCLE = 2,
    SPHERE = 3
  };
  double           delta;
  LeastSquaresType lsType; // algebraic or geometric least squares

  inline void
  Estimate2D(std::vector<Point<double, dimension> *> & data, std::vector<double> & parameters);
  inline void
  Estimate3D(std::vector<Point<double, dimension> *> & data, std::vector<double> & parameters);
  inline void
  EstimateND(std::vector<Point<double, dimension> *> & data, std::vector<double> & parameters);

  class SumSquaresSpherePointsDistanceFunction : public vnl_least_squares_function
  {
  public:
    SumSquaresSpherePointsDistanceFunction(std::vector<Point<double, dimension> *> * data);
    virtual void
    f(const vnl_vector<double> & x, vnl_vector<double> & fx);
    virtual void
    gradf(const vnl_vector<double> & x, vnl_matrix<double> & jacobian);

  private:
    std::vector<Point<double, dimension> *> * data;
  };
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSphereParametersEstimator.txx" //the implementation is in this file
#endif

#endif //_SPHERE_PARAM_ESTIMATOR_H_
