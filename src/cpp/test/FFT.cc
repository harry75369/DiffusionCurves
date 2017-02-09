#include <iostream>
#include <vector>
#include <cmath>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "../lib/Logger.hpp"

using namespace std;

/*****************************************************************************/

/**
 * Concrete Numerical Type - Complex Number
 *
 * Only limited support for complex numbers.
 * More features are planned to be added in the future.
 */
struct Complex {

  /*
   * Type definitions
   */
  typedef double ElemType;

  /*
   * Constructors
   */
  Complex() : m_re(0), m_im(0) {}

  Complex(ElemType re) : m_re(re), m_im(0) {}

  Complex(ElemType re, ElemType im) : m_re(re), m_im(im) {}

  /*
   * Accessors
   */
  const ElemType& re() const
  {
    return m_re;
  }

  const ElemType& im() const
  {
    return m_im;
  }

  ElemType modulus() const
  {
    return sqrt(m_re*m_re+m_im*m_im);
  }

  Complex conjugate() const
  {
    return Complex(m_re, -m_im);
  }

  /*
   * Operations
   */
  Complex operator+(const ElemType &e) const
  {
    return Complex(m_re+e, m_im);
  }

  Complex operator+(const Complex &other) const
  {
    return Complex(m_re+other.m_re, m_im+other.m_im);
  }

  Complex operator-(const ElemType &e) const
  {
    return Complex(m_re-e, m_im);
  }

  Complex operator-(const Complex &other) const
  {
    return Complex(m_re-other.m_re, m_im-other.m_im);
  }

  Complex operator*(const ElemType &e) const
  {
    return Complex(m_re*e, m_im*e);
  }

  Complex operator*(const Complex &other) const
  {
    return Complex(m_re*other.m_re-m_im*other.m_im,
                   m_im*other.m_re+m_re*other.m_im);
  }

  Complex& operator+=(const ElemType &e)
  {
    m_re += e;
    return (*this);
  }

  Complex& operator+=(const Complex &other)
  {
    m_re += other.m_re;
    m_im += other.m_im;
    return (*this);
  }

  Complex& operator-=(const ElemType &e)
  {
    m_re -= e;
    return (*this);
  }

  Complex& operator-=(const Complex &other)
  {
    m_re -= other.m_re;
    m_im -= other.m_im;
    return (*this);
  }

  Complex& operator*=(const ElemType &e)
  {
    m_re *= e;
    m_im *= e;
    return (*this);
  }

  Complex& operator*=(const Complex &other)
  {
    ElemType re = m_re*other.m_re-m_im*other.m_im;
    ElemType im = m_im*other.m_re+m_re*other.m_im;
    m_re = re;
    m_im = im;
    return (*this);
  }

  Complex operator=(const ElemType &e)
  {
    m_re = e;
    m_im = 0;
    return (*this);
  }

  Complex& operator=(const Complex &other)
  {
    m_re = other.m_re;
    m_im = other.m_im;
    return (*this);
  }

  bool operator==(const Complex &other) const
  {
    static ElemType epsilon = 1e-9;
    if ( abs(m_re-other.m_re) < epsilon
      && abs(m_im-other.m_im) < epsilon )
      return true;
    return false;
  }

  bool operator!=(const Complex &other) const
  {
    return !((*this)==other);
  }

  /*
   * Static utility functions
   */
  static Complex random()
  {
    return Complex(::rand()%100, ::rand()%100);
  }

private:
  ElemType m_re;
  ElemType m_im;
};

ostream& operator<<(ostream &os, const Complex &c)
{
  os << c.re();
  if ( c.im() < 0 ) os << c.im() << "i";
  else if ( c.im() > 0 ) os << "+" << c.im() << "i";
  return os;
}

/*****************************************************************************/

/**
 * Template Vector Type
 *
 * Only limited support for vectors.
 * More features are planned to be added in the future.
 */
template <class T>
struct Vector : public vector<T> {

  /*
   * Type definitions
   */
  typedef vector<T> Base;
  typedef T         ElemType;

  /*
   * Constructors
   */
  Vector() { dim[0] = 0; }

  Vector(const Vector &other) : Base(other) { dim[0] = other.dim[0]; }

  /*
   * Modifiers
   */
  void reset(size_t n)
  {
    dim[0] = n;
    Base::clear();
    Base::resize(n);
  }

  /*
   * Accessors
   */
  vector<size_t> dims() const
  {
    return {dim[0]};
  }

  ElemType& operator()(size_t i)
  {
    return (*this)[i];
  }

  const ElemType& operator()(size_t i) const
  {
    return (*this)[i];
  }

  /*
   * Operations
   */
  Vector& operator+=(const ElemType &e)
  {
    ASSERT(Base::size()==dim[0]);

    for ( size_t i=0; i < dim[0]; i++ )
      (*this)(i) += e;

    return (*this);
  }

  Vector& operator*=(const ElemType &e)
  {
    ASSERT(Base::size()==dim[0]);

    for ( size_t i=0; i < dim[0]; i++ )
      (*this)(i) *= e;

    return (*this);
  }

  Vector& operator=(const Vector &vec)
  {
    Base::assign(vec.begin(), vec.end());
    dim[0] = vec.dim[0];
    return (*this);
  }

  bool operator==(const Vector &vec) const
  {
    if ( Base::size() != vec.size() ) return false;
    if ( dim[0] != vec.dim[0] ) return false;
    for ( size_t i=0; i < dim[0]; ++i )
      if ( (*this)(i) != vec(i) )
        return false;
    return true;
  }

  bool operator!=(const Vector &vec) const
  {
    return !((*this)==vec);
  }

  /*
   * Utility functions
   */
  void randomize(size_t n)
  {
    reset(n);
    for ( size_t i=0; i < n; ++i )
      (*this)(i) = ElemType::random();
  }

public:
  size_t dim[1];
};

template <class T>
ostream& operator<<(ostream &os, const Vector<T> &v)
{
  if ( v.size() == 0 )
  {
    os << "[]" << endl;
    return os;
  }

  ASSERT(v.size()==v.dim[0]);

  os << "[";
  for ( size_t i=0; i < v.dim[0]; ++i )
  {
    os << "\t" << v(i);
    if ( i+1 < v.dim[0] )
      os << ";";
  }
  os << "]" << endl;

  return os;
}

typedef Vector<Complex> ComplexVector;

/*****************************************************************************/

/**
 * Template Matrix Type
 *
 * Only limited support for matrices.
 * More features are planned to be added in the future.
 */
template <class T>
struct Matrix : public vector<T> {

  /*
   * Type definitions
   */
  typedef vector<T> Base;
  typedef T         ElemType;

  /*
   * Constructors
   */
  Matrix() { dim[0] = dim[1] = 0;}

  Matrix(const Matrix &other) : Base(other) { dim[0] = other.dim[0]; dim[1] = other.dim[1]; }

  /*
   * Modifiers
   */
  void reset(size_t m, size_t n)
  {
    dim[0] = m;
    dim[1] = n;
    Base::clear();
    Base::resize(m*n);
  }

  /*
   * Accessors
   */
  vector<size_t> dims() const
  {
    return {dim[0], dim[1]};
  }

  ElemType& operator()(size_t i, size_t j)
  {
    return (*this)[i*dim[1]+j];
  }

  const ElemType& operator()(size_t i, size_t j) const
  {
    return (*this)[i*dim[1]+j];
  }

  /*
   * Operations
   */
  Matrix& operator+=(const ElemType &e)
  {
    ASSERT(Base::size()==dim[0]*dim[1]);

    for ( size_t i=0; i < dim[0]; ++i )
      for ( size_t j=0; j < dim[1]; ++j )
        (*this)(i, j) += e;

    return (*this);
  }

  Matrix& operator*=(const ElemType &e)
  {
    ASSERT(Base::size()==dim[0]*dim[1]);

    for ( size_t i=0; i < dim[0]; ++i )
      for ( size_t j=0; j < dim[1]; ++j )
        (*this)(i, j) *= e;

    return (*this);
  }

  Matrix& operator=(const Matrix &mat)
  {
    Base::assign(mat.begin(), mat.end());
    dim[0] = mat.dim[0];
    dim[1] = mat.dim[1];
    return (*this);
  }

  bool operator==(const Matrix &mat) const
  {
    if ( Base::size() != mat.size() ) return false;
    if ( dim[0] != mat.dim[0] ) return false;
    if ( dim[1] != mat.dim[1] ) return false;
    for ( size_t i=0; i < dim[0]; ++i )
      for ( size_t j=0; j < dim[1]; ++j )
        if ( (*this)(i, j) != mat(i, j) )
          return false;
    return true;
  }

  bool operator!=(const Matrix &mat) const
  {
    return !((*this)==mat);
  }

  /*
   * Utility functions
   */
  void randomize(size_t m, size_t n)
  {
    reset(m, n);
    for ( size_t i=0; i < m; ++i )
      for ( size_t j=0; j < n; ++j )
        (*this)(i, j) = ElemType::random();
  }

public:
  size_t dim[2];
};

template <class T>
ostream& operator<<(ostream &os, const Matrix<T> &m)
{
  if ( m.size() == 0 )
  {
    os << "[]" << endl;
    return os;
  }

  ASSERT(m.size()==m.dim[0]*m.dim[1]);

  os << "[";
  for ( size_t i=0; i < m.dim[0]; ++i )
  {
    for ( size_t j=0; j < m.dim[1]; ++j )
      os << "\t" << m(i, j);
    if ( i+1 < m.dim[0] )
      os << ";" << endl;
  }
  os << "]" << endl;

  return os;
}

/**
 * Concrete Matrix Type - Complex Matrix
 *
 * Adding utility functions specially for Complex Matrix.
 */
struct ComplexMatrix : public Matrix<Complex> {

  /*
   * Utility functions
   */
  void dftMatrixInPlace(size_t n)
  {
    reset(n, n);
    Complex one(1,0);
    Complex::ElemType theta(2*M_PI/n);
    Complex w(cos(theta), -sin(theta));
    Complex wi = one;

    for ( size_t i=0; i < dim[0]; ++i )
    {
      (*this)(i, 0) = one;
      for ( size_t j=1; j < dim[1]; ++j )
        (*this)(i, j) = (*this)(i, j-1) * wi;
      wi *= w;
    }
  }

  void conjugateInPlace()
  {
    for ( size_t i=0; i < dim[0]; ++i )
      for ( size_t j=0; j < dim[1]; ++j )
        (*this)(i, j) = (*this)(i, j).conjugate();
  }

  ComplexMatrix conjugate()
  {
    ComplexMatrix mat(*this);
    mat.conjugateInPlace();
    return mat;
  }

  static ComplexMatrix dftMatrix(size_t n)
  {
    ComplexMatrix mat;
    mat.dftMatrixInPlace(n);
    return mat;
  }

};

/*****************************************************************************/

/*
 * Actual Matrix-by-Vector Algorithms
 */

/**
 * Naive version
 *
 * Time Complexity: O(N^2)
 */
template <class T>
void naive_multiply(Vector<T> &y, const Matrix<T> &A, const Vector<T> &x)
{
  ASSERT(A.dim[1]==x.dim[0]);

  y.reset(A.dim[0]);
  for ( size_t i=0; i < A.dim[0]; ++i )
  {
    y(i) = T();
    for ( size_t j=0; j < A.dim[1]; ++j )
      y(i) += A(i, j) * x(j);
  }
}

/**
 * FFT version
 *
 * Time Complexity: O(N logN)
 */
template <class T>
void fft_multiply(Vector<T> &y, const Matrix<T> &A, const Vector<T> &x)
{
}

/**
 * FMM version
 *
 * Time Complexity: O(N)
 */
template <class T>
void fmm_multiply(Vector<T> &y, const Matrix<T> &A, const Vector<T> &x)
{
}

/*****************************************************************************/

int main(int argc, char *argv[])
{
  size_t size = 3;

  // Get size if provided
  if ( argc > 1 )
  {
    sscanf(argv[1], "%u", &size);
    INFO("Size = %u", size);
    if ( !size ) WARN("Size is zero! You have been warned!");
  }

  // Init random seed
  srand(time(0));

  // Init A and x
  ComplexVector x;
  ComplexMatrix A;
  x.randomize(size);
  A.dftMatrixInPlace(size);
  cout << "x = " << x << endl;
  cout << "A = " << A << endl;

  // Algorithm 1: Naive version
  ComplexVector y1;
  naive_multiply(y1, A, x);
  cout << "y1 = " << y1 << endl;

  // Algorithm 2: FFT version
  ComplexVector y2;
  fft_multiply(y2, A, x);
  cout << "y2 = " << y2 << endl;

  // Algorithm 3: FMM version
  ComplexVector y3;
  fmm_multiply(y3, A, x);
  cout << "y3 = " << y3 << endl;

  // Test Case 1: Identity after both DFT and inverse-DFT
  ComplexVector x1;
  naive_multiply(x1, A.conjugate(), y1);
  x1 *= (1./size);
  cout << "x1 = " << x1 << endl;
  if ( x == x1 ) INFO("Test Case 1: success!");
  else WARN("Test Case 1: failed!");

  // Test Case 2: Equivalence of Naive and FFT
  if ( y1 == y2 ) INFO("Test Case 2: success!");
  else WARN("Test Case 2: failed!");

  // TODO: Test Case 3: Equivalence of Naive and FMM within certain tolerance

  return 0;
}
