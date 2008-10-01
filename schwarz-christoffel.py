#!/Library/Frameworks/Python.framework/Versions/Current/bin/python2.5
# coding=utf-8

# Python program to produce Figure 2 in PlanetMath encyclopedia entry:
# http://planetmath.org/encyclopedia/SchwarzChristoffelTransformationCircularVersion.html
#
# Both the PyX and SciPy packages, along with Python 2.x, 
# are required to run this program.
#
# Author: Steve Cheng <steve@gold-saucer.org>

from numpy.core import pi, sum, array, exp, log, cumsum, zeros, inner
from numpy.lib import linspace, real, angle
from scipy.special.orthogonal import p_roots
from pyx import canvas, path, unit, deco
import optparse

# Defines the geometric figures available
figures = {
  'star': [ 8+0j, 6-5j, 0-6j, 4-10j, 3-16j, 
            8-13j, 13-16j, 12-10j, 16-6j, 10-5j ],
  'triangle': [ 1.0, 1.0j, -1.0],
  'atry': [ 10.0, 10.0j, -10.0, -10.0j] }

# Parse command-line options to determine what figure to display
opt_parser = optparse.OptionParser()
opt_parser.add_option("-f", "--figure", type="choice", 
                  choices=figures.keys(),
                  dest="figure", default="triangle", 
                  help="which figure to display", 
                  metavar="FIG")
opt_parser.add_option("-c", "--curve",
                  dest="curved_contours", action="store_true", default=False,
                  help="use curve interpolation to draw contours")
opt_parser.add_option("-s", "--shape",
                  dest="shape", type="float", default=0.4,
                  help="shape parameter for curve interpolation")
opt_parser.add_option("-M", "--radius-grid-size",
               dest="M",
               action="store", type="int", default=18, metavar="p",
               help="number of discretization points along ray from origin")
opt_parser.add_option("-N", "--angle-grid-size",
               dest="N",
               action="store", type="int", default=20, metavar="p",
               help="multiplier for number of discretization points along circle")
opt_parser.add_option("-d", "--domain",
               dest="show_domain",
               action="store_true", default=False,
               help="show the domain contours instead of the image figure")
opt_parser.add_option("-r", "--rays",
               dest="show_rays", action="store_true", default=False,
               help="show the direction of propagation of conformal mapping")
opt_parser.add_option("-R", "--no-rays",
             dest="show_rays", action="store_false", 
             help="do not show the direction of propagation of conformal mapping")
opt_parser.add_option("-w", "--wavefronts", action="store_true",
               dest="show_wavefronts", default=True,
               help="show the \"wavefront\" contours")
opt_parser.add_option("-W", "--no-wavefronts",
               dest="show_wavefronts", action="store_false",
               help="do not show the \"wavefront\" contours")
opt_parser.add_option("-o", "--output",
               dest="output_file",
               action="store", default="schwarz-christoffel.eps", metavar="FILE",
               help="output file name for produced figure")
(opts, args) = opt_parser.parse_args()

# Performs 32-point Gauss quadrature
gauss32_abscissa, gauss32_weights = p_roots(32)
gauss32_abscissa = (real(gauss32_abscissa)+1.0)/2.0
gauss32_weights /= 2.0
def gauss_quad32(func, args):
  return sum( gauss32_weights * func(gauss32_abscissa, *args) )

# Partially determines the coefficients for the Schwarz-Christoffel
# formula.  The angles $\alpha_k \pi$ can be determined exactly.
# But the points $z_k$ probably requires numerical root-finding;
# we do not do this, but simply place the $z_k$ equally spaced
# on the unit circle.
#   a = $z_k$,      b = $\alpha_k - 1$
def schwarz_christoffel_coeff(points):
  a = exp(2j*pi*linspace(0, 1, len(points), endpoint=False))
  a.shape = (1, -1)

  p = [points[-1]] + points + points[0:1]
  b = array([ (angle(  (p[k-1]-p[k])/(p[k+1]-p[k]) )/pi)%2.0 - 1.0
              for k in xrange(1, len(p)-1) ])
  b.shape = (1,-1)

  return (a,b)

# The integrand for the Schwarz-Christoffel formula.
# Note that we skip the computation of $\log i z_k$ as discussed
# in the PlanetMath article, since it amounts to a multiplicative
# constant on the transformation $f$ anyway.
def schwarz_christoffel_integrand(t, a, b, z, dz):
  return dz*exp(inner(log( (z+dz*t.reshape(-1,1)-a)/(a*1j) ), b))

# Use Catmull-Rom interpolation to draw a smooth Bézier curve
# through the given points Z.  The shape parameter controls the degree
# of curving.  The points Z are assumed to form a loop (a closed curve)
# if loop is set to True.
def interpolated_path(Z, shape=1.0, loop=True):
  shape /= 6.0

  I = range(0, len(Z)-2)
  if loop:
    I += [-1]

  segments = [ ( Z[i] + (Z[i+1]-Z[i-1])*shape,
                 Z[i+1] - (Z[i+2]-Z[i])*shape,
                 Z[i+1] )  
               for i in I ]
  
  pa = path.path( path.moveto(Z[0].real, Z[0].imag),
                  path.multicurveto_pt(
                    [ (unit.topt(W[0].real), unit.topt(W[0].imag),
                       unit.topt(W[1].real), unit.topt(W[1].imag),
                       unit.topt(W[2].real), unit.topt(W[2].imag) ) 
                      for W in segments ]))
  if loop:
    pa.append(path.closepath())

  return pa

# Connects the given points Z by straight line segments.
# Assumes that the points Z form a closed loop if loop is set to True.
def polygonal_path(Z, loop=True):
  pa = path.path( path.moveto(Z[0].real, Z[0].imag),
                  path.multilineto_pt(
                     [ ( unit.topt(z.real), unit.topt(z.imag) ) 
                       for z in Z[1:] ] ))
  if loop:
    pa.append(path.closepath())

  return pa


# Get transformation coefficients, and print them out
figure = figures[opts.figure]
a, b = schwarz_christoffel_coeff(figure)
print a
print b+1.0

# Set up discretization points for the grid
M = opts.M
N = opts.N * len(figure)
R = 1.0 - 2**linspace(0, -M, M+1, endpoint=True)
Theta = linspace(0, 2*pi, N, endpoint=False)

if not opts.show_domain:
  # Compute f(z) over grid
  W = zeros(shape=(M, N), dtype=complex)
  for v in xrange(0, N):
    Z = R * exp(1j*Theta[v])
    for u in xrange(0, M):
      W[u,v] = gauss_quad32(schwarz_christoffel_integrand, 
                          (a, b, Z[u], Z[u+1]-Z[u]))
  W = cumsum(W, axis=0)
else:
  # Domain contours are just concentric circles
  W = 36.0 * R[1:].reshape(-1,1) * exp(1j*Theta).reshape(1,-1)

# Start vector drawing with PyX
unit.set(uscale=0.075)
cvs = canvas.canvas()

if opts.show_wavefronts:
  if opts.curved_contours:
    for u in xrange(0, M-1):
      cvs.stroke(interpolated_path(W[u,:], shape=opts.shape))
    cvs.stroke(polygonal_path(W[M-1,:]))
  else:
    for u in xrange(0, M):
      cvs.stroke(polygonal_path(W[u,:]))

if opts.show_rays:
  if opts.curved_contours:
    for v in xrange(0, N):
      U = [ 0j ] + [ w for w in W[:,v] ]
      cvs.stroke(interpolated_path(U, shape=opts.shape, loop=False))
  else:
    for v in xrange(0, N):
      U = [ 0j ] + [ w for w in W[:,v] ]
      cvs.stroke(polygonal_path(U, loop=False))

cvs.writeEPSfile(opts.output_file)



