#!/usr/bin/env python

from pyx import *
from math import cos, sin, pi, degrees, atan2

unit.set(uscale=2.0)

#
# Figure 3
#

cvs = canvas.canvas()
cvs.stroke(path.circle(0,0,1))

M = 3
N = 16
Theta = [ 2*pi*i/N for i in xrange(0, N) ]
R = [ 1.0 - 2.0**(-j) for j in xrange(0, M+1) ]
# Rays from origin
for theta in Theta:
  for j in xrange(0, M):
    cvs.stroke( path.line(R[j]*cos(theta),R[j]*sin(theta), 
                          R[j+1]*cos(theta), R[j+1]*sin(theta)),
                [ deco.earrow ])

cvs.writeEPSfile("parameter.eps")

#
# Figure 1
#

cvs = canvas.canvas()

N = 5
Theta = [ 2*pi*i/N for i in xrange(0, N) ]

# Half-disc around labelled point
theta = Theta[1]
x, y = (cos(theta), sin(theta))
cvs.fill( path.path( path.arc(x, y, 0.5, degrees(theta+pi/2), 
                                           degrees(theta-pi/2)),
                       path.closepath() ),
          [ color.grey(0.8) ])

# Circle with points
cvs.stroke(path.circle(0,0,1))
for theta in Theta:
  cvs.fill(path.circle(cos(theta), sin(theta), 0.02))

# Labelled point
theta = Theta[1]
cvs.text( 1.1*x, 1.1*y, r"$z_k$",
          [ text.halign.boxcenter, text.valign.middle ] )

# Tangent line
cvs.stroke( path.line( x + cos(theta+pi/2), 
                       y + sin(theta+pi/2),
                       x - cos(theta+pi/2),
                       y - sin(theta+pi/2)),
            [ style.linewidth.thin, style.linestyle.dashed ] )

cvs1 = canvas.canvas()
cvs1.insert(cvs, [ trafo.translate(2.0, 0.0) ])
cvs = canvas.canvas()

Z = [ (-12-6j)/10.0,(-3-5j)/10.0, 0j, (-4+6j)/10.0, (-11+7j)/10.0 ]

def angle_deg(z):
  return degrees(atan2(z.imag, z.real))

# Half-disc
cvs.fill(path.path( 
           path.arc( Z[2].real, Z[2].imag, 0.25,
                     angle_deg(Z[3]-Z[2]), angle_deg(Z[1]-Z[2]) ), 
         path.lineto(Z[2].real, Z[2].imag),
         path.lineto(Z[3].real, Z[3].imag)),
        [ color.grey(0.8) ] )
cvs.text(Z[2].real - 0.6, Z[2].imag, r"$\alpha_k \pi$", [ text.valign.middle ])

# Polygon with points
pa = path.path( path.moveto(Z[0].real, Z[0].imag),
                *[ path.lineto(z.real, z.imag) for z in Z] )
pa.append(path.closepath())
cvs.stroke(pa)

for z in Z:
  cvs.fill(path.circle(z.real, z.imag, 0.02))

# Label corner point
cvs.text( Z[2].real + 0.05, Z[2].imag, r"$f(z_k)$", [ text.valign.middle ])

cvs1.insert(cvs)

# Mapping arrow
cvs1.stroke ( path.path( path.arc( 0.5, 0.5, 0.7, 45, 160) ),
              [ deco.earrow, style.linestyle.dotted ] )

cvs1.text( 0.5, 1.0, r"$f$")

cvs1.writeEPSfile("clamp.eps")


