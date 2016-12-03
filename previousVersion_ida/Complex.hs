-- Math needed for complex numbers
module Complex where

-- square function for complex numbers as tuples
square (r, im) = (r^2 - im^2, 2*r*im)

-- add function for complex numbers 
-- real part on the left, imaginary on the right 
-- (a + bi) + (c + di) = (a+c) + (b+d)i
add (r1, im1) (r2, im2) = ((r1 + r2), (im1 + im2))

-- absolute value, magnitue of complex number
c_abs (r, im) = sqrt (r^2 + im^2)
