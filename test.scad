use <x.scad>
include <y.scad>
// test comment
translate(0, 0, 0) translate(1,1,1) translate(1,1,1) translate(1,1,1) translate(1,1,1) cube([1, 2, 3]);
translate(0, 0, 0) translate(1,1,1) cube([1.00001, 2, 3]);

x = [0.01,2.002,3];

y = x[0];

a    = false;
b =     true;