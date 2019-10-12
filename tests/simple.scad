// testing
module test_implicit_block(y, x=1) {
  circle(r=1);
  cube([1,2,3]);
}

module test_block(y, x=1) {
  difference(x) {
    circle(r=1);
    cube([1,2,3]);
  }
}

test_block(2,3);
