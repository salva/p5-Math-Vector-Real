#!/usr/bin/perl

use Test::More tests => 13;

use Math::Vector::Real;

my $u = V(1, 0, 0);
my $v = V(0, 1, 0);
my $w = V(0, 0, 1);

is (abs($_), 1) for ($u, $v, $w);
is (abs($u + $w), sqrt(2));
ok (cos(atan2($u, $v)) < 1e-4);
ok ($u + $v == [1, 1, 0]);
ok ($u + $w != [1, 1, 1]);
ok ($u - $v == [1, -1, 0]);
ok (-$v - $w * 2 == [0, -1, -2]);
ok (-2 * $v - $w == [0, -2, -1]);
is ($u * $v, 0);
is (($u + $v) * $v, 1);
ok ($u x $v == $w);

