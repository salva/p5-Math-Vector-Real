#!/usr/bin/perl

use strict;

use Test::More tests => 82;

use Math::Vector::Real::Test qw(eq_vector eq_vector_norm);

use Math::Trig;
use Math::Vector::Real;

my ($a,$b,$c,$d) = map {rand} 1..4;
my ($e,$f,$g,$h) = map {rand} 1..4;
my @randoms = map {rand} 1..4;

sub random_vector {
    my $dim = shift;
    V(map rand(2) - 1, 1..$dim)
}

my $q1 = random_vector(4);
my $q2 = random_vector(4);
my $q3 = random_vector(4);

my $q = V(1,2,3,4);
my $p = $q->clone;

my $q1c = ~$q1;
my $q1i = 1/$q1;

eq_vector($q1 x $q1c, [$q1->norm2, 0, 0, 0], "multiplying with a conjugate gives the squared norm");
eq_vector($q1 x $q1i, [1, 0, 0, 0], "multiplying with inverse gives unit");
eq_vector($q1 x ($q2 + $q3), $q1 x $q2 + $q1 x $q3,
          "quaternion multiplication is left-distributive with the addition");
eq_vector(($q1 + $q2) x $q3, $q1 x $q3 + $q2 x $q3,
          "quaternion multiplication is right-distributive with the addition");
eq_vector($q1 x ($q2 - $q3), $q1 x $q2 - $q1 x $q3,
          "quaternion multiplication is left-distributive with the subtraction");
eq_vector(($q1 - $q2) x $q3, $q1 x $q3 - $q2 x $q3,
          "quaternion multiplication is right-distributive with the subtraction");

TODO: {
    local $TODO = 'perl handling of overloaded "x" operator is broken';
    eq_vector($q1 x ($q2 x $q3), ($q1 x $q2) x $q3,
              "quaternion multiplication is associative");
};

eq_vector($q x [1, 3, 7, 11], [ -70, 10, 0, 20], "mul $q x {1,3,7,11}");

eq_vector_norm($q->versor, 1, "versor");

eq_vector(~$q->versor, 1/$q->versor, "the inverse of a unit quaternion is its conjugate");

my $nine = V(-9, 0, 0, 0);
my $nine_nine = $nine x $nine;
eq_vector($nine_nine, [81, 0, 0, 0], "real multiplied by itself");



eq_vector($q x [1,2,3], [-20, 2, 0, 4], "quaternion x 3d");
eq_vector([1,2,3] x $q, [-20, 0, 4, 2], "3d x quaternion");
eq_vector([0,1,2,3] x $q, [-20, 0, 4, 2], "explicit 3d x quaternion");
