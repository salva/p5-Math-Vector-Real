#!/usr/bin/perl

use strict;
use warnings;

use 5.010;

use Data::Dumper;
use GD;

use Math::Vector::Real;
use Math::Vector::Real::Random;
use Math::Vector::Real::Algorithms2d qw(convex_hull);

my $n = shift || 20;

#my @p = map Math::Vector::Real->random_normal(2, 0.3), 1..$n;
my @p = map Math::Vector::Real->random_in_sphere(2), 1..$n;

my @p = map { my $u = $_->clone; $u->[1] *= .5; ($_, $u) } @p;

my @r = convex_hull(@p);

# print Dumper \@r;

my $width = 1024;
my $im = GD::Image->new($width, $width);

sub scl { map $width * ($_ / 4 + .5), @_ }


my $white = $im->colorAllocate(255, 255, 255);
my $blue = $im->colorAllocate(100, 100, 255);
my $dark_red = $im->colorAllocate(200, 0, 0);
my $orange = $im->colorAllocate(255, 200, 0);

for (@p) {
    $im->filledEllipse(scl(@$_), 5, 5, $blue);
}

for (@r) {
    $im->filledEllipse(scl(@$_), 8, 8, $orange);
}

open my $fh, '>output.png';
print $fh $im->png;
close $fh;
